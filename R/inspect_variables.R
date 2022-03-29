#' Inspect variables
#'
#' Inspect use in other scripts of variables found in a given script. By default
#' the given script is the active script and other scripts are all other scripts
#' found recursively in the working directory. The "i" icon is used when a variable
#' was defined first in a script then potentially used or modified, the "!" icon
#' is used when a variable is used or modified without being defined first, a sign
#' that it has been defined in another script.
#'
#' Note that the use of non standard evaluation creates false positives, if they
#' annoy you too much you can set them to `NULL` on top of the script. or call
#' `globalVariables("your_var")`. Both are steps toward getting a package that
#' passes CRAN checks without notes.
#'
#' Use of functions like `get()` or `assign()` will also make the results less reliable.
#'
#' @param path The path to fetch vinspected variable names from
#' @param scope A vector of files or directory to inspect
#' @param recursive Whether the directories given to `scope` should be inspected
#'   recursively
#' @param vars Subset of variables to consider
#' @param exclude variables to exclude, so common variable names such as `x` don't
#'   clutter the results.
#' @param only_first Whether to show anly the first variable occurrence in a script
#' @param focus "inputs", "outputs" or both (default)
#'
#' @return Returns the source markers silently, called mainly for side effects
#' @export
inspect_variables <- function(
  path = rstudioapi::documentPath(rstudioapi::getSourceEditorContext()$id),
  scope = ".",
  recursive = TRUE,
  vars = NULL,
  exclude = NULL, # c("x", "y", "i", "path", "file"),
  only_first = FALSE,
  focus = c("inputs", "outputs")) {
  focus <- match.arg(focus, several.ok = TRUE)
  scope <- normalizePath(flatten_paths(scope))
  path <- normalizePath(path)
  # fetch all names used in script outside of function calls and calls to `$` or `::`
  path_markers <- get_raw_markers(path, only_first)
  if (is.null(vars)) {
    vars <- path_markers$var
  } else {
    vars <- intersect(vars, path_markers$var)
  }
  vars <- setdiff(vars, c(exclude, eval(quote(globalVariables()), .GlobalEnv)))
  # remove names that are accessible from above .GlobalEnv (i.e. attached by packages)
  vars <- Filter(function(nm) !exists(nm, parent.env(.GlobalEnv)), vars)
  if(!length(vars)) {
    message("No variable to inspect")
    return(invisible(NULL))
  }
  path_markers <- path_markers[path_markers$var %in% vars,, drop = FALSE]
  # get names from scripts ins scope
  scope_markers <- lapply(scope, get_raw_markers, only_first)
  scope_markers <- do.call(rbind, scope_markers)
  scope_markers <- scope_markers[scope_markers$file != path & scope_markers$var %in% path_markers$var,, drop = FALSE ]

  if(!"inputs" %in% focus) {
    # we focus on outputs only so we keep definitions in path_markers and use/modification in scope_markers
    path_markers <- path_markers[path_markers$file_action %in% c("define", "modify"),, drop = FALSE]
  }
  if(!"outputs" %in% focus) {
    path_markers <- path_markers[path_markers$file_action %in% c("use", "modify"),, drop = FALSE]
    scope_markers <- scope_markers[scope_markers$file_action %in% c("define", "modify"),, drop = FALSE]
  }
  markers <- rbind(path_markers, scope_markers)

  source_markers("Variables", markers, top = path)
}

fetch_names_from_script <- function(path) {
  code <- parse(path)
  code <- as.call(c(quote(`{`), as.list(code)))
  code <- remove_function_defs(code)
  nms <- all.names(code)
  nms <- Filter(function(nm) !exists(nm, parent.env(.GlobalEnv)), nms)
  nms
}

remove_function_defs <- function(call) {
  if(!is.call(call)) return(call)
  if(length(call) == 3 &&
    identical(call[[1]], quote(`<-`)) &&
    is.call(call[[3]]) &&
    identical(call[[c(3,1)]], quote(`function`))
  ) {
    return(call[[2]])
  }
  as.call(lapply(call, remove_function_defs))
}

# get_raw_markers <- function(path, only_first) {
#   data <- getParseData(parse(path))
#   # remove function calls
#   id_remove <- unique(data$parent[data$token == "FUNCTION"])
#   while (length(id_remove)) {
#     data <- data[!data$id %in% id_remove,, drop = FALSE]
#     id_remove <- data$id[data$parent %in% id_remove]
#   }
#   data <- data[data$text != "", , drop = FALSE]
#   # remove dollar calls and namespaced calls
#   id_remove <- unique(data$parent[data$token %in% c("NS_GET", "NS_GET_INT", "'$'")])
#   data <- data[!data$parent %in% id_remove, , drop = FALSE]
#   data <- data[data$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), c("line1", "col1", "text"), drop = FALSE]
#   data <- setNames(data, c("line", "column", "message"))
#   if(only_first) data <- data[!duplicated(data$message),]
#   if (nrow(data)) data$file <- path
#   data
# }

# FIXME: not DRY, use source_markers
get_raw_markers <- function(path, only_first) {
  data <- getParseData(parse(path))
  if(any(data$token == "RIGHT_ASSIGN")) {
    #stop("The right assignment is not yet supported by `get_defined_variables_markers()`")
  }
  #
  data <- data[data$parent >= 0,, drop = FALSE]
  data_assign <- data
  assign_parents <- data_assign$parent[data$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN", "FOR")]

  # find parent assignment for every row
  ids <- data$id

  repeat {
    #print(ids[1:20])
    new_ids <- ifelse(ids %in% c(assign_parents, 0), ids, data$parent[match(ids, data$id)])
    if (all(new_ids == ids)) break
    ids <- new_ids
  }
  #}
  data_assign$assignment_id <- ids
  data_assign <- data_assign[data_assign$assignment_id != 0,, drop = FALSE]
  data_assign$assign_fun <- ave(data_assign$token, data_assign$assignment_id, FUN = function(x) {
    "SYMBOL_FUNCTION_CALL" %in% x && match("SYMBOL_FUNCTION_CALL", x) < match("SYMBOL", x)
    })
  data_assign$for_loop <- ave(data_assign$token, data_assign$assignment_id, FUN = function(x) x[[2]] == "FOR")

  data_assign <- data_assign[data_assign$token == "SYMBOL",, drop = FALSE]
  # check if assignment or variable modification
  data_assign$action <-
    data_assign$for_loop == "TRUE" |
    data_assign$assign_fun == "FALSE" &
    ave(data_assign$text, data_assign$assignment_id, FUN = function(x) sum(x == x[[1]]) == 1) == "TRUE"
  data_assign$action <- ifelse(data_assign$action, "define", "modify")
  data_assign <- data_assign[!duplicated(data_assign$assignment_id),, drop = FALSE]

  # remove bodies of function definitions
  id_remove <- unique(data$parent[data$token == "FUNCTION"])
  while (length(id_remove)) {
    data <- data[!data$id %in% id_remove,, drop = FALSE]
    id_remove <- data$id[data$parent %in% id_remove]
  }
  data <- data[data$text != "", , drop = FALSE]
  # remove dollar calls and namespaced calls
  id_remove <- unique(data$parent[data$token %in% c("NS_GET", "NS_GET_INT", "'$'")])
  data <- data[!data$parent %in% id_remove, , drop = FALSE]

  #
  data_symbols <- merge(data, data_assign, all.x = TRUE)
  data_symbols$action[is.na(data_symbols$action)] <- "use"
  data_symbols <- data_symbols[data_symbols$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"),, drop = FALSE]
  data_symbols$file_action <- ave(data_symbols$action, data_symbols$text, FUN = function(x) {
    if(x[[1]] == "define") "define" else {
      if(any(c("modify", "define") %in% x)) "modify"  else "use"
    }
  })
  data_symbols$type <- c(define = "info", modify = "error", use = "warning")[
    data_symbols$file_action
  ]

  if (nrow(data_symbols)) {
    data_symbols$message <- paste0(data_symbols$action, " `", data_symbols$text, "`")
  } else {
    data_symbols$message <- character(0)
  }

  markers <- data_symbols[c("line1", "col1", "message", "type", "text", "file_action")]

  markers <- setNames(markers, c("line", "column", "message", "type", "var", "file_action"))
  if(only_first) markers <- markers[!duplicated(markers$var),, drop = FALSE]
  if (nrow(markers)) markers$file <- path
  markers
}

