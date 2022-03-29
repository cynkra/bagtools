is_infix <- function(x) {
  startsWith(x, "%") & endsWith(x, "%")
}

#' Find all uses of a package's functions
#'
#' This will show false positives, but guarantees that we don't miss any instance.
#'
#' @param pkg A string. The name of the target package
#' @param path A string. The path to a file or the folder to explore
#'   By default explores the working directory.
#' @param recursive A boolean. Passed to `list.files()` if `path` is a directory
#' @param exclude A character vector of function names to dismiss.
#' @param include_namespaced Boolean
#'
#' @return Returns its input invisibly, called for side effects
#' @export
find_pkg_funs <- function(pkg, path = ".", recursive = TRUE, exclude = NULL, include_namespaced = TRUE
                          # , include_s3_generics = FALSE
                          ) {
  # fetch package functions and their origin
  imports <- getNamespaceImports(pkg)
  imports <- Map(function(fun, pkg) data.frame(fun, pkg), imports, names(imports))
  imports <- do.call(rbind, imports)
  row.names(imports) <- NULL
  exports <- data.frame(fun = getNamespaceExports(pkg))
  exports <- exports[!exports$fun %in% exclude, , drop = FALSE]
  pkg_funs <- merge(exports, imports, all.x = TRUE)

  # ns <- asNamespace(pkg)
  # s3_methods  <- Filter(function(x) isS3method(x, envir = ns), ls(ns))
  # s3_generics <- sub("^([^.]+)\\..*$", "\\1", s3_methods)
  # s3 <- data.frame(method = s3_methods, generic = s3_generics)
  # s3 <- subset(s3, !generic %in% ls(ns))


  # fetch parse data
  if(dir.exists(path)) {
    files <- list.files(path, full.names = TRUE, recursive = recursive, pattern = "\\.[rR]$")
  } else {
    if (!file.exists(path)) stop(sprintf("Invalid value for `path`, '%s' doesn't exist", path))
    files <- path
  }

  parse_data <- get_parse_data(files)
  i_namespaced <- which(parse_data$text %in% c("::", ":::")) + 1

  if (include_namespaced) {
    i_namespaced_pkg <- i_namespaced[which(parse_data$text[i_namespaced-2] == pkg)]
    parse_data_namespaced <- parse_data[i_namespaced_pkg,, drop = FALSE]
    if(nrow(parse_data_namespaced)) {
    parse_data_namespaced$text <- paste0(pkg, "::", parse_data_namespaced$text)
    parse_data_namespaced$message <- sprintf("Found `%s`", parse_data_namespaced$text)
    parse_data_namespaced$type <- "info"
    } else {
      parse_data_namespaced$text <- character()
      parse_data_namespaced$message <- character()
      parse_data_namespaced$type <- character()
    }
    parse_data_namespaced <- parse_data_namespaced[c("line1", "col1", "text", "file", "message", "type")]

  }

  if(length(i_namespaced)) parse_data <- parse_data[-i_namespaced,, drop = FALSE]
  parse_data <- parse_data[! parse_data$token %in% c("SYMBOL_SUB", "SYMBOL_PACKAGE"),, drop = FALSE]

  # merge datasets
  merged <- merge(parse_data, pkg_funs, by.x = "text", by.y = "fun")
  merged$message <- ifelse(
    is.na(merged$pkg),
    ifelse(
      is_infix(merged$text),
      sprintf("Found `%s`, do we want `library(%s, include.only = '%s')` ?", merged$text, pkg, merged$text),
      sprintf("Found `%s`, do we want `%s::%s` ?", merged$text, pkg, merged$text)
    ),
    ifelse(
      is_infix(merged$text),
      sprintf("Found `%s`, do we want `library(%s, include.only = '%s')` (or more directly `library(%s, include.only = '%s')`)?", merged$text, pkg, merged$text, merged$pkg, merged$text),
      sprintf("Found `%s`, do we want `%s::%s` (or more directly `%s::%s`) ?", merged$text, pkg, merged$text, merged$pkg, merged$text)
    )
  )

  merged <- merged[c("line1", "col1", "text", "file", "message")]
  if(nrow(merged)) merged$type <- "warning" else merged$type <- character()
  if (include_namespaced) {
    merged <- rbind(parse_data_namespaced, merged)
  }
  source_markers(
    "Functions that might come from",
    merged,
    no_row_message = sprintf("No potential function calls from {%s} were found in the code", pkg))
}

