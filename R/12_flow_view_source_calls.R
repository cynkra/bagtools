
extract_strings <- function(call) {
  rec <- function(call) {
    if(!is.call(call)) return(call) else lapply(call, rec)
  }
  unlist(Filter(is.character,unlist(rec(call))))
}

#' Draw diagram of source dependencies
#'
#' Assuming a project where file source each other, draw their dependency diagram.
#' This evaluates the `file` argument of `source` in the global environment,
#' when this fails, as it might with constructs like `for (file in files) source(file)`
#' the unevaluated argument is printed instead between backticks. Obviously this messes
#' up the relationships in the graph, an warning is thus issued. In a case like `source(file.path(my_dir, "foo.R")`
#' defining `my_dir` will be enough to solve the issue.
#' In other cases you might need to attach or/and install a package.
#'
#' @param path Paths to scripts or folders containing scripts
#'   By default explores the working directory.
#' @param recursive Passed to `list.files()` when `paths` contains directories
#' @param smart Whether to parse complex source calls for strings that look like script and
#'   match those to files found in `paths`
#'
#' @return Returns invisibly a data, called for side effects.
#' @export
flow_view_source_calls <- function(paths = ".", recursive = TRUE, basename = TRUE, extension = FALSE, smart = TRUE, out = NULL) {
  paths <- flatten_paths(paths)

  svg <- is.null(out) || endsWith(out, ".html") || endsWith(out, ".html")
  env <- parent.frame()
  fetch_source <- function(file) {
    code <- as.list(parse(file))
    source_calls <- Filter(function(x) is.call(x) && identical(x[[1]], quote(source)), code)
    if (!length(source_calls)) return(NULL)
    paths <- sapply(source_calls, function(x) {
      tryCatch({
        child <- eval(x[[2]], env)
        if(basename) child <- basename(child)
        if(!extension) child <- sub("\\.[rR]$", "", child)
        child
      },
      error = function(e) {
        if (smart) {
          strings <- extract_strings(x)
          file <- grep("\\.[rR]$", strings, value = TRUE)
          if(length(file) == 1) {
            file <- paths[basename(file) == basename(paths)]
            if (length(file) == 1) return(file)
          }
        }
        #"UNKNOWN"
        paste0("`", paste(deparse(x[[2]]), collapse = " "), "`")
      }
      )
    })
    data.frame(parent = file, child = paths)
  }

  graph <- do.call(rbind, lapply(paths, fetch_source))
  if(is.null(graph)) {
    message("The selected files don't use `source()`")
    return(invisible(NULL))
  }

  if(basename) graph$parent <- basename(graph$parent)
  if(!extension)  graph$parent <- sub("\\.[rR]$", "", graph$parent)

  # give useful warning for unevaled source
  # not clean but will do for now
  unevaled <- unique(graph$child[startsWith(graph$child, "`")])
  if(length(unevaled)) {
    calls <- parse(text = substr(unevaled, 2, nchar(unevaled)- 1))
    calls <- lapply(as.list(calls), remove_namespaced_calls)
    all_names <- unique(unlist(lapply(calls, all.names)))
    non_existent <- Filter(function(x) ! exists(x, .GlobalEnv), all_names)
    warning(paste0(
      "Some `file` arguments in `source()` calls could not be evaluated, ",
      "because some objects could not be found:\n",
      toString(paste0("`", non_existent, "`"))))
  }
  if(identical(out, NA)) return(invisible(graph))

  nomnoml_code <- c("#direction: right", sprintf("[%s] -> [%s]", graph$parent, graph$child))
  nomnoml_code <- paste(nomnoml_code, collapse = "\n")
  out <- save_nomnoml(nomnoml_code, svg, out)
  if (inherits(out, "htmlwidget"))
    out
  else invisible(out)
}


save_nomnoml <- function(code, svg, out) {
  ## buildwidget
  x <- list(code = code, svg = svg)
  widget <- do.call(
    htmlwidgets::createWidget,
    c(list(name = "nomnoml", x,package = "nomnoml")))

  ## is the out argument NULL ?
  if (is.null(out)) {
    ## return the widget
    return(widget)
  }

  ## flag if out is a temp file shorthand
  is_tmp <- out %in% c("html", "htm", "png", "pdf", "jpg", "jpeg")

  ## is it ?
  if (is_tmp) {
    ## set out to a temp file with the right extension
    out <- tempfile("flow_", fileext = paste0(".", out))
  }

  ## extract extension from path
  ext <- sub(".*?\\.([[:alnum:]]+)$", "\\1", out)

  ## is `out` a path to a web page ?
  if (tolower(ext) %in% c("html", "htm")) {
    ## save to file
    do.call(htmlwidgets::saveWidget, c(list(widget, out)))
  } else {
    ## save to a temp html file then convert to required output
    html <- tempfile("flow_", fileext = ".html")
    do.call(htmlwidgets::saveWidget, c(list(widget, html)))
    webshot::webshot(html, out, selector = "canvas")
  }

  ## was the out argument a temp file shorthand ?
  if (is_tmp) {
    ## print location of output and open it
    message(sprintf("The diagram was saved to '%s'", gsub("\\\\","/", out)))
    browseURL(out)
  }
  ## return the path to the output invisibly
  invisible(out)
}

remove_namespaced_calls <- function(call) {
  if(!is.call(call)) return(call)
  if(length(call) == 3 && (
    identical(call[[1]], quote(`::`)) ||
    identical(call[[1]], quote(`:::`))
  )) {
    return(NULL)
  }
  as.call(lapply(call, remove_namespaced_calls))
}
