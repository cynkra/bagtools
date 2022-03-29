globals <- new.env()
# set to FALSE on top of tests
globals$source_markers <- TRUE

flatten_paths <- function(paths, recursive = TRUE) {
  paths_are_dirs <- sapply(paths, dir.exists)
  dirs <- paths[paths_are_dirs]
  paths_from_dirs <- unlist(lapply(dirs, list.files, recursive = recursive, pattern = "\\.[rR]$"))
  paths <- c(paths[!paths_are_dirs], paths_from_dirs)
  paths
}


# A wrapper that doesn't call `rstudioapi::sourceMarkers`
#   if !globals$source_markers
# It also :
#   Returns `markers` invisibly in any case
#   Renames line1 into line and col1 into column, so we don't have to worry about renaming cols from getParseData()
#   trims markers from extra cols
#   sorts by path, line, col if `sort = TRUE`
#   Puts `top` paths on top if given
source_markers <- function(
  name,
  markers,
  basePath = NULL,
  autoSelect = c("none", "first", "error"),
  sort = TRUE,
  top = NULL,
  no_row_message = "No result to show"
) {
  if(!nrow(markers)) {
    message(no_row_message)
    return(invisible(NULL))
  }
  autoSelect <- match.arg(autoSelect)
  nms <- names(markers)
  names(markers)[nms == "line1"] <- "line"
  names(markers)[nms == "col1"] <- "column"
  markers <- markers[c("line", "column", "type", "message", "file")]
  if(sort) {
    markers <- markers[order(markers$file, markers$line, markers$column),, drop = FALSE]
  }
  if(!is.null(top)) {
    markers <- rbind(
      markers[markers$file %in% top,, drop = FALSE],
      markers[!markers$file %in% top,, drop = FALSE]
    )
  }
  if(globals$source_markers)
    rstudioapi::sourceMarkers(name, markers, basePath, autoSelect)
  invisible(markers)
}

# A wrapper around `getParseData()` that takes paths directly, adds a column `file`
# And concats data fetched from several scripts
# FIXME: option to remove function definitions, namespaced calls, dollared calles directly there ?
get_parse_data <- function(paths) {
  parse_data <- lapply(paths, function(file) {
    data <- getParseData(parse(file))
    transform(data, file = file)
  })
  parse_data <- do.call(rbind, parse_data)
  parse_data
}
