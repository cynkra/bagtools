#' Detect similar code blocks
#'
#' This is a wrapper around `dupree::dupree()`. It analyses provided files looking for
#'   similar code. It is a bit slow so it can be impractical on big projects to run
#'   it with default (all files contained in working directory recursively), in
#'   this case it is wiser to run it on one or more specific `paths` or/and
#'   or to filter the files using the `pattern` argument.
#'
#' @param paths Paths to scripts or folders containing scripts
#' @param recursive Whether folders should be explorer recursively
#' @param pattern A regular expression used to filter files
#'
#' @export
detect_similar_code <- function(paths = ".", recursive = TRUE, pattern = NULL) {
  paths <- flatten_paths(paths, recursive)
  if (!is.null(pattern)) paths <- grep(pattern, paths, value = TRUE)
  dups <- dupree::dupree(paths)
  dups <- as.data.frame(dups)
  dups <- dups[order(-dups$score),]
  dups <- transform(
    dups,
    i = 1:nrow(dups),
    message = paste0("duplicate #", 1:nrow(dups), " (score: ", round(score,2),")"))
  dups <-
    rbind(
      setNames(dups[c("file_a", "line_a", "message", "i")], c("file", "line", "message", "i")),
      setNames(dups[c("file_b", "line_b", "message", "i")], c("file", "line", "message", "i"))
    )

  dups <- transform(dups[order(dups$i),], type ="info", column = 1)
  source_markers("dupree", dups, sort = FALSE)
}
