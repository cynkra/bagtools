#' Detect ambiguous `summarize()` calls
#'
#' @param paths Paths to scripts or folders containing scripts
#' @param recursive Whether folders should be explorer recursively
#' @param pattern A regular expression used to filter files
#'
#' @export
detect_ambiguous_summarize_calls <- function(paths = ".", recursive = TRUE, pattern = NULL) {
  paths <- flatten_paths(paths, recursive)
  if (!is.null(pattern)) paths <- grep(pattern, paths, value = TRUE)
  parse_data <- get_parse_data(paths)
  parse_data$id <- paste0(parse_data$file, parse_data$id)
  parse_data$parent <- paste0(parse_data$file, parse_data$parent)
  summarize_ids <- parse_data$id[
    parse_data$text %in% c("summarize", "summarise") & parse_data$token == "SYMBOL_FUNCTION_CALL"]
  summarize_parents <- parse_data$parent[parse_data$id %in% summarize_ids]
  summarize_grandparents <- parse_data$parent[parse_data$id %in% summarize_parents]
  bad_filter <- sapply(summarize_grandparents, function(x) !any(
    parse_data$parent == x & parse_data$token == "SYMBOL_SUB" & parse_data$text == ".groups"))

  parse_data <- parse_data[parse_data$id %in% summarize_ids[bad_filter],, drop = FALSE]
  if(!nrow(parse_data)) {
    message("No problematic `summarize()` call was found")
    return(invisible(NULL))
  }
  parse_data$message <- "`summarize()` called without `.groups` arg"
  parse_data$type <- "warning"
  source_markers("`summarize()` calls", parse_data)
}
