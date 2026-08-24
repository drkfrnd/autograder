# @param df output of check_hw()
#' @export
gradescope_list <- function(df, ...) {
  df$result <- c(correct = "passed", incorrect = "failed")[df$result]
  names_dict <- c(
    pts = "score",
    pts_pos = "max_score",
    error = "output",
    name = "name",
    result = "status"
  )
  names(df)[match(names(names_dict), names(df))] <- names_dict
  rownames(df) <- NULL
  lst <- list(
    visibility = "visible",
    stdout_visibility = "hidden",
    ...,
    tests = df
  )
  return(lst)
}
