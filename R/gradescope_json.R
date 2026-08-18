# @param df output of check_hw()
#' @export
gradescope_json <- function(df, ...) {
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
    time = tm["elapsed"],
    visibility = "after_due_date",
    stdout_visibility = "visible",
    ...,
    tests = df
  )
  return(jsonlite::toJSON(lst))
}
