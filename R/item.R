list2 <- function(...) {
  items <- list(...)
  # browser()
  # browser()
  if (is.null(names(items))) {
    has_name <- rep(FALSE, length(items))
  } else {
    has_name <- names(items) != ""
  }
  names(items)[!has_name] <- as.character(substitute(list(...))[-1])[!has_name]
  # names(items) <- as.character(match.call()[-1])
  return(items)
}

#' @title Create a grading item
#' @description Creates an grading item. The item stores any data passed to
#' @export
item <- function(..., compare_fun = grade_equal, pts = 1) {
  structure(
    list(
      objects = list2(...),
      compare_fun = compare_fun,
      pts = pts
    ),
    class="grade_item"
  )
}

