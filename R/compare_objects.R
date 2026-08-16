


check_object <- function(hw, key, nm, compare_fun, ...) {
  obj_error <- NA
  if (!(nm %in% names(hw))) {
    obj_status <- "missing"
  } else {
    try_compare <- try(compare_fun(hw[[nm]], key[[nm]], ...))
    # try_compare <- try(testthat::expect_equal(hw[[nm]], key[[nm]], label = nm, expected.label = paste0(nm, " (key)")))

    is_error <- inherits(try_compare, "try-error")
    obj_status <- if(is_error) "incorrect" else "correct"
    obj_error <- if(is_error) as.character(try_compare) else NA
  }
  return(list(
    obj_status = obj_status,
    error = obj_error
  ))
}

# grade_item <- function(item, pts = 1) {
#
# }

#' @title Compare homework answers to an answer key
#' @description Given a list representing homework answers and a list containing
#'   the correct answers, compares the correct answers to the homework answers
#' @param hw named list containing the objects created in the homework script.
#'   Note that any named elements that don't have a corresponding named element
#'   in `key` are ignored.
#' @param key named list containing the objects that represent the correct
#'   answers. All elements in this list are compared to the similarly named
#'   elements in `hw`.
#' @returns a `data.frame` with one row per name in `key`. The columns are:
#'   * `name`: name of the list element
#'   * `obj_status`: either "correct" (homework answers is the same as the key),
#'     "incorrect" (homework answer is different from the key), or "missing"
#'     (object is not present in `hw`)
#'   * `error`: If the answer was incorrect, contains the output from the
#'     comparison
.compare_objects <- function(hw, key, funs = NULL) {
  eval_list <- lapply(names(key), function(nm_i) {
    check_object(hw, key, nm_i, testthat::expect_equal, label = nm_i, expected.label = paste0(nm_i, " (key)"))
    # obj_error <- NA
    # if (!(nm_i %in% names(hw))) {
    #   obj_status <- "missing"
    # } else {
    #   try_compare <- try(testthat::expect_equal(hw[[nm_i]], key[[nm_i]], label = nm_i, expected.label = paste0(nm_i, " (key)")))
    #
    #   is_error <- inherits(try_compare, "try-error")
    #   obj_status <- if(is_error) "incorrect" else "correct"
    #   obj_error <- if(is_error) as.character(try_compare) else NA
    # }
    # return(list(
    #   obj_status = obj_status,
    #   error = obj_error
    # ))
  })
  names(eval_list) <- names(key)

  return(data.frame(
    name = names(key),
    result = sapply(eval_list, `[[`, "obj_status"),
    error = sapply(eval_list, `[[`, "error")
  ))
}
