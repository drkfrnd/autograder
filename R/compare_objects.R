#
# grade_equal <- function(hw, key, nms) {
#   stopifnot(length(nms) == 1)
#   testthat::expect_equal(hw[[nms]], key[[nms]], label = nms, expected.label = paste0(nms, " (key)"))
# }


# grade_equal <- function(hw, key, nms) {
#   lapply(nms, function(nm_i) {
#     testthat::expect_equal(hw[[nm_i]], key[[nm_i]], label = nms, expected.label = paste0(nm_i, " (key)"))
#   })
# }

# eval_simple_formula <- function(formula, envir = parent.frame()) {
#   if (length(formula) == 3) {
#     stop("This function only works with formulas that only have a right side.")
#   }
#   rhs <- formula[[2]]
#   eval(rhs, envir = envir)
# }
#
# grade_function <- function(hw, key, inputs = list()) {
#
# }

#' @export
grade_exists <- function(hw, key) {
  is_in_hw <- names(key) %in% names(hw)
  if (!all(is_in_hw)) {
    stop(paste0("The following object(s) were not created in the homework script: \n",
                paste0("* ", names(key)[!is_in_hw], collapse = "\n ")),
         call. = FALSE)
  }
}

#' @param hw list containing ALL objects created in the assignment
#' @param key list containing the relevant objects
#' @export
grade_equal <- function(hw, key) {
  grade_exists(hw, key)
  lapply(names(key), function(nm_i) {
    test <- try(testthat::expect_equal(hw[[nm_i]], key[[nm_i]], label = nm_i, expected.label = paste0(nm_i, " (key)")))
    if (inherits(test, "try-error")) {
      error_message <- gsub("Error : ", "", as.character(test), fixed = TRUE)
      stop(paste0(
        error_message,
        .print_mismatch(hw[[nm_i]], key[[nm_i]])
      ), call. = FALSE)
    }
  })
}

# .check_object <- function(hw, key, compare_fun, ...) {
#   obj_error <- NA
#   if (!is.null(key) && !all(names(key) %in% names(hw))) {
#     obj_status <- "missing"
#   } else {
#     # try_compare <- try(compare_fun(hw[[nm]], key[[nm]], ...))
#     try_compare <- try(compare_fun(hw, key, ...))
#     # try_compare <- try(testthat::expect_equal(hw[[nm]], key[[nm]], label = nm, expected.label = paste0(nm, " (key)")))
#
#     is_error <- inherits(try_compare, "try-error")
#     obj_status <- if(is_error) "incorrect" else "correct"
#     obj_error <- if(is_error) as.character(try_compare) else NA
#   }
#   return(list(
#     obj_status = obj_status,
#     error = obj_error
#   ))
# }

# .check_item <- function(hw, key, compare_fun, ...) {
.check_item <- function(hw, item) {
  # try_compare <- try(compare_fun(hw[[nm]], key[[nm]], ...))
  # try_compare <- try(compare_fun(hw, key, ...), silent = TRUE)
  try_compare <- try(item$compare_fun(hw, item$objects), silent = TRUE)
  # try_compare <- try(testthat::expect_equal(hw[[nm]], key[[nm]], label = nm, expected.label = paste0(nm, " (key)")))
  is_error <- inherits(try_compare, "try-error")
  return(data.frame(
    result = if(is_error) "incorrect" else "correct",
    error = if(is_error) as.character(try_compare) else NA,
    pts = if(is_error) 0 else item$pts,
    pts_pos = item$pts
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
# .compare_objects <- function(hw, key, funs = NULL) {
#   eval_list <- lapply(names(key), function(nm_i) {
#     # .check_object(hw, key, nm_i, testthat::expect_equal, label = nm_i, expected.label = paste0(nm_i, " (key)"))
#     .check_object(hw, key, nm_i, grade_equal)
#     # obj_error <- NA
#     # if (!(nm_i %in% names(hw))) {
#     #   obj_status <- "missing"
#     # } else {
#     #   try_compare <- try(testthat::expect_equal(hw[[nm_i]], key[[nm_i]], label = nm_i, expected.label = paste0(nm_i, " (key)")))
#     #
#     #   is_error <- inherits(try_compare, "try-error")
#     #   obj_status <- if(is_error) "incorrect" else "correct"
#     #   obj_error <- if(is_error) as.character(try_compare) else NA
#     # }
#     # return(list(
#     #   obj_status = obj_status,
#     #   error = obj_error
#     # ))
#   })
#   names(eval_list) <- names(key)
#
#   return(data.frame(
#     name = names(key),
#     result = sapply(eval_list, `[[`, "obj_status"),
#     error = sapply(eval_list, `[[`, "error")
#   ))
# }


.check_items <- function(hw, items) {

  # eval_list <- lapply(names(items), function(nm_i) {
    # nm_i <- names(key)[1]
    # obj <- key[[nm_i]]$objects
    # .check_item(hw, items[[nm_i]])
    # .check_item(hw, key[[nm_i]]$objects, key[[nm_i]]$compare_fun)
    # .check_object(hw, key, nm_i, testthat::expect_equal, label = nm_i, expected.label = paste0(nm_i, " (key)"))
    # .check_object(hw, key, nm_i, grade_equal)
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
  # })
  eval_list <- lapply(items, \(item_i) .check_item(hw, item_i))
  eval_df <- do.call(rbind, eval_list)
  eval_df$name <- names(items)
  return(eval_df)
  # names(eval_list) <- names(items)
#
#   return(data.frame(
#     name = names(items),
#     result = sapply(eval_list, `[[`, "obj_status"),
#     error = sapply(eval_list, `[[`, "error")
#   ))
}
