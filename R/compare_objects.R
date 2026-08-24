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

# wrapper around expect_equal that prints out a more descriptive error message
.expect_equal2 <- function(actual, expected, ...) {
  test <- try(testthat::expect_equal(actual, expected, ...))
  if (inherits(test, "try-error")) {
    error_message <- gsub("Error : ", "", as.character(test), fixed = TRUE)
    stop(paste0(
      error_message,
      .print_mismatch(actual, expected)
    ), call. = FALSE)
  }
}

eval_simple_formula <- function(formula, envir = parent.frame()) {
  if (length(formula) == 3) {
    stop("This function only works with formulas that only have a right side.")
  }
  rhs <- formula[[2]]
  eval(rhs, envir = envir)
}

#' @export
grade_function <- function(hw, key, object_names, args = list()) {
  # args <- as.list(...)
  if (length(object_names) > 1) {
    stop("Only one function can be graded at a time.")
  }
  autograder::grade_exists(hw, key, object_names)

  lapply(args, function(args_i) {
    is_formula <- sapply(args_i, inherits, "formula")
    if (any(is_formula)) {
      args_i[is_formula] <- lapply(args_i[is_formula], eval_simple_formula, envir = key)
    }
    fx_str <- paste0(object_names, "(", paste0(args_i, collapse = ", "), ")")
    hw_result <- do.call(hw[[object_names]], args_i)
    key_result <- do.call(key[[object_names]], args_i)
    # testthat::expect_equal(,
    .expect_equal2(
      hw_result,
      key_result,
      label = fx_str,
      expected.label = paste0(fx_str, " (key)")
    )
  })
  invisible(NULL)
}

#' @export
grade_exists <- function(hw, key, object_names) {
  # browser()
  is_in_hw <- object_names %in% names(hw)
  if (!all(is_in_hw)) {
    stop(paste0("The following object(s) were not created in the homework script: \n",
                paste0("* ", object_names[!is_in_hw], collapse = "\n ")),
         call. = FALSE)
  }
  invisible(NULL)
}

#' @param hw list containing ALL objects created in the assignment
#' @param key list containing the relevant objects
#' @export
grade_equal <- function(hw, key, object_names) {
  print("testing")
  autograder::grade_exists(hw, key, object_names)

  lapply(object_names, function(nm_i) {
    .expect_equal2(
      hw[[nm_i]],
      key[[nm_i]],
      label = nm_i,
      expected.label = paste0(nm_i, " (key)")
    )

    # test <- try(testthat::expect_equal(hw[[nm_i]], key[[nm_i]], label = nm_i, expected.label = paste0(nm_i, " (key)")))
    # if (inherits(test, "try-error")) {
    #   error_message <- gsub("Error : ", "", as.character(test), fixed = TRUE)
    #   stop(paste0(
    #     error_message,
    #     .print_mismatch(hw[[nm_i]], key[[nm_i]])
    #   ), call. = FALSE)
    # }
  })
  invisible(NULL)
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
.check_item <- function(key_item, key_objects, hw_objects) {
  # try_compare <- try(compare_fun(hw[[nm]], key[[nm]], ...))
  # try_compare <- try(compare_fun(hw, key, ...), silent = TRUE)
  try_compare <- try({
    do.call(
      key_item$compare_fun,
      c(
        list(hw_objects, key_objects, key_item$object_names),
        key_item$compare_fun_args
      )
    )
    # key_item$compare_fun(hw_objects, key_objects, key_item$object_names), silent = TRUE
  }, silent = TRUE)
  # try_compare <- try(testthat::expect_equal(hw[[nm]], key[[nm]], label = nm, expected.label = paste0(nm, " (key)")))
  is_error <- inherits(try_compare, "try-error")
  return(data.frame(
    id = key_item$id,
    result = if(is_error) "incorrect" else "correct",
    error = if(is_error) as.character(try_compare) else NA,
    pts = if(is_error) 0 else key_item$pts,
    pts_pos = key_item$pts,
    objects = paste0(key_item$object_names, collapse = ";")
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


.check_items <- function(key_items, key_objects, hw_objects) {

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
  eval_list <- lapply(key_items, \(item_i) .check_item(item_i, key_objects, hw_objects))
  eval_df <- do.call(rbind, eval_list)
  eval_df$name <- names(key_items)
  return(eval_df)
  # names(eval_list) <- names(items)
#
#   return(data.frame(
#     name = names(items),
#     result = sapply(eval_list, `[[`, "obj_status"),
#     error = sapply(eval_list, `[[`, "error")
#   ))
}
