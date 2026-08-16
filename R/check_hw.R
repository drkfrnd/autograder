#' @title Check homework answers
#' @description Runs an R script and compares the objects created in the script
#'   to an answer key. Note that the R script is run in a new session, so if the
#'   script references objects not created in the script, an error will be
#'   thrown.
#' @param path character of length 1; path to an R script to check.
#' @param verbose logical of length 1; if `TRUE`, prints the result of checking
#'   the homework assignment to the console.
#' @returns invisibly returns the returned value from `.check_answers`
#' @inheritParams render_all
#' @export
check_hw <- function(path, key = NULL, verbose = TRUE) {
  if (!file.exists(path)) {
    stop(paste0("File \"", path, "\" not found in the current working directory (", getwd(), ").\n",
                "- Check that the file name is spelled correctly.\n",
                "- Also ensure that your working directory is set to the directory that contains the R script.\n"))
  }

  key <- .process_key(key, path)

  result <- callr::r(.check_answers, args = list(path = path, key = key), package = "autograder")

  if (verbose) {
    cat(.print_results(result))
  }

  invisible(result)
}
