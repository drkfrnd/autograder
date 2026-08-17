#' @title Run a script and store the created objects
#' @description Sources an R script and saves the objects created by the script
#'   into a user-provided environment.
#' @param path character of length 1; path to the R script (or .Rmd)
#' @param env environment; the objects created by the script will be saved in
#'   this environment
#' @returns If `source()` runs successfully, returns the output of `source()`.
#'   Otherwise returns a `try-error`.
#' @export
.run_source <- function(path, env) {
  print(tools::file_ext(path))
  if (tools::file_ext(path) == "Rmd") {
    tf <- tempfile(fileext = ".R")
    path <- knitr::purl(path, output = tf)
    print(path)
  }
  try(source(path, local = env), silent = TRUE)
}

#' @title Renders an R or Rmarkdown file
#' @description Renders an R or Rmarkdown file while store the created objects
#'   in a user-provided environment.
#' @param path character of length 1; path to the file to render
#' @param env environment; the objects created by the script will be saved in
#'   this environment
#' @param out_dir character of length(1); path to the directory in which to save
#'   the rendered file.
#' @export
.run_render <- function(path, env, out_dir) {
  try({
    rmarkdown::render(path, envir = env, output_dir = out_dir)
    # output_format = rmarkdown::html_document(
    #   pandoc_args = c("--metadata=author:")
    # ))
  }, silent = TRUE)
}

#' @title Runs a homework script and compares the answers to the answer key
#' @param path character of length 1; path to the script to run
#' @param key named list containing the objects that represent the correct
#'   answers.
#' @param source_fun function for running the script. Must accept at least two
#'   parameters - the first should be a character of length 1 that contains the
#'   path to the script to run. The second should be an environment, and all
#'   objects created in the script should be saved in this environment. If the
#'   source script doesn't run, this function should return a `try-error`.
#'   `.run_source` and `.run_render` have been designed to work with this
#'   function.
#' @param ... additional arguments passed to `source_fun`
#' @returns list with three elements:
#' * `result`: output from `.compare_objects` (data frame)
#' * `source_error`: character; error message from running the script. `NA` if
#'   no error occurred.
#' * `answers`: list containing the objects created from running the homework
#'   script. Only contains the objects whose name appears in `key`.
#' * `key`: the passed to the `key` parameter
.check_answers <- function(path, key, source_fun = .run_source, ...) {
  hw_env <- new.env()
  source_error <- source_fun(path, hw_env, ...)

  if (inherits(source_error, "try-error")) {
    source_error <- paste0(" ", as.character(source_error))
    hw <- NULL
    df <- data.frame(name = names(key), result <- "failed to run", error = NA)
  } else {
    source_error <- NA
    hw <- as.list(hw_env)
    df <- .check_items(hw, key)
  }

  # df <- df[order(df$name), ]
  return(
    list(
      result = df,
      source_error = source_error,
      # answers = hw[intersect(names(hw), names(key))],
      answers = hw,
      key = key
    )
  )
}


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
