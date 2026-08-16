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
    df <- .compare_objects(hw, key)
  }

  df <- df[order(df$name), ]
  return(
    list(
      result = df,
      source_error = source_error,
      answers = hw[intersect(names(hw), names(key))],
      key = key
    )
  )
}

#' @title Prints the returned value of `.check_answers`
#' @param result output from `.check_answers`
#' @returns Returns a character vector of length one containing the printed
#'   results.
.print_results <- function(result) {
  message <- ""
  append <- function(...) {
    message <<- paste0(message, ...)
  }
  if (!is.na(result$source_error)) {
    append(paste0(
      "The R script failed to run with the following error:\n\n",
      result$source_error, "\n\n",
      "If the code is working fine when you run it, check the following:\n\n",
      "- Try clearing all objects from your workspace and re-running the code. ",
      "It's possible that you're referencing a variable that isn't ",
      "created in the code. (This often happens if you rename a variable and forget ",
      "to change the name at every instance where the variable is used.) Clearing the workspace ",
      "and re-running the code will expose this error.\n ",
      "- Try terminating the session (from the top menu, select Session > Terminate R). It's possible ",
      "that you're using a function from a package but aren't loading the package ",
      "in the code (e.g. library(mypackage)).\n\n",
      "These errors can arise because this function runs your code in a new R session ",
      "with an empty workspace and no packages loaded.",
      sep = ""
    ))
  } else {
    header <- function(title) {
      hr <- "==============================\n"
      char_dif <-  (nchar(hr) - 1) - nchar(title)
      padding <- ""
      if (char_dif > 1) {
        padding <- paste0(rep(" ", floor(char_dif / 2)), collapse = "")
      }
      paste0(hr, padding, title, "\n", hr)
    }

    append(header("Summary:"))

    n_correct <- sum(result$result$result == "correct")
    n_total <- nrow(result$result)
    append(n_correct, "/", n_total, " correct\n")
    append("\n", header("Results:"))

    result2 <- result$result[order(result$result$name), c("name", "result")]
    names(result2) <- c("OBJECT", "RESULT")
    result2_str <- capture.output(print(result2, row.names = FALSE, right = FALSE))
    append(paste0(result2_str, collapse = "\n"), "\n")

    if (any(!is.na(result$result$error))) {
      append("\n", header("Incorrect answers:"))
      errors <- result$result[!is.na(result$result$error), ]
      for (name_i in errors$name) {

        line <- paste0(rep("-", nchar(name_i) + 4,), collapse = "")
        append(line, "\n  ", name_i, "  \n", line,"\n", errors$error[errors$name == name_i])
        append("\nYour answer was:\n", .shorten_output(result$answers[[name_i]], 1000), "\n\n")
        append("The correct answer is:\n", .shorten_output(result$key[[name_i]], 1000), "\n")
      }
    }
  }
  return(message)
}
