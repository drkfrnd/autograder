.center <- function(str, width) {
  char_dif <- width - nchar(str)
  padding <- ""
  if (char_dif > 1) {
    padding <- paste0(rep(" ", floor(char_dif / 2)), collapse = "")
  }
  paste0(padding, str)
}

.hr <- function(width, symbol = "=") {
  paste0(paste0(rep(symbol, width), collapse = ""), "\n")
}

.header <- function(title, width = 30, ...) {
  paste0(.hr(width, ...), .center(title, width), "\n", .hr(width, ...))
}

.header1 <- function(...) {
  .header(..., symbol = "=")
}

.header2 <- function(title, symbol = "-", padding = 2) {
  .header(title, width = nchar(title) + padding * 2, symbol = symbol)
}


#' @title Prints the returned value of `.check_answers`
#' @param result output from `.check_answers`
#' @returns Returns a character vector of length one containing the printed
#'   results.
.print_results <- function(result, width = 30) {
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
    append(.header1("Score:", width = width))
    df <- result$result
    # df$score <- paste0(df$pts, " / ", df$pts_pos)
    n_correct <- sum(df$result == "correct")
    # score <- sum(df$pts)
    # score_pos <- sum(df$pts_pos)
    # n_total <- nrow(df)
    summary <- paste0(sum(df$pts), " / ", sum(df$pts_pos), "\n")
    append(.center(summary, width = width))
    append("\n", .header1("Results:", width = width))
    # browser()
    df2 <- df[order(df$name), c("name", "result", "pts", "pts_pos")]
    names(df2) <- c("NAME", "RESULT", "SCORE", "OUT_OF")
    df2_str <- capture.output(print(df2, row.names = FALSE, right = FALSE))
    append(paste0(df2_str, collapse = "\n"), "\n")

    if (any(!is.na(df$error))) {
      append("\n", .header1("Incorrect answers:", width = width))
      errors <- df[!is.na(df$error), ]
      for (name_i in errors$name) {

        line <- paste0(rep("-", nchar(name_i) + 4,), collapse = "")
        append(.header2(name_i), errors$error[errors$name == name_i])
        # append(line, "\n  ", name_i, "  \n", line,"\n", )
        # append("\nYour answer was:\n", .shorten_output(result$answers[[name_i]], 1000), "\n\n")
        # append("The correct answer is:\n", .shorten_output(result$key[[name_i]], 1000), "\n")
      }
    }
  }
  return(message)
}
