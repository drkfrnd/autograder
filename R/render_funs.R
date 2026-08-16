


#' @title Inserts homework check results into a rendered HTML file
#' @description This function is used to insert the results of checking the
#'   homework into the top of the rendered HTML file. This provides a summary
#'   of what answers were correct.
#' @param result output from `.check_answers`
#' @param path character of length 1; path to the HTML file to insert the
#'   results summary into.
.insert_results_into_html <- function(result, path) {
  message <- .print_results(result)
  html_doc <- rvest::read_html(path)
  html_doc |>
    xml2::xml_find_first('//div[@id="header"]') |>
    xml2::xml_add_sibling(.value = "pre") |>
    xml2::xml_find_first("//pre") |>
    xml2::xml_add_child("code", message)
  xml2::write_xml(html_doc, path)
}

#' @title Render homework to an HTML file
#' @description Renders a homework script to an HTML file. The resulting HTML
#'   file includes output at the top that summarizes the result of comparing the
#'   homework answers to the key
#' @param path character of length 1; path to the homework script
#' @param key named list containing the objects that represent the correct
#'   answers.
#' @param out_dir character of the length 1; path to the directory in which to
#'   save the resulting HTML. If `NULL` outputs to the current working
#'   directory.
#' @export
render_hw <- function(path, key, out_dir = NULL) {
  result <- callr::r(.check_answers,
                     args = list(path = path,
                                 key = key,
                                 source_fun = .run_render,
                                 out_dir = out_dir),
                     package = "autograder")
  html <- file.path(out_dir, paste0(tools::file_path_sans_ext(basename(path)), ".html"))
  if (file.exists(html)) {
    .insert_results_into_html(result, html)
  }
  return(result)
}

#' @title Renders multiple homework assignments
#' @description Runs all R or Rmd files in a directory and creates an HTML file
#'   for each script that contains the rendered code as well as a summary of
#'   which answers were correct/incorrect. The function also optionally outputs
#'   a CSV giving a summary of the results.
#' @param in_dir character of length 1; path to directory containing R or Rmd
#'   scripts.
#' @param out_dir character of length 1; path to directory in which to output
#'   the HTML files
#' @param key One of the following: `NULL`, a character of length 1, or a named
#'   list. See details for more.
#' @param summary_csv logical of length 1; if `TRUE`, outputs a CSV with a
#'   summary of the results for each script.
#' @param pattern character of length 1; regular expression used to select
#'   the scripts in `out_dir`
#' @details There are three possible kinds of inputs to `key`:
#'   * character of length 1: in this case `key` is taken to be the name of a
#' key stored in the GitHub repository (https://github.com/drkfrnd/STAT212). The
#' key is read from GitHub and used to check the answers
#'   * `NULL`: if `key` is `NULL` (the default) the name of the key is guessed
#' from `in_dir` by trying to extract a string matching the regular expression
#' "hw\\d{2}". If found, uses this as the key name. Otherwise an error is
#' thrown.
#'   * named list containing the answers (as in `render_hw()`).
#' @export
render_all <- function(in_dir, out_dir, key = NULL, summary_csv = TRUE,
                       pattern = "[.](R|Rmd)$") {
  key <- .process_key(key, in_dir)
  hw_files <- dir(in_dir, pattern = pattern, full.name = TRUE)

  if (!dir.exists(out_dir)) dir.create(out_dir)
  lst <- lapply(hw_files, function(file_i) {
    render_hw(file_i, key, out_dir)
  })
  student_names <- sapply(strsplit(tools::file_path_sans_ext(basename(hw_files)), "_"),
                          `[`, 1)
  names(lst) <- student_names

  df <- lapply(names(lst), function(nm_i) {
    bool <- as.integer(lst[[nm_i]]$result$result == "correct")
    names(bool) <- lst[[nm_i]]$result$name
    data.frame(
      name = nm_i,
      ran = is.na(lst[[nm_i]]$source_error),
      total = sum(bool),
      possible = length(bool),
      as.list(bool)
    )
  }) |>
    do.call(rbind, args = _)
  if (summary_csv) {
    write.csv(df, file.path(out_dir, "_summary.csv"), row.names = FALSE)
  }
  invisible(list(results = lst, summary = df))
}

#' @title Render a script without the code
#' @description Renders a script while ignoring the code
#' @param path character of length 1; path to the R or Rmd file
#' @param output_file character of length 1; path to the output file
#' @export
render_no_code <- function(path, output_file = NULL) {
  on.exit({
    opts_hooks$set(eval = function(options) { options })
  })
  knitr::opts_hooks$set(eval = function(options) {
    options$eval <- FALSE
    options$include <- FALSE
    options
  })

  rmarkdown::render(path, output_file = NULL)
}
