
#' @title Load a .RData file as a list
#' @description Given a .RData file, loads the objects it contains into a list.
#' @param path character of length 1; path to the .RData file
#' @returns list containing the R objects
.load_list <- function(path) {
  env <- new.env()
  load(path, envir = env)
  return(as.list(env))
}

#' @title Load a .RData file as a list
#' @description Given an answer key stored as a .RData file, loads the objects
#'   it contains into a list.
#' @param path character of length 1; path to the .RData file
#' @returns list containing the R objects
#' @export
load_key <- function(path) {
  .load_list(path)
}

#' @title Shortens the printed version of an object
#' @description Given an object and a number of characters, captures the output
#'   from printing the object and then shortens it to the desired number of
#'   characters (if necessary).
#' @param x object to print
#' @param n integer; maximum number of characters
#' @returns character vector of length 1 with the shortened output.
.shorten_output <- function(x, n) {
  out <- capture.output(print(x))
  str <- paste0(out, collapse = "\n")
  if (nchar(str) > n) {
    str <- paste0(substr(str, 1, n), "\n...\nOutput truncated to ", n, " characters.")
  }
  return(str)
}


#' @title Read a homework key from the GitHub repository.
#' @description The homework keys can be found in this repository:
#' https://github.com/drkfrnd/STAT212.
#' @param name name of the homework key. Can include the .RData file extension
#'   but doesn't need to.
#' @returns Returns the key as a named list.
#' @export
get_key <- function(name) {
  if (!grepl("[.]rdata$", tolower(name))) name <- paste0(name, ".RData")

  url <- paste0(
    "https://github.com/drkfrnd/STAT212/raw/refs/heads/master/rdata/",
    name
  )
  tf <- tempfile(fileext = ".RData")
  result <- try(download.file(url, tf), silent = TRUE)
  if (inherits(result, "try-error")) {
    stop(paste0("Download was unsuccessful. Did you enter the correct name?\n",
                "URL: ", url))
  }
  lst <- .load_list(tf)
  return(lst)
}

#' @title Guess the homework name from a file path
#' @param path character of length 1; file path
#' @param pattern character of length 1; regular expression used to extract the
#'   homework name
#' @returns Returns the extracted homework name. Returns `NA` if no match was
#'   found
.guess_hw_name_from_path <- function(path, pattern = "hw\\d{2}") {
  path_lower <- tolower(path)
  re <- regexpr(pattern, path_lower)

  if (re == -1) {
    return(NA)
  } else {
    return(regmatches(path_lower, re))
  }
}

#' @title Process key input
#' @param key named list or character of length 1. If a named list, this value
#'   is returned. If a character of length 1, this is used to try and retrieve
#'   the key from the GitHub repository. If `NULL` it tries to guess the
#'   homework name from `path`
#' @param path character of length 1; file path used to guess the homework name
#' @param pattern character of length 1; regular expression used to extract the
#'   homework name from `path`.
#' @description Processes the input to `key` in the way described in the
#'   `details` section of the `render_all` documentation. Throws an error if it
#'   is unable to guess the homework name.
.process_key <- function(key, path, pattern = "hw\\d{2}") {
  if (is.null(key)) {
    hw_name <- .guess_hw_name_from_path(path, pattern)
    if (is.na(hw_name)) {
      stop("No answer key provided, and no homework number was able to be extracted from the file path.")
    } else {
      message(paste0("No value for 'key' provided, extracted \"", hw_name, "\" from the file path."))
      key <- get_key(hw_name)
    }
  } else if (is.character(key)) {
    key <- get_key(key)
  }
  return(key)
}


