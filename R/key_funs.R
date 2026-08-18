#' @title Get a function for appending to a key
#' @description This function is intended to be used when creating a key. This
#' function returns a function, and you can then use that function to add to the
#' key. The goal is to minimize typing and make it quick and easy to add items
#' to a key.
#' @returns Returns a function with the following function signature:
#' `function(name, ...)`. The parameters are:
#' * `name`: character of length 1; name to assign to this item. If this parameter
#'   is missing no item is added and the key is returned.
#' * `...`: arguments passed to `item()`.
#' @examples
#' k <- key_fun()
#' x <- 1
#' k("q1", x, pts = 1)
#' y <- "hello"
#' k("q2", y, pts = 1.5)
#' k() # return the key
#' # saveRDS(k(), "mykey.rds")
#' @export
key_fun <- function() {
  key <- list()
  add_to_key <- function(name, ...) {
    if (missing(name)) return(key)
    if (name %in% names(key)) key[[name]] <<- NULL
    key <<- c(key, setNames(list(autograder::item(...)), name))
    invisible(key)
  }
  return(add_to_key)
}


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
  # key <- .load_list(path)[[1]] # assumes that the key is the only
  readRDS(path)
}




#' @title Read a homework key from the GitHub repository.
#' @description The homework keys can be found in this repository:
#' https://github.com/drkfrnd/STAT212.
#' @param name name of the homework key. Can include the .RData file extension
#'   but doesn't need to.
#' @returns Returns the key as a named list.
#' @export
get_key <- function(name) {
  if (!grepl("[.]rds$", tolower(name))) name <- paste0(name, ".rds")

  url <- paste0(
    "https://github.com/drkfrnd/STAT212/raw/refs/heads/master/rds/",
    name
  )
  tf <- tempfile(fileext = ".rds")
  result <- try(download.file(url, tf), silent = TRUE)
  if (inherits(result, "try-error")) {
    stop(paste0("Download was unsuccessful. Did you enter the correct name?\n",
                "URL: ", url))
  }
  # lst <- .load_list(tf)

  # return(lst)
  return(load_key(tf))
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
    # return(NA)
    return(basename(tools::file_path_sans_ext(path)))
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


