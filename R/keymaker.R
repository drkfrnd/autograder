
#' @export
Keymaker <- R6::R6Class(
  classname = "Keymaker",
  private = list(
    .key = list(),
    .levels = integer(0),
    .add_level = function(n_to_add) {
      # browser()
      new_levels <- rep(1, n_to_add)
      private$.levels <- c(private$.levels, new_levels)
      # browser()
      if (length(private$.key) > 0) {
        names(private$.key) <- paste0(
          names(private$.key) , ".",
          paste0(new_levels, collapse = ".")
        )
      }
    },
    .get_new_level = function(level) {
      n_levels <- length(private$.levels)
      if (level > n_levels) {
        private$.add_level(level - n_levels)
      }
      if (n_levels != 0) {
        private$.levels[level] <- private$.levels[level] + 1
      }
      if (level < length(private$.levels)) {
        private$.levels[(level + 1):length(private$.levels)] <- 1
      }
      name <- paste0(private$.levels, collapse = ".")
      return(name)
    },
    .get_clean_names = function() {
      df <- data.frame(do.call(rbind, strsplit(names(private$.key), "[.]")))
      splt_rec <- function(x, col = 1) {
        if (col == ncol(x)) {
          return(x)
        }
        fac <- factor(x[[col]], unique(x[[col]]))
        splt <- split(x, fac)
        nr <- sapply(splt, nrow)
        splt[nr == 1] <- lapply(splt[nr == 1], \(x) {
          x[(col+1):ncol(x)] <- NA
          x
        })
        splt[nr > 1] <- lapply(splt[nr > 1], splt_rec, col = col + 1)
        return(do.call(rbind, splt))
      }
      df_na <- splt_rec(df)
      new_nms <- apply(df_na, 1, \(x) {
        paste0(x[!is.na(x)], collapse = ".")
      })
      return(new_nms)
    }
  ),
  public = list(
    initialize = function() {},
    add = function(..., name = NULL, level = 1) {
      if (is.null(name) && ...length() == 0) {
        stop("There must be a least argument provided to ... or name")
      }
      if (is.null(name)) {
        name <- private$.get_new_level(level)
        # private$.levels[level] <- private$.levels[level] + 1
        # n_levels <- length(private$.levels)
        # if (level < n_levels) {
        #   private$.levels[(level + 1):n_levels] <- 1
        # } else if (level > n_levels){
        #   new_levels <- rep(1, level - n_levels)
        #   private$.levels <- c(private$.levels, new_levels)
        #   names(private$.key) <- paste0(
        #     names(private$.key),
        #     paste0(new_levels, collapse = ".")
        #   )
        # }
        # name <- paste0(private$.levels, collapse = ".")
      }
      if (name %in% names(private$.key)) private$.key[[name]] <- NULL
      private$.key <- c(private$.key, setNames(list(item(...)), name))
      invisible(NULL)
    },
    a = function(...) {
      self$add(...)
    },
    save = function(path, ...) {
      saveRDS(self$key(...), path)
      invisible(NULL)
    },
    key = function(clean_names = TRUE) {
      key2 <- private$.key
      if (clean_names) {
        names(key2) <- private$.get_clean_names()
      }
      return(key2)
    }
  )
)

# x <- 3
# k <- Keymaker$new()
# k$add(x, level = 1)
# names(k$key())
# k$add(x, level = 3)
# names(k$key())
