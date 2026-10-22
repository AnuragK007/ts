#' Practical code sets for Time Series Analysis
#'
#' \code{set1()} to \code{set6()} print (or run) the code of each practical
#' question set, split topic-wise.
#'
#' @param topic Topic number (1, 2, ...) or a word from the topic title.
#'   \code{NULL} (default) means all topics of the set.
#' @param run If \code{TRUE}, run the code in the console instead of printing it.
#' @param copy If \code{TRUE}, also copy the code to the clipboard (needs clipr).
#' @return Invisibly, the code lines that were shown.
#' @name sets
#' @examples
#' \dontrun{
#' set1()
#' set1(2)
#' set3("regression")
#' set5(1, run = TRUE)
#' }
NULL

#' @rdname sets
#' @export
set1 <- function(topic = NULL, run = FALSE, copy = FALSE) .show_set(1L, topic, run, copy)

#' @rdname sets
#' @export
set2 <- function(topic = NULL, run = FALSE, copy = FALSE) .show_set(2L, topic, run, copy)

#' @rdname sets
#' @export
set3 <- function(topic = NULL, run = FALSE, copy = FALSE) .show_set(3L, topic, run, copy)

#' @rdname sets
#' @export
set4 <- function(topic = NULL, run = FALSE, copy = FALSE) .show_set(4L, topic, run, copy)

#' @rdname sets
#' @export
set5 <- function(topic = NULL, run = FALSE, copy = FALSE) .show_set(5L, topic, run, copy)

#' @rdname sets
#' @export
set6 <- function(topic = NULL, run = FALSE, copy = FALSE) .show_set(6L, topic, run, copy)

#' List all sets and their topics
#' @return Invisibly, a data.frame of sets and topics.
#' @export
ts_index <- function() {
  res <- do.call(rbind, lapply(1:6, function(s) {
    tp <- .topics(s)
    data.frame(set = s, topic = tp$topic, title = tp$title, stringsAsFactors = FALSE)
  }))
  for (s in 1:6) {
    cat(sprintf("\nset%d()\n", s))
    r <- res[res$set == s, ]
    cat(sprintf("  %d. %s\n", r$topic, r$title), sep = "")
  }
  cat("\nUsage: set1(), set1(2), set1(2, run = TRUE), set1('AR', copy = TRUE)\n")
  invisible(res)
}

#' Install the packages needed to run the code
#' @return Invisibly, the names of packages that were missing.
#' @export
ts_install_deps <- function() {
  pk <- c("forecast", "tseries", "lmtest", "FinTS", "rugarch", "clipr")
  miss <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(miss)) {
    message("All packages already installed.")
  } else {
    for (p in miss) {
      tryCatch(utils::install.packages(p),
               error = function(e) message("Could not install ", p, ": ", conditionMessage(e)))
    }
  }
  invisible(miss)
}
