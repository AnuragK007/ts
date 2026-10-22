.code_dir <- function() system.file("code", package = "ts2")

.topics <- function(set) {
  files <- list.files(.code_dir(),
                      pattern = sprintf("^set%d_[0-9]+\\.R$", set),
                      full.names = TRUE)
  if (!length(files)) stop("No code files found for set ", set, call. = FALSE)
  num <- as.integer(sub(".*_([0-9]+)\\.R$", "\\1", files))
  files <- files[order(num)]
  titles <- vapply(files, function(f) {
    sub("^#\\s*TITLE:\\s*", "", readLines(f, n = 1L, warn = FALSE))
  }, character(1))
  data.frame(topic = seq_along(files), title = unname(titles),
             file = files, stringsAsFactors = FALSE)
}

.pick <- function(tp, topic) {
  if (is.null(topic)) return(tp$topic)
  hit <- if (is.character(topic)) {
    which(grepl(topic, tp$title, ignore.case = TRUE))
  } else {
    topic
  }
  if (!length(hit) || any(!hit %in% tp$topic)) {
    stop("Topic not found. Choose one of:\n",
         paste0("  ", tp$topic, ". ", tp$title, collapse = "\n"), call. = FALSE)
  }
  hit
}

.check_deps <- function(code) {
  m <- regmatches(code, regexpr("library\\(([A-Za-z0-9.]+)\\)", code))
  pk <- unique(sub("library\\(([A-Za-z0-9.]+)\\)", "\\1", m))
  miss <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop("Missing package(s): ", paste(miss, collapse = ", "),
         ". Run ts2::ts_install_deps() first.", call. = FALSE)
  }
}

.show_set <- function(set, topic, run, copy) {
  tp  <- .topics(set)
  idx <- .pick(tp, topic)
  bar <- strrep("=", 70)
  out <- character(0)
  for (i in idx) {
    code <- readLines(tp$file[i], warn = FALSE)[-1L]
    code <- code[seq(which(nzchar(trimws(code)))[1L], length(code))]
    head <- sprintf("SET %d | %s", set, tp$title[i])
    if (run) {
      .check_deps(code)
      cat(bar, "\n", head, "\n", bar, "\n", sep = "")
      source(tp$file[i], local = globalenv(), echo = TRUE,
             max.deparse.length = Inf, prompt.echo = "> ")
    } else {
      cat(bar, "\n# ", head, "\n", bar, "\n", sep = "")
      cat(code, sep = "\n")
      cat("\n")
    }
    out <- c(out, paste0("# ", head), code, "")
  }
  if (copy) {
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      clipr::write_clip(out)
      message("Copied to clipboard.")
    } else {
      message("Clipboard not available. Install 'clipr' to use copy = TRUE.")
    }
  }
  invisible(out)
}
