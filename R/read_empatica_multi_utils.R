aggregate_avro <- function(d, info, varnames) {

  result <-
    length(varnames) %>%
    vector("list", .) %>%
    stats::setNames(varnames)

  for (varname in varnames) {

    # varname <- varnames[1]

    if (length(d[[1]][[varname]]) == 0) {
      result[[varname]] <- d[[1]][[varname]]
      next
    }

    x <- lapply(d, `[[`, varname)

    is_df <- is.data.frame(x[[1]])

    intervals <-
      if (is_df) {
        lapply(x, `[[`, "timestamp") %>%
        item_interval(.)
      } else {
        item_interval(x)
      }

    overlaps <- mapply(
      lubridate::int_overlaps,
      int1 = intervals[2:length(intervals) - 1],
      int2 = intervals[2:length(intervals)]
    )

    if (any(overlaps)) {
      warning("Detected overlaps among the ", varname, " files", call. = FALSE)
      browser()
    }

    gaps <- mapply(
      function(X, Y) {
        difftime(lubridate::int_start(Y), lubridate::int_end(X)) %>%
        as.numeric("secs")
      },
      X = intervals[2:length(intervals) - 1],
      Y = intervals[2:length(intervals)]
    )

    max_gap <-
      sapply(x, attr, "samp_freq") %>%
      {1 / .} %>%
      min(.)

    if (any(gaps > max_gap)) {

    }

  }
  intervals <- sapply(
    varnames,
    function(x, d) {
      if (length(d[[1]][[x]]) == 0) {
        NULL
      } else if (is.data.frame(d[[1]][[x]])) {
        lapply(d, `[[`, x) %>%
        lapply(`[[`, "timestamp") %>%
        item_interval(.)
      } else {
        lapply(d, `[[`, x) %>%
        item_interval(.)
      }
    },
    d = d,
    simplify = FALSE
  )

  test <- sapply(
    varnames,
    function(x, d) {
      if (is.data.frame(d[[1]][[x]])) {
        purrr::map_df(d, `[[`, x)
      } else {
        lapply(d, `[[`, x) %>%
        do.call(c, .)
      }
    },
    d = d,
    simplify = FALSE
  )

}


item_interval <- function(x) {

  stopifnot(is.list(x))

  lapply(x, range) %>%
  lapply(function(y) {
    stopifnot(inherits(y, "POSIXt"), length(y) == 2, which.min(y) == 1)
    lubridate::interval(y[1], y[2])
  }) %>%
  do.call(c, .)

}
