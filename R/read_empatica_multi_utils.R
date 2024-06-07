aggregate_avro <- function(d, info, varnames) {

  ## Check overlaps and gaps

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

    sapply(
      varnames,
      function(varname, intervals, d) {

        x <- intervals[[varname]]

        if (is.null(x)) return(NULL)

        overlaps <- mapply(
          lubridate::int_overlaps,
          int1 = x[2:length(x) - 1],
          int2 = x[2:length(x)]
        )

        if (any(overlaps)) warning(
          "Detected overlaps in the ", varname, " elements.",
          "\n(Aggregated data frame may contain errors.)\n",
          call. = FALSE
        )

        gaps <- mapply(
          function(X, Y) {
            difftime(
              lubridate::int_start(Y),
              lubridate::int_end(X)
            ) %>%
            as.numeric("secs") %>%
            round(3)
          },
          X = x[2:length(x) - 1],
          Y = x[2:length(x)]
        )

        excessive_threshold <-

          if ("samp_freq" %in% names(attributes(d[[1]][[varname]]))) {

            sapply(d, `[`, varname) %>%
            sapply(attr, "samp_freq") %>%
            {1 / .} %>%
            min(.) %>%
            round(3)

          } else if (!is.data.frame(d[[1]][[varname]])) {

            if (!inherits(d[[1]][[varname]], "POSIXt")) stop(
              "Expecting ", sQuote(varname), " to be a POSIX variable,",
              " but its class is ", sQuote(class(d[[1]][[varname]])[1]),
              call. = FALSE
            )

            lapply(d, `[[`, varname) %>%
            sapply(function(x)
              diff(x) %>%
              as.numeric("secs") %>%
              round(3) %>%
              max(.)
            ) %>%
            max(.)

          } else {

            lapply(d, `[[`, varname) %>%
            lapply(`[[`, "timestamp") %>%
            sapply(function(x)
              diff(x) %>%
              as.numeric("secs") %>%
              round(3) %>%
              unique(.)
            ) %>%
            max(.)

          }

        if (excessive_threshold < 1) excessive_threshold %<>% ceiling(.)
        if (all(gaps > 1)) gaps %<>% round(0)

        is_excessive <- gaps > excessive_threshold

        if (any(is_excessive)) {
          which(is_excessive) %>%
          {paste0(
            lubridate::int_end(x[.]),
            " -- ",
            lubridate::int_start(x[. + 1]),
            " (", round(gaps[.], 3), "-sec gap versus ",
            excessive_threshold, "-sec threshold)"
          )} %>%
          {warning(
            "Detected ", sum(is_excessive), " gap(s) in the ",
            varname, " data:\n---> ", paste(., collapse = "\n     "),
            "\n\nBe alert that no missing values are imputed to fill the gap(s) or",
            "\nstandardize the sampling rate reflected in the data frame\n",
            call. = FALSE
          )}
        }

      },
      intervals = intervals,
      d = d
    )

  ## Aggregate results

    sapply(
      varnames,
      function(x, d) {
        if (is.data.frame(d[[1]][[x]])) {
          purrr::map_df(d, `[[`, x, .id = "file_number") %>%
          dplyr::relocate(file_number)
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
