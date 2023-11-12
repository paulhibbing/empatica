read_empatica_multi <- function(
  dirname, pattern, verbose = FALSE,
  return_filenames = FALSE, disaggregate = FALSE
) {

  # dirname <- "data-raw/sample_data"
  # pattern <- "1-1-TEST"
  # verbose <- TRUE
  # return_filenames <- FALSE

  ## Get set up and run some checks

    stopifnot(dir.exists(dirname))

    files <- list.files(dirname, pattern, full.names = TRUE)

    if (return_filenames) return(files)

    is_avro <- grepl(".avro$", files)

    if (length(files) == 0) stop(
      "`pattern` did not match any files",
      call. = FALSE
    )

    if (!all(is_avro)) stop(
      "`pattern` matched the following non-avro files:\n  ",
      paste(files[!is_avro], collapse = "\n  "),
      call. = FALSE
    )

    if (verbose) {
      timer <- PAutilities::manage_procedure(
        "Start",
        "\nProcessing the following", length(files), "file(s):\n ",
        paste( basename(files),  collapse = "\n  " ),
        "\n",
        verbose = verbose
      )
    }

  ## Read the files

    d <-
      files %>%
      lapply(
      function(x) {
        cat(".")
        read_empatica_avro(x, verbose = FALSE) ## Avoid printing conflicts
      })

    if (verbose) cat("\n")

  ## Check the resulting variables names

    varnames <-
      lapply(d, names) %>%
      unique(.)

    if (length(varnames) != 1) {
      warning(
        "Returning disaggregated list of file data",
        " (variable names are not identical across files)",
        call. = FALSE
      )
      return(d)
    } else {
      varnames %<>% unlist(.)
    }

  info <-
    lapply(d, attr, "info") %>%
    unique(.)

  if (length(info) != 1) {
    warning(
      "Returning disaggregated list of file data",
      " (`info` attribute is not identical across files)",
      call. = FALSE
    )
    return(structure(d, info = info))
  }

  if (length(info) == 1) {
    warning(
      "Only one file detected -- No further aggregation needed",
      call. = FALSE
    )
    return(d[[1]])
  }

  aggregate_avro(d, info, varnames)

}
