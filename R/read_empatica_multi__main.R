read_empatica_multi <- function(
  dirname, pattern, verbose = FALSE,
  return_filenames = FALSE, disaggregate = FALSE,
  exclude
) {

  # dirname <- "data-raw/sample_data"
  # pattern <- "1-1-TEST"
  # verbose <- TRUE
  # return_filenames <- FALSE
  # disaggregate <- FALSE

  ## Get set up and run some checks

    if (!dir.exists(dirname)) stop(
      dirname, " does not exist -- cannot search for avro",
      " files matching pattern ", pattern
    )

    files <- list.files(dirname, pattern, full.names = TRUE)

    if (return_filenames) return(files)

    if (length(files) == 0) stop(
      "pattern = ", dQuote(pattern), " did not match any files",
      call. = FALSE
    )

    if (length(files) == 1) {
      warning(
        "pattern = ", dQuote(pattern), " matched only one file",
        " -- Returning result from the plain `read_empatica_avro` function"
      )
      return(
        read_empatica_avro(f, verbose = FALSE) ## Avoid printing clashes
      )
    }

    is_avro <- grepl(".avro$", files)

    if (!all(is_avro)) stop(
      "The following file(s) are not avro files:\n  ",
      paste(basename(files)[!is_avro], collapse = "\n  "),
      call. = FALSE
    )

    timer <- PAutilities::manage_procedure(
      "Start",
      "\nProcessing the following", length(files), "file(s):\n\n ",
      paste( basename(files),  collapse = "\n  " ),
      "\n\n",
      verbose = verbose
    )

  ## Read the files

    d <-
      files %>%
      lapply(
      function(x, verbose) {
        if (verbose) cat(".")
        read_empatica_avro(x, verbose = FALSE) ## Avoid printing clashes
      }, verbose = verbose)

    if (verbose) cat("\n")

    if (disaggregate) {
      PAutilities::manage_procedure("End", timer = timer, verbose = verbose)
      return(d)
    }

  ## Check the resulting variable names

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

  if (!missing(exclude)) {

    if (!is.character(exclude)) stop(
      "Exclude must be a character vector of element names to exclude from",
      " aggregation (e.g., 'steps' or 'systolicPeaks'"
    )

    if (any(!exclude %in% varnames)) warning(
      "The following elements of 'exclude' do not correspond with",
      " element names in the raw data:\n  ",
      paste(setdiff(exclude, varnames), collapse = ", ")
    )

    varnames %<>% setdiff(exclude)

  }

  aggregate_avro(d, info, varnames) %>%
  structure(info = info[[1]]) %T>%
  {PAutilities::manage_procedure(
    "End", "\nProcess complete. Elapsed time",
    PAutilities::get_duration(timer), "minutes.\n"
  )}

}
