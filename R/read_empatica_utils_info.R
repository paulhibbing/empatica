# Piecewise assembly ------------------------------------------------------

  extract_enrollment_info <- function(info) {

    stopifnot(is_info(info))

    enrollment_varnames <- select_info_varnames(info$info, "enrollment")

    info$info[enrollment_varnames] %>%
    purrr::map_df(
      ~
        c(.x, stringsAsFactors = FALSE) %>%
        do.call(data.frame, .)
    ) %>%
    as_info(enrollment_varnames, info$info)

  }


  extract_device_info <- function(info) {

    stopifnot(is_info(info))

    device_varnames <- select_info_varnames(info$info, "device")

    info$info[device_varnames] %>%
    c(stringsAsFactors = FALSE) %>%
    do.call(data.frame, .) %>%
    merge(info$collected, .) %>%
    as_info(device_varnames, info$info)

  }


  extract_version_info <- function(info) {

    stopifnot(is_info(info))

    version_varnames <- select_info_varnames(info$info, "Version$")

    info$info[version_varnames] %>%
    purrr::map_chr(
      ~ c(.x, sep = ".") %>%
        do.call(paste, .)
    ) %>%
    structure(info$collected, versions = .) %>%
    as_info(version_varnames, info$info)

  }


  extract_timezone_info <- function(info) {

    stopifnot(is_info(info))

    timezone_varnames <- select_info_varnames(info$info, "timezone$")

    info$info[timezone_varnames] %>%
    c(use.names = FALSE) %>%
    do.call(c, .) %>%
    structure(info$collected, timezone = .) %>%
    as_info(timezone_varnames, info$info)

  }


# Helper functions --------------------------------------------------------

  select_info_varnames <- function(info, pattern) {

    result <-
      names(info) %>%
      grep(pattern, ., value = TRUE)

    if (!length(result)) stop(
      "Unable to find schema variables",
      " matching the following pattern: ",
      pattern, call. = FALSE
    )

    result

  }


  trim_info <- function(info, varnames) {

    names(info) %>%
    setdiff(varnames) %>%
    setdiff("collected") %>%
    info[.]

  }


  as_info <- function(result, varnames, info) {

    list(
      collected = result,
      info = trim_info(info, varnames)
    )

  }


  is_info <- function(info) {

    stopifnot(
      is.list(info),
      length(info) == 2,
      setequal(names(info), c("collected", "info"))
    )

    TRUE

  }


  final_info <- function(info) {

    stopifnot(
      is_info(info),
      length(info$info) == 0
    )

    info$collected

  }

