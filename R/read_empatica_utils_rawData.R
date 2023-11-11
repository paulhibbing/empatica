# Initialize raw data -----------------------------------------------------

  create_frames <- function(d, info) {

    tz <-
      attr(info, "timezone") %>%
      unname(.)

    for (x in names(d)) {

      if (any(
        !exists("timestampStart", d[[x]]),
        !exists("samplingFrequency", d[[x]])
      )) {
        next
      }

      if (any(
        length(d[[x]][["timestampStart"]]) != 1,
        length(d[[x]][["samplingFrequency"]]) != 1
      )) {
        next
      }

      increment <- 1/d[[x]][["samplingFrequency"]]

      d[[x]] %<>% remove_frame_variable("samplingFrequency")

      start <- empatica_time(
        d[[x]][["timestampStart"]],
        tz
      )

      d[[x]] %<>% remove_frame_variable("timestampStart")

      imu_exists <- "imuParams" %in% names(d[[x]])

      if (imu_exists) {

        imu <-
          {d[[x]][["imuParams"]]} %>%
          c(stringsAsFactors = FALSE) %>%
          do.call(data.frame, .)

        d[[x]] %<>% remove_frame_variable("imuParams")

      }

      d[[x]] %<>%
        c(stringsAsFactors = FALSE) %>%
        do.call(data.frame, .)

      if (length(d[[x]]) == 0) {
        next
      }

      if (setequal(names(d[[x]]), "values")) {
        names(d[[x]]) <- x
      }

      d[[x]] %<>%
        dplyr::mutate(
          timestamp =
            {1:dplyr::n()} %>%
            {. - 1} %>%
            {increment * .} %>%
            {start + .}
        )  %>%
        dplyr::relocate(timestamp) %>%
        merge(info, .)

      if (imu_exists) {
        d[[x]] %<>% structure(., imu_params = imu)
      }

    }

    d

  }


  remove_frame_variable <- function(frame, variable) {

    names(frame) %>%
    setdiff(variable) %>%
    frame[.]

  }


# Format individual frames ------------------------------------------------

  format_accelerometer <- function(d) {

    if (!"accelerometer" %in% names(d)) return(d)

    if (length(d$accelerometer) == 0) return(d[names(d) != "accelerometer"])

    if (!"imu_params" %in% names(attributes(d$accelerometer))) {
      warning(
        "Returning accelerometer dataframe as-is",
        " (no IMU parameters found)",
        call. = FALSE
      )
      return(d)
    }

    if (!all(.accel_names %in% names(d$accelerometer))) {
      warning(
        "Returning accelerometer dataframe as-is",
        " (does not contain columns called 'x', 'y', and 'z')",
        call. = FALSE
      )
      return(d)
    }

    scale_factor <-
      attr(d$accelerometer, "imu_params") %T>%
      {stopifnot(
        setequal(names(.), .param_names),
        sign(.[.param_names]) == c(-1, 1, -1, 1)
      )} %>%
      {c(
        negative = .$digitalMin / .$physicalMin,
        positive = .$digitalMax / .$physicalMax
      )} %T>%
      {stopifnot(dplyr::n_distinct(.) == 1)} %>%
      unique(.)

    d$accelerometer %<>% dplyr::mutate(
      dplyr::across(
        dplyr::all_of(.accel_names),
        ~ .x / scale_factor
      )
    )

    d

  }


  format_tags <- function(d) {

    if (!"tags" %in% names(d)) return(d)

    if (length(d$tags) == 0) return(d[names(d) != "tags"])

    stopifnot(is.list(d$tags))

    if ( all(sapply(d$tags, length) == 0) ) return(d[names(d) != "tags"])

    d

  }


  format_peaks <- function(d, info) {

    if (!"systolicPeaks" %in% names(d)) return(d)

    if (length(d$systolicPeaks) == 0) return(d[names(d) != "systolicPeaks"])

    stopifnot(
      setequal(names(d$systolicPeaks), "peaksTimeNanos")
    )

    d$systolicPeaks %<>% {empatica_time(
      .$peaksTimeNanos,
      attr(info, "timezone"),
      e = 9
    )}

    d

  }
