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
        length(d["timestampStart"]) != 1,
        length(d["samplingFrequency"]) != 1
      )) {
        next
      }

      increment <- 1/d[[x]][["samplingFrequency"]]

      d[[x]] %<>% remove_frame_variable("samplingFrequency")

      start <- d[[x]][["timestampStart"]]

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

      d[[x]] %<>%
        dplyr::mutate(
          timestamp =
            {1:dplyr::n()} %>%
            {. - 1} %>%
            {increment * 1} %>%
            {start + .} %>%
            {. * 10^-6} %>%
            {. + tz} %>%
            as.POSIXct(tz = "UTC", origin = "1970-01-01")
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

    if (!"imu_params" %in% names(attributes(d$accelerometer))) {
      warning(
        "No IMU parameters found for accelerometer data --",
        " Returning unscaled values", call. = FALSE
      )
      return(d)
    }

    ## APPLY SCALING
    attr(d$accelerometer, "imu_params")

    ## RETURN
    d

  }
