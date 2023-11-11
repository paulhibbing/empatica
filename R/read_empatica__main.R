#' Read an Empatica raw data file
#'
#' @param f character. Path to the file
#'
#' @return a list of formatted data elements
#' @export
#'
#' @examples
#'
#' f <- system.file("extdata/test_file.avro", package = "empatica")
#' head( read_empatica_avro(f) )
#'
read_empatica_avro <- function(f) {

  ## Initial file checks

    if (!grepl("avro$", f)) stop("`f` must be a file with avro extension")

    stopifnot(file.exists(f))


  ## Attempt reading

    d <- try(
      read_avro(f),
      silent = TRUE
    )

    if (inherits(d, "try-error")) stop(
      "`read_avro` failed. Do you have Python and fastavro",
      " installed and configured? Are you using Windows?"
    )


  ## Begin parsing

    stopifnot("rawData" %in% names(d))

    info <-
      names(d) %>%
      setdiff("rawData") %>%
      d[.] %>%
      as_info(data.frame(), "", .)

    d %<>% .[["rawData"]]


  ## Collect attributes

    info <-
      extract_enrollment_info(info) %>%
      extract_device_info(.) %>%
      extract_version_info(.) %>%
      extract_timezone_info(.) %>%
      final_info(.)


  ## Assemble data

    d %<>%
      create_frames(info) %>%
      format_accelerometer(.) %>%
      structure(info = info)

}
