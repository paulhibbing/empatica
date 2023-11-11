empatica_time <- function(x, tz, e = 6) {

  {x * 10^-e} %>%
  {. + tz} %>%
  as.POSIXct(
    tz = "UTC",
    origin = "1970-01-01"
  )

}
