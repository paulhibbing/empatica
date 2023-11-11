read_avro <- function(f) {

  paste("fastavro", f) %>%
  system(intern = TRUE) %>%
  paste(collapse = "") %>%
  jsonlite::fromJSON(.)

}
