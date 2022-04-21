#'safe_trackdown_upload
#'
#' @param file file path to rmd to upload
#' @param ... other argumetns passed to trackdown::upload_file
#' @export

safe_trackdown_upload <- function(file, ...){

  requireNamespace("trackdown", quietly = TRUE)

  tempfile <- tempfile(fileext = ".rmd")
  cat(crayon::blue("copying rmd to temp location...\n"))
  file.copy(file, tempfile)

  cat(crayon::blue("attempting to upload to googledrive...\n"))
  trackdown::upload_file(file = tempfile, ...)

}
