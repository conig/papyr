#' save_csl
#'
#' Writes a csl to file and returns its path
#' @param style the name of the csl style
#' @export


save_csl = function(style){
  requireNamespace("seasl", quietly = TRUE)
  origin <- seasl::csl_styles(style)
  destination = glue::glue("reference_styles/{style}.csl")
  if(!dir.exists("reference_styles")){
    dir.create("reference_styles")
  }

  if(file.exists(destination)){
    res <- select.list(choices = c("overwrite","cancel"), title = "File exists, overwrite?")
    if(res == "cancel") return(destination)
  }

  file.copy(origin, destination, overwrite = TRUE)
  destination
}
