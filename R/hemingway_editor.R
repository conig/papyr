#' hemingway_editor
#'
#' Open the hemingway editor
#' @export hemingway_editor
hemingway_editor = function(){

#file = system.file("Hemingway Editor.html", package = "papyr")
rstudioapi::viewer("http://www.hemingwayapp.com/")

}
