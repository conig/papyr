#' open_citation
#'
#' Write a bibtex citation to a temporary file and open it. This may assist with loading R references into Zotero
#' @param package the package name
#' @export

open_citation = function(package = NULL){
  citation <- citation(package)
  bib <- utils::toBibtex(citation)
  path <- tempfile(fileext = ".bib")
  write(bib, path)
  shell.exec(path)
}
