#' open_citation
#'
#' Write a bibtex citation to a temporary file and open it. This may assist with loading R references into Zotero
#' @param citation citation object to open with zotero
#' @param pacakge the package name
#' @export

open_citation = function(citation = NULL, package = NULL){
  if(!is.null(package)) citation <- citation(package)
  bib <- utils::toBibtex(citation)
  path <- tempfile(fileext = ".bib")
  write(bib, path)
  shell.exec(path)
}
