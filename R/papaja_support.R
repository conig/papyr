#' papaja_affiliation
#'
#' Papaja affiliations
#' @param researchers list, name, reference
#' @param affiliations list, reference, affiliation string
#' @param corresponding character, name (must match researcher name perfectly)
#' @param corres_address character
#' @param corres_email character
#' @param return_clipboard if TRUE results are not printer but sent to the clipboard
#' @export
#' @examples
#' library(papyr)
#' papaja_affiliation(
#'  researchers = list(
#'    "James H. Conigrave" = c("aff1"),
#'    "Other person" = c("aff3", "aff2")
#'  ),
#'  affiliations = list(
#'    "aff1" = "Affiliation 1",
#'    "aff2" = "Affiliation 2",
#'    "aff3" = "Affiliation 3"
#'  ),
#'  corresponding = "James H. Conigrave",
#'  corres_address = "Drug Health Services, Level 6, King George V Building, Royal Prince Alfred Hospital, Camperdown NSW 2050, AUSTRALIA",
#'  corres_email = "gmail.com",
#'  return_clipboard = FALSE
#')

papaja_affiliation <- function(researchers, affiliations, corresponding, corres_address, corres_email, return_clipboard = TRUE){

  if(any(!unlist(researchers) %in% names(affiliations))) stop("Some affiliation codes listed for researchers are not listed under affiliations")
  if(!corresponding %in% names(researchers)) stop("The corresponding author is not listed as a researcher")

  affil_order <- unique(unlist(researchers))
  affil_numeric <- seq_along(affil_order)
  names(affil_numeric) <- affil_order
  affil_content <- affiliations[affil_order]


  affils <- paste(glue::glue('  - id            : "{affil_numeric}"
    institution   : "{affil_content}"', .trim = FALSE),collapse = "\n")

  authors <- sapply(seq_along(researchers), function(i){
    i_affil = paste(affil_numeric[affil_order %in% unlist(researchers[i])],collapse = ",")

    if(names(researchers)[i] == corresponding){
      extra <- as.character(glue::glue("    corresponding : yes
    address       : \"{corres_address}\"
    email         : \"{corres_email}\"\n", .trim = FALSE))
    }else{
      extra <- ""
    }

    glue::glue("  - name          : \"{names(researchers)[i]}\"
    affiliation   : \"{i_affil}\"\n{extra}", .trim = FALSE)


  })
  authors <- paste(authors, collapse = "\n")

  out <- glue::glue("author:
            {authors}
            affiliation:
            {affils}")

  if(return_clipboard){
  clipr::write_clip(out)
  return(cat(crayon::blue("<content sent to clipboard>")))
  }

  out

}
