#' p_to_OR
#'
#' Convert two probabilities to an odds ratio
#' @export

p_to_OR <- function(p1,p2){

  odd1 <- p1/(1-p1)
  odd2 <- p2/(1-p2)
  odd1/odd2

}
