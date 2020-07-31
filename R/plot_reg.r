#' plot_reg
#'
#' function
#' @param lm a linear model
#' @param x character, predictor
#' @export

plot_reg <- function(lm, x = NULL) {
function_call = match.call()
form = as.formula(lm$call$formula)
outcome = as.character(form[2])

if (is.null(x)) {

  split = function(x, split) trimws(strsplit(x, split = split)[[1]][1])

  pred = split(as.character(form[3]), split = "\\+")
  pred =  split(pred, "\\*")
  pred =  split(pred, "\\:")

} else{
  pred = function_call$x
}

call  <- list(formula = as.formula(glue::glue("{outcome} ~ {pred}")),
              data = lm$call$data)

coefs = coef(lm)

intercept <- coefs[[1]]
slope = coefs[names(coefs) == pred]

do.call(plot, call)

abline(a = intercept, b = slope)

}
