#' plot_reg
#'
#' function
#' @param lm a linear model
#' @param x character, predictor
#' @param ... extra arguments passed to plot
#' @export

plot_reg <- function(lm, x = NULL, ...) {
  function_call = match.call()
  form = as.formula(lm$call$formula)
  outcome = as.character(form[2])
  args <- list(...)

  if (is.null(x)) {
    split = function(x, split) trimws(strsplit(x, split = split)[[1]][1])

    pred = split(as.character(form[3]), split = "\\+")
    pred =  split(pred, "\\*")
    pred =  split(pred, "\\:")
    pred =  split(pred, "\\-")

  } else{
    pred = function_call$x
  }

  call  <-
    list(formula = as.formula(glue::glue("{outcome} ~ {pred}")),
         data = lm$call$data)
  if(length(args) > 0){
  call = append(call, args)
  }

  coefs = coef(lm)

  intercept <- coefs[grepl("intercept", names(coefs), ignore.case = T)]
  intercept <- ifelse(length(intercept) == 0, 0 , intercept)
  slope = coefs[names(coefs) == pred]

  do.call(plot, call)

  abline(a = intercept, b = slope)

}
