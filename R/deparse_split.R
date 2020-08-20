#' deparse_split

deparse_split = function(x, y) {
  call = match.call()
  df <- with(x, split(x, eval(call$y)))
  lapply(df, function(x) {
    with(x, subset(x, select = -eval(call$y)))
  })
}
