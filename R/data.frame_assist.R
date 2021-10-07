#' find_vars
#'
#' Find vars in a dataframe
#' @param data data.frame
#' @param x regex search string
#' @param ... other arguments to regex
#' @export


find_vars <- function(data, x,...){

  names(data)[grepl(x,names(data), ...)]

}
