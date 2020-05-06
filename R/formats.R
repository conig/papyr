#' digits
#'
#' Allows the user to specify the exact number of digits
#' @param x a numeric
#' @param n a numeric. The number of digits to round to.
#' @export digits

digits = function(x, n = 2) {
  x = round(x, n)
  x[] = sapply(x, function(i) {
         ifelse(!is.na(i), trimws(format(round(as.numeric(as.character(i)), n), nsmall = n)),NA)
      })
  return(x)
}

#' glue_bracket
#'
#' @param x a character or numeric
#' @param ... a function. If provided will transform effects and confidence intervals.
#' @param round a scalar.
#' @param brackets a vector with character strings to replace left and right brackets.
#' @param collapse a string. separator for numbers within brackets
#' @export glue_bracket

glue_bracket = function(x, ..., round = 2, brackets = c("(",")"), collapse = ", ") {
  # warnings and errors -----------------------------------
  if(length(brackets)!=2) stop("brackets must be length 2")

  # grab extra numbers ------------------------------------
  others = list(...) %>%
    do.call(cbind,.)

  # round if requested ------------------------------------
  if(!is.null(round)){
    x = digits(x,round)
    others = digits(others, round)
  }

  together = as.character(c(x, others))
  if(all(is.na(together))) return(NA) # if everything supplied is NA, just return NA.

  if(!is.null(dim(others))){
  bracks = apply(others,1, function(x) paste(x, collapse = collapse))
  }

  out = lapply(seq_along(bracks), function(i) paste0(x[[i]], " ", brackets[1],bracks[[i]], brackets[2])) %>%
    unlist
  return(out)
}

#' m_iqr
#'
#' takes in a numeric vector, calculates a median and iqr and returns as text.
#' @param x a character vector
#' @param round a numeric.
#' @param quantiles a bool.
#' @param na.rm a bool.
#' @param in.brack a bool.
#' @export m_iqr


m_iqr = function(x,round = 1,quantiles = F,na.rm =T, in.brack = F){
  median = digits(median(x,na.rm=na.rm), round)

  if(quantiles == F){
  IQR = IQR(x,na.rm=na.rm) %>% digits(round)
    if(!in.brack){
  text = paste0(median," (IQR = ",IQR,")")
    }else{
     text = paste0("(median = ",median,", IQR = ",IQR,")")
    }

  }else{
    quant = stats::quantile(x, na.rm = na.rm)
    q.25 = quant[2] %>% digits(round)
    q.75 = quant[4] %>% digits(round)

    if(!in.brack){
    text = paste0(median, " (IQR = ",q.25,", ",q.75,")")
    }else{
      text = paste0("(median = ",median, ", IQR = [",q.25,", ",q.75,"])")
    }
  }

  return(text)
}

#' round p
#'
#' Rounds p value to specified digits and uses less symbol if result it zero.
#' @param p a p value, or vector of p-values
#' @param n a numeric. The number of digits to round to.
#' @param stars a numeric vector, add a star every time p is less than a respective star
#' @param leading.zero a bool. If FALSE, leading zeros will be removed
#' @param apa_threshold output will indicate p value is less than this value regardless of rounding
#' @param simplify if greater or equal to this numeric, two decimal places will be used
#' @export round_p
round_p =  function(p, n = 3, stars = c(), leading.zero = T, apa_threshold = 0.001, simplify = .1){
  rounded = digits(p,n)
  lapply(seq_along(rounded), function(x){

    if(!is.na(rounded[x])){
    #message(x)
    original = p[x]
    r_original = rounded[x]
    r = rounded[x]

    if(as.numeric(r) == 0){
      r = strsplit(r,split="")[[1]]
      r[length(r)] = 1
      r = paste(r,collapse = "")
    }

    #  add stars --------------
    stars_to_add = c()
    if(!is.null(stars)){
     stars_to_add = lapply(stars,function(s){
       if(as.numeric(original) < s){
         return("*")
       }else{
         return(NA)
       }
      }) %>% unlist %>%
       stats::na.omit() %>%
       paste(collapse = "")

    }

    if(!leading.zero){
      r = sub("^(-)?0[.]",
              "\\1.", r)
    }

    if(r_original < as.numeric(r)){
      r = paste0("< ",r)
    }

    if(original < apa_threshold){
      r = paste0("< ", apa_threshold)
    }

    if(original >= simplify){
      r = digits(original, 2)
    }

    r = paste0(r,stars_to_add)

    return(r)

    }else{
      NA
    }
  }) %>% unlist

}

