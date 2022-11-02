#' most_freq
#'
#' Returns most frequent value
#' @param x a vector
#' @param na.rm bool. Should missing values be removed?
#' @export

most_freq <- function(x, na.rm = TRUE){
  if(!na.rm & any(is.na(x))){
    return(NA)
  }

  if(any(is.na(x))){
    x <- na.omit(x)
  }

  u_x <- unique(x)
  u_x[which.max(tabulate(match(x, u_x)))]

}

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

#' digit_transformer
#'
#' A digit transformer for glue::glue
#' @export

digit_transformer <- function(text, envir){
 digits(eval(parse(text = text, keep.source = FALSE), envir),2)
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
#' @param na.rm a bool.
#' @param pattern a string to supply to glue. Can use the values median, IQR, Q.25, or Q.75 (the latter being the 25th and 75th quantiles)
#' @export m_iqr


m_iqr <- function(x, round = 2, na.rm =T, pattern = "{median} (IQR = {IQR})"){

  median = digits(median(x,na.rm=na.rm), round)

  IQR = IQR(x,na.rm=na.rm) |>  digits(round)

    quant <- stats::quantile(x, na.rm = na.rm)
    Q.25 <- stats::quantile(x, na.rm = na.rm, probs = .25) |>
      digits(round)
    Q.75 <- stats::quantile(x, na.rm = na.rm, probs = .75) |>
      digits(round)

   glue::glue(pattern)

}

#' m_sd
#'
#' Print mean and sd
#' @param x vector
#' @param na.rm logical, should missing values be removed?
#' @param digits number of digits to round to
#' @param pattern glue pattern mean -> m, std deviation -> sd
#' @export

m_sd <- function(x ,na.rm = TRUE, digits = 2 , pattern = "{m} (SD = {sd})"){

  m <- digits(mean(x, na.rm = na.rm), n = digits)
  sd <- digits(sd(x, na.rm = na.rm) , n = digits)
  glue::glue(pattern)

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
round_p <-
  function(p,
           n = 3,
           stars = c(),
           leading.zero = T,
           apa_threshold = 0.001,
           simplify = .1) {
    rounded = digits(p, n)
    sapply(seq_along(rounded), function(x) {
      if (!is.na(rounded[x])) {
        original = p[x]
        r_original = rounded[x]
        r = rounded[x]

        if (as.numeric(r) == 0) {
          r = strsplit(r, split = "")[[1]]
          r[length(r)] = 1
          r = paste(r, collapse = "")
        }

        #  add stars --------------
        stars_to_add = c()
        if (!is.null(stars)) {
          stars_to_add = lapply(stars, function(s) {
            if (as.numeric(original) < s) {
              return("*")
            } else{
              return(NA)
            }
          }) %>% unlist %>%
            stats::na.omit() %>%
            paste(collapse = "")

        }

        if (r_original < as.numeric(r)) {
          r = paste0("< ", r)
        }

        if (original < apa_threshold) {
          r = paste0("< ", apa_threshold)
        }

        if (original >= simplify) {
          r = digits(original, 2)
        }

        if (!leading.zero) {
          r <- gsub("0\\.", ".", r)
        }

        r = paste0(r, stars_to_add)

        r

      } else{
        NA
      }
    })

  }

#' print_t
#'
#' print the contents of a t.test in APA format
#' @param x the t.test --- an object of class "htest"
#' @export print_t

print_t = function(x, round = 2, pattern = "$t$ = {t}, $df$ = {df}; $p$ = {p}"){

  t = digits(x$statistic, round)
  df = round(x$parameter, round)
  p = round_p(x$p.value)

glue::glue(pattern)

}

#' rINT
#'
#' Inverse-normal transformation
#' @param x vector
#' @details https://www.biostars.org/p/80597/
#' @export

rINT <- function(x) qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x)))
