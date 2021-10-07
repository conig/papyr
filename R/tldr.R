#' brief
#'
#' Summarises text based on desired number of sentences
#' @param x character including all text
#' @param n number of sentences desired
#' @export brief

brief = function(x,n = 3){

  requireNamespace("LSAfun", quietly = TRUE)
  requireNamespace("lexRankr", quietly = TRUE)

  top = lexRankr::lexRank(x,n)
  order_of_appearance = order(as.integer(gsub("_","",top$sentenceId)))
  ordered_top = top[order_of_appearance, "sentence"]

  # standout_sentence <- trimws(LSAfun::genericSummary(x, k = 1))
  sentences <- unique(c(ordered_top))

  return(cat(paste(sentences, collapse = "\n")))

}
