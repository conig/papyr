#' introduction
#'
#' Scaffold an introduction
#' @param known What is currently known about the topic
#' @param unknown what is unknown?
#' @param objectives what are the objectives of the current study?
#' @export


introduction = function(known = "",
                        unknown = "",
                        objectives = ""){
  pattern = "

  <!-- what is known -->

  {known}

  <!-- what is unknown -->

  {unknown}

  <!-- what we are doing -->

  {objectives}
  "
  glue::glue(pattern)

}

#' discussion
#'
#' Scaffold a discussion section
#' @param key_findings Main outcome of the study
#' @param other_findings Secondary findings
#' @param context Compare to other findings
#' @param limitations What prevents firms conclusions being made?
#' @param so_what Why should anyone care about the findings?
#' @param conclusion restate main findings, final take-home message
#' @export


discussion = function(key_finding = "",
                      other_findings = "",
                      context = "",
                      limitations = "",
                      whats_next = "",
                      so_what = "",
                      conclusion = ""){

  pattern = "We found that {key_finding}. Additionally, we demonstrated that {other_findings}.

  {context}

  Our findings are limited as {limitations}

  {whats_next}

  {so_what}

  In conclusion, {key_finding}. {conclusion}
  "
  glue::glue(pattern)
}
