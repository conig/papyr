#' frequency_plot
#'
#' Visualise the percentage of responses to likert questions
#' @param x data.frame containing only factors with identical levels
#' @param limit numeric. At what threshold should the percent label not be shown
#' @import ggplot2 data.table
#' @importFrom tidyr pivot_longer
#' @export

frequency_plot = function(x, limit = 10){

  test1 <- any(sapply(x, class) != "factor")
  levels <- lapply(x, levels)
  test2 <- !all(unlist(lapply(seq_along(levels), function(x) all.equal(levels[[x]], levels[[1]]))))

  if(test1 + test2 > 0) stop("all columns must be factors with the same levels")

  levels <- levels(unlist(x[,1]))

  x = tibble::tibble(x)

  pcs <- lapply(seq_along(x), function(i) {
    y = data.frame(prop.table(table(x[,i])) * 100)
    y$name = names(x)[i]
    y
  })

  dat <- data.table::rbindlist(pcs)
  names(dat)[1:2] = c("Response","pc")
  dat$Response = factor(dat$Response, levels = levels)

  dat$label = ifelse(
    dat$pc > limit, paste0(digits(dat$pc, 1), "%"),
    ""
  )

  ggplot(dat, aes(x = name, y = pc, fill = Response)) +
    geom_bar(stat = "identity", colour = "black") + coord_flip() +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5), size = 3.5) +
    theme_bw() +
    theme(text = element_text(family = "serif")) + labs (x = "", y = "Percent (%)", fill = "Response") +
    scale_fill_grey(start = 0.5,
                    end = 1)

}
