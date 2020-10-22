#' to_rowhead
#'
#' Takes a frist column of a table and converts to a rowheader
#' @param data the table
#' @param x the columnt to convert to a rowheader
#' @param italics should row headers be italicized?
#' @export to_rowhead

to_rowhead = function(data, x, italics = FALSE) {
  x = tidyselect::vars_select(colnames(data), {{x}})
  row_head = as.character(unlist(data[, x]))
  new_data = data[, !names(data) %in% x]
  new_data$indent_ = T

  new_head = lapply(seq_along(row_head), function(i) {
    new_head = T
    i = unlist(i)
    if (i > 1) {
      if (row_head[[i]] == row_head[[i - 1]]) {
        new_head = F
      }
    }
    return(new_head)
  })

  new_head = unlist(new_head)

  table_out = lapply(seq_along(new_head), function(i) {
    if (new_head[i]) {
      new_row = new_data[i,]
      new_row[, 1] = unlist(data[i, x])

      if(italics){
        new_row[,1] = paste0("*",new_row[,1],"*")
      }

      new_row[, 2:ncol(new_row)] = ""
      new_row$indent_ = F

      return(rbind(new_row, new_data[i,]))

    } else{
      return(new_data[i,])
    }
  })

  return(do.call(rbind, table_out))

}

#' conv_md_latex
#'
#' Convert markdown to latex
#' @param x data.frame

conv_md_latex <- function(x){
  markdown_to_latex <- function(x) {
    while(grepl("\\*\\*.*\\*\\*", x)){ # while enclosed double asterisks remain
      x <- sub("(\\*\\*)", "\\\\textbf{", x)
      x <- sub("(\\*\\*)", "}", x)
    }
    while (grepl("\\*.*\\*", x)) { # while enclosed single asterisks remain
      x <- sub("(\\*)", "\\\\emph{", x)
      x <- sub("(\\*)", "}", x)
    }
    x = tibble::tibble(x)
    x
  }

  names(x) = sapply(names(x), markdown_to_latex)
  x <- apply(x, c(1,2), markdown_to_latex)
  x
}
