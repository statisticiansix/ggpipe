#' Add a theme to all plots
#'
#' @param .data Data frame with nested data column
#' @param theme The ggplot2 theme you want to add
#' @description This function adds a theme to the plots contained within the column \code{plot}

ggtheme <- function(.data,theme){
  out <- .data %>%
    mutate('plot'=map(plot,function(plot,theme){
      plot+
        theme
    },theme=theme)
    )
  return(out)
}
