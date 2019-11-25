ggtheme <- function(.data,theme){
  out <- .data %>%
    mutate('plot'=map(plot,function(plot,theme){
      plot+
        theme
    },theme=theme)
    )
  return(out)
}
