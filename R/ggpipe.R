ggpipe <- function(.data,data_column=data){
  out <- .data %>%
      mutate('plot'=map(!!sym(quo_name(enquo(data_column))),function(x)ggplot(x)))
  return(out)
}
