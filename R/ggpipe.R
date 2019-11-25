ggpipe <- function(.data){
  out <- .data %>%
    nest(data=colnames(.data)[!colnames(.data)%in%dplyr::group_vars(.data)])%>%
    mutate('plot'=map(data,function(x)ggplot(x))
    )
  return(out)
}
