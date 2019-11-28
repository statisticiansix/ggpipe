ggpipe <- function(.data,nested=FALSE){
  if(!nested){
    out <- .data %>%
      nest(data=colnames(.data)[!colnames(.data)%in%dplyr::group_vars(.data)])%>%
      mutate('plot'=map(data,function(x)ggplot(x))
      )
  }else{
    out <- .data %>%
      mutate('plot'=map(data,function(x)ggplot(x))
      )
  }

  return(out)
}
