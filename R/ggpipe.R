ggpipe <- function(.data,column_name=data,nested=FALSE){
  column_name <- quo_name(enquo(column_name))
  print(column_name)
  if(!nested){
    out <- .data %>%
      nest(!!column_name:=colnames(.data)[!colnames(.data)%in%dplyr::group_vars(.data)])%>%
      mutate('plot'=map(!!sym(column_name),function(x)ggplot(x))
      )
  }else{
    out <- .data %>%
      mutate('plot'=map(!!sym(column_name),function(x)ggplot(x)))
  }

  return(out)
}
