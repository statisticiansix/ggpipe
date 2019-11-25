gglayer <- function(.data,layer_use,...){
  out <- .data %>%
    mutate('plot'=map(plot,function(plot,layer_use,...){
      plot+
        layer_use(...)
    },layer_use=layer_use,...)
    )
  return(out)
}
