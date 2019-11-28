gglayer <- function(.data,layer_use,...){
  additionalArgs <- as.list(match.call())[-c(1:3)]

  out <- .data %>%
    mutate('plot'=map(plot,function(plot,layer_use,additionalArgs){
      plot+
        do.call(layer_use,additionalArgs)
    },layer_use=layer_use,additionalArgs=additionalArgs)
    )
  return(out)
}
