gglayer <- function(.data,layer_use,...){
  additionalArgs <<- as.list(match.call())[-c(1:3)]

  if("data"%in%names(additionalArgs)){
    updatedArgs <<- additionalArgs[names(additionalArgs)!='data']
    additionalArgs$data = sym(quo_name(additionalArgs$data))
    print(additionalArgs$data)
    out <- .data %>%
      mutate('plot'=map(plot,function(plot,data,layer_use,updatedArgs){
        plot+
          exec(layer_use,!!!updatedArgs,data=(!!additionalArgs$data)[[1]])
      },layer_use=layer_use,updatedArgs=updatedArgs)
      )
  }else{
    updated <<- additionalArgs
    out <- .data %>%
      mutate('plot'=map(plot,function(plot,layer_use,additionalArgs){
        plot+
          exec(layer_use,!!!additionalArgs)
      },layer_use=layer_use,additionalArgs=additionalArgs)
      )
  }
  #

  return(out)
}
