#' Add a geom to all plots
#'
#' @param .data Data frame with nested data column
#' @param layer_use The ggplot2 geom that you want to add
#' @param ... Other arguments passed onto the geom specified in \code{layer_use}
#' @description This function adds a geom to the plots contained within the column \code{plot}.
#' @examples
#' eg <- mtcars %>%
#'   group_by(am) %>%
#'   nest('data'=-am)%>%
#'   mutate('colour'=ifelse(am==0,'red','purple'),
#'          'title'=sprintf('AM: %i',am),
#'          'subtitle'=sprintf('Data from %i different models',map_dbl(data,nrow)),
#'          'minlimit'=ifelse(am==0,-1000,-250),
#'          'maxlimit'=map_dbl(data,function(x)max(x$disp)))
#'
#' out <- eg %>%
#'   ggpipe("data") %>%
#'   gglayer(geom_point,aes(x=disp,y=hp),colour=colour)%>%
#'   gglayer(scale_x_continuous,limits=c(minlimit,maxlimit))%>%
#'   gglayer(labs,title=title)%>%
#'   gglayer(facet_wrap,cyl~.)%>%
#'   ggtheme(theme_bw())
#'
#' out$plot

gglayer <- function(.data,layer_use,...){
  additionalArgs <<- as.list(match.call(expand.dots = FALSE))[['...']]

  if("data"%in%names(additionalArgs)){
    updatedArgs <- additionalArgs[names(additionalArgs)!='data']
    additionalArgs$data = sym(quo_name(additionalArgs$data))
    out <- .data %>%
      mutate('plot'=map(plot,function(plot,data,layer_use,updatedArgs){
        plot+
          exec(layer_use,!!!updatedArgs,data=(!!additionalArgs$data)[[1]])
      },layer_use=layer_use,updatedArgs=updatedArgs)
      )
  }else{
    out <- .data %>%
      mutate('plot'=map(plot,function(plot,layer_use,additionalArgs){
        plot+
          exec(layer_use,!!!additionalArgs)
      },layer_use=layer_use,additionalArgs=additionalArgs)
      )
  }

  return(out)
}
