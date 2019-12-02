#' Create a new ggplot
#'
#' @param .data Data frame with nested data column
#' @param data_column Column where the nested data is located and the default data to use within \code{ggplot}
#' @param ... Other arguments passed onto \code{ggplot}
#' @description "ggpipe()" creates a ggplot object in the column \code{plot} within the piped data frame. Unlike \code{ggplot} it requires a set of default data defined under \code{data_column}.
#' @examples # Let us first recreate the examples within ?ggplot
#' =====
#' Generate some sample data, then compute mean and standard deviation
#' in each group
#' df <- data.frame(
#'   gp = factor(rep(letters[1:3], each = 10)),
#'   y = rnorm(30)
#' )
#'
#' ds <- df %>%
#'   group_by(gp)%>%
#'   summarise(mean=mean(y),
#'             sd=sd(y))
#'
#' data <- bind_cols(df %>% nest('df'=everything()),
#'                   ds %>% nest('ds'=everything()))
#'
#' # The summary data frame ds is used to plot larger red points on top
#' # of the raw data. Note that we don't need to supply data or mapping
#' # in each layer because the defaults from ggplot() are used.
#' example1 <- data %>%
#'   ggpipe(df,aes(gp,y)) %>%
#'   gglayer(geom_point) %>%
#'   gglayer(geom_point,data=ds,aes(y=mean),colour='red',size=3)
#'
#' example1$plot
#'
#' # Same plot as above, declaring only the data frame in ggplot().
#' # Note how the x and y aesthetics must now be declared in
#' # each geom_point() layer.
#' example2 <- data %>%
#'   ggpipe(df) %>%
#'   gglayer(geom_point,aes(gp,y)) %>%
#'   gglayer(geom_point,data=ds,aes(gp,mean),colour='red',size=3)
#'
#' example2$plot
#'
#' # Within ggpipe we cannot fully specify the plot in each layer, we
#' # currently need to define a default data set within the plot initiation.
#' # Apart from that we can recreate the final example from ?ggplot
#'
#' example3 <- data %>%
#'   ggpipe(df) %>%
#'   gglayer(geom_point,aes(gp,y)) %>%
#'   gglayer(geom_point,data=ds,aes(gp,mean),colour='red',size=3)%>%
#'   gglayer(geom_errorbar, data = ds,
#'           aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
#'           colour = 'red',
#'           width = 0.4
#'   )
#'
#' # We will now demonstrate some of the ways in which we can use the ggpipe
#' # to our advantage when creating plots, particularly with respect to groups.
#'
#' # Let us start by creating a grouped set of data similar to the one above
#' data <- data.frame(
#'   gp = rep(rep(letters[1:3], each = 10),3),
#'   set = factor(rep(LETTERS[1:3], each = 30)),
#'   y = rnorm(90)
#' ) %>%
#'   nest('df'=-set)%>%
#'   mutate('ds'=map(df,function(x){
#'     x %>%
#'       group_by(gp)%>%
#'       summarise(mean=mean(y),
#'                 sd=sd(y))
#'   }))
#'
#' # Let's recreate the plot from above for each of our sets
#'
#' example4 <- data %>%
#'   ggpipe(df) %>%
#'   gglayer(geom_point,aes(gp,y)) %>%
#'   gglayer(geom_point,data=ds,aes(gp,mean),colour='red',size=3)%>%
#'   gglayer(geom_errorbar, data = ds,
#'           aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
#'           colour = 'red',
#'           width = 0.4
#'   )
#'
#' example4$plot
#'
#' # As we have 3 similar plots it is difficult to tell them apart. We can use
#' # gglayer to define titles for each plot, however first we need to ensure the
#' # data frame is grouped - if the data frame is not grouped then gglayer will
#' # produce an error.
#' #
#' # We have defined a column "title" within the main (unnested) part of the
#' # data frame and we can call this within "gglayer" to produce a title.
#'
#' example5 <- example4 %>%
#'   group_by(set)%>%
#'   mutate('title'=sprintf('Plot of %s',set))%>%
#'   gglayer(labs,title=title)
#'
#' example5$plot
#'
#' # We can also do the same with colours, again ensuring that the data table
#' # is grouped
#' # N.B. ggpipe will currently overwrite any column called plot, so if you
#' # want to keep the plot it is best to use "rename" to preserve plots
#' example6 <- example5 %>%
#'   mutate('colour'=case_when(set=='A'~'red',
#'                             set=='B'~'green',
#'                             set=='C'~'blue'))%>%
#'   ggpipe(df) %>%
#'   gglayer(geom_point,aes(gp,y)) %>%
#'   gglayer(geom_point,data=ds,aes(gp,mean),colour=colour,size=3)%>%
#'   gglayer(geom_errorbar, data = ds,
#'           aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
#'           colour = colour,
#'           width = 0.4
#'   )%>%
#'   gglayer(labs,title=title)
#'
#' example6$plot
#'

ggpipe <- function(.data,data_column=data,...){

  additionalArgs <<- as.list(match.call(expand.dots = FALSE))[['...']]

  out <- .data %>%
    mutate('plot'=map(!!sym(quo_name(enquo(data_column))),function(x,additionalArgs){
      exec(ggplot,data=x,!!!additionalArgs)
    },additionalArgs=additionalArgs))
  return(out)
}

