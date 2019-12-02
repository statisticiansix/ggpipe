#' Create a new ggplot
#'
#' @param .data Data frame with nested data column
#' @param data_column Column where the nested data is located and the default data to use within ggplot
#'

ggpipe <- function(.data,data_column=data,...){
  # out <- .data %>%
  #     mutate(!!sym(quo_name(enquo(plot_column))):=map(!!sym(quo_name(enquo(data_column))),function(x,additionalArgs){
  #       exec(ggplot,data=x,!!!additionalArgs)
  #       }))
  #
  additionalArgs <<- as.list(match.call(expand.dots = FALSE))[['...']]

  out <- .data %>%
    mutate('plot'=map(!!sym(quo_name(enquo(data_column))),function(x,additionalArgs){
      exec(ggplot,data=x,!!!additionalArgs)
    },additionalArgs=additionalArgs))
  return(out)
}
