# I don't think this is needed anymore
# ====
# ggtitles <- function(.data,title='Plot',subtitle=NULL,caption=NULL){
#   out <- .data %>%
#     mutate('plot'=map(plot,function(plot,theme){
#       plot+
#         labs(title=title,subtitle=subtitle,caption=caption)
#     },theme=theme)
#     )
#   return(out)
# }
#
