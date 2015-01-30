#' Plot SSB biomass
#'
#'@param atl Atlantis output returned from read_atlantis()
#'@param use.log Should a log transformation be used?
#'@param save.pdf Should a PDF be saved?
#'@param funcode Which functional groups to plot? Vector containing the Atlantis codes
#'@export

ssb_plot <- function(atl, use.log = TRUE, save.pdf = FALSE, funcode = NULL, ...){
  type <- select(atl$fg, variable = Code, InvertType)
  long_form <- melt(atl$ssb, id.vars = "Time")
  long_form <- left_join(long_form, type)
  if(exists("funcode")){
    long_form <- filter(long_form, variable %in% funcode)
  }
  if(use.log){
    g0 <- ggplot(aes(x = Time, y = log(value), color = InvertType), data = long_form) + geom_line() + facet_wrap(~variable) + theme(legend.position="bottom") + theme(legend.title=element_blank(), legend.position="bottom") + labs(y="log(mg N)", x = "Time")
  } else{
    g0 <- ggplot(aes(x = Time, y = value, color = InvertType), data = long_form) + geom_line() + facet_wrap(~variable) + theme(legend.title=element_blank(), legend.position="bottom") + labs(y="mg N", x = "Time")
  }
  if(save.pdf){
    pdf(file = "ssb_plot.pdf", width = 16, height = 10)
    print(g0)
    dev.off()
  }
  print(g0)
} 