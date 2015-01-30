#' Atlantis Plots
#'
#' Provides a suite of Atlantis output plots
#' 
#' \code{atplot} provides a suite of plot for Atlantis plots. Presently, it is able to plot spawning stock biomass (\code{type = "ssb"}) or relative biomass (\code{type = "relbio"}). Both types of plots are available simulataneously by specifying \code{type = "all"}. However, no plots will be printed to the \code{R} console but instead will be saved directly as PDFs. 
#' 
#'@param atl Atlantis output returned from \code{read_atlantis()}
#'@param type \code{ssb}, \code{relbio}, or \code{all}. See details. 
#'@param use.log Should a log transformation be used?
#'@param save.pdf Should a PDF be saved?
#'@param funcode Which functional groups to plot? Vector containing the Atlantis codes.
#'
#'@export

atplot <- function(atl, type, use.log = TRUE, save.pdf = FALSE, funcode = NULL){
  
  codes <- select(atl$fg, variable = Code, InvertType)
  
  ## SSB plot
  if(type == "ssb" | type == "all"){
    long_form <- melt(atl$ssb, id.vars = "Time")
    long_form <- left_join(long_form, codes)
    long_form$variable <- factor(long_form$variable, levels = unique(long_form$variable))
    long_form$variable <- droplevels(long_form$variable)
    str(long_form)
    
    ## Check if user wants specific functional groups
   if(is.null(funcode) == FALSE){
     long_form <- filter(long_form, variable %in% funcode)
   }
    
    if(use.log){
      ssb <- ggplot(aes(x = Time, y = log(value), color = InvertType), data = long_form) + geom_line() + facet_wrap(~variable) + theme(legend.position="bottom") + theme(legend.title=element_blank(), legend.position="bottom") + labs(y="log(mg N)", x = "Time")
      } else{
        ssb <- ggplot(aes(x = Time, y = value, color = InvertType), data = long_form) + geom_line() + facet_wrap(~variable) + theme(legend.title=element_blank(), legend.position="bottom") + labs(y="mg N", x = "Time")
      }
    if(save.pdf){
      pdf(file = "ssb_plot.pdf", width = 16, height = 10)
      print(ssb)
      dev.off()
    }
    if(type == "ssb"){
      print(ssb)
    }
  }
  
  ## Relative Biomass Plot
  if(type == "relbio" | type == "all"){
    
    # Subset only the relative biomasses
    rel_names <- grep("Rel", colnames(atl$bm))
    rel_biomass <- atl$bm[,rel_names]
    
    # Split the variable names in order to use subsetting
    name_split <- strsplit(colnames(rel_biomass), "Rel")
    colnames(rel_biomass) <- sapply(name_split, "[[", 2)
    rel_biomass$Time <-atl$bm$Time
    
    long_form <- melt(rel_biomass, id.vars = "Time")
    long_form <- left_join(long_form, codes)
    long_form$variable <- factor(long_form$variable, levels = unique(long_form$variable))
    long_form$variable <- droplevels(long_form$variable)
    
    if(is.null(funcode) == FALSE){
      long_form <- filter(long_form, variable %in% funcode)
    }
    
    if(use.log){
      rel <- ggplot(aes(x = Time, y = log(value), color = InvertType), data = long_form) + geom_line() + facet_wrap(~variable) + theme(legend.position="bottom") + theme(legend.position="none") + labs(y="Relative Biomass", x = "Time")
    } else{
      rel <- ggplot(aes(x = Time, y = value, color = InvertType), data = long_form) + geom_line() + facet_wrap(~variable) + theme(legend.position="none") + labs(y="Relative Biomass", x = "Time")
    }
    if(save.pdf){
      pdf(file = "relbiomass_plot.pdf", width = 16, height = 10)
      print(rel)
      dev.off()
    }
    if(type == "relbio"){
      print(rel)
    }
  }
} 