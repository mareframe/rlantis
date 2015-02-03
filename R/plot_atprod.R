#' Production plots from Atlantis
#' 
#' Produces production plots from Eat, Growth, Spawning Size, and Ratios from the read_prod() function.
#' 
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. See below
#' @param type Either total or average
#' @import ggplot2
#' @export
#'  
plot_atprod <- function(data, type, grpname, agestruct = TRUE, save = FALSE){
  if(type == "total"){
    prod_data <- ddply(data, c("tracer", "time"), summarize, value = sum(value))
  }
  if(type == "average"){
    prod_data <- ddply(data, c("tracer", "time"), summarize, value = mean(value))
  }
  if(type != "average" & type != "total"){
    stop("Please specify \"average\" or \"total\" for type")
  }
  
  
  # Create a tracer with group name
  prod_data$grpname <- NA
  for(i in grpname){
    prod_data$grpname[grep(i, prod_data$tracer)] <- i
  }
  
  prod_data$type <- NA
  prod_data$type[grep("Eat", prod_data$tracer)] <- "Eat"
  prod_data$type[grep("Growth", prod_data$tracer)] <- "Growth"
  prod_data$type[grep("SpwnSze", prod_data$tracer)] <- "Spawn Size"
  prod_data$type[grep("Prodn", prod_data$tracer)] <- "Production"
  prod_data$type[grep("Grazing", prod_data$tracer)] <- "Grazing"
  
  prod_data$age <- NA
  for(i in 1:10){
  prod_data$age[grep(i, prod_data$tracer, fixed = TRUE)] <- i
  }
  prod_data$age <- factor(prod_data$age, levels = unique(prod_data$age))
  prod_data$time <- as.double(as.character(prod_data$time))
  
  if(agestruct){
    for(i in grpname){
      # Growth Plot
      grow <- subset(prod_data, prod_data$type == "Growth" & prod_data$grpname == i)
      g0 <- ggplot(aes(y = value, x = time), data = grow) + geom_line(aes(color = age))  + ylab(paste(grow$type, "(mg N)")) + xlab("Time") + theme(legend.position = "none")  + coord_cartesian(xlim=c(min(floor(grow$time)), max(ceiling(grow$time))))
      
      # Eat Plot
      eat <- subset(prod_data, prod_data$type == "Eat" & prod_data$grpname == i)
      e0 <- ggplot(aes(y = value, x = time), data = eat) + geom_line(aes(color = age)) + ylab(paste(eat$type, "(mg N)")) + xlab("Time") + theme(legend.position="bottom", legend.title=element_blank()) + coord_cartesian(xlim=c(min(floor(eat$time)), max(ceiling(eat$time))))
      # Print the production plot
      grid.arrange(g0, e0)
    
      if(save){
        pdf(file = paste(i, "_prod.pdf", sep = ""), width = 16, height = 10)
        print(grid.arrange(g0, e0))
        dev.off()
      }
      rm(g0, e0)
    }
  }
  
  if(agestruct == FALSE){
    for(i in grpname){
      prod_tmp <- subset(prod_data, prod_data$grpname == i)
      
      if(sum(prod_tmp$type == "Grazing")>1){
        # Grazing Plot
        graze <- subset(prod_tmp, prod_tmp$type == "Grazing" & prod_tmp$grpname == i)
        g0 <- ggplot(aes(y = value, x = time), data = graze) + geom_line() + facet_wrap(~ type) + ylab(paste(graze$type, "(mg N)")) + xlab("Time") + theme(legend.position = "none")  + coord_cartesian(xlim=c(min(floor(graze$time)), max(ceiling(graze$time))))
      }
      
      # Production Plot
      production <- subset(prod_tmp, prod_tmp$type == "Production")
      p0 <- ggplot(aes(y = log(value), x = time), data = production) + geom_line() + facet_wrap(~ type) + ylab(paste("log ",production$type, "(mg N)")) + xlab("Time") + theme(legend.position="bottom", legend.title=element_blank()) + coord_cartesian(xlim=c(min(floor(production$time)), max(ceiling(production$time))))
    }
    
      if(exists("g0")){
        # Print the production plot
        print(grid.arrange(g0, p0))
      } else(print(p0))
    
    if(save){
      if(exists("g0")){
        pdf(file = paste(i, "_prod.pdf", sep = ""), width = 16, height = 10)
        print(grid.arrange(g0, p0))
        dev.off()
        rm(g0, p0)
      } else{
        pdf(file = paste(i, "_prod.pdf", sep = ""), width = 16, height = 10)
        print(p0)
        dev.off()
        rm(p0)
      }
    }
  }
}