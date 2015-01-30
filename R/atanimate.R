#' Animated Atlantis Plots
#'
#' Create animated Atlantis plots of all the tracers
#' 
#' This function will create GIFs of all Atlantis tracers by default. WARNING: This can take a while. It is probably much, much better to specify certain groups!
#' 
#' @param atl Atlantis output returned from \code{read_atlantis()}
#' @param interval Speed of animination (unit in seconds)
#' @param codes Vector of Atlantis functional group codes to create GIFs
#' 
atanimate <- function(atl, interval = .2, codes){
  
  # Bring in the necessary data
  bio_agg <- atl$boxbio
  colnames(bio_agg)[2] <- "boxid"
  numboxes <- length(grep("# Box number", atl$bgm))
  
  if(exists("codes")){
    codes <- bio_agg[,codes]
    bio_agg <- cbind(bio_agg[,1:2], codes)
  }
  
  # extract the box vertices
  vertices <- data.frame()
  for(i in 1:numboxes){
    vert_tmp <- grep(paste("box", i - 1, ".vert ", sep = ""), atl$bgm)
    vertices <- rbind(vertices, cbind(i - 1, atl$bgm[vert_tmp]))
  }
  
  # extract lat and long
  coords_tmp <- str_split(vertices$V2, pattern = " ")
  x <- sapply(coords_tmp,`[`,2)
  y <- sapply(coords_tmp,`[`,3)
  
  # recombine into data.frame
  map_base <- data.frame(boxid = vertices$V1, x = x, y = y)
  map_base$x <- as.double(as.character(map_base$x))
  map_base$y <- as.double(as.character(map_base$y))
  
  
  #Set delay between frames when replaying
  ani.options(interval=.2)
  
  # Begin animation loop
  # Note the brackets within the parentheses
  for(j in colnames(bio_agg)[c(-1,-2)]){
    saveGIF({
      # For the most part, it’s safest to start with graphical settings in
      # the animation loop, as the loop adds a layer of complexity to
      # manipulating the graphs. For example, the layout specification needs to
      # be within animation loop to work properly.
      # layout(matrix(c(1, rep(2, 5)), 6, 1))
      
      # Adjust the margins a little
      #  par(mar=c(4,4,2,1) + 0.1)
      for(i in unique(bio_agg$Time)){
        
        plot1 <- qplot(1,5) + coord_cartesian(xlim = c(100,max(bio_agg$Time))) + theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none", panel.background=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line = element_line(colour = "black"), axis.line.y = element_blank()) +
          geom_vline(xintercept = i, size = 2, colour = "cornflowerblue") +
          geom_vline(xintercept = i - 10, size = 2, colour = "cornflowerblue") +
          geom_vline(xintercept = i - 20, size = 2, colour = "cornflowerblue") 
        
        
        agg_tmp <- bio_agg[bio_agg$Time == i, c("Time", "boxid", j)]
        agg_map_data <- merge(map_base, agg_tmp)
        agg_map_data[agg_map_data$boxid == 19 | agg_map_data$boxid == 52, j] <- NA
        y <- agg_map_data[,5] > 0
        y <- agg_map_data[,5][y]
        plot2 <- ggplot(data = agg_map_data, aes(x = x, y = y)) + 
          geom_polygon(aes(group = boxid, fill = agg_map_data[,5]), colour = "black") +
          theme_bw() + xlab("Longitude") + ylab("Latitude") +
          scale_fill_gradient2(high = "cornflowerblue", low = "white")  + guides(fill = guide_legend(keywidth = 2, keyheight = 1, override.aes = list(colour = NULL))) + theme(legend.title=element_blank(), legend.position = "bottom") 
        grid.arrange(plot1, plot2, nrow=2, ncol=1, heights=.1:2)
      }
    }, movie.name = paste(j,"-aggbio.gif", sep = ""))
  }
}


