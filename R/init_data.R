#' Format Atlantis initial conditions data
#' 
#' This function creates the correct format for the \code{gen_init} function for the Atlantis cdf.
#' 
#' @param layers A vector containing the boundaries of the vertical layers
#' @param sed Number of sediment layers. Defaults to 1.
#' @param bgm_file Name of the bgm file. Include the file extension!
#' @param output_file Name of the output file. Defaults to init_data.csv
#' @details At present this function generates nominal_dz, dz, and numlayers 
#' @export

init_data <- function(layers, sed = 1, bgm_file, output_file = "init_data.csv"){
  # Calc difference and reverse order ------------
  diff_layers <- diff(layers)
  
  # Read in bgm file ------------------------------
  bgm_file <- readLines(bgm_file)
  
  ## Pull out depths -----------------------------
  bgm_depths <- bgm_file[grep("botz\t", bgm_file)]
  
  ## Split the strings -----------------------------
  depth_tmp <- strsplit(bgm_depths,split ="\t")
  depths <- NULL
  for(i in 1:length(depth_tmp)){
    depths[i] <- depth_tmp[[i]][2]
  }
  depths <- -1*as.numeric(depths)
  
  ## Calculate length of layers --------------------
  len_lay <- length(layers)
  
  ## Number of vertical layers  -------------------
  numlayers <- NULL
  for(i in 1:length(depths)){
    if(depths[i] == 0){
      numlayers[i] <- 0
    } else numlayers[i] <- sum(depths[i] >= layers)
  }

  ## Create nominal_dz --------------------------
  nominal_dz <- NULL
  for(i in 1:length(depths)){
    if(depths[i]>0){
      
      tmp <- c(depths[i] - layers[numlayers[i]], diff_layers[(numlayers[i]-1):1])
      
      ## If there are few than the maximum number of layers ------------------
      if(length(tmp) < len_lay){
        tmp <- c(tmp, rep(NA, len_lay - length(tmp)), sed)
        } else{
          tmp <- c(tmp, sed)
        }
    } else{
      # For islands in the system
      tmp <- rep(NA,length(layers) + sed)
    }
    nominal_dz <- append(nominal_dz, tmp)
  }
  nominal_dz[is.na(nominal_dz)] <- "_"
  dz <- nominal_dz
  length(numlayers) <- length(nominal_dz)
  num_data <- data.frame(nominal_dz,dz,numlayers)

  write.csv(num_data, file = output_file, row.names = FALSE)
}