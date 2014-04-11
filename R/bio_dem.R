#' Generate demersel flag from Fishbase
#' 
#' This function generates the demersal flags sections of the biological parameter file of Atlantis based on Fishbase and inserts them in biological parameter file.
#' 
#' @param biol_prm Either user-specified or \code{\link{biol_template}}
#' @param fun_groups Functional group in data.frame format.
#' @param output_file Name of the output file. Defaults to biol.prm
#' 
#' @description This function generates the demersal flag based on Fishbase data. If data are missing from Fishbase, then the flag is assigned 999.
#' @export 
#' @import rfishbase
#' 
bio_dem <- function(biol_prm = NULL, fun_groups, output_file = "biol.prm"){
  
  if(is.null(biol_prm)){
    data(biol_template)
    biol_prm <- biol_template
  } else biol_prm <- readLines(biol_prm)
  
  data(fishbase)
  
  ## Query Fishbase for target species
  myfish <- findSpecies(fun_groups$Long.Name, fish.data)
  
  not_live <- c("CARRION", "REF_DET", "LAB_DET")
  
  ## Create the demersal flag -------------------------
  out <- getEnviroClimateRange(fish.data[myfish])
  match <- grep("demersal|benth", out)
  fish_db <- names(out)
  fish_db <- gsub("_", " ", fish_db)
  d <- vector(length = length(fish_db))
  d[match] <- 1
  d[-(match)] <- 0
  dem_fdb <- data.frame(Long.Name = fish_db, dem_flag = d)
  tmp_dem <- fun_groups
  dem_biol <- merge(tmp_dem, dem_fdb, all = TRUE)
  dem_biol[is.na(dem_biol$dem_flag),"dem_flag"] <- 999  
    
  dem_loc <- grep("# layers when in depths where there were less than complete set of depth layers. 0 is top, 1 is demersal", biol_prm)
  dem_biol <- dem_biol[order(dem_biol$Index),]
  dem_write <- NULL
  for(i in 1:nrow(dem_biol)){
    if(any(dem_biol$InvertType[i] == not_live) == FALSE){
      dem_text <- paste("flagdem", dem_biol$Code[i], " ", dem_biol$dem_flag[i], " ", dem_biol$Code[i],": 1 = on, 0 = off 1", sep = "")
      dem_write <- append(dem_write,dem_text)
    }
  }
  biol_prm <- append(biol_prm, dem_write, after = dem_loc)
  write(biol_prm, file = output_file)
}

