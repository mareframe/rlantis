#' Generate biological parameter file for Atlantis using user-specified data and Fishbase
#' 
#' This function generates sections of the biological parameter file of Atlantis based on both user-specified data and Fishbase
#' 
#' @param biol_prm Either user-specified or \code{\link{biol_template}}
#' @param fun_groups Functional group in data.frame format.
#' @param output_file Name of the output file. Defaults to biol.prm
#' 
#' @description This function generates comments at the top of the biological parameter file. This adds no actual data just comments.
#' @export 
#' 
bio_com <- function(biol_prm = NULL, fun_groups, output_file = "biol.prm"){
  
  if(is.null(biol_prm)){
    data(biol_template)
    biol_prm <- biol_template
  } else biol_prm <- readLines(biol_prm)
  
  ## Vertebrate flag -----------------------------------
  vert_id <- c("FISH", "SHARK", "BIRD", "MAMMAL")
  
  ## Insert comments -----------------------------------
  bio_names <- NULL
  for(i in 1:nrow(fun_groups)){
   if(any(fun_groups$InvertType[i] == vert_id)){
     bio_tmp <- paste("#",fun_groups$Long.Name[i], fun_groups$Code[i], "special")
     } else if(fun_groups$isSiliconDep[i] == 1){
       bio_tmp <- paste("#",fun_groups$Long.Name[i], fun_groups$Code[i], "mg N m-3 mg Si m-3")
       } else bio_tmp <- paste("#",fun_groups$Long.Name[i], fun_groups$Code[i], "mg N m-3")
   bio_names <- append(bio_names, bio_tmp)
  }
  com_loc <- grep("# Description Symbol Units", biol_prm)
  biol_prm <- append(biol_prm, bio_names, after = com_loc)
  write(biol_prm, file = output_file)
}

