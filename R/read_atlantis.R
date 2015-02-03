#' Read in Atlantis Files
#'
#' This function read in the appropriate input and output files for other rlantis routines
#' 
#' This function will sets up an rlantis object, at present just a list, which the other output functions call on.
#' 
#' @param indir The directory containing the Atlantis model input files
#' @param outdir The directory containing the Atlantis model output files
#' @param outfile Name of output file (i.e. given after -o flag) 
#' @param bgm Read in model BGM file (default: TRUE)
#' @param ssb Read in spawning stock biomass file (default: TRUE)
#' @param fg Read in functional group CSV (default: TRUE)
#' @param boxbio Read in the box biomass file (default: TRUE)
#' @param vertsize Read in the vertebrate size text file (default: TRUE)
#' @export

read_atlantis <- function(indir, outdir, bgm = TRUE, ssb = TRUE, fg = TRUE, bm = TRUE, boxbio = TRUE, vertsize = TRUE){
  # Create a list to save our data
  output <- list()
  
  # Save contents from the input and output directory to cherry pick files from later
  con_in <- dir(indir)
  con_out <- dir(outdir)
  
    # Read in the BGM files
  if(bgm){
    bgmname <- grep("bgm", con_out)
    output[['bgm']] <- readLines(paste(outdir, con_out[bgmname], sep = ""))
  }
  
  # Read in the SSB
  if(ssb){
    ssbname <- grep("SSB", con_out)
    output[['ssb']] <- read.table(paste(outdir, con_out[ssbname], sep = ""), header = T, stringsAsFactors = FALSE)
  }
  
  # Read in Functional Group
  if(fg){
    fgname <- grep(".csv", con_in)
    output[['fg']] <- read.csv(paste(indir, con_in[fgname], sep = ""), stringsAsFactors = FALSE)
  }

  # Read in Biomass
  if(bm){
    bmname <- grep("BiomIndx", con_out)
    output[['bm']] <- read.table(paste(outdir, con_out[bmname], sep = ""), header = T,  stringsAsFactors = FALSE)
  }

  # Read in Box Biomass
  if(boxbio){
    boxname <- grep("BoxBiomass.txt", con_out)
    output[['boxbio']] <- read.table(paste(outdir, con_out[boxname], sep = ""), header = T)
  }
  
  # Read in Vertical Size txt file
  if(vertsize){
    vertsize <- grep("VertSize.txt", con_out)
    output[['vertsize']] <- read.table(paste(outdir, con_out[vertsize], sep = ""), header = T)
  }
  return(output)
}
