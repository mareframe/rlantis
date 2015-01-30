#' Read in Atlantis Files
#'
#' This function read in the appropriate input and output files for other rlantis routines
#' 
#' @param indir The directory containing the Atlantis model input files
#' @param outdir The directory containing the Atlantis model output files
#' @param outfile Name of output file (i.e. given after -o flag) 
#' @param bgm Read in model BGM file (default: TRUE)
#' @param ssb Read in spawning stock biomass file (default: TRUE)
#' @param fg Read in functional group CSV (default: TRUE)
#' @export

read_atlantis <- function(indir, outdir, bgm = TRUE, ssb = TRUE, fg = TRUE){
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
    output[['ssb']] <- read.table(paste(outdir, con_out[ssbname], sep = ""), header = T)
  }
  
  # Read in Functional Group
  if(fg){
    fgname <- grep(".csv", con_in)
    output[['fg']] <- read.csv(paste(indir, con_in[fgname], sep = ""))
  }
  return(output)
}
