#' Read in netCDF production file for vertebrates and save it as a data.table
#' 
#' @param grpname Which functional groups to plot? Vector containing the Atlantis functional group names. Omit for ratio indicators.
#' @param ratios Extract other ratio biomass indicators? Default: FALSE
#' @param startyear Starting year of the model
#' @param agestruct Is the functional group age structured?
#' @importFrom ncdf4 nc_open ncvar_get
#' @export

read_prod <- function(file, grpname, startyear, agestruct = TRUE, ratios = FALSE){
  
  # Get netcdf file in proper format
  fopen <- nc_open(file)
  prod_names <- names(fopen$var)
  t <- fopen$dim$t$vals
  time <- t/60/60/24/365
  time <- startyear + time
  b <- fopen$dim$b$vals
  z <- fopen$dim$z$vals
  
  if(ratios){
    match_groups <- c("PelDem_ratio", "PiscPlank_ratio", "InfEpi_ratio", "DivCount")
  } else {
    
    # For functional groups
    match_groups <- unique(grep(paste(grpname, collapse ="|"), prod_names, value = TRUE))
  }
  
  # Read in the data and put it into long format
  tmp_all <- list()
  for(i in match_groups){
  tmp_all[[i]] <- ncvar_get(fopen, i)
  cat("#### Import of ", i, " done ####\n", sep = "" )
  }
  tmp_wide <- ldply(tmp_all, rbind, .id = "id")
  colnames(tmp_wide) <- c("tracer", time)
  tmp_long <- melt(data = tmp_wide, id.vars = "tracer", variable.name = "time")
  return(tmp_long)
}