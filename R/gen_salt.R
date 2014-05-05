#' Generate salinity forcing data for Atlantis
#' 
#' This function generates the salinity forcing file for Atlantis.
#' 
#' @param b Number of boxes
#' @param z Number of vertical boxes (i.e. layers)
#' @param time A vector of time points (in seconds) between consecutive measurements
#' @param salt A \code{b} by \code{z} by \code{time} data.frame or matrix with each entry corresponding to a salinity measurement at a specific time.  
#' @param start_time String variable of the format YYYY-MM-DD HH:MM:SS +0 
#' @param fill_value Fill value for missing data. Defaults to 35.
#' @param model_name Name of the model.
#' @param bgm_file Name of the bgm file. Include the file extension!
#' @param gen_nc Generate the nc binary? Defaults to FALSE and required netcdf-bin to be installed.
#' @param keep_cdf Keep the readable cdf file? Defaults to TRUE.
#' @details This function generates the salinity forcing data for Atlantis in the ncdf4 file format and generates a binary. This function can compress the resultant cdf file if \code{gen_nc = TRUE} is set and can clean up after itself if \code{keep_cdf = FALSE is set}. 
#' @export
#' @examples
#' salt <- matrix(c(35.1345, 35.1319, 35.1598, 35.1781, 35.1754, 35.1649, 35.1345, 35.1241, 35.1951, 35.2213, 35.2308, 35.2257, 35.2146, 35.1241),nrow = 2)
#' b <- 1
#' z <- 7
#' bgm_file <- "atlantis_test.bgm"
#' model_name <- "atlantis"
#' start_time = "2014-01-01 00:00:00 +0"
#' time <- c(12,24)
#' gen_salt(b=b,z=z,time=time, salt = salt, start_time = start_time, model_name= model_name, bgm_file=bgm_file, gen_nc=TRUE)
#' @seealso \code{\link{gen_temp}},\code{\link{gen_init},\code{\link{dummy_hydro}}} 
#' 
#'
gen_salt <- function(b, z, time, salt, start_time, fill_value = 35, model_name, bgm_file, gen_nc = FALSE, keep_cdf = TRUE){

  # Name the output CDF file
  output_file_t <- paste(model_name,"_salt",sep="")
  
  # Create the header ---------------------------------------------------------------
  header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
              "\t","t = UNLIMITED ; // ", "\n",
              "\t","b = ",b," ;","\n",
              "\t","z = ",z," ;", "\n",
              "variables:\n")
  
  # Define the variables ------------------------------------------------------------
  var_defn <- c("\tdouble t(t) ; \n",
                "\t\tt:units = \"seconds since ",start_time,"\" ;\n",
                "\t\tt:dt = ",time[2] - time[1], ". ;\n",
                "\tdouble salinity(t, b, z) ;\n",
                "\t\tsalinity:_FillValue = ", fill_value,". ;\n\n",
                "// global attributes:\n",
                "\t\t:title = \"trivial\" ;\n",
                "\t\t:geometry = \"",bgm_file,"\" ;\n",
                "\t\t:parameters = \"\" ; \n",
                "data:\n\n")
  
  # Get time & salt into the correct format
  salt <- as.matrix(salt)
  salt[is.na(salt)] <- "_"
  salt <- paste(apply(salt, 1, paste, collapse=", "), collapse=",\n  ")
  
  # Generate the salt conditions file ------------------------------------------------
  sink(file=paste(output_file_t,".cdf",sep=""))
  cat(header,sep="")
  cat(var_defn,sep="")
  cat("t = ")
  cat(time, sep = ", ")
  cat(" ;\n\n")
  
  # This loop creates all the dummy data
  cat("salinity = \n  ")
  cat(salt)
  cat(" ;\n}")
  sink()
  
  if(gen_nc){
    system(paste("ncgen -b ", output_file_t, ".cdf", sep = ""))
    cat("##------ MESSAGE ------##\nThe ", output_file_t,".nc binary has been created in ",getwd(),"\n##---------------------##\n", sep = "")
  }
}
