#' Generate dummy hydro data for Atlantis
#' 
#' This function generates dummy flux, salinity, and temperature files for Atlantis.
#' 
#' @param b Number of boxes
#' @param z Number of vertical boxes (i.e. layers)
#' @param t Time. Defaults to 86400.
#' @param model_name Name of the model.
#' @param bgm_file Name of the bgm file. Include the file extension!
#' @param hydro Generate the flux data. Defaults to TRUE.
#' @param salt Generate the dummy salinity data. Defaults to TRUE.
#' @param temp Generate the dummy temperature data. Defaults to TRUE
#' @param gen_nc Generate the nc binary? Defaults to FALSE and required netcdf-bin to be installed.
#' @param keep_cdf Keep the readable cdf file? Defaults to TRUE.
#' @keywords gen
#' @details This function generates dummy hydrography data, namely flux, salinity, and temperature data, in the Atlantis ncdf4 file format. This function can compress the resultant cdf file if \code{gen_nc = TRUE} is set and can clean up after itself if \code{keep_cdf = FALSE is set}. It can handle any arbitrary number of boxes (set by \code{b}) and vertical layers (set by \code{z}).  
#' @export
#' @examples
#' dummy_hydro(b = 4, z, model_name = "dummy", bgm_file = "dummy.bgm", gen_nc = TRUE, keep_cdf = FALSE)
#' @seealso \code{\link{gen_init}},\code{\link{required_init}} 
#' 
#' 
dummy_hydro <- function(b, z, t = 86400, model_name, bgm_file, hydro = TRUE, salt = TRUE, temp = TRUE, gen_nc = FALSE, keep_cdf = TRUE){
  
  
  
  # One destination
  dest <- 1
  
  if(hydro){
    # Create variables ------------------------------------------------
    var_names = c("exchange","dest_b","dest_k")
    
    # Insert fill values for exchange, dest_b and dest_k
    fill_exch <- 0
    fill_destb <- -1
    fill_destk <- -1
    
    # Name the output CDF file
    output_file_h <- paste(model_name,"_hydro", sep = "")
    
    # Create the header ---------------------------------------------------------------
    header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
                "\t","t = UNLIMITED ; // ", "\n",
                "\t","b = ",b," ;","\n",
                "\t","z = ",z-1," ;", "\n",
                "\tdest = ",dest," ;\n",
                "variables:\n")
    
    # Define the variables ------------------------------------------------------------
    var_defn <- c("\tdouble t(t) ; \n",
                  "\t\tt:units = \"seconds since 2008-06-01 00:00:00 +10\" ;\n",
                  "\t\tt:dt = 86400. ;\n",
                  "\tdouble exchange(t, b, z, dest) ;\n",
                  "\t\texchange:_FillValue = ", fill_exch,". ;\n",
                  "\tint dest_b(t, b, z, dest) ;\n",
                  "\t\tdest_b:_FillValue = ", fill_destb," ;\n",
                  "\tint dest_k(t, b, z, dest) ;\n",
                  "\t\tdest_k:_FillValue = ", fill_destk," ;\n\n",
                  "// global attributes:\n",
                  "\t\t:title = \"dummy\" ;\n",
                  "\t\t:geometry = \"",bgm_file,"\" ;\n",
                  "\t\t:parameters = \"\" ; \n",
                  "data:\n\n")
    
    # Create dummy data,fill them with "_", and get them in correct format
    dummy_data <- matrix(nrow = b*dest, ncol= z)
    dummy_data[is.na(dummy_data)] <- "_"
    dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
    
    # Generate the inital conditions file ------------------------------------------------
    sink(paste(output_file_h,".cdf",sep=""))
    cat(header,sep="")
    cat(var_defn,sep="")
    
    # This loop creates all the dummy data
    cat(" t = ",t," ;\n\n",sep="")
    for(i in 1:length(var_names)){
      if(i == length(var_names)){
        cat(paste(var_names[i], " = ","\n  ", dummy_data,";","\n",sep=""))
      } else cat(paste(var_names[i], " = ","\n  ", dummy_data,";","\n\n",sep=""))
    }
    cat("}")
    sink()
    
    if(gen_nc){
      system(paste("ncgen -b ", output_file_h, ".cdf", sep = ""))
      cat("##------ MESSAGE ------##\nThe ", output_file_h,".nc binary has been created in ",getwd(),"\n##---------------------##\n", sep = "")
    }
  }
  
  if(salt){
    # Variable names
    var_names = c("salinity")
    
    # Insert fill values for exchange, dest_b and dest_k
    fill_salt <- 33

    # Name the output CDF file
    output_file_s <- paste(model_name,"_salt",sep="")
    
    # Create the header ---------------------------------------------------------------
    header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
                "\t","t = UNLIMITED ; // ", "\n",
                "\t","b = ",b," ;","\n",
                "\t","z = ",z," ;", "\n",
                "variables:\n")
    
    # Define the variables ------------------------------------------------------------
    var_defn <- c("\tdouble t(t) ; \n",
                  "\t\tt:units = \"seconds since 2008-06-01 00:00:00 +10\" ;\n",
                  "\t\tt:dt = 86400. ;\n",
                  "\tdouble salinity(t, b, z) ;\n",
                  "\t\tsalinity:_FillValue = ", fill_salt,". ;\n\n",
                  "// global attributes:\n",
                  "\t\t:title = \"dummy\" ;\n",
                  "\t\t:geometry = \"",bgm_file,"\" ;\n",
                  "\t\t:parameters = \"\" ; \n",
                  "data:\n\n")
    
    # Create empty matrices,fill them with "_", and get them in correct format
    dummy_data <- matrix(nrow = b*dest, ncol= z)
    dummy_data[is.na(dummy_data)] <- "_"
    dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
    
    # Generate the inital conditions file ------------------------------------------------
    
    sink(file=paste(output_file_s,".cdf",sep=""))
    cat(header,sep="")
    cat(var_defn,sep="")
    
    # This loop creates all the dummy data
    cat(" t = ",t," ;\n\n",sep="")
    for(i in 1:length(var_names)){
      if(i == length(var_names)){
        cat(paste(var_names[i], " = ","\n  ", dummy_data,";","\n",sep=""))
      } else cat(paste(var_names[i], " = ","\n  ", dummy_data,";","\n\n",sep=""))
    }
    cat("}")
    sink()
    
    if(gen_nc){
      system(paste("ncgen -b ", output_file_s, ".cdf", sep = ""))
      cat("##------ MESSAGE ------##\nThe ", output_file_s,".nc binary has been created in ",getwd(),"\n##---------------------##\n", sep = "")
    }
  }
  
  if(temp){
    # Variable names
    var_names = c("temperature")
    
    # Insert fill values for exchange, dest_b and dest_k
    fill_temp <- 20

    # Name the output CDF file
    output_file_t <- paste(model_name,"_temp",sep="")
    
    # Create the header ---------------------------------------------------------------
    header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
                "\t","t = UNLIMITED ; // ", "\n",
                "\t","b = ",b," ;","\n",
                "\t","z = ",z," ;", "\n",
                "variables:\n")
    
    # Define the variables ------------------------------------------------------------
    var_defn <- c("\tdouble t(t) ; \n",
                  "\t\tt:units = \"seconds since 2008-06-01 00:00:00 +10\" ;\n",
                  "\t\tt:dt = 86400. ;\n",
                  "\tdouble temperature(t, b, z) ;\n",
                  "\t\ttemperature:_FillValue = ", fill_temp,". ;\n\n",
                  "// global attributes:\n",
                  "\t\t:title = \"dummy\" ;\n",
                  "\t\t:geometry = \"",bgm_file,"\" ;\n",
                  "\t\t:parameters = \"\" ; \n",
                  "data:\n\n")
    
    # Create empty matrices,fill them with "_", and get them in correct format
    dummy_data <- matrix(nrow = b*dest, ncol= z)
    dummy_data[is.na(dummy_data)] <- "_"
    dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
    
    # Generate the inital conditions file ------------------------------------------------
    
    sink(file=paste(output_file_t,".cdf",sep=""))
    cat(header,sep="")
    cat(var_defn,sep="")
    
    # This loop creates all the dummy data
    cat(" t = ",t," ;\n\n",sep="")
    for(i in 1:length(var_names)){
      if(i == length(var_names)){
      cat(paste(var_names[i], " = ","\n  ", dummy_data,";","\n",sep=""))
      } else cat(paste(var_names[i], " = ","\n  ", dummy_data,";","\n\n",sep=""))
    }
    
    cat("}")
    sink()
    
    if(gen_nc){
      system(paste("ncgen -b ", output_file_t, ".cdf", sep = ""))
      cat("##------ MESSAGE ------##\nThe ", output_file_t,".nc binary has been created in ",getwd(),"\n##---------------------##\n", sep = "")
    }
  }
  
  if(keep_cdf == FALSE){
    if(exists("output_file_h")){
      system(paste("rm ", output_file_h, ".cdf", sep = ""))
    }
    if(exists("output_file_s")){
      system(paste("rm ", output_file_s, ".cdf", sep = ""))
    }
    if(exists("output_file_t")){
      system(paste("rm ", output_file_t, ".cdf", sep = ""))
    }
    cat("##------ MESSAGE ------##\nThe cdf file(s) have been deleted from", getwd(),"\n##---------------------##\n")
  }
}
  

    
    