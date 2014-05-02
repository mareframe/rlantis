gen_temp <- function(b, z, time, temperature, start_time, fill_value = 0, model_name, bgm_file, gen_nc = FALSE, keep_cdf = TRUE){

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
                "\t\tt:units = \"",start_time,"\" ;\n",
                "\t\tt:dt = ",time[2] - time[1], ". ;\n",
                "\tdouble temperature(t, b, z) ;\n",
                "\t\ttemperature:_FillValue = ", fill_value,". ;\n\n",
                "// global attributes:\n",
                "\t\t:title = \"trivial\" ;\n",
                "\t\t:geometry = \"",bgm_file,"\" ;\n",
                "\t\t:parameters = \"\" ; \n",
                "data:\n\n")
  
  # Get time & temperature into the correct format
  temperature <- as.matrix(temperature)
  temperature[is.na(temperature)] <- "_"
  temperature <- paste(apply(temperature, 1, paste, collapse=", "), collapse=",\n  ")
  
  
  
  # Generate the Temperature conditions file ------------------------------------------------
  sink(file=paste(output_file_t,".cdf",sep=""))
  cat(header,sep="")
  cat(var_defn,sep="")
  cat("t = ")
  cat(time, sep = ", ")
  cat(" ;\n\n")
  
  # This loop creates all the dummy data
  cat("temperature = \n  ")
  cat(temperature)
  cat(" ;\n}")
  sink()
  
  if(gen_nc){
    system(paste("ncgen -b ", output_file_t, ".cdf", sep = ""))
    cat("##------ MESSAGE ------##\nThe ", output_file_t,".nc binary has been created in ",getwd(),"\n##---------------------##\n", sep = "")
  }
}
