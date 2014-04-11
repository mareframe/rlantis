#' Generate an Atlantis initial conditions file
#' 
#' 
#' This function generates the initial conditions file for Atlantis
#' @param b Number of boxes.
#' @param z Number of vertical boxes (i.e. layers).
#' @param output_file Name of the output_file. Defaults to init.
#' @param timesteps Number of timesteps. Defaults to UNLIMITED.
#' @param fun_groups Functional group in data.frame format.
#' @param model_name Name of the model. Defaults to model_name.
#' @param bgm_file Name of the bgm file. Include the file extension!
#' @param start Specify the start date. Defaults to date().
#' @param timezone Set the timezone. Defaults to UTC.
#' @param data Initial conditions data as a csv. Defaults to generate blank data. See Details.
#' @param fill_value txt or CSV file (see below). Defaults to 0 or Beth's defaults.
#' @param gen_nc Generate the nc binary? Defaults to FALSE and required netcdf-bin to be installed.
#' @param keep_cdf Keep the readable cdf file? Defaults to TRUE.
#' 
#' @details This function generates the initial conditions file in the Atlantis ncdf4 file format. This function can compress the resultant cdf file if \code{gen_nc = TRUE} is set and can clean up after itself if \code{keep_cdf = FALSE is set}. By default, \code{gen_init} will generate empty data matrices that will be filled by the specified \code{fill_value} if it is provided. If it is not provided, the function specifies 0 for all the user defined functional groups and uses the fill values for the required variables from the SETas_model_New example (namely, \code{init_vmpa_setas_25032013.nc}). The function combines the essential variables in the \code{required} data set with those specified in the functional group csv. The data CSV should have one column of data per variable. The length of each column should be either \code{b}*1 for 2D variables or \code{b}*\code{z} for 3D variables. The  \code{init_data} function can be used to help create this CSV. \code{fill_value} can at present it takes a txt file created by the ECCALbioparams Excel spreadsheet. Future implementations will allow a CSV that has two columns. First column, variable name and the second column the fillValue.
#' @keywords gen
#' @examples 
#' gen_init(b = 3, z = 2, fun_groups = fun_groups, bgm_file = "model.bgm", gen_nc = TRUE)
#' @seealso \code{\link{required_init}},\code{\link{dummy_hydro}},\code{\link{init_data}}  
#' @export

gen_init <- function(b, z, output_file = "init", timesteps = "UNLIMITED", set_groups, model_name = "model_name", bgm_file, start = NULL, timezone = "UTC", data = NULL, fill_value = NULL, gen_nc = FALSE, keep_cdf = TRUE){
  
  data(required_init)
  
  # Set UTC time by default
  if(timezone == "UTC"){
    timezone = 0
  }
  
  if(is.null(start)){
    start <- date()
  } 
  
  # Header of cdf file ------------------------------------
  header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
              "\t","t = ",timesteps," ; // ", "\n",
              "\t","b = ",b,";","\n",
              "\t","z = ",z,";", "\n","variables:","\n")
  
  # Middle attributes of cdf file --------------------------
  middle <- c("\n// global attributes:\n",
              "\t\t:title = \"", model_name, " run\" ;\n",
              "\t\t:geometry = \"", bgm_file, "\" ;\n",
              "\t\t:parameters = \" \" ;\n",
              "\t\t:wcnz = ", z - 1, " ;\n",
              "\t\t:sednz = 1 ;\n",
              "\t\t:history = \" \" ;\n",
              "\t\t:NCO = \"4.0.8\" ;\n",
              "data:\n\n")

  ## Initial Variable Definitions ------------------

  ## Vertebrates Initial ---------------------------
  vert_id <- c("FISH", "SHARK", "BIRD", "MAMMAL")
  age_classes <- c("10", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  vert_vars <- c("_Nums", "_ResN", "_StructN")
  
  ## Epibenthic Initial ----------------------------
  epiben_id <- c("SED_EP_FF", "SED_EP_OTHER", "MOB_EP_OTHER", "PHYTOBEN")
  
  ## Other organisms Initial -----------------------
  other_id <- c("LG_INF", "LG_ZOO", "LG_PHY", "DINOFLAG","SM_PHY", "MED_ZOO", "SM_ZOO", "PL_BACT", "SED_BACT", "SM_INF", "LAB_DET", "REF_DET", "CARRION")
  wc_id <- c("LG_ZOO", "MED_ZOO", "SM_ZOO", "PL_BACT")
  decomp_id <- c("LAB_DET", "REF_DET", "CARRION")
  
  # Light vars
  light_vars <- c("(t, b, z) ;", ":bmtype = \"tracer\" ;" , ":units = \"PSU\" ;", ":long_name = \"Light Adaptation of __\" ;",":sumtype = 0 ;", ":dtype = 0 ;", ":inwc = 0 ;", ":insed = 0 ;", ":dissol = 1 ;", ":partic = 0 ;", ":decay = 0. ;", ":_FillValue = 0. ;")
  
  ## Store Species in species_var
  species_var <- NULL
  
  ## Define the variables
  for(i in 1:nrow(fun_groups)){
    
    # Initilization dummy variable
    init <- NULL
    
    ## Vertebrates first -----------------------
    if(any(fun_groups$InvertType[i] == vert_id)){
      vars <- c("(t, b, z) ;", ":bmtype = \"tracer\" ;", ":units = \"mg N\" ;", ":long_name = \"Numbers of LONGNAME\" ;", ":sumtype = 0 ;", ":dtype = 0 ;", ":inwc = 0 ;", ":insed = 0 ;", ":dissol = 0 ;", ":decay = 0. ;", ":partic = 1 ;", ":passive = 0 ;", ":svel = 0. ;", ":xvel = 0. ;", ":psize = 10. ;", ":b_dens = 1000000000. ;", ":i_conc = 200000000. ;", ":f_conc = 200000000. ;", ":_FillValue = 0. ;")
      age_group <- NULL
      
      for(age in age_classes){
        vert_type <- NULL
        for(group in vert_vars){
          vert_type_group <- paste("\t\t",fun_groups$Name[i],age, group, vars, sep="")
          vert_type <- c(vert_type, vert_type_group)
        }
        
        
        ## Place insert double --------------
        match_type <- grep("(t, b, z)",vert_type)
        for(doub in match_type){
          vert_type[doub] <- gsub("\t\t","\t double ",vert_type[doub])
        }
        
        ## Only for Numbers not Struc or Res Weight -------
        match_unit <- grep("Nums:units = ", vert_type)
        vert_type[match_unit] <- gsub("mg N", "1", vert_type[match_unit])
        
        ## Clean up the long names -----------------------
        match_ln <- grep("LONGNAME",vert_type)
        name_num <- paste(fun_groups$Long.Name[i],"cohort",age)
        res_num <- paste("Individ reserve N for", fun_groups$Long.Name[i], "cohort",age)
        str_num <- paste("Individ structural N for", fun_groups$Long.Name[i], "cohort",age)
        vert_type[match_ln[1]] <- gsub("LONGNAME", name_num, vert_type[match_ln[1]])
        vert_type[match_ln[2]] <- gsub("Numbers of LONGNAME", res_num, vert_type[match_ln[2]])
        vert_type[match_ln[3]] <- gsub("Numbers of LONGNAME", str_num, vert_type[match_ln[3]])
        
        ## Store age data, remove vert_type, and go to age+1
        age_group <- append(age_group,vert_type)
        rm(vert_type)
      }
      
      ## Age less group -------------------------------------------------
      vars_nag <- c("(t, b, z) ;", ":bmtype = \"tracer\" ;", ":units = \"mg N m-3\" ;", ":long_name = \"LONGNAME total N\" ;", ":sumtype = 1 ;", ":dtype = 0 ;", ":inwc = 0 ;", ":insed = 0 ;", ":dissol = 1 ;", ":decay = 0. ;", ":partic = 1 ;", ":_FillValue = 0. ;")
      vert_type_nag <- paste("\t\t",fun_groups$Name[i],"_N", vars_nag, sep="")
      vert_type_nag[1] <- gsub("\t\t","\t double ", vert_type_nag[1])
      vert_type_nag[4] <- gsub("LONGNAME", fun_groups$Long.Name[i], vert_type_nag[4])
      
      age_group <- append(age_group, init)
      
      # Add vertebrate data to init
      init <- append(init,age_group)
    }  
    
    ## Finished with Vertebrates -------------------
    ## Move onto Age-Structured Invertebrates ------
    if(fun_groups$NumCohorts[i] > 1 && fun_groups$NumCohorts[i] < 10){
      vars <- c("(t, b, z) ;", ":bmtype = \"tracer\" ;", ":units = \"mg N m-3\" ;", ":long_name = \"Numbers of LONGNAME\" ;", ":sumtype = 0 ;", ":dtype = 0 ;", ":inwc = 0 ;", ":insed = 0 ;", ":dissol = 0 ;", ":decay = 0. ;", ":partic = 1 ;", ":passive = 0 ;", ":svel = 0. ;", ":xvel = 0. ;", ":psize = 10. ;", ":b_dens = 1000000000. ;", ":i_conc = 200000000. ;", ":f_conc = 200000000. ;", ":_FillValue = 0. ;")
      for(j in 1:fun_groups$NumCohorts[i]){
        invert_group <- paste("\t\t",fun_groups$Name[i],"_N", j, vars, sep="")
        invert_group[1] <- gsub("\t\t","\t double ",invert_group[1])
        name_num <- paste(fun_groups$Long.Name[i], j, " Nitrogen", sep = "")
        invert_group[4] <- gsub("Numbers of LONGNAME", name_num, invert_group[4])
        invert_group[5] <- gsub("0","1", invert_group[5])
        invert_group[7] <- gsub("0","1", invert_group[7])
        init <- append(init, invert_group)
      }
      
      ## Place insert double --------------
      match_type <- grep("(t, b, z)", init)
      for(doub in match_type){
        init[doub] <- gsub("\t\t","\t double ", init[doub])
      }
    }
    
    ## Finished with Age-Struc Inverts -----------
    ## Now move on to Epibenthic -----------------
    
    if(any(fun_groups$InvertType[i] == epiben_id)){
      vars <- c("(t, b) ;", ":bmtype = \"epibenthos\" ;", ":units = \"mg N m-2\" ;", ":long_name = \"__ Nitrogen\" ;", ":sumtype = 1 ;", ":dtype = 0 ;", ":_FillValue = 0. ;")
      init <- paste("\t\t",fun_groups$Name[i],"_N",vars,sep="")
      init[4] <- gsub("__",fun_groups$Long.Name[i], init[4])
      
      ## If Species is cover -------------------
      if(fun_groups$IsCover[i] == 1){
        epi_cov <- paste("\t\t",fun_groups$Name[i],"_Cover",vars, sep="")
        cov_name <- paste("Percent cover by", fun_groups$Long.Name[i])
        epi_cov[4] <- gsub("__ Nitrogen",cov_name, epi_cov[4])
      }
      
      ## Add the cover data, if it exists
      if(exists("epi_cov")){
        init <- append(init, epi_cov)
        rm(epi_cov)
      } 
     
      ## Place insert double --------------
      match_type <- grep("(t, b)",init)
      for(doub in match_type){
        init[doub] <- gsub("\t\t","\t double ", init[doub])
      }
    }  
    
    ## Finished with Epibenthic -----------------
    ## Move on to Microphytobenthos -------------
    if(fun_groups$InvertType[i] == "MICROPHTYBENTHOS"){
      vars <- c("(t, b, z) ;", ":bmtype = \"tracer\" ;", ":units = \"mg N m-3\" ;", ":long_name = \"MicroPhytoBenthos Nitrogen\" ;", ":sumtype = 1 ;", ":dtype = 0 ;", ":inwc = 1 ;", ":insed = 1 ;", ":dissol = 0 ;", ":decay = 0. ;", ":partic = 1 ;", ":passive = 1 ;", ":svel = -2.893e-06 ;", ":xvel = 0. ;", ":psize = 1.e-05 ;", ":b_dens = 1000000000. ;", ":i_conc = 200000000. ;", ":f_conc = 200000000. ;", ":_FillValue = 0. ;")
      
      ## Nitrogen ----------------
      init <- paste("\t\t",fun_groups$Name[i],"_N",vars,sep="")
      init[4] <- gsub("__",fun_groups$Long.Name[i], init[4])
      
      ## Silicon -------------------
      init_s <- paste("\t\t", fun_groups$Name[i],"_S", vars, sep = "")
      init_s[4] <- gsub("Nitrogen", "Silicon", init_s[4])
      init <- append(init, init_s)
      
      ## Light
      init_l <- paste("\t\tLight_Adaptn_MB",light_vars, sep = "")
      init_l[3] <- gsub("PSU", "MBU", init_l[3])
      init_l[4] <- gsub("__",fun_groups$Long.Name[i], init_l[4])
      init <- append(init, init_l)
    
      ## Place insert double --------------
      match_type <- grep("(t, b, z)",init)
      for(doub in match_type){
        init[doub] <- gsub("\t\t","\t double ", init[doub])
      }
    }
    
    ## Move on to other groups ------------------
    if(any(fun_groups$InvertType[i] == other_id)){
      vars <- c("(t, b, z) ;", ":bmtype = \"tracer\" ;", ":units = \"mg N m-3\" ;", ":long_name = \"__ Nitrogen\" ;", ":sumtype = 1 ;", ":dtype = 0 ;", ":inwc = 0 ;", ":insed = 1 ;", ":dissol = 0 ;", ":decay = 0. ;", ":partic = 1 ;", ":passive = 0 ;", ":svel = 0. ;", ":xvel = 0. ;", ":psize = 10. ;", ":b_dens = 1000000000. ;", ":i_conc = 200000000. ;", ":f_conc = 200000000. ;", ":_FillValue = 0. ;")
      init <- paste("\t\t",fun_groups$Name[i],"_N",vars,sep="")
      init[4] <- gsub("__",fun_groups$Long.Name[i], init[4])
      
      ## Zooplankton ---------------------------
      if(any(fun_groups$InvertType[i] == wc_id)){
        inwc <- grep(":inwc", init)
        insed <- grep(":insed", init)
        pass <- grep(":passive", init)
        psize <- grep(":psize", init)
        init[inwc] <- gsub("0", "1", init[inwc])
        init[insed] <- gsub("1", "0", init[insed])
        init[pass] <- gsub("0", "1", init[pass])
        
        ## Small zooplankton --------------------
        if(fun_groups$InvertType[i] == "SM_ZOO"){
          init[psize] <- gsub("10.", "0.1", init[psize])
        }
        
        ## Medium zooplankton ------------------
        if(fun_groups$InvertType[i] == "MED_ZOO"){
          init[psize] <- gsub("10.", "0.001", init[psize])
        }
        
        ## Small zooplankton -------------------
        if(fun_groups$InvertType[i] == "SM_ZOO" | fun_groups$InvertType[i] == "PL_BACT"){
          init[psize] <- gsub("10.", "1.e-05", init[psize])
        }
      }
      
      ## Diatoms ------------------------------
      if(fun_groups$InvertType[i] == "LG_PHY"){
        
        # Nitrogen ----------------------------
        sumtype <- grep(":sumtype", init)
        init[sumtype] <- gsub("1", "0", init[sumtype])
        
        # Silicon -----------------------------
        init_s <- paste("\t\t", fun_groups$Name[i],"_S", vars, sep = "")
        init_s[4] <- gsub("__ Nitrogen", "Diatom Silicon", init_s[4])
        
        # Light -------------------------------
        init_l <- paste("\t\tLight_Adaptn_PL",light_vars, sep = "")   
        init_l[4] <- gsub("__",fun_groups$Long.Name[i], init_l[4])
        
        # Setting the flags ------------------
        unit <- grep(":units = ", init_s)
        inwc <- grep(":inwc", init_s)
        pass <- grep(":passive", init_s)
        ln <- grep(":long_name", init_s)
        svel <- grep(":svel", init_s)
        psize <- grep(":psize", init_s)
        init_s[unit] <- gsub("mg N m-3", "mg Si m-3", init_s[unit])
        init_s[inwc] <- gsub("0", "1", init_s[inwc])
        init[inwc] <- gsub("0", "1", init[inwc])
        init_s[pass] <- gsub("0", "1", init_s[pass])
        init[pass] <- gsub("0", "1", init[pass])
        init_s[svel] <- gsub("0.", "-2.893e-06", init_s[svel])
        init[svel] <- gsub("0.", "-2.893e-06", init[svel])
        init_s[psize] <- gsub("10.", "1.e-05", init_s[psize])
        init[psize] <- gsub("10.", "1.e-05", init[psize])
        init <- append(init, init_s)
        init <- append(init, init_l)
      }
      
      ## Dinoflagellates ---------------------
      if(fun_groups$InvertType[i] == "DINOFLAG"){
        inwc <- grep(":inwc", init)
        pass <- grep(":passive", init)
        psize <- grep(":psize", init)
        init[inwc] <- gsub("0", "1", init[inwc])
        init[pass] <- gsub("0", "1", init[pass])
        init[psize] <- gsub("10.", "1.e-05", init[psize])
      }
      
      ## Small Phytoplankton ------------------
      if(fun_groups$InvertType[i] == "SM_PHY"){
        inwc <- grep(":inwc", init)
        pass <- grep(":passive", init)
        psize <- grep(":psize", init)
        init[inwc] <- gsub("0", "1", init[inwc])
        init[pass] <- gsub("0", "1", init[pass])
        init[psize] <- gsub("10.", "1.e-05", init[psize])
        
        # Light -------------------------------
        init_l <- paste("\t\tLight_Adaptn_PS",light_vars, sep = "")
        init_l[4] <- gsub("__",fun_groups$Long.Name[i], init_l[4])
        init <- append(init,init_l)
      }
      
      ## Meiobenthos ---------------------------
      if(fun_groups$InvertType[i] == "SM_INF"){
        psize <- grep(":psize", init)
        init[psize] <- gsub("10.", "0.001", init[psize])
      }
      
      ## Sedimentary Bacteria ----------------
      if(fun_groups$InvertType[i] == "SED_BACT"){
        pass <- grep(":passive", init)
        psize <- grep(":psize", init)
        init[pass] <- gsub("0", "1", init[pass])
        init[psize] <- gsub("10.", "1.e-05", init[psize])
      }
      
      ## Small Infauna -----------------------
      if(fun_groups$InvertType[i] == "SM_INF"){
        init <- paste("\t\t",fun_groups$Name[i],"_N",vars,sep="")
        psize <- grep(":psize", init)
        init[psize] <- gsub("10.", "0.001", init[psize])
      }
      
      ## Decomposition -------------------
      if(any(fun_groups$InvertType[i] == decomp_id)){
        inwc <- grep(":inwc", init)
        psize <- grep(":psize", init)
        decay <- grep(":decay", init)
        pass <- grep(":pass", init)
        svel <- grep(":svel", init)
        init[inwc] <- gsub("0", "1", init[inwc])
        init[psize] <- gsub("10.", "1.e-05", init[psize])
        
        ## Labile Detritus -----------
        if(fun_groups$InvertType[i] == "LAB_DET"){
          init[svel] <- gsub("0.", "-3.472e-06", init[svel])
          init[decay] <- gsub("0.", "1.e-07", init[decay])
          init[pass] <- gsub("0", "1", init[pass])
        }
        
        ## Refactory Detritus ----------
        if(fun_groups$InvertType[i] == "REF_DET"){
          init[svel] <- gsub("0.", "-2.314e-06", init[svel])
          init[decay] <- gsub("0.", "1.e-07", init[decay])
          init[pass] <- gsub("0", "1", init[pass])
        }
        
        ## Carrion ---------------
        if(fun_groups$InvertType[i] == "CARRION"){
          init[svel] <- gsub("0.", "-3.472e-05", init[svel])
        }
      }
      
      ## Prefix dimensions with double --------
      match_type <- grep("(t, b, z)",init)
      for(doub in match_type){
        init[doub] <- gsub("\t\t","\t double ", init[doub])
      }
    }
    species_var <- append(species_var, init)
  }
  
  ## Fill value -----------------------------------------------------------
  
  if(exists("fill_value")){
    
    ## If it's a txt file
    match <- grep(".txt",fill_value)
    if(length(match) > 0){
      fillvalue_tmp <- readLines(fill_value)
      fillvalue_tmp <- paste("\t\t",fillvalue_tmp, sep = "")
      
      ##  Keep those only the names and values --------------------------
      fill_ns <- strsplit(fillvalue_tmp, split = " =")
      fill_names <- NULL
      for(i in 1:length(fill_ns)){
        fill_names[i] <- fill_ns[[i]][1]
      }
      
      for(j in 1:length(fill_names)){
        match <- grep(fill_names[j],species_var)
        if(length(match)>0){
          species_var[match] <- fillvalue_tmp[j]
        }
      }
    }
  }
  
  ## Set correct start date and timezone --------
  ## Defaults to correct date() and UTC ---------
  for(i in 1:length(required_init)){
    match <- grep("1983-01-01", required_init[i])
    if(length(match) > 0){
      required_init[i] <- gsub("1983-01-01", start, required_init[i])
      required_init[i] <- gsub("+10", timezone, required_init[i])
    }
  }
  
  # Combine the functional groups with the essential variables ------
  comb_vars <- c(species_var, required_init)
  
  # SUBJECT TO CHANGE ----------------------------------
  # This is in-progress and will change ----------------
  # Generate dummy data  -------------------------------
  data_type <- matrix(rep(NA,length(comb_vars),nrow=1))
  
  ## Determine dimensionality of variable --------------
  # 1 - specify data in (b, z) dimensions
  # 2 - specify data in (1, b) dimensions
  # 3 - scalar for time, probably fine at 0
  for(i in 1:length(required_init)){
    match <-grep("(t, b, z)", comb_vars, fixed = TRUE)
    if(length(match) > 0){
      data_type[match] <- 1
    }
    match.a <-grep("(b, z)", comb_vars, fixed = TRUE)
    if(length(match.a) > 0){
      data_type[match.a] <- 1
    }
    match.b <-grep("(t, b)", comb_vars, fixed = TRUE)
    if(length(match.b) > 0){
      data_type[match.b] <- 2
    }
    match.c <-grep("seconds", comb_vars, fixed = TRUE)
    if(length(match.c) > 0){
      data_type[match.c] <- 3
    }
  }
  
  # Rename missing data to -999 in order to subset it ---------
  data_type[is.na(data_type)] = -999
  comb_names <- comb_vars[data_type > 0]
  
  # Call variable names to split strings ----------------------
  match <- grep("short",comb_names)
  for(i in match){
    comb_names[i] <- gsub("short", "double", comb_names[i])
  }
  match <- grep("\tdouble",comb_names, fixed = TRUE)
  for(i in match){
    comb_names[i] <- gsub("\tdouble", "\t double", comb_names[i])
  }
  match <- grep("\t\tt:units",comb_names, fixed = TRUE)
  for(i in match){
    comb_names[i] <- gsub("\t\tt", "\t double t(", comb_names[i])
  }

  # Need to split twice to get the variable name
  # First string split ---------------------------------
  var_split <- strsplit(comb_names, "\t double ")
  first_save <- NULL
  for(i in 1:length(var_split)){
    first_save[i] <- var_split[[i]][[2]]
  }
  # Second split to get name ---------------------------
  var_split2 <- strsplit(first_save,"(", fixed = TRUE)
  second_save <- NULL
  for(i in 1:length(var_split2)){
    second_save[i] <- var_split2[[i]][[1]]
  }
  
  # Create Data Names ---------------------------------
  data_names <- paste(second_save," = \n  ", sep = "")
  
  # Retain data indicators
  data_ind <- (data_type[data_type>0])
  
  ## Enter initial data -------------------------------
  ## A user may only add a data for some variables  ---
  ## So this function needs to be flexible ------------
  
  if(is.null(data) == FALSE){
    data <- read.csv(data, header = T)
    data <- as.list(data)
    data <- lapply(data, na.omit)
    
    # Counter variable
    num <- 1
    
    # What data are available ---------------------------
    data_cont <- matrix(nrow = length(data_names), ncol = 1)
    
    for(i in 1:length(data_names)){
      x <- (grep(paste("\\b",names(data)[i],"\\b", sep =""),data_names))
      data_cont[x,] <- num
      num <- num + 1
    }
    
    data_cont[is.na(data_cont)] <- 0
    data_store <- list()
    
    for(i in 1:length(data_ind)){
      if(data_cont[i] == 0){
        if(data_ind[i] == 1){
          dummy_data <- matrix(nrow = b, ncol= z)
          dummy_data[is.na(dummy_data)] <- "_"
          dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
        }
        if(data_ind[i] == 2){
          dummy_data <- matrix(nrow = 1, ncol = b)
          dummy_data[is.na(dummy_data)] <- "_"
          dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
        }
        if(data_ind[i] == 3){
          dummy_data <- "_"
        }
        data_store[[i]] <- dummy_data
      } else{
        if(length(as.vector(data[[data_cont[i]]])) == b*1){
          dummy_data <- matrix(data[[data_cont[i]]], nrow = 1, ncol = b)
          dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
          data_store[[i]] <- dummy_data 
        }
        if(length(as.vector(data[[data_cont[i]]])) == b*z){
          dummy_data <- matrix(data[[data_cont[i]]], nrow = b, ncol = z, byrow = TRUE)
          dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
          data_store[[i]] <- dummy_data
        }
      }
    }
  }
  
  if(is.null(data)){
    data_store <- list()
    for(i in 1:length(data_ind)){
      if(data_ind[i] == 1){
        dummy_data <- matrix(nrow = b, ncol= z)
        dummy_data[is.na(dummy_data)] <- "_"
        dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
      }
      if(data_ind[i] == 2){
        dummy_data <- matrix(nrow = 1, ncol = b)
        dummy_data[is.na(dummy_data)] <- "_"
        dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")
      }
      if(data_ind[i] == 3){
        dummy_data <- "_"
      }
      data_store[[i]] <- dummy_data
    }
  }
  
  ## Print the File To Data
  sink(file = paste(output_file,".cdf",sep=""))
  cat(header, sep = "")
  cat(comb_vars, sep = "\n")
  cat(middle, sep = "")
  for(i in 1:length(data_names)){
    if(i == length(data_names)){
      cat(data_names[i],data_store[[i]],";\n",sep="")
      } else cat(data_names[i],data_store[[i]],";\n\n",sep="")
  }
  cat("}")
  sink()

  
  ## Generate binary nc file if requested ------------------
  if(gen_nc){
    system(paste("ncgen -b ", output_file,".cdf", sep = ""))
    cat("##------ MESSAGE ------##\nThe ", output_file,".nc binary has been created in ",getwd(),"\n##---------------------##\n", sep = "")
  }
  
  ## Remove the raw cdf file if requested ------------------
  if(keep_cdf == FALSE){
    system(paste("rm ", output_file,".cdf",sep=""))
    cat("##------ MESSAGE ------##\nThe cdf file has been deleted from", getwd(),"\n##---------------------##\n")
  }
}