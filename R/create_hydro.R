# Number of layers
z = 7

# Number of boxes
b = 53

# Define t, this will become a vector ...
t = 86400

# 
dest <- 1

# Name for model
model_name = "iceland_hydro"

# Variable names
var_names = c("exchange","dest_b","dest_k")

# Insert fill values for exchange, dest_b and dest_k
fill_exch <- 0
fill_destb <- -1
fill_destk <- -1

# Name of BGM file
bgm_file <- "atlantis_L93.bgm"

# Name the output CDF file
init_file <- paste(model_name,".cdf",sep="")

# Create the header ---------------------------------------------------------------
header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
	"\t","t = UNLIMITED ; // ", "\n",
	"\t","b = ",b," ;","\n",
	"\t","z = ",z," ;", "\n",
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
		"\t\t:title = \"trivial\" ;\n",
		"\t\t:geometry = \"",bgm_file,"\" ;\n",
		"\t\t:parameters = \"\" ; \n",
		"data:\n\n")

# Create empty matrices,fill them with "_", and get them in correct format
dummy_data <- matrix(nrow = b*dest, ncol= z)
dummy_data[is.na(dummy_data)] <- "_"
dummy_data <- paste(apply(dummy_data, 1, paste, collapse=", "), collapse=",\n  ")

# Generate the inital conditions file ------------------------------------------------

sink(file=init_file)
cat(header,sep="")
cat(var_defn,sep="")

# This loop creates all the dummy data
cat(" t = ",t," ;\n\n",sep="")
for(i in 1:length(var_names)){
cat(paste(var_names[i], " = ","\n  ",
	dummy_data,";","\n\n",sep=""))
}
sink()
