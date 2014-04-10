# fun <- read.csv("/home/chris/Dropbox/hi/atlantis/iceland_atlantis/SETasGroupsIceland.csv", sep = "\t")
# head(fun)
# vert_id <- c("FISH", "SHARK", "BIRD", "MAMMAL")
# bio_names <- NULL
# for(i in 1:nrow(fun)){
#   if(any(fun$InvertType[i] == vert_id)){
#     bio_names[i] <- paste("#",fun$Long.Name[i], fun$Code[i], "special", "\n")
#   } else if(fun$isSiliconDep[i] == 1){
#     bio_names[i] <- paste("#",fun$Long.Name[i], fun$Code[i], "mg N m-3 mg Si m-3", "\n")
#   } else bio_names[i] <- paste("#",fun$Long.Name[i], fun$Code[i], "mg N m-3", "\n")
# }
# cat(bio_names, sep = "")
# 
# not_ani <- c("CARRION", "REF_DET", "LAB_DET")
# flag_dem <- NULL
# for(i in 1:nrow(fun)){
#   if(any(fun$InvertType[i] == not_ani) == FALSE){
#     flag_dem[i] <- paste("flagdem", fun$Code[i], " 0 ", fun$Code[i],": 1 = on, 0 = off 1", "\n", sep = "")
#   }
# }
# cat(flag_dem, sep = "")
# 
# vert_id <- c("FISH", "SHARK", "BIRD", "MAMMAL")
# flag_plan <- NULL
# for(i in 1:nrow(fun)){
#   if(any(fun$InvertType[i] == vert_id)){
#     flag_plan[i] <- paste("flagplankfish",fun$Code[i], " 0 ", fun$Code[i],": 1 = on, 0 = off 1", "\n", sep = "")
#   }
# }
# cat(flag_plan, sep = "")
