library(dplyr)

#Making a composite mapping document for local authorities, local education districts with their new and old codes. 

#Read in LA to County mapping document (152)
LA_county <- read.csv("Local_Authority_District_to_County_April_2019_Lookup_in_England.csv", 
                stringsAsFactors = FALSE)
#Tidy column names
colnames(LA_county)[1] <- "LA.code"
colnames(LA_county)[3] <- "County.code"
colnames(LA_county)[4] <- "County.name"


#Read in the Old to New Codes mapping 
LEA_old_new <- read.csv("nlac-2011.csv",
                          stringsAsFactors = FALSE)

#Read in a list of all 326 English Local Authorities (which is what is used in migration data, loneliness data and population data)
#Taking this from the population data 
LAs_Eng <- readRDS("LA_population.RDS")
LAs_Eng <- unique(LAs_Eng[grep('^E', LAs_Eng$LA.code),1:2])

#Now we need a composite mapping document which takes from every Local Authority to every School District
#Which Local Authorities do not direct map to the education authorities?

#mapping county codes to old/new education districts
LEA_county_old_new <- left_join(LEA_old_new, LA_county, by = c("New.LA.Code" = "LA.code"))

#Figure out which local authorities are not LEAs. LA_not_LEA are the codes that will need to map to counties. 
LA_not_LEA <- anti_join(LAs_Eng, LEA_county_old_new, by = c("LA.code" = "New.LA.Code"))
#This gives 203

#See how many we can map to counties using LA_county
LA_not_LEA <- left_join(LA_not_LEA, LA_county, by = "LA.code")
LA_not_LEA <- LA_not_LEA[,c(1:2,4:5)] #Just keep the LA code / name and County code/name

#Manual lookup gives: 
suffolk_rows <- c("E07000201", "E07000204", "E07000205", "E07000206")
dorset_rows <- c("E07000048", "E07000049", "E07000050", "E07000051", "E07000052", "E07000053")
somerset_rows <- c("E07000190", "E07000191")
northumberland_rows <- c("E06000057")

#Rename Northumberland using Northumberland code from the LEA list
LA_not_LEA$County.code[LA_not_LEA$LA.code == "E06000057"] <- "E06000048"
LA_not_LEA$County.name[LA_not_LEA$LA.code == "E06000057"] <- "Northumberland"

#Rename Suffolk
LA_not_LEA$County.code[which(LA_not_LEA$LA.code %in% suffolk_rows)] <- "E10000029"
LA_not_LEA$County.name[which(LA_not_LEA$LA.code %in% suffolk_rows)] <- "Suffolk"

#Rename Somerset
LA_not_LEA$County.code[which(LA_not_LEA$LA.code %in% somerset_rows)] <- "E10000027"
LA_not_LEA$County.name[which(LA_not_LEA$LA.code %in% somerset_rows)] <- "Somerset"

#Rename Dorset
LA_not_LEA$County.code[which(LA_not_LEA$LA.code %in% dorset_rows)] <- "E10000009"
LA_not_LEA$County.name[which(LA_not_LEA$LA.code %in% dorset_rows)] <- "Dorset"

#Recode Gateshead (code has been changed recently so doesn't match otherwise) with code from LEA_old_new
LA_not_LEA$County.code[LA_not_LEA$LA.code == "E08000037"] <- "E08000020"
LA_not_LEA$County.name[LA_not_LEA$LA.code == "E08000037"] <- "Gateshead"



LEA_to_LA <- LA_not_LEA
rm(LA_not_LEA)
colnames(LEA_to_LA)[3] <- "LEA.code" 

#Now we need to create a full map
LA_is_LEA <- inner_join(LAs_Eng, LEA_old_new, by = c("LA.code" = "New.LA.Code"))
LA_is_LEA$LEA.code <- LA_is_LEA$LA.code
#Attach old LA codes to LEA_to_LA
LEA_to_LA <- left_join(LEA_to_LA, LEA_old_new, by = c("LEA.code" = "New.LA.Code"))

#Reorganise before binding
LEA_to_LA <- LEA_to_LA[,c(2, 3, 5, 1, 4)]
LA_is_LEA <- LA_is_LEA[,c(2, 5, 3, 1, 4)]
colnames(LA_is_LEA)[5] <- "County.name"
colnames(LA_is_LEA)[3] <- "Old.LA.Code"
colnames(LEA_to_LA)[3] <- "Old.LA.Code"

#make sure that the old code is not numeric
composite_map$Old.LA.Code <- as.character(composite_map$Old.LA.Code)

#row bind the two data sets to get a complete mapping document for all the types of LA code. 
composite_map <- bind_rows(LEA_to_LA, LA_is_LEA)

#Delete all the unneeded dataframes
rm(LEA_county_old_new, LEA_old_new, LEA_to_LA, LA_is_LEA, LA_codes, LAs_Eng, LA_county)

saveRDS(composite_map, "composite_map.RDS")



