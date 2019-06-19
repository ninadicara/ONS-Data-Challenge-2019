library(plyr)
library(dplyr)

#Load Data
loneliness <- read.csv("loneliness_data.csv",
                       stringsAsFactors = FALSE)

#Set up loneliness data 
#Rename the columns
loneliness <- loneliness %>% rename(LA.code = laua, urb_rur = ru11ind, GP_ID = ï..pcstrip,
                                    GP_population = NUMBER_OF_PATIENTS, year = Year)

#Delete unneeded columns
loneliness <- loneliness[ , -which(names(loneliness) %in% c("SHA", "PCT", "oseast1m", "osnrth1m"))]

#Change year from int to char to avoid problems later...
loneliness$year <- as.character(loneliness$year)

#Ensures there is only English data by getting rid of LA codes that don't begin with E (I.e. N, S or W)
loneliness <- loneliness[grep('^E', loneliness$LA.code),]

saveRDS(loneliness, "loneliness.RDS")
