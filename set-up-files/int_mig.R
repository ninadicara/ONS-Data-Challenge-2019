library(plyr)
library(dplyr)

#Read in Internal Migration Data
IM2016 <- read.csv(file="laandregionssex5age2016.csv",
                          stringsAsFactors = FALSE)
IM2017 <- read.csv(file="laandregionssex5age2017.csv",
                         stringsAsFactors = FALSE)

#Tidy IM2016
IM2016 <- IM2016[-c(1:5), -c(7:12)] #Delete the unneccessary columns/rows
colnames(IM2016) <- IM2016[1, ] # The first row will be the header
IM2016 <-  IM2016[-1, ]  #Delete the first row from the data
#As numeric, whilst getting rid of the commas
IM2016[4:6] <- lapply(IM2016[4:6],function(x){as.numeric(gsub(",", "", x))})
#Delete rouge empty row at index 6613
IM2016 <- IM2016[-c(6613),]


#Tidy IM2017
IM2017 <- IM2017[-c(1:6), -c(7:12)] #Delete the unneccessary columns/rows
colnames(IM2017) <- IM2017[1, ] # The first row will be the header
IM2017 <- IM2017[-1, ]  #Delete the first row from the data
#As numeric, whilst getting rid of the commas
IM2017[4:6] <- lapply(IM2017[4:6],function(x){as.numeric(gsub(",", "", x))})


#Split by age
IM2016 <- full_join(IM2016[IM2016$Age == "5-9", ], IM2016[IM2016$Age == "10-14", ], by = "LA code")
IM2017 <- full_join(IM2017[IM2017$Age == "5-9", ], IM2017[IM2017$Age == "10-14", ], by = "LA code")

#2016
#Make composite inflow/outflow for ages 5-14
IM2016$inflow <- IM2016$Inflow.x + IM2016$Inflow.y
IM2016$outflow <- IM2016$Outflow.x + IM2016$Outflow.y
#Delete the working columns
IM2016 <- IM2016[,-c(3:11)]
#Add a year column ready for joining
IM2016$year <- "2016"

#2017
#Make composite inflow/outflow for ages 5-14
IM2017$inflow <- IM2017$Inflows.x + IM2017$Inflows.y
IM2017$outflow <- IM2017$Outflows.x + IM2017$Outflows.y
#Delete the working columns
IM2017 <- IM2017[,-c(3:11)]
#Add a year column ready for joining
IM2017$year <- "2017"

#Combine 2017 and 2016
int_mig <- bind_rows(IM2016, IM2017)

#Delete un-needed data objects
rm(IM2016, IM2017)

#Rename LA Code so it consistently matches with other datasets
colnames(int_mig)[1] <- "LA.code"
colnames(int_mig)[2] <- "LA.name"

#Choose just English LAs
int_mig <- int_mig[grep('^E', int_mig$LA.code),]


#Save out data to an object for re-use
saveRDS(int_mig, file = "int_mig.RDS")
