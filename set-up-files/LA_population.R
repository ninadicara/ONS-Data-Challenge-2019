library(plyr)
library(dplyr)
library(tidyr)


#Read in LA population estimates
LA_population <- read.csv("LA_populations.csv",
                          stringsAsFactors = FALSE)

#Tidy LA_population
LA_population <- LA_population[-c(1:6),] #Delete the unneccessary columns/rows
colnames(LA_population) = c("LA.name", "LA.code", "2015", "2016", "2017") # The first row will be the header

#As numeric, whilst getting rid of the commas
LA_population[3:5] <- lapply(LA_population[3:5],function(x){as.numeric(gsub(",", "", x))})

#Change data from wide to long for each year
LA_population <- gather(LA_population, `2015`, `2016`, `2017`, key = "year", value = "LA.population")

saveRDS(LA_population, "LA_population.RDS")
