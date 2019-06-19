library(plyr)
library(dplyr)

#READ loneliness data as df since it is the 'main' dataframe
df <- readRDS("set-up-files/loneliness.RDS")
#READ int_mig
int_mig <- readRDS("set-up-files/int_mig.RDS")
#READ LA_population
LA_population <- readRDS("set-up-files/LA_population.RDS")
#READ school desireability score
SDS <- readRDS("set-up-files/desirability.RDS")
SDS$LA.code <- as.character(SDS$LA.code)
#READ composite mapping dataframe
composite_map <- readRDS("set-up-files/composite_map.RDS")


#First make sure all have the same years - looking at 2016 and 2017
df <- subset(df, year == "2016" | year == "2017")
LA_population <- subset(LA_population, year == "2016" | year == "2017")
#int_mig is already 2016/2017 only


#Now we want to make sure that we have summarised all the data to the level of the LEA, as this is what the school data is
int_mig <- int_mig %>% 
                  left_join(select(composite_map, "LA.code", "Old.LA.Code", "County.name"), by = "LA.code")
#Now, sum the data that we have by Old.LA.Code
int_mig_LEA <-  int_mig %>% 
                group_by(Old.LA.Code, year) %>%
                summarise(inflow_LEA = sum(inflow), outflow_LEA = sum(outflow))

#Same for LA_population
LA_population <- LA_population %>% 
                  left_join(select(composite_map, "LA.code", "Old.LA.Code", "County.name"), by = "LA.code")
LEA_population <-  LA_population %>% 
                        group_by(Old.LA.Code, year) %>%
                        summarise( LEA.population = sum(LA.population))
#delete NA rows which are from non-english LAs
LEA_population <- LEA_population[3:306,]

#Map the old codes to GP loneliness information, migration data and population data
df <- df %>% 
          left_join(select(composite_map, "LA.code", "Old.LA.Code", "County.name"), by = "LA.code") %>% 
            left_join(int_mig_LEA, by = (c("Old.LA.Code", "year")))%>% 
              left_join(LEA_population, by = c("Old.LA.Code", "year")) %>% 
                left_join(SDS, by = c("Old.LA.Code" = "LA.code"))

#Tidy and reorganise dataframe
df <- df[, c(2, 11, 12, 15, 17, 13, 14, 1, 3, 6, 9, 10)]
colnames(df)[2] <- "Old.LEA.code"
colnames(df)[3] <- "LEA.name"
colnames(df)[5] <- "SDS"
#year, Old.LA.Code, County.name, LEA.population, SDS, inflow, outflow, GP_ID, GP_population, urb_rur, imd, loneliness_zscore


#Calculate 'migration rate 'Net Migration Rate'
df$NMR <- ((df$inflow_LEA - df$outflow_LEA)/df$LEA.population)*1000
#Population change score
df$pop_change <- ((df$inflow_LEA + df$outflow_LEA)/df$LEA.population)*1000

#save out
saveRDS(df, "df.RDS")

#Delete environment objects
rm("composite_map", "df", "int_mig", "int_mig_LEA", "LA_population", "LEA_population", "SDS")
