require(xlsx)
require(dplyr)

# Create list of special schools to remove, as these will skew attainment results
special_schools <- c("ACCS", "ACS", "CYS", "FD", "FDS", "INDSPEC", "NMSS")

# Read in ks2 data
ks2_final_1718 <- read.csv(file = "england_ks2final.csv", stringsAsFactors = FALSE)
# Select columns we need
ks2_final_1718 <- ks2_final_1718 %>% 
                  filter(!(NFTYPE %in% special_schools), !(PTKS1GROUP_H %in% c('','SUPP'))) %>%
                  select(LEA, URN, SCHNAME, PTKS1GROUP_H) %>% 
                                     rename(LA.code = LEA,
                                            percentage.high.KS1 = PTKS1GROUP_H,
                                            School.name = SCHNAME) 

# Remove and create metric 
ks2_final_1718$percentage.high.KS1 <- as.numeric(lapply(ks2_final_1718$percentage.high.KS1, function(x){(gsub("%", "", x))})) / 100


ks4_final_1718 <- read.csv(file = "england_ks4final.csv", stringsAsFactors = FALSE)

# Filter columns selected
ks4_final_1718 <- ks4_final_1718 %>% 
                  filter(!(NFTYPE %in% special_schools), !(ATT8SCR %in% c('','SUPP','NE'))) %>%
                  select(LEA, URN, SCHNAME, ATT8SCR) %>%
                  rename(LA.code = LEA,
                         avg.attainment.per.pupil = ATT8SCR,
                         School.name = SCHNAME) %>%
                  mutate(avg.attainment.per.pupil = as.numeric(avg.attainment.per.pupil) / 100)


cfr_full_1718 <- read.csv(file = "cfrfull.csv", stringsAsFactors = FALSE)
cfr_full_1718 <- cfr_full_1718 %>% 
                 filter(!grepl("special", Establishment.type)) %>%
                 select(LA.code,	LA.name,	URN,	School.name, Phase.of.education, Number.of.Pupils..FTE., 
                        E19.Learning.resources..not.ICT.equipment., E20..ICT.learning.resources)

cfr_full_1718$E19.Learning.resources..not.ICT.equipment. <- as.numeric(lapply(cfr_full_1718$E19.Learning.resources..not.ICT.equipment., function(x){(gsub(",", "", x))}))
cfr_full_1718$E20..ICT.learning.resources <- as.numeric(lapply(cfr_full_1718$E20..ICT.learning.resources, function(x){(gsub(",", "", x))}))
cfr_full_1718$Number.of.Pupils..FTE. <- as.numeric(lapply(cfr_full_1718$Number.of.Pupils..FTE., function(x){(gsub(",", "", x))}))

cfr_full_1718 <- cfr_full_1718 %>% 
                 mutate(total.spend.learning.resources = E19.Learning.resources..not.ICT.equipment. + E20..ICT.learning.resources) %>%
                 mutate(spend.per.pupil = total.spend.learning.resources / Number.of.Pupils..FTE.)

# Find min and max value of spend per pupil
min_spend_value <- min(cfr_full_1718$spend.per.pupil)
max_spend_value <- max(cfr_full_1718$spend.per.pupil)

# Create metric from 0 to 1
cfr_full_1718$spend.metric <- (cfr_full_1718$spend.per.pupil - min_spend_value) / (max_spend_value - min_spend_value)

staff_numbers_17 <- read.csv("Staff number 2017.csv")
staff_numbers_17 <- staff_numbers_17 %>%
                    filter(!grepl("special", School.Type.Description)) %>%
                    select(LA.Number, URN, Total.Number.of.Classroom.Teachers..Headcount.,
                           Total.Number.of.Classroom.Teachers..Full.time.Equivalent., Total.Number.of.Teachers..Headcount.) %>%
                    rename(LA.code = LA.Number)

# Join staff numbers to cfr_full for pupil numbers
cfr_full_1718 <- cfr_full_1718 %>% select(LA.code, LA.name, URN, School.name, Phase.of.education, Number.of.Pupils..FTE., spend.metric)

desirability <- left_join(cfr_full_1718, staff_numbers_17, by = c("LA.code", "URN"))
desirability$Total.Number.of.Teachers..Headcount. <- as.numeric(as.character(desirability$Total.Number.of.Teachers..Headcount.))
desirability$Total.Number.of.Classroom.Teachers..Full.time.Equivalent. <- as.numeric(as.character(desirability$Total.Number.of.Classroom.Teachers..Full.time.Equivalent.))
desirability$pupil.per.staff <- desirability$Number.of.Pupils..FTE. / desirability$Total.Number.of.Teachers..Headcount.

desirability$pupil.per.staff[is.infinite(desirability$pupil.per.staff)]<-NA

min_pupil_per_staff <- min(desirability$pupil.per.staff, na.rm = TRUE)
max_pupil_per_staff <- max(desirability$pupil.per.staff, na.rm = TRUE)

desirability$staff.metric <-  1- ((desirability$pupil.per.staff - min_pupil_per_staff) / (max_pupil_per_staff - min_pupil_per_staff))

desirability <- left_join(desirability, ks2_final_1718)
desirability <- left_join(desirability, ks4_final_1718)

## Spend per pupil by LA
#spend_per_pupil <- read.csv("C:/Users/HR612KP.UK/Documents/Loneliness challenge/Spend per pupil.csv")

### Ofsted 2018
ofsted_scores <- read.csv("england_ofsted-schools.csv",stringsAsFactors = FALSE)
ofsted_scores <- ofsted_scores %>%
                 filter(!grepl("Special", Ofsted.phase)) %>%
                 mutate(Ofsted.metric = 1 - ((Overall.effectiveness - 1)/ 3))
  
desirability <- left_join(desirability, ofsted_scores, by = c("URN", "School.name"))
ks2_schools <- c("Infant and junior", "Junior")

desirability <- desirability %>%
                filter(Phase.of.education != "Infant")%>%
                mutate(attainment.metric = ifelse(Phase.of.education %in% ks2_schools, percentage.high.KS1, avg.attainment.per.pupil)) %>%
                select(LA.code, LA.name, URN, School.name, Phase.of.education, spend.metric, staff.metric,
                       attainment.metric, Ofsted.metric) # remove infant schools as they have no KS1 metric
                
desirability <- na.omit(desirability)
desirability_by_school <- desirability %>%
                mutate(desirability.metric = (attainment.metric + spend.metric + staff.metric + Ofsted.metric) / 4)

saveRDS(desirability_by_school, file = "desirability_by_school.RDS")
desirability <- desirability_by_school %>%
                group_by(LA.code, LA.name) %>%
                summarise(desirability.metric = mean(desirability.metric))

#look at the spread of desireability within LAs - mean absolute deviation
desirability_range <- desirability_by_school %>%
                                     group_by(LA.code, LA.name) %>%
                                    summarise(desirability.metric = mad(desirability.metric))
  
saveRDS(desirability, file = "desirability.RDS")
saveRDS(desirability_range, file = "desirability_range.RDS")


desirability <- readRDS("desirability.RDS")
df <- readRDS(file = "df.RDS")
composite_map <- readRDS(file = "composite_map.RDS")


require(dplyr)
desirability$LA.code <- as.character(desirability$LA.code)
joined_data <- left_join(df,desirability, by = c("Old.LEA.code" = "LA.code", "LEA.name" = "LA.name"))


View(joined_data)


joined_data <- joined_data %>% filter(!is.na(desirability.metric))

### finding the r^2 value
metric = lm(pop_change ~ desirability.metric, data=joined_data2)
summary(metric)$r.squared 

install.packages("plotly")
require(plotly)

saveRDS(joined_data, "joined_data.RDS")

## plot desirability against population change
### not sure if this one is that useful, the r^2 looks pretty small ~ 0.08
joined_data2 <- joined_data %>% filter(year == '2017')
plot_ly(x =joined_data2$desirability.metric,
        y = joined_data2$pop_change, 
        type = "scatter", 
        name = "Desirability Metric against Population change") %>%
  add_trace(x = joined_data2$desirability.metric,
            y = fitted(lm(joined_data2$pop_change ~ joined_data2$desirability.metric)),
            mode = "lines",
            name = "Correlation between desirability metric and population change") %>%
  layout(xaxis = list(title = "Desirability metric"),
         yaxis = list(title = "Population change"),
         showlegend = FALSE)
##

## 
## bar chart of desirability by LEA
desirability_by_lea <- joined_data %>% select(LEA.name, desirability.metric) %>% group_by(LEA.name) %>% summarise(desirability.metric = mean(desirability.metric))
desirability_by_lea$LEA.name <- factor(desirability_by_lea$LEA.name, levels = unique(desirability_by_lea$LEA.name)[order(desirability_by_lea$desirability.metric, decreasing = TRUE)])

plot_ly(x = desirability_by_lea$LEA.name, y = desirability_by_lea$desirability.metric, type = "bar") %>%
  layout(xaxis = list(title = "Local Authority Name"),
         yaxis = list(title = "Desirability metric"),
         showlegend = FALSE)



## desirability across LAs - looks kinda crazy as there are so many LAs though 
plot_ly(y = desirability_by_school$desirability.metric, x = desirability_by_school$LA.name,
        type = "box",
        colors = "#00546b",
        name = "Desirability metric across LAs")%>%
  layout(xaxis = list(title = "Local Authority Name"),
         yaxis = list(title = "Desirability metric"),
         showlegend = FALSE)


aggregate_loneliness <- joined_data %>%
                        filter( year == "2017")%>%
                        group_by(LEA.name) %>%
                        summarise(loneliness_zscore = mean(loneliness_zscore),
                                  pop_change = mean(pop_change),
                                  SDS = mean(SDS))

# plot of pop change bs loneliness score for 2017
plot_ly(x =aggregate_loneliness$pop_change,
        y = aggregate_loneliness$loneliness_zscore, 
        type = "scatter"
        ) %>%
  add_trace(x = aggregate_loneliness$pop_change,
            y = fitted(lm(aggregate_loneliness$loneliness_zscore ~ aggregate_loneliness$pop_change)),
            mode = "lines") %>%
  layout(xaxis = list(title = "Population change"),
         yaxis = list(title = "Loneliness score"),
         showlegend = FALSE)

metric = lm(loneliness_zscore ~ SDS, data=aggregate_loneliness)
summary(metric)$r.squared 

# plot of sds vs loneliness score for 2017
plot_ly(x =aggregate_loneliness$SDS,
        y = aggregate_loneliness$loneliness_zscore, 
        type = "scatter"
) %>%
  add_trace(x = aggregate_loneliness$SDS,
            y = fitted(lm(aggregate_loneliness$loneliness_zscore ~ aggregate_loneliness$SDS)),
            mode = "lines") %>%
  layout(xaxis = list(title = "SDS"),
         yaxis = list(title = "Loneliness score"),
         showlegend = FALSE)


#install.packages("spdplyr")
#install.packages("rgdal")
#require(rgdal)
#require(spdplyr)
#boundaries <- read.csv("C:/Users/HR612KP.UK/Documents/Loneliness challenge/england_lsoa_2011.csv", stringsAsFactors = FALSE)

## Convert to Spatial Data Frame

#boundaries$ID <- 1:nrow(boundaries)


#boundaries <- left_join(boundaries, composite_map, by = c("LA.code" = "LA.code"))
#boundaries$Old.LA.Code <- as.numeric(boundaries$Old.LA.Code)
#boundaries <- left_join(boundaries, desirability, by = c("Old.LA.Code" = "LA.code"))
#boundaries <- subset(boundaries, x != "" | y != "")
#boundaries <- subset(boundaries, !is.na(desirability.metric))

#coords <- cbind(boundaries$x,boundaries$y)

#boundaries_uk <- SpatialPointsDataFrame(coords, data = data.frame(boundaries$LA.Name,boundaries$ID,boundaries$desirability.metric), 
#                                        proj4string = CRS("+init=epsg:27700"))


#install.packages("tmap")
#library("tmap")

#boundaries_uk$type <- ifelse(boundaries_uk$desirability.metric < 0.5, "potato", "peas")

#qtm(shp = boundaries_uk, fill = "desirability.metric", fill.palette = "-Blues") #
#wgs84 = '+proj=longlat +datum=WGS84'
#lnd_wgs = spTransform(boundaries_uk, CRS(wgs84))
#install.packages("OpenStreetMap")
#require(OpenStreetMap)
#osm_tiles = tmaptools::read_osm(bbox(lnd_wgs))

#tm_shape(osm_tiles) + tm_raster() + tm_shape(lnd_wgs) + 
 # tm_fill("desirability.metric")



