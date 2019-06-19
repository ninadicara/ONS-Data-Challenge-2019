library(dplyr)
#install.packages("fmsb")
library(fmsb)
library(lme4)
library(car)

df <- readRDS("df.RDS")
#delete the data for Isles of Scilly (NA for school so can't use it.)
df <- na.omit(df)

#Following exploring outliers, decide to get rid of some influential outliers
influential_outliers <- df[c(10567, 5395, 2053, 9839, 9836), ] 
saveRDS(influential_outliers, "set-up-files/influential_outliers.RDS")

#Delete from df influential outliers
df <- df[-c(10567, 5395, 2053, 9839, 9836), ]

#Get rid of values which are identified as have high leverage
df$loneliness_zscore[df$loneliness_zscore > 10] <- 10




#Scale numerical variables
df_scaled <- df
df_scaled[, c(5, 11:14)] <- scale(df_scaled[, c(5, 11:14)], center = TRUE, scale = TRUE) #scale & centre numerical, non population data
df_scaled[, c(4, 6, 7, 9)] <- scale(df_scaled[, c(4, 6, 7, 9)], center = FALSE, scale = TRUE) #only scale for pop data
#making it positive for box cox transformation
df_scaled$loneliness_zscore_bc <- df_scaled$loneliness_zscore + 10

saveRDS(df_scaled, "df_scaled.RDS")

#Select only 2017
df <- df%>%filter(year == '2017')

df_scaled <- df_scaled%>%filter(year == '2017')

install.packages("Hmisc")
library(Hmisc)
hist.data.frame(df)



#####
# Multiple Linear Regression

boxplot(df_scaled[,c(4:7, 9, 11:14)],xlab = "Variable", ylab = "Results")

#multiple linear regression 
mlr <- lm(loneliness_zscore ~ SDS*pop_change + inflow_LEA + outflow_LEA + urb_rur + imd + NMR, data = df_scaled)
summary(mlr)

mean(mlr$residuals) #close to zero so okay


# Assessing Outliers
outlierTest(mlr) # Bonferonni p-value for most extreme obs
qqPlot(mlr, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(mlr) # leverage plots


# Influential Observations
# added variable plots 
avPlots(mlr)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(df_scaled)-length(mlr$coefficients)-2)) 
plot(mlr, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(mlr, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#check for multicollinearity
VIF(mlr)
# It's around 1 which is fine 
#investigate box cox transformation
library(MASS)
bc <- boxcox(mlr)
lambda <- bc$x[which.max(bc$y)]
#lamba is around 1.3

#boxcox transformation: 
df_scaled$loneliness_zscore_bc <- ((df_scaled$loneliness_zscore_bc^1.3)-1)/1.3

mlr_bc <-lm(loneliness_zscore_bc ~ SDS*pop_change + inflow_LEA + outflow_LEA + urb_rur + imd + NMR, data = df_scaled)
qqPlot(mlr_bc, main="QQ Plot") #qq plot for studentized resid 


#Models

mlr1 <- lm(loneliness_zscore ~ SDS, data = df_scaled)
mlr_imd <- lm(loneliness_zscore ~ imd, data = df_scaled)
mlr_imd_SDS <- lm(SDS ~ imd, data = df_scaled)

mlr2 <- lm(loneliness_zscore ~ pop_change, data = df_scaled)
mlr3 <- lm(loneliness_zscore ~ pop_change*SDS, data = df_scaled)
mlr4 <- lm(pop_change ~ SDS, data = df_scaled)
mlr5 <- lm(loneliness_zscore ~ pop_change*SDS + urb_rur + LEA.population + imd, data = df_scaled)

summary(mlr1)
summary(mlr2)
summary(mlr3)
summary(mlr4)
summary(mlr5)


plot(mlr5)


#####
# Multilevel Modelling

library(lme4)
#Check that nesting is significant
mlm1 <- lmer(loneliness_zscore ~  1 + (1|Old.LEA.code), 
             data = df_scaled, REML = FALSE)
mlm2 <- lmer(loneliness_zscore ~ 1 + (1|Old.LEA.code/GP_ID), 
             data = df_scaled, REML = FALSE)

anova(mlm1, mlm2)
#Nesting is significant

mlm3 <- lmer(loneliness_zscore ~  SDS*pop_change + (1|Old.LEA.code/GP_ID),
             data = df_scaled, REML = FALSE)

summary(mlm3)


#Trying model selection
install.packages("LMERConvenienceFunctions")
library(LMERConvenienceFunctions)
mlm4 <- lmer(loneliness_zscore ~ LEA.population + SDS + inflow_LEA + outflow_LEA + GP_population + urb_rur + imd +
               NMR + pop_change + (1|Old.LEA.code/GP_ID), data = df_scaled, REML = FALSE)

fitLMER.fnc(mlm4, method = "AIC")
#find that the variables worth keeping are urb_rur, GP_population, pop_change. Ditched the rest of them !

#Is it worth modelling the crossed random effects for urban_rural, along with the actual geographical structure of the data?
mlm5 <- lmer(loneliness_zscore ~ 1 + (1|Old.LEA.code/GP_ID) + (1|urb_rur), data = df_scaled, REML = FALSE)

#If we compare model 5 and model 3 there is an improvement, and this accounts better for the effects on the structure, 
#but not sure it accounts for enough for it to be worthwhile in terms of complication. 

mlm6 <- lmer(loneliness_zscore ~ pop_change*SDS + urb_rur + GP_population + LEA.population + (1|Old.LEA.code/GP_ID), data = df_scaled, REML = TRUE)

summary(mlm6)

# calculate ICC for lmer model object
# From https://gist.github.com/benwhalley/1297dc3b1d616de41837ca015f338b10
icc <- function(m){
  vc <- as.data.frame((VarCorr(m)))
  l <- vc$vcov
  data_frame(grp=vc$grp, icc=sapply(l, function(x){x/sum(l)}))
}

icc(mlm6)

#####
# Decision Tree

#see what decision tree looks like
install.packages("rpart")
library(rpart)
install.packages(("rpart.plot"))
library(rpart.plot)
# CART model
tree = rpart(loneliness_zscore ~ pop_change + SDS, data=df)
tree2 = rpart(loneliness_zscore ~ pop_change, data=df)
tree3 = rpart(loneliness_zscore ~ SDS, data=df)
# Plot the tree using prp command defined in rpart.plot package
prp(tree)
prp(tree2)
prp(tree3)
