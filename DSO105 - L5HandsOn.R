# lesson 5 HandsOn


# Load Data
library("rcompanion")
library("fastR2")
library("car")
library("dplyr")

honey.df <- honey

# Data Wrangling
honey.df$year <- as.character(honey.df$year)
honey.df$year <- as.factor(honey.df$year)

# Postive skew
plotNormalHistogram(honey.df$totalprod)

# Log transformation looks great
plotNormalHistogram(log(honey.df$totalprod))

honey.df$totalprodLOG <- log(honey.df$totalprod)


#Check for Assumptions

#Passed assumption of homogenity of variance 
leveneTest(totalprodLOG ~ year, data=honey.df)

#Run the Analysis
RManova <- aov(totalprodLOG~year+Error(state), honey.df)
summary(RManova)

RManova <- aov(log(totalprod)~year, honey.df)
summary(RManova)

