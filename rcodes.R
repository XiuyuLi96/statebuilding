library(foreign)
library(haven)
library(tidyverse)
library(Hmisc)
library(MASS)
library(ordinal)
afrobrmt2 <- read_sav("D:/research/state building/data/afro/merged_r2_data.sav")
afrobrmt3 <- read_sav("D:/research/state building/data/afro/merged_r3_data.sav")
afrobrmt4 <- read_sav("D:/research/state building/data/afro/merged_r4_data.sav")
afrobrmt5 <- read_sav("D:/research/state building/data/afro/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav")
afrobrmt6 <- read_sav("D:/research/state building/data/afro/merged_r6_data_2016_36countries2.sav")
afrobrmt7 <- read_sav("D:/research/state building/data/afro/r7_merged_data_34ctry.release.sav")
afrobrmt7 <- read_sav("D:/research/state building/data/afro/r7_merged_data_34ctry.release.sav")

######afro7 cross-sectional######
######convert data to dataframe######
afrobrmt7 <- as.data.frame(afrobrmt7)
labeledvariables <- map(afrobrmt7, function(x) attr(x, "class")[1] == "haven_labelled")%>% unlist()%>%names() ###get variable names
extractedvariables <- labeledvariables[c(1,8:26, 39, 40,42,43,44:62,64,72:78,81,82,85:90,106,107,116:120,127:137,177,178,
                                      183,184,185:187,207:213,222:228,235,238,253:263,266:279,283,284,284,285,290)]  ####useful variables
factorvariables <- c("COUNTRY","Q97","Q98","Q101","Q84","Q95A","Q96B","Q70","Q2A","Q2B","Q22","Q55PT1","Q55PT2","Q55PT3") ####categorical variables
wave7 <- afrobrmt7%>%mutate_at( vars(factorvariables), as_factor)
wave7 <- wave7[,extractedvariables]
ordinalvariables <- names(wave7)[-which(names(wave7) %in% factorvariables)]
ordinalvariables <- ordinalvariables[-length(ordinalvariables)]
lapply(wave7[,ordinalvariables],table)
w7.cleaned <- wave7
for (i in 1:length(ordinalvariables)) {
  missingdata <- w7.cleaned[,ordinalvariables[i]] %in% c(-1,7,8,9,88,99,98)
  w7.cleaned[missingdata,ordinalvariables[i]] <- NA 
  
}

######modeling####
w7.cleaned[,"Q85B"] <- as.factor(w7.cleaned[,"Q85B"])
model1 <- clm(Q85B~ EA_SVC_A + EA_SVC_B + EA_SVC_C + EA_SVC_D+EA_SEC_A+EA_SEC_B+EA_SEC_E+EA_ROAD_C+
                Q4A+Q4B+Q5+COUNTRY+Q98+Q101, 
              data = w7.cleaned, na.action = na.omit)
summary(model1)




