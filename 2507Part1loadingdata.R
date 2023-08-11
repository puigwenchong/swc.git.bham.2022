
options(bitmapType = "cairo")

setwd("/rds/projects/g/gkoutosg-variant-prediction/MScData/Gwen")


###########impport libraries 
require(data.table)
library(tidyverse)
library(finalfit)
library(janitor)
library(tidylog)
library(lubridate)  
library(plyr)
library(ggpubr) 
library(dplyr)
library(tableone)
library(patchwork)
library(gridExtra)
library(pROC)


##load data############################################


DiseasesInformation <- fread("/rds/projects/g/gkoutosg-variant-prediction/MScData/DiseasesInformation.csv")


LifestyleData <- fread("/rds/projects/g/gkoutosg-variant-prediction/MScData/LifestyleData.csv") %>% 
  setnames("f.eid", "eid")

TrajectoriesInformation <- fread("/rds/projects/g/gkoutosg-variant-prediction/MScData/TrajectoriesInformation.csv")

JustCategoriesInformation <- TrajectoriesInformation %>% 
  dplyr::select(-c("Mean.corpuscular.volume..MCV.": "Plasma.C.reactive.protein.level", date, Type, age,  funsPhen, PhenoAge, Count, size, r,Result, Situation)) %>% 
  setnames( "Situation2","Category" ) %>%
  unique()

ToDate <- function(x){
  y <- ymd(as.character(x))
  return(y)
}

DiseasesAndTrajectories <- left_join(JustCategoriesInformation, DiseasesInformation) %>% 
  dplyr::select(-c(Death, Type))


ICD10 <- fread("/rds/projects/g/gkoutosg-variant-prediction/MScData/Mapping_ICD10_Categories.csv")

Smoke <- read.csv( "/rds/projects/g/gkoutosg-variant-prediction/MScData/SmokingReadCodeInfo.csv") %>%
  dplyr::select(-X) %>%
  filter(!Final %in% c("Strange"))

BMI <- read.csv( "/rds/projects/g/gkoutosg-variant-prediction/MScData/BMIReadCodeInfo.csv")

Age <- LifestyleData %>%
  dplyr::select(eid,Age=`Age when attended assessment centre0.0` , Date = `Date of attending assessment centre0.0`) 

LifestyleData$eid <- as.factor(LifestyleData$eid)
JustCategoriesInformation$eid <- as.factor(JustCategoriesInformation$eid)

LifestyleAll <- left_join(JustCategoriesInformation, LifestyleData) 

LifestyleFirstVisit <- LifestyleAll %>%  #just want info on first visit (0.0)
  dplyr::select(names(JustCategoriesInformation), contains("0.0")) 


names(LifestyleFirstVisit) <- names(LifestyleFirstVisit) %>%
  str_replace_all(., "0.0","")



########################################################
