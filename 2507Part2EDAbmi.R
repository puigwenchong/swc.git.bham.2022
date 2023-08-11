Window <- 5



##Sudden BMI change studies: Prepocessing#####################################################
#BMI_stats ############################################
BMI_stats <- BMI %>%
  group_by(eid) %>%
  mutate(
    ULBMI = mean(value1) + sd(value1),
    LLBMI = mean(value1) - sd(value1),
    sdBMI = sd(value1),
    xbarBMI = mean(value1))  %>%
  mutate(
    outlierBMI = case_when(
      value1 > ULBMI | value1 < LLBMI ~ value1,
      TRUE ~ NA
    ))  %>%
  mutate(
    eventBMI = case_when(
      value1 > ULBMI | value1 < LLBMI ~ event_dt,
      TRUE ~ NA
    ) 
  ) %>%
  mutate(BMIchange = case_when(
    value1 > ULBMI ~ "increased",
    value1 < LLBMI ~ "decreased",
    TRUE ~ "unchanged")) %>%
  add_tally() %>%
  dplyr::rename(BmiAllRecord = nn)


BMI_stats$eid <- as.factor(BMI_stats$eid)
Age$eid <- as.factor(Age$eid)

BMIx <- left_join(BMI_stats, Age) %>%
  dplyr::select(-c(X, data_provider,n, value2, value3, Read, Value,term_description)) 

#AgeBMI ###################################################################
AgeBMI <- BMIx %>%
  left_join(JustCategoriesInformation) %>% #merge the column of intercept, AgeCoeff,Category
  drop_na(eventBMI) %>% #remove NA value in eventBMI column
  mutate(AgeBMI= Age + time_length(interval(as.Date(Date), as.Date(eventBMI)), "years")) %>% #find the age when outrange BMI happen
  #select(-c(Age,Date)) %>%
  drop_na(Category) %>%
  mutate(InterpolatedPhenoAge = Intercept + AgeCoeffs*AgeBMI)


#BMICrossing###############################################################
BMICrossing <- AgeBMI %>%
  mutate(Crossing = case_when(Category == "Healthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              Category == "Unhealthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              TRUE ~ 120
  ) ) 



#number of participants at Crossing in the entire bmi dataset we are working with: 58171
length(unique(BMICrossing$eid))



#BMICrossing_filtered, filter out 120, only look at health_cross & unhealthy_cross ################################
BMICrossing_filtered <- BMICrossing %>%
  dplyr::select(-c(sdBMI)) %>%
  #5 year window
  mutate(BMIRangeCrossing = ifelse(abs(AgeBMI - Crossing) < Window, 1,0)) %>%
  filter(Category %in% c("Healthy_Cross", "Unhealthy_Cross")) %>%
  filter(Crossing != 120) %>%
  filter(BMIRangeCrossing == 1) %>%
  mutate(BMI_Category = case_when(
    value1 < 18.4999 ~ "Underweight",
    value1 >= 18.5 & value1 <= 24.9999 ~ "Normal_weight",
    value1 >= 25.0 & value1 <= 29.9999 ~ "Overweight",
    value1 >= 30.0 & value1 <= 39.9999 ~ "Non-severe_obesity",
    value1 >= 40 ~ "Severe_obesity")) %>%
  group_by(eid) %>% 
  add_tally() %>%
  dplyr::rename(WindowBmiRecord = n) %>%
  mutate(RatioRecords = WindowBmiRecord/BmiAllRecord*100) %>%
  filter(BMIchange != "unchanged") %>%
  mutate(WeightChanges = case_when(all(BMIchange == "increased") ~ "BmiIncreased", 
                                   all(BMIchange == "decreased") ~ "BmiDecreased", 
                                  TRUE ~ "BmiFluctuated"))


#number of participants at Crossing in the bmi dataset we are working with: 22347
length(unique(BMICrossing_filtered$eid))


BMICrossing_filtered$Category <- BMICrossing_filtered$Category   %>%
  str_replace_all(., "Healthy_Cross", "UBH") %>%
  str_replace_all(., "Unhealthy_Cross", "HBU") %>%
  str_replace_all(., "Unhealthy_High", "URU") %>%
  str_replace_all(., "Healthy_Low", "HRH") 




#Group_by BMI categories at crossing########################################################
BMIPrevCrossing <- BMICrossing_filtered %>%
  group_by(WeightChanges) %>%
  mutate(Prevalence = n() ) %>%
  ungroup() 



#Relative Frequency Ratio per BMI_categorgy at crossing ########################################################
BMIPrevCrossingPlot <- BMIPrevCrossing %>%
  filter(BMIRangeCrossing == 1) %>%
  select(eid, Category,WeightChanges, Crossing, Prevalence) %>% 
  unique() %>%
  group_by(WeightChanges, Category) %>%
  #unique() %>%     #should take out unique??
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(relative_frequency_ratio = N/Prevalence)



pdf(paste0(IncludeFigHere, "/ggdotBMICrossingCategorgy.pdf"), 5, 5)
#Frequency Plot of BMI category at crossing #############################################

print(
ggdotchart(BMIPrevCrossingPlot, x = "WeightChanges", y = "relative_frequency_ratio",
           color = "Category",                                # Color by groups
           palette = c( "#FC4E07","#00AFBB"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Category",                                # Order by groups
           dot.size = 8,                                 # Large dot size
           label = round(BMIPrevCrossingPlot$relative_frequency_ratio, digits = 2),  # Add values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
) 
)
dev.off()

#Out-range BMI related Disease at Crossing #################################
BMIDiseasePrevCrossing <- BMIPrevCrossing %>%
  select(c(eid, WeightChanges)) %>%
  left_join(PrevCrossing) 


#Relative Frequency Ratio of disease per BMI category at crossing ######################
BMIDiseasePrevCrossingPlot <- BMIDiseasePrevCrossing %>%
  filter(DiseasesRangeCrossing == 1) %>%
  select(eid, Category,WeightChanges,Disease, Crossing, Prevalence) %>% #Multimorb and Death out
  unique() %>%
  group_by(Disease, WeightChanges) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(relative_frequency_ratio = N/Prevalence)

pdf(paste0(IncludeFigHere, "/ggdotBMICrossingDiseases.pdf"), 15, 15)

print(
ggdotchart(BMIDiseasePrevCrossingPlot, x = "Disease", y = "relative_frequency_ratio",
           color = "WeightChanges",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07","#B19CD9","#00BA38"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "WeightChanges",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(BMIDiseasePrevCrossingPlot$relative_frequency_ratio, digits = 3),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 5, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
)


dev.off()

#Participants with sudden change in BMI at crossing: "1000312","1000131"#######################################

#select people with more records 

BMIpatient1 <- BMI %>%
  filter(eid %in% c("1065110")) %>% #100312
  select(value1, event_dt) %>%
  na.omit(BMIpatient1)


BMIpatient1 <- BMI_stats %>%
  filter(eid %in% c("1065110"))%>%
  select(event_dt, value1,outlierBMI, eventBMI)

BMIpatient1$event_dt <- ymd(BMIpatient1$event_dt)

ULBMIp1 <- mean(BMIpatient1$value1) + sd(BMIpatient1$value1)
LLBMIp1 <- mean(BMIpatient1$value1) - sd(BMIpatient1$value1)
sdBMIp1 <- sd(BMIpatient1$value1)
xbarBMIp1 <- mean(BMIpatient1$value1)
maxBMIp1 <- max(BMIpatient1$value1)
DatemaxBMI1 <- BMIpatient1$event_dt[which.max(BMIpatient1$value1 == maxBMIp1)]
xlim1 <- min(BMIpatient1$event_dt)
xlim2 <- max(BMIpatient1$event_dt)

pdf(paste0(IncludeFigHere, "/BMIPatient1.pdf"), 9, 4)

ggplot(BMIpatient1 %>% head(300), aes(x = as.Date(event_dt), y = value1)) +
  geom_point(pch = 10, size = 1) +
 # geom_ribbon(aes(ymin = xbarBMIp1 - sdBMIp1, ymax = xbarBMIp1 + sdBMIp1), fill = "grey70", alpha = 0.2) +
  geom_line(aes(y = value1)) +
  geom_hline(yintercept = ULBMIp1, color = "red", lty = 2) +
  geom_hline(yintercept = xbarBMIp1, color = "red") +
  geom_hline(yintercept = LLBMIp1, color = "red", lty = 2) +
  geom_vline(xintercept = as.Date(DatemaxBMI1), color = "blue", lty = 3) +
  theme_bw() +
  labs(x = "Date", y = "BMI") #+
#scale_x_date(limits = c(xlim1, xlim2))

dev.off()

BMIExample <- aggregate(BMI_Category ~ eid, data = BMICrossing_filtered, FUN = function(x) length(unique(x))) 
#BMICrossing_filtered %>%
#group_by(eid) %>%
#summarize(NumBMIRecord = n_distinct(BMI_Category))


Diseasepatient1 <- DiseasesInformation %>%
  filter(eid %in% c("1065110")) #100312

#crossing at 48.4
#first visit at 2010-03-22, age of 61
#Cancer at 2013-01-15, aged 64
#no record of smoking and exercise

#Medical history of HBU participant in BMI dataset - many disease record
unique(Diseasepatient1$Disease)
# "SOLID NEOPLASMS" ***                                
# "BLOOD AND BLOOD FORMING ORGAN DISEASES"          
# "THYROID DISEASES"                                
# "OBESITY" ***                                       
# "OTHER PSYCHIATRIC AND BEHAVIORAL DISEASES"       
# "NEUROTIC, STRESS-RELATED AND SOMATOFORM DISEASES"
# "EPILEPSY"                                        
# "CEREBROVASCULAR DISEASE" ***                        
# "SLEEP DISORDERS"                                 
# "OTHER NEUROLOGICAL DISEASES"                     
# "CATARACT AND OTHER LENS DISEASES" ***                
# "OTHER EYE DISEASES"                              
# "DEAFNESS, HEARING IMPAIRMENT"                    
# "HYPERTENSION"                                    
# "ISCHEMIC HEART DISEASE"                          
# "VENOUS AND LYMPHATIC DISEASES"                   
# "OTHER CARDIOVASCULAR DISEASES"                   
# "COPD, EMPHYSEMA, CHRONIC BRONCHITIS"             
# "COLITIS AND RELATED DISEASES"                    
# "CHRONIC ULCER OF THE SKIN"                       
# "INFLAMMATORY ARTHROPATHIES"                      
# "CHRONIC KIDNEY DISEASES"    ***                     
# "OTHER GENITOURINARY DISEASES"                    
# "OTHER DIGESTIVE DISEASES"                        
# "OTHER MUSCULOSKELETAL AND JOINT DISEASES" ***

BMIpatient2 <- BMI %>%
  filter(eid %in% c("1001711")) %>% #100131
  select(value1, event_dt) %>%
  na.omit(BMIpatient2)

BMIpatient2$event_dt <- ymd(BMIpatient2$event_dt)

ULBMIp2 <- mean(BMIpatient2$value1) + sd(BMIpatient2$value1)
LLBMIp2 <- mean(BMIpatient2$value1) - sd(BMIpatient2$value1)
sdBMIp2 <- sd(BMIpatient2$value1)
xbarBMIp2 <- mean(BMIpatient2$value1)
maxBMIp2 <- max(BMIpatient2$value1)
DatemaxBMI2 <- BMIpatient2$event_dt[which.max(BMIpatient2$value1 == maxBMIp2)]


pdf(paste0(IncludeFigHere, "/BMIPatient2.pdf"), 9, 4)

ggplot(BMIpatient2 %>% head(300), aes(x = as.Date(event_dt), y = value1)) +
  geom_point(pch = 10, size =1) +
  #geom_ribbon(aes(ymin = xbarBMIp2 - sdBMIp2, ymax = xbarBMIp2 + sdBMIp2), fill = "grey70", alpha = 0.2) +
  geom_line(aes(y = value1)) +
  geom_hline(yintercept = ULBMIp2, color = "red", lty = 2) +
  geom_hline(yintercept = xbarBMIp2, color = "red") +
  geom_hline(yintercept = LLBMIp2, color = "red", lty = 2) +
  geom_vline(xintercept = as.Date(DatemaxBMI2), color = "blue", lty = 3) +
  theme_bw() +
  labs(x = "Date", y = "BMI")

dev.off()



Diseasepatient2 <- DiseasesInformation %>%
  filter(eid %in% c("1001711")) %>%
  select(Disease, Dates)

Diseasepatient2$Dates <- as.Date(Diseasepatient2$Dates)

#Medical history of UBH participant in BMI dataset 
unique(Diseasepatient2$Disease)
# [1] "DIABETES"                                
# [2] "HYPERTENSION"                            
# [3] "ISCHEMIC HEART DISEASE"                  
# [4] "ESOPHAGUS, STOMACH AND DUODENUM DISEASES"

#crossing at 57.6
#first visit at 2008-05-01, age of 54
#Diabetes, HTN, IHD, Digestive disease at 2016-08-26, aged 62
#Avoids even trivial exercise at 56, received advice on exercise 4 times since then
#no record of smoking 

pdf(paste0(IncludeFigHere, "/PhenoAgeBMIPatient2.pdf"), 9, 4)

BMI2 <- ggplot(BMICrossing_filtered %>% #1001711
                 filter(eid %in% c("1065110"))%>% #"1000131", 1000312
                 head(100), aes(x = AgeBMI, y = InterpolatedPhenoAge, group = eid )) + #
  geom_point(aes(colour = BMI_Category, size = 1.2)) + geom_line() +
  geom_abline(aes(
    intercept = Intercept, 
    slope = AgeCoeffs,
    alpha = 0.9)) + #factor(Situation)
  xlim(0, 100) +
  ylim(0, 100) +
  geom_abline(slope=1, intercept = 0, linetype="dashed", colour = "blue") + 
  theme_bw() +
  labs(y = "Predicted Phenoage", x = "Age")

print(BMI2)
dev.off()

