Window <- 5

#Smoking Studies Prepossessing###################################################
Smoke$eid <- as.factor(Smoke$eid)
Age$eid <- as.factor(Age$eid)

#SmokingAge #########################################
SmokingAge <- Smoke %>% 
  left_join(., Age) %>%
  left_join(., JustCategoriesInformation) %>% 
  filter(!Final %in% c("Strange")) %>%
  mutate(AgeSmoking= Age + time_length(interval(as.Date(Date), as.Date(event_dt)), "years")) %>%
  drop_na(Category) %>%
  mutate(InterpolatedPhenoAge = Intercept + AgeCoeffs * AgeSmoking)

#number of participants in the smoking dataset we are working with :117007
length(unique(SmokingAge$eid))



SmokingAge <- SmokingAge %>%
  mutate(Final2 = recode_factor(Final, 
                                "Smoker" = "Started_Smoking",
                                "Never" = "Never_Smoked",
                                "StartedSmoking" = "Started_Smoking",
                                "StoppedSmoking" = "Stopped_Smoking")) 


#SmokingCrossing #######################################
SmokingCrossing <- SmokingAge %>%
  mutate(Crossing = case_when(Category == "Healthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              Category == "Unhealthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              TRUE ~ 120
  ) ) 


SmokingCrossing$Category <- SmokingCrossing$Category   %>%
  str_replace_all(., "Healthy_Cross", "UBH") %>%
  str_replace_all(., "Unhealthy_Cross", "HBU") %>%
  str_replace_all(., "Unhealthy_High", "URU") %>%
  str_replace_all(., "Healthy_Low", "HRH") 


#SmokingCrossing_filtered == 1 #######################################
SmokingCrossing_filtered <- SmokingCrossing %>%
  mutate(SmokingRangeCrossing = ifelse(abs(AgeSmoking - Crossing) < Window , 1,0)) %>%
  filter(Category %in% c("HBU", "UBH")) %>%
  filter(Crossing != 120) %>%
  filter(SmokingRangeCrossing == 1) 

#number of participants in the disease dataset we are working with : 31499
length(unique(SmokingCrossing_filtered$eid))

PrevCrossing$eid <- as.factor(PrevCrossing$eid)


SmokingPrevCrossing <- left_join(PrevCrossing, Smoke) %>% 
  #select(-c(Age))  %>%
  drop_na(Final) %>%
  mutate(Final2 = recode_factor(Final, 
                                "Smoker" = "Started_Smoking",
                                "Never" = "Never_Smoked",
                                "StoppedSmoking" = "Stopped_Smoking",
                                "StartedSmoking" = "Started_Smoking")) 



pdf(paste0(IncludeFigHere, "/PhenoAgeSmoker.pdf"), 9, 4)

#Participants with Health transition in the smoking dataset :"1006873","1000238"###################
Smoker <- ggplot(SmokingAge %>%
                   filter(eid %in% c("1044812")) %>% #1069297,"1004993","1006873","1000238" 
                   head(100), aes(x = AgeSmoking, y = InterpolatedPhenoAge, group = eid)) + 
  geom_point(aes(colour = Final, size = 1.2)) + geom_line() +
  geom_abline(aes(
    intercept = Intercept, 
    slope = AgeCoeffs,
    alpha = 0.9)) + #factor(Situation)
  xlim(10, 100) +
  ylim(10, 100) +
  geom_abline(slope=1, intercept = 0, linetype="dashed", colour = "blue") + 
  theme_bw() +
  labs(y = "Predicted Phenoage", x = "Age")

print(Smoker)
dev.off()


SmokingExample <- aggregate(term_description ~ eid, data = SmokingCrossing_filtered, FUN = function(x) length(unique(x))) 
#SmokingCrossing_filtered %>% 
#group_by(eid) %>%
#summarize(NumSmokingRecord = n_distinct(term_description))


Diseasepatient5 <- DiseasesInformation %>%
  filter(eid %in% c("1044812"))%>%
  select(Disease, Dates)
 
# BMIpatient5 <- BMI_stats %>%
#   filter(eid %in% c("1044812"))%>%
#   select(event_dt, value1,outlierBMI, eventBMI)
 
#eid 1044812
#first visit at 2010-05-19, aged 67,
#crossing at 67.70
# COPD at 67
# have 2 advice on exercise at 67
# quit smoking at 69
# underweight at 66 and 74


Diseasepatient5$Dates <- as.Date(Diseasepatient5$Dates)

#Medical history of HBU participant in smoking dataset 
unique(Diseasepatient5$Disease)
# [1] "ANEMIA"                                              
# [2] "OTHER METABOLIC DISEASES"                            
# [3] "DYSLIPIDEMIA"                                        
# [4] "CATARACT AND OTHER LENS DISEASES"                    
# [5] "HYPERTENSION"                              ***          
# [6] "ISCHEMIC HEART DISEASE"                              
# [7] "PERIPHERAL VASCULAR DISEASE"                         
# [8] "COPD, EMPHYSEMA, CHRONIC BRONCHITIS"       ***          
# [9] "OSTEOARTHRITIS AND OTHER DEGENERATIVE JOINT DISEASES"
# [10] "DORSOPATHIES"                                        
# [11] "OTHER MUSCULOSKELETAL AND JOINT DISEASES"            
# [12] "OSTEOPOROSIS"                             ***           
# [13] "OTHER GENITOURINARY DISEASES"


Diseasepatient6 <- DiseasesInformation %>%
  filter(eid %in% c("1069297")) %>%
  select(Disease, Dates)

#first visit at 2009-10-13, age 65
#crossing at 67.20
#current smoker at 62, then quited smoking at 63
#overweight and DM, HTN, asthma at 69
#stopped exercise at age 66, become active again at age 70

Diseasepatient6$Dates <- as.Date(Diseasepatient6$Dates)

#Medical history of UBH participant in smoking dataset
unique(Diseasepatient6$Disease)
# [1] "ANEMIA"                                  
# [2] "DIABETES"                                
# [3] "HYPERTENSION"                            
# [4] "ASTHMA"                                   ****
# [5] "ESOPHAGUS, STOMACH AND DUODENUM DISEASES"
# [6] "INFLAMMATORY ARTHROPATHIES"              
# [7] "OTHER GENITOURINARY DISEASES"  

#Group_by Smoking_status(Final2) at crossing: Stopped_Smoking, Never_Smoked, Started_Smoking ################
SmokingCrossingFinal <- SmokingCrossing_filtered %>%
  group_by(Final2) %>%
  mutate(Prevalence = n() ) %>%
  ungroup() 


#Relative_Frequency_ratio per smoking status(Final2) at crossing : Stopped_Smoking, Never_Smoked, Started_Smoking ###########
SmokingCrossingFinalPlot <- SmokingCrossingFinal %>%
  filter(SmokingRangeCrossing == 1) %>%
  select(eid, Category,Final2, Crossing, Prevalence) %>% 
  unique() %>%
  group_by(Final2, Category) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(relative_frequency_ratio = N/Prevalence)



#Frequency Plot of Smoking_status (Final2) at crossing ##############################
pdf(paste0(IncludeFigHere, "/ggdotSmokingCrossingCategorgyFinal2.pdf"), 5, 5)


print(
ggdotchart(SmokingCrossingFinalPlot, x = "Final2", y = "relative_frequency_ratio",
           color = "Category",                                # Color by groups
           palette = c("#FC4E07","#00AFBB"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Category",                                # Order by groups
           dot.size = 8,                                 # Large dot size
           label = round(SmokingCrossingFinalPlot$relative_frequency_ratio, digits = 2),               # Add values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
)

dev.off()

#Group_by (Disease, Final2)#################
#Relative Frequency ratio of Smoking disease per fina2 at crossing #######################################################
SmokingPrevCrossingPlot <- SmokingPrevCrossing %>%
  filter(DiseasesRangeCrossing == 1) %>%
  filter(Final != "Strange") %>%
  select(eid, Category,Final2,Disease, Crossing, Prevalence) %>% 
  unique() %>%
  group_by(Disease, Final2) %>%
  mutate(N = n()) %>%
  ungroup() %>% # do we need unique() here?
  mutate(relative_frequency_ratio = N/Prevalence)


#Disease Frequency Plot per Smoking_status(Final2) at crossing #######################################################
pdf(paste0(IncludeFigHere, "/ggdotSmokingCrossingDiseasesFinal2.pdf"), 20, 20)

print(
ggdotchart(SmokingPrevCrossingPlot, x = "Disease", y = "relative_frequency_ratio",
           color = "Final2",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Final2",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(SmokingPrevCrossingPlot$relative_frequency_ratio, digits = 3),                        # Add values as dot labels
           font.label = list(color = "white", size = 5, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
)

dev.off()

#number of participants at Crossing in the smoking disease dataset (Final2) we are working with: 28671
length(unique(SmokingPrevCrossingPlot$eid))


