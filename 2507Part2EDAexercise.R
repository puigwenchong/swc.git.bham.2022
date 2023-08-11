Window <- 5

WithDescription <- fread("../GpClinicalWDescription.csv") ##needed for exercise

#Exercise Studies Prepossessing #################################################################
Exercise <- dplyr::filter(WithDescription, grepl("exercise",term_description))
ExerciseCodes <- data.frame(table(Exercise$term_description))

#PA status: ActiveEx, InactiveEx, AdviceEx
#Final: StartedEx, InactiveEx, ActiveEx, StoppedEx

ExerciseFilter <- Exercise %>%
  filter(term_description %in% c("Active exercise","Aerobic exercise 3+ times/week",
                                 "Aerobic exercise three or more times per week",
                                 "Regular exercise","Aerobic exercise five times a week",
                                 "Aerobic exercise three times a week","Aerobic exercise four times a week",
                                 'Aerobic exercise 2 times/week','Aerobic exercise 1 time/week',
                                 'Aerobic exercise twice a week','Aerobic exercise once a week',
                                 
                                 "Gets no exercise","Gets little exercise",
                                 "Avoids even trivial exercise","Aerobic exercise zero times a week",
                                 "Aerobic exercise 0 times/week","Takes inadequate exercise",                                 
                                 "[V]Lack of physical exercise",
                                 
                                 "Patient advised about exercise",
                                 "Health education - exercise","Lifestyle advice regarding exercise",
                                 "Health ed. - exercise","Advice about exercise","Patient advised re exercise",
                                 "Referred for exercise programme","Referral for exercise therapy",
                                 "Patient given exercise advice","Advice to exercise",
                                 "Referral to exercise on referral programme","Advice about aerobic exercise")) %>%
  mutate(PA_status = dplyr::recode(term_description,                                               
                                   'Active exercise' = "ActiveEx",
                                   'Regular exercise' = "ActiveEx",
                                   "Aerobic exercise 3+ times/week" = "ActiveEx",
                                   "Aerobic exercise three or more times per week" = "ActiveEx",
                                   "Aerobic exercise five times a week" = "ActiveEx",
                                   "Aerobic exercise three times a week" = "ActiveEx",
                                   "Aerobic exercise four times a week" = "ActiveEx",
                                   'Aerobic exercise 2 times/week' = "ActiveEx",
                                   'Aerobic exercise twice a week' = "ActiveEx",
                                   'Aerobic exercise 1 time/week' = "ActiveEx",
                                   'Aerobic exercise once a week' = "ActiveEx",
                                   
                                   'Gets no exercise' = "InactiveEx",
                                   'Gets little exercise' = "InactiveEx",
                                   'Avoids even trivial exercise' = "InactiveEx",
                                   "Aerobic exercise zero times a week" = "InactiveEx",
                                   "Aerobic exercise 0 times/week" = "InactiveEx",
                                   "Takes inadequate exercise" = "InactiveEx",
                                   "[V]Lack of physical exercise" = "InactiveEx",
                                   
                                   "Patient advised about exercise" = "AdviceEx",
                                   "Health education - exercise" = "AdviceEx",
                                   "Lifestyle advice regarding exercise" = "AdviceEx",
                                   "Advice about exercise" = "AdviceEx",
                                   "Patient advised re exercise" = "AdviceEx",
                                   "Referred for exercise programme" = "AdviceEx",
                                   "Referral for exercise therapy" = "AdviceEx",
                                   "Patient given exercise advice" = "AdviceEx",
                                   "Advice to exercise" = "AdviceEx",
                                   "Referral to exercise on referral programme" = "AdviceEx",
                                   "Health ed. - exercise" = "AdviceEx",
                                   "Advice about aerobic exercise" = "AdviceEx")
  )





ExerciseFilter <- ExerciseFilter %>%
  select(-c(data_provider,value1, value2, value3, Read, Value))

#Number of participants in the exercise dataset we are working with: 105739
length(unique(ExerciseFilter$eid))


ExerciseFilter$event_dt <- as.Date(ExerciseFilter$event_dt, format = "%d/%m/%Y") 
ExerciseFilter <- ExerciseFilter[!duplicated(paste(ExerciseFilter$eid, ExerciseFilter$event_dt)), ]

#FinalExerciseFilter #################################################
FinalExerciseFilter <- ExerciseFilter %>%
  arrange(eid, event_dt) %>%
  group_by(eid) %>% 
  mutate(
    Final = case_when(
      #If the current row has "ActiveEx" status and the previous row has "InactiveEx" or "AdviceEx" status, it is marked as "StartedEx".
      lag(PA_status) %in% c("InactiveEx", "AdviceEx") & PA_status %in% c("ActiveEx") ~ "StartedEx",
      #If the current row has "ActiveEx" status and the next row has "InactiveEx" or "AdviceEx" status, it is marked as "Strange".
      lag(PA_status) %in% c("ActiveEx") & PA_status %in% c("InactiveEx", "AdviceEx") ~ "StoppedEx",
      TRUE ~ PA_status
    )
  ) %>%
  mutate(ExerciseChanges = case_when(all(Final == "ActiveEx") ~ "ActiveExercise", 
                                     all(Final == "InactiveEx") ~ "InactiveExercise",
                                     all(Final == "AdviceEx") ~ "InactiveExercise",
                                     TRUE ~ "ExerciseChanges")) %>%
  ungroup()#

# filter(Final_Status == "AdviceEx")

#check rules for categorization
#filter(FinalExerciseFilter, eid %in% filter(FinalExerciseFilter, PA_status == 'ActiveEx' & Final =="StartedEx")$eid[1:300000])
#filter(FinalExerciseFilter, eid %in% filter(FinalExerciseFilter, PA_status == 'ActiveEx' & Final =="StoppedEx")$eid[1:300000])
#filter(FinalExerciseFilter, eid %in% filter(FinalExerciseFilter, PA_status == 'InactiveEx' & Final =="StartedEx")$eid[1:300000])
#filter(FinalExerciseFilter, eid %in% filter(FinalExerciseFilter, PA_status == 'InactiveEx' & Final =="StoppedEx")$eid[1:300000])
#filter(FinalExerciseFilter, eid %in% filter(FinalExerciseFilter, PA_status == 'AdviceEx' & Final =="StoppedEx")$eid[1:300000])
#filter(FinalExerciseFilter, eid %in% filter(FinalExerciseFilter, PA_status == 'AdviceEx' & Final =="StartedEx")$eid[1:300000])

FinalExerciseFilter$eid <- as.factor(FinalExerciseFilter$eid)

#ExerciseAge ###########################################
ExerciseAge <- FinalExerciseFilter %>% 
  left_join(., Age) %>%
  left_join(., JustCategoriesInformation)


ExerciseAge$Date <- as.Date(ExerciseAge$Date)
ExerciseAge$event_dt <- as.Date(ExerciseAge$event_dt)
ExerciseAge <- ExerciseAge %>%   
  mutate(AgePA = Age + time_length(interval(Date, event_dt), "years"))


ExerciseAge <- ExerciseAge %>%   
  # select(-c(Age, Date)) %>%
  drop_na(Category) %>%
  mutate(InterpolatedPhenoAge = Intercept + AgeCoeffs * AgePA)

ExCrossing <- ExerciseAge %>%
  mutate(Crossing = case_when(Category == "Healthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              Category == "Unhealthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              TRUE ~ 120
  ) ) 


#ExCrossing ,filter out 120, health_cross & unhealthy_cross #############################################
ExCrossing <- ExCrossing %>%
  #5 year window
  mutate(ExRangeCrossing = ifelse(abs(AgePA - Crossing) < Window, 1,0)) %>%
  filter(Category %in% c("Healthy_Cross", "Unhealthy_Cross")) %>%
  filter(Crossing != 120) %>%
  filter(ExRangeCrossing == 1) 

ExCrossing$Category <- ExCrossing$Category %>%
  str_replace_all(., "Healthy_Cross", "UBH") %>%
  str_replace_all(., "Unhealthy_Cross", "HBU") %>%
  str_replace_all(., "Unhealthy_High", "URU") %>%
  str_replace_all(., "Healthy_Low", "HRH") 


#Number of participants at crossing in the exercise dataset we are working with: 28555
length(unique(ExCrossing$eid))


#Group_by exercise_status(Final) at crossing: "ActiveEx","AdviceEX","InactiveEx","StoppedEx","StartedEx"###################################
ExCrossingFinal <- ExCrossing %>%
  group_by(Final) %>%
  mutate(Prevalence = n() ) %>%
  ungroup() 

#Relative Frequency Ratio per exercise status (final) at crossing :StoppedEx, StartedEx, InactiveEx, AdviceEx, ActiveEX ####################################
ExCrossingFinalPlot <- ExCrossingFinal %>%
  filter(ExRangeCrossing == 1) %>%
  select(eid, Category,Final, Crossing, Prevalence) %>% #Multimorb and Death out
  unique() %>%
  group_by(Final, Category) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(relative_frequency_ratio = N/Prevalence)

#Frequency Plot per Category ######################################################
pdf(paste0(IncludeFigHere, "/ggdotExCrossingCategorgy.pdf"), 5, 5)

print(
ggdotchart(ExCrossingFinalPlot, x = "Final", y = "relative_frequency_ratio",
           color = "Category",                                # Color by groups
           palette = c( "#FC4E07","#00AFBB"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Category",                                # Order by groups
           dot.size = 8, #8                                 # Large dot size
           label = round(ExCrossingFinalPlot$relative_frequency_ratio, digits = 2),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, #9
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
)

dev.off()
#Number of participants at exercise crossing :28555
length(unique(ExCrossingFinalPlot$eid))

table(ExCrossingFinalPlot$Final,ExCrossingFinalPlot$Category, ExCrossingFinalPlot$relative_frequency_ratio)

#Group_by(Final)##############################################

ExDiseasePrevCrossing <- ExCrossingFinal %>%
  select(c(eid, Final)) %>%
  left_join(PrevCrossing) 


#Disease Frequency per exercise status(Final) at crossing: StoppedEx, StartedEx, InactiveEx, AdviceEx, ActiveEX ################
ExDiseasePrevCrossingPlot <- ExDiseasePrevCrossing %>%
  filter(DiseasesRangeCrossing == 1) %>%
  select(eid, Category,Final,Disease, Crossing, Prevalence) %>% 
  unique() %>%
  group_by(Disease, Final) %>%
  mutate(N = n()) %>%
  #unique() %>%  #need unique?
  ungroup() %>%
  mutate(relative_frequency_ratio = N/Prevalence)

pdf(paste0(IncludeFigHere, "/ggdotExCrossingDiseases.pdf"), 20, 20)
print(
ggdotchart(ExDiseasePrevCrossingPlot, x = "Disease", y = "relative_frequency_ratio",
           color = "Final",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07","#B19CD9","#00BA38"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Final",                                # Order by groups
           dot.size = 6, #8                                 # Large dot size
           label = round(ExDiseasePrevCrossingPlot$relative_frequency_ratio, digits = 3),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 5, #9
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
)
dev.off()


ExerciseExample <- aggregate(Final ~ eid, data = ExCrossing, FUN = function(x) length(unique(x)))
#ExCrossing %>%
#group_by(eid) %>%
#summarize(NumBMIRecord = n_distinct(Final))



#Participant at crossing in the exercise dataset ########################################################          
Ex2 <- ggplot(ExCrossing %>% # **
                filter(eid %in% c("2598117"))%>% #UBH:2276975,4317031,1322731","1204350
                head(100), aes(x = AgePA, y = InterpolatedPhenoAge, group = eid)) + 
  geom_point(aes(colour = Final, size = 1.2)) + geom_line() +
  geom_abline(aes(
    intercept = Intercept, 
    slope = AgeCoeffs,
    alpha = 0.9)) + #factor(Situation)
  xlim(25, 100) +
  ylim(25, 100) +
  geom_abline(slope=1, intercept = 0, linetype="dashed", colour = "blue") + 
  theme_bw() +
  labs(y = "Predicted Phenoage", x = "Age")


pdf(paste0(IncludeFigHere, "/PhenoAgeEx2.pdf"), 9, 4)
print(Ex2)
dev.off()


#HBU participant in exercise dataset #################################
Diseasepatient3 <- DiseasesInformation %>%
  filter(eid %in% c("1047349"))%>%
  select(Disease, Dates)

Diseasepatient3$Dates <- as.Date(Diseasepatient3$Dates)

#Medical history of HBU participant in smoking dataset 
unique(Diseasepatient3$Disease)
# 
# [1] "HEMATOLOGICAL NEOPLASMS"               
# [2] "BLOOD AND BLOOD FORMING ORGAN DISEASES"
# [3] "THYROID DISEASES"                      
# [4] "DIABETES"                              
# [5] "DYSLIPIDEMIA"                          
# [6] "OTHER NEUROLOGICAL DISEASES"           
# [7] "HYPERTENSION"                          
# [8] "ISCHEMIC HEART DISEASE"                
# [9] "COPD, EMPHYSEMA, CHRONIC BRONCHITIS"   
# [10] "COLITIS AND RELATED DISEASES"          
# [11] "DORSOPATHIES"                          
# [12] "OTHER GENITOURINARY DISEASES"


pdf(paste0(IncludeFigHere, "/PhenoAgeExercisePatient1.pdf"), 9, 4)

#PhenoAge Plot#########################################
ggplot(ExCrossing %>% #1047349 **
         filter(eid %in% c("1025855"))%>% 
         head(100), aes(x = AgePA, y = InterpolatedPhenoAge, group = eid)) + 
  geom_point(aes(colour = Final, size = 1.2)) + geom_line() +
  geom_abline(aes(
    intercept = Intercept, 
    slope = AgeCoeffs,
    alpha = 0.9)) + #factor(Situation)
  xlim(25, 100) +
  ylim(25, 100) +
  geom_abline(slope=1, intercept = 0, linetype="dashed", colour = "blue") + 
  theme_bw() +
  labs(y = "Predicted Phenoage", x = "Age")

dev.off()

#UBH participant in exercise dataset ###########################################
Diseasepatient4 <- DiseasesInformation %>% #    1047349,1051457
  filter(eid %in% c("1025855"))%>%
  select(Disease, Dates)

#crossing at 64.6, never_smoked at 64
#first visit at 62 (2009)
#overweight at 59, non-severe obesity at 67 

Diseasepatient4$Dates <- as.Date(Diseasepatient4$Dates)

#Medical history of HBU participant in smoking dataset 
unique(Diseasepatient4$Disease)
# [1] "SOLID NEOPLASMS"              "DIABETES"                    
# [3] "HYPERTENSION"                 "COLITIS AND RELATED DISEASES"

#Group_by(PA_status)################################################################
# 
# ExCrossingPAStatus <- ExCrossing %>%
#   group_by(PA_status) %>%
#   mutate(Prevalence = n() ) %>%
#   ungroup() 

#Relative Frequency of PA_status per categorgy at crossing################################################################

# ExCrossingPAStatusPlot <- ExCrossingPAStatus %>%
#   filter(ExRangeCrossing == 1) %>%
#   select(eid, Category,PA_status, Crossing, Prevalence) %>% #Multimorb and Death out
#   group_by(PA_status, Category) %>%
#   unique() %>%
#   mutate(N = n()) %>%
#   ungroup() %>%
#   mutate(relative_frequency_ratio = N/Prevalence)

#Frequency Plot of PA Status per category################################################################

# pdf(paste0(IncludeFigHere, "/ggdotExCrossingCategorgyPA_status.pdf"), 5, 5)
# 
# print(
# ggdotchart(ExCrossingPAStatusPlot, x = "PA_status", y = "relative_frequency_ratio",
#            color = "Category",                                # Color by groups
#            palette = c( "#FC4E07","#00AFBB"), # Custom color palette
#            sorting = "descending",                       # Sort value in descending order
#            add = "segments",                             # Add segments from y = 0 to dots
#            rotate = TRUE,                                # Rotate vertically
#            group = "Category",                                # Order by groups
#            dot.size = 6, #8                                 # Large dot size
#            label = round(ExCrossingPAStatusPlot$relative_frequency_ratio, digits = 2),                        # Add values as dot labels
#            font.label = list(color = "white", size = 5, #9
#                              vjust = 0.5),               # Adjust label parameters
#            ggtheme = theme_pubr()                        # ggplot2 theme
# ))
# dev.off()
# 
# #Disease Frequency at exercise crossing############################################
# ExDiseasePrevCrossingPAStatus <- ExCrossingPAStatus %>%
#   select(c(eid, PA_status)) %>%
#   left_join(PrevCrossing) 
# 
# 
# #Disease Frequency per exercise status(PA_status) at crossing: StoppedEx, StartedEx, InactiveEx, AdviceEx, ActiveEX##############
# ExDiseasePrevCrossingPlotPAStatus <- ExDiseasePrevCrossingPAStatus %>%
#   filter(DiseasesRangeCrossing == 1) %>%
#   select(eid, Category,PA_status,Disease, Crossing, Prevalence) %>% 
#   group_by(Disease, PA_status) %>%
#   mutate(N = n()) %>%
#   #unique() %>%  #need unique?
#   ungroup() %>%
#   mutate(relative_frequency_ratio = N/Prevalence)
# 
# pdf(paste0(IncludeFigHere, "/ggdotExCrossingDiseasesPAstatus.pdf"), 20, 20)
# 
# print(ggdotchart(ExDiseasePrevCrossingPlotPAStatus, x = "Disease", y = "relative_frequency_ratio",
#                  color = "PA_status",                                # Color by groups
#                  palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
#                  sorting = "descending",                       # Sort value in descending order
#                  rotate = TRUE,                                # Rotate vertically
#                  dot.size = 2,                                 # Large dot size
#                  y.text.col = FALSE,                            # Color y text by groups
#                  ggtheme = theme_pubr()                        # ggplot2 theme
#                  
# )+theme_cleveland()
# )
# 
# dev.off()
