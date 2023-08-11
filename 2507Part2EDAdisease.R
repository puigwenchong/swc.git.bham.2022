Window <- 5

#Comorbidity Studies Propossessing ########################################

#PrevCrossing ##################################
PrevCrossing <- DiseasesAndTrajectories %>%
  mutate(Crossing = case_when(Category == "Healthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              Category == "Unhealthy_Cross" ~ -Intercept/(AgeCoeffs-1), 
                              TRUE ~ 120
  ) ) %>%
  group_by(eid) %>%
  ungroup() %>%
  filter(Category != "Unknown") %>%
  mutate(DiseasesRangeCrossing = ifelse(abs(Age - Crossing) < Window, 1,0)) %>%
  group_by(Disease) %>%
  mutate(Prevalence = n() ) %>%
  ungroup() %>%
  mutate(InterpolatedPhenoAge = Intercept + AgeCoeffs * Age)

#number of participants in the disease dataset we are working with :199216 
length(unique(PrevCrossing$eid))


PrevCrossing$Category <- PrevCrossing$Category  %>%
  str_replace_all(., "Healthy_Cross", "UBH") %>%
  str_replace_all(., "Unhealthy_Cross", "HBU") %>%
  str_replace_all(., "Unhealthy_High", "URU") %>%
  str_replace_all(., "Healthy_Low", "HRH") 

#Relative Frequency Ratio of disease per category ##########################
PrevCrossingPlot <- PrevCrossing %>%
  filter(DiseasesRangeCrossing == 1) %>% 
  select(eid, Category,Disease, Age, Crossing, Prevalence) %>% 
  group_by(Disease, Category) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(relative_frequency_ratio = N/Prevalence)

#number of participants at Crossing in the disease dataset we are working with: 52708 
length(unique(PrevCrossingPlot$eid))

NumberPartDisease <- PrevCrossingPlot %>% 
  select(eid, Category) %>%
  unique() 

#number of participants at Crossing in the disease dataset per category: HBU=25617, UBH=27091
table(NumberPartDisease$Category)


#Participants with Health Transition in PhenoAge plot: "2283447","4141659" (will change) ############
#need someone with change in Lifestyle habit

pdf(paste0(IncludeFigHere, "/PhenoAgeDiseasesCrossing.pdf"), 10,10)

print(
  
  ggplot(PrevCrossing %>%
           filter(eid %in% c("1009878"))%>% 
           head(100), aes(x = Age, y = InterpolatedPhenoAge, group = eid)) + 
    geom_point(aes(colour = Disease, size = 1.2)) + geom_line() +
    geom_abline(aes(
      intercept = Intercept, 
      slope = AgeCoeffs,
      alpha = 0.9)) + #factor(Situation)
    xlim(30, 100) +
    ylim(30, 100) +
    geom_abline(slope=1, intercept = 0, linetype="dashed", colour = "blue") + 
    theme_bw() +
    labs(y = "Predicted Phenoage", x = "Age")
)


dev.off()



Diseasepatient7 <- DiseasesInformation %>%
  filter(eid %in% c("1009878"))%>%
  select(Disease, Dates)

#first visit at 2009-08-26, aged 57
#crossing at 51.6
# COPD at  2018, age 66,
# smoking at 51.9
# non severe obesity at 54, become overweight at 55 
Diseasepatient7$Dates <- as.Date(Diseasepatient7$Dates)

#Medical history of HBU participant in smoking dataset 
unique(Diseasepatient7$Disease)
# [1] "DYSLIPIDEMIA"                                        
# [2] "PERIPHERAL NEUROPATHY"                               
# [3] "OTHER NEUROLOGICAL DISEASES"                         
# [4] "HYPERTENSION"                                        
# [5] "ISCHEMIC HEART DISEASE"                              
# [6] "HEART FAILURE"                                       
# [7] "PERIPHERAL VASCULAR DISEASE"                         
# [8] "COPD, EMPHYSEMA, CHRONIC BRONCHITIS"                 
# [9] "ESOPHAGUS, STOMACH AND DUODENUM DISEASES"            
# [10] "COLITIS AND RELATED DISEASES"                        
# [11] "INFLAMMATORY ARTHROPATHIES"                          
# [12] "OSTEOARTHRITIS AND OTHER DEGENERATIVE JOINT DISEASES"
# [13] "DORSOPATHIES"                                        
# [14] "OTHER MUSCULOSKELETAL AND JOINT DISEASES"            
# [15] "OSTEOPOROSIS"                                        
# [16] "CHROMOSOMAL ABNORMALITIES"                           
# [17] "OTHER GENITOURINARY DISEASES"           

# No record
# Exercisepatient7 <- ExCrossing %>%
#      filter(eid %in% c("1009878"))%>%
#      select(term_description, AgePA)

# Smokingpatient7 <- SmokingCrossing_filtered %>%
#   filter(eid %in% c("1009878"))%>%
#   select(term_description, AgeSmoking)


pdf(paste0(IncludeFigHere, "/ggdotCrossingDiseases.pdf"), 20, 20)
#Disease Frequency Plot at crossing ########################################################################
print(
ggdotchart(PrevCrossingPlot, x = "Disease", y = "relative_frequency_ratio",
           color = "Category",                                # Color by groups
           palette = c( "#FC4E07","#00AFBB"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "Category",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(PrevCrossingPlot$relative_frequency_ratio, digits = 3),                        # Add values as dot labels
           font.label = list(color = "white", size = 5, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
)

dev.off()



QuickTable <- CreateCatTable(vars = c("Disease"), 
               strata = c("Category"), data = PrevCrossingPlot, test = F, addOverall = F)

QuickTableplot <- data.frame(QuickTable$HBU)


pdf(paste0(IncludeFigHere, "/QuickTable.pdf"), 18, 25)
    
grid.table(QuickTableplot, rows = NULL)
    
dev.off()

