#import library 
library(ggpubr) 
library(gtExtras)

#Baseline characteristics ###############################################
LifestyleTable <- LifestyleFirstVisit %>% 
  filter(Category != "Unknown") %>% 
  select(-c(AgeCoeffs, Intercept)) %>%
  rename_with(~ if_else(.x == "Age when attended assessment centre", "Age",
                        if_else(.x == "Townsend deprivation index at recruitment", "Townsend deprivation index",
                                if_else(.x == "Body mass index (BMI)", "BMI",
                                        if_else(.x == "Genetic sex", "Sex",
                                                if_else(.x == "Smoking status", "Never smoked", .x)))))) %>%
  clean_names() %>%
  mutate(across(c(sex, never_smoked), factor),
         across(where(is.numeric), ~round(., digits = 2))) %>%
  mutate(never_smoked = recode_factor(never_smoked, 
                                      "1" = "Previous", 
                                      "2" = "Current", 
                                      "0" = "Never", 
                                      "-3" = "Prefer not to answer"),
         sex = recode_factor(sex, 
                             "0" = "Female", 
                             "1" = "Male"))


LifestyleTable <- LifestyleTable %>%
  select(-c(date_of_attending_assessment_centre,overall_health_rating,mothers_age_at_death,fathers_age_at_death,ethnic_background,age_at_death,had_major_operations))


#rename Linear trajectory####################################################
LifestyleTable$category <- LifestyleTable$category   %>%
  str_replace_all(., "Healthy_Cross", "UBH") %>%
  str_replace_all(., "Unhealthy_Cross", "HBU") %>%
  str_replace_all(., "Unhealthy_High", "URU") %>%
  str_replace_all(., "Healthy_Low", "HRH") 


LifestyleTable <- LifestyleTable %>%
  mutate(BMI_Category = case_when(
    bmi < 18.4999 ~ "Underweight",
    bmi >= 18.5 & bmi <= 24.9999 ~ "Normal_weight",
    bmi >= 25.0 & bmi <= 29.9999 ~ "Overweight",
    bmi >= 30.0 & bmi <= 39.9999 ~ "Non-severe_obesity",
    bmi >= 40 ~ "Severe_obesity"))

#count comorbidity#############################
diseases_per_patient <- # aggregate(Disease ~ eid, data = DiseasesAndTrajectories, FUN = function(x) length(unique(x)))
  DiseasesAndTrajectories %>%
  group_by(eid)  %>%
  dplyr::summarise(Disease = length(unique(Disease)))


diseases_per_patient$eid <- as.factor(diseases_per_patient$eid)
LifestyleTable$eid <- as.factor(LifestyleTable$eid)


#including baseline comorbidity 
LifestyleTable <- left_join(LifestyleTable, diseases_per_patient)

#write.csv(LifestyleTable,"LifestyleTable.csv")

LifestyleTable1 <- LifestyleTable %>%
  drop_na()

#stat tables################################
library(tableone) 
# # library(Hmisc)
#  library(psych)
# # describe(LifestyleTable)
#  describeBy(LifestyleTable, LifestyleTable$category)

 Table1 <- CreateTableOne(vars = c("met_minutes_per_week_for_walking","met_minutes_per_week_for_vigorous_activity","met_minutes_per_week_for_moderate_activity","summed_met_minutes_per_week_for_all_activity","Disease","bmi", "age", "basal_metabolic_rate", "townsend_deprivation_index", "body_fat_percentage"), 
                          strata = c("category"), data = LifestyleTable, test = T, addOverall = F)
 table_1 <- print(Table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

 table_1$Variables <- rownames(table_1)
# gt(table_1) #%>%
   # gtsave("table_1.png", expand = 10, paste0(IncludeFigHere, "/table_1.png"))
 
 write.csv(table_1, paste0(IncludeFigHere, "/table_1.csv"))

 Table2 <- CreateCatTable(vars = c("sex","never_smoked","BMI_Category"), #,
                          strata = c("category"), data = LifestyleTable, addOverall = F, test = T)

 table_2 <- print(Table2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
 write.csv(table_2, paste0(IncludeFigHere, "/table_2.csv"))
 # gt(table_2) #%>%
    #gtsave("table_2.png", expand = 10, paste0(IncludeFigHere, "/table_2.png"))
  
 # Baseline_result1 <- data.frame(table_1) %>%
 #   gt()
 # 
  # Baseline_result2 <- data.frame(table_2) %>%
  #   gt()

#Bmi distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityBMICategorgy.pdf"), 8, 6)
B2a <-ggdensity(LifestyleTable1, x = "bmi",
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))

print(B2a)
dev.off()

#Age distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityAgeCategorgy.pdf"), 8, 6)
B2b <-ggdensity(LifestyleTable1, x = "age",
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2b)
dev.off()

#Comorbidity distribution
pdf(paste0(IncludeFigHere, "/ggddensityComorbidityCategorgy.pdf"), 8, 6)
B2c <-ggdensity(LifestyleTable1, x = "Disease", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2c)
dev.off()

#Smoking status######################################################

pdf(paste0(IncludeFigHere, "/ggbarSmokingCategorgy.pdf"), 8, 6)
B2d <-ggplot(LifestyleTable1) +
  aes(x = never_smoked, fill =never_smoked) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(category), ncol = 1L)
print(B2d)
dev.off()

#BMI categories######################################################

pdf(paste0(IncludeFigHere, "/ggbarBMICategorgy.pdf"), 8, 6)
B2e <- ggplot(LifestyleTable1) +
  aes(x = BMI_Category, fill =BMI_Category) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(category), ncol = 1L)
print(B2e)
dev.off()


#Sex######################################################

pdf(paste0(IncludeFigHere, "/ggbarSexCategorgy.pdf"), 8, 6)

B2f <-ggplot(LifestyleTable1) +
  aes(x = sex, fill = sex) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(category), ncol = 1L)

print(B2f)
dev.off()


#body fat % distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityBodyfatCategorgy.pdf"), 8, 6)

B2g <-ggdensity(LifestyleTable1, x = "body_fat_percentage", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2g)
dev.off()


#basal metabolic rate distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensitymetabolicrateCategorgy.pdf"), 8, 6)

B2h <-ggdensity(LifestyleTable1, x = "basal_metabolic_rate", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2h)
dev.off()


#met_minutes_per_week_for_walking distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityMETwalkingCategorgy.pdf"), 8, 6)

B2i <-ggdensity(LifestyleTable1, x = "met_minutes_per_week_for_walking", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2i)
dev.off()

#met_minutes_per_week_for_moderate_activity distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityMETmoderateCategorgy.pdf"), 8, 6)

B2j <-ggdensity(LifestyleTable1, x = "met_minutes_per_week_for_moderate_activity", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2j)
dev.off()


#met_minutes_per_week_for_vigorous_activity distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityMETvigorousCategorgy.pdf"), 8, 6)

B2k <-ggdensity(LifestyleTable1, x = "met_minutes_per_week_for_vigorous_activity", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2k)
dev.off()

#summed_met_minutes_per_week_for_all_activity distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensityMETsumCategorgy.pdf"), 16, 8)
B2l <-ggdensity(LifestyleTable1, x = "summed_met_minutes_per_week_for_all_activity", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2l)
dev.off()



#summed_met_minutes_per_week_for_all_activity distribution######################################################

pdf(paste0(IncludeFigHere, "/ggddensitytownsend.pdf"), 8, 6)
B2m <-ggdensity(LifestyleTable1, x = "townsend_deprivation_index", 
                add = "mean", rug = TRUE,
                color = "category", fill = "category",
                palette = c("#00AFBB", "#E7B800","#FC4E07","#B19CD9"))
print(B2m)
dev.off()

pdf(paste0(IncludeFigHere, "/Allbasalline.pdf"), 20, 20)
print(B2a + B2b + B2c + B2d + B2f + B2g + B2h + B2l + B2m)
dev.off()

#odd ratio########################################
LifestyleTable <- LifestyleTable %>%
  mutate(sex.factor = factor(sex) %>%          
           fct_recode("Female" = "0",
                      "Male"   = "1") %>% 
           ff_label("Sex"), 
         
         category.factor = factor(category) %>% 
           fct_recode("HBU"  = "0",
                      "UBH" = "1",
                      "URU" = "2",
                      "HRH" = "3") %>% 
           #fct_relevel("HRH") %>% 
           ff_label("category"),
         
         smoking.factor = factor(never_smoked) %>% 
           fct_recode("<NA>" = "999",
                      "Prefer not to answer" = "-3",
                      "Previous" = "2",
                      "Current" = "1",
                      "Never"  = "0") %>% 
           ff_label("Smoking status"),
         
         age  = ff_label(age,  "Age (years)"),      
         
         townsend_deprivation_index  = ff_label(townsend_deprivation_index,  "Townsend deprivation index"),
         
         body_fat_percentage  = ff_label(body_fat_percentage,  "Body Fat percentage(%)"),
         
         basal_metabolic_rate   = ff_label(basal_metabolic_rate ,  "Basal metabolic rate"),
         
         Disease   = ff_label(Disease ,  "Multimorbidity"),
         
         met_minutes_per_week_for_walking  = ff_label(met_minutes_per_week_for_walking,  "MET minutes for walking (per week)"),
         
         met_minutes_per_week_for_moderate_activity  = ff_label(met_minutes_per_week_for_moderate_activity,  "MET minutes for moderate activity (per week)"),
         
         met_minutes_per_week_for_vigorous_activity  = ff_label(met_minutes_per_week_for_vigorous_activity,  "MET minutes for vigorous activity (per week)"),
         
         summed_met_minutes_per_week_for_all_activity  = ff_label(summed_met_minutes_per_week_for_all_activity,  "MET minutes for all activities (per week)"),
         
         bmi = ff_label(bmi, "BMI (kg/m2)"),
         
         BMI_Category.factor = factor(BMI_Category ) %>% 
           fct_recode("NA" = "999",
                      "Underweight"  = "0",
                      "Normal_weight" = "1",
                      "Overweight" = "2",
                      "Non-severe_obesity" = "3",
                      "Severe_obesity" = "4") %>% 
           #fct_relevel("Alive") %>% 
           ff_label("BMI_Category"))



 dependent <- "category.factor"
 explanatory <- c("age", 
                  "sex.factor",
                  "smoking.factor",
                  "bmi", 
                  "BMI_Category.factor", 
                  "body_fat_percentage",
                  "basal_metabolic_rate",
                  "Disease",
                  "townsend_deprivation_index",
                  "met_minutes_per_week_for_walking",
                  "met_minutes_per_week_for_moderate_activity", 
                  "met_minutes_per_week_for_vigorous_activity", 
                  "summed_met_minutes_per_week_for_all_activity") # 
 LifestyleTable %>% 
   summary_factorlist(dependent, explanatory, p = TRUE,
                      add_dependent_label = TRUE) -> t1 
 
 write.csv(LifestyleTable, paste0(IncludeFigHere, "/LSTable.csv"))
 
 write.csv(t1, paste0(IncludeFigHere, "/t1.csv"))
 
 pdf(paste0(IncludeFigHere, "/t1.pdf"), 20, 20)
  #print(gt(t1))
 print(grid.table(t1, rows = NULL))
 dev.off()
 
# knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r")) 


library(nnet)
# Fit the multinomial logistic regression model
O1 <- multinom(category.factor ~ age + 
                 sex.factor + 
                 smoking.factor +
                 bmi + 
                 BMI_Category.factor + 
                 body_fat_percentage + 
                 basal_metabolic_rate + 
                 Disease + 
                 townsend_deprivation_index + 
                 met_minutes_per_week_for_walking + 
                 met_minutes_per_week_for_moderate_activity + 
                 met_minutes_per_week_for_vigorous_activity + 
                 summed_met_minutes_per_week_for_all_activity, data = LifestyleTable)

library(broom.mixed)
odds_ratio_table <- tidy(O1, exponentiate = TRUE, conf.int = TRUE) 




filtered_odds_ratio <-   odds_ratio_table %>% 
  filter(term != "(Intercept)" & y.level %in% c("UBH", "HRH", "HBU")) %>%
  mutate_at(vars(-term, -y.level), round, digits = 3)


pdf(paste0(IncludeFigHere, "/Oddratio.pdf"), 10, 10)

coef_fplot <-filtered_odds_ratio %>%
  mutate(y.level = case_when(
    y.level == "UBH" ~ "UBH",
    y.level == "URU" ~ "URU",
    y.level == "HBU" ~ "HBU"),
    term = case_when(
      term == "age" ~ "age",
      term == "bmi" ~  "BMI",
      term == "sex.factorMale" ~  "sex Male",
      term == "BMI_Category.factorUnderweight" ~  "BMI_Category Underweight",
      term == "BMI_Category.factorNormal_weight" ~  "BMI_Category Normal weight",
      term == "BMI_Category.factorOverweight" ~  "BMI_Category Overweight",
      term == "BMI_Category.factorSevere_obesity" ~  "BMI_Category Severe_obesity",
      term == "body_fat_percentage" ~  "body fat percentage",
      term == "basal_metabolic_rate" ~  "basal metabolic rate",
      term == "NumDiseases" ~  "Multimorbidity",
      term == "townsend_deprivation_index" ~  "townsend deprivation index",
      term == "met_minutes_per_week_for_walking" ~  "met minutes per week for walking",
      term == "met_minutes_per_week_for_moderate_activity" ~  "met minutes per week for moderate activity",
      term == "met_minutes_per_week_for_vigorous_activity" ~  "met minutes per week for vigorous_activity",
      term == "summed_met_minutes_per_week_for_all_activity" ~  "summed met minutes per week for all activity"))%>%
  ggplot(aes(y = term, x = estimate, pch = y.level, label = estimate)) + 
  geom_point(aes(y = term, x=estimate), color= "#FF6666") +  
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low,height = .12), color ="#FF6666",size = 0.6) +  
  geom_vline(xintercept =1, linetype = "dashed") +
  scale_shape_manual(values = c(0,2,19)) +
  geom_text(size = 2, nudge_x = 2,vjust = -0.1) + 
  facet_grid(.~y.level) +
  scale_x_continuous(name ="Regression Coefficients with Odds Ratio", limits=c(0.5,1.8)) +
  theme(legend.position = "bottom") 

print(coef_fplot)
dev.off()



grid.table(ICD10, rows = NULL)
dev.off()
