

#Classification Model Building ###########################################################
library(dplyr)
#extract data from comorbidity ################################## 
model_disease <- PrevCrossing %>%
  filter(DiseasesRangeCrossing == 1) %>% 
  filter(Category %in% c("HBU","UBH")) %>% 
  dplyr::select(eid,Disease, Category) %>% # maybe have to recheck tha smokers are now coded as started smoking
  unique() %>%
  mutate(Values = 1) %>%
  pivot_wider(names_from = Disease, values_from = Values, values_fill = 0)

model_smoking <- SmokingCrossing_filtered %>%
  filter(SmokingRangeCrossing == 1) %>% 
  dplyr::select(eid,Final2, Category ) %>% # maybe have to recheck tha smokers are now coded as started smoking
  unique() %>%
  mutate(Values = 1) %>%
  pivot_wider(names_from = Final2, values_from = Values, values_fill = 0) #%>%
  #dplyr::select(-c(Never_Smoked))


#extract data from sudden change bmi ###########################################################

model_bmi <- BMICrossing_filtered %>%
  filter(BMIRangeCrossing == 1) %>%
  dplyr::select(eid, WindowBmiRecord, BmiAllRecord, Category, RatioRecords, WeightChanges) %>%  #
  unique() %>%
  mutate(Values = 1) %>%
  pivot_wider(names_from = WeightChanges, values_from = Values, values_fill = 0)


#extract data from change in exercise habit ############################################################

#too many missing data, take out from model
 
 model_exercise <- ExCrossing %>%
   filter(ExRangeCrossing == 1) %>%
   dplyr::select(eid, ExerciseChanges, Category)%>%  #
   unique() %>%
   mutate(Values = 1) %>%
   pivot_wider(names_from = ExerciseChanges, values_from = Values, values_fill = 0) #%>%
   #dplyr::select(-c(InactiveExercise,ActiveExercise))



#Combine data as one dataframe ################################
model_disease$eid <- as.factor(model_disease$eid)
model_bmi$eid <- as.factor(model_bmi$eid)
model_exercise$eid <- as.factor(model_exercise$eid)
model_smoking$eid <- as.factor(model_smoking$eid)


#dflasso ###############################

dflasso <- model_smoking %>%
  inner_join(., model_exercise) %>%
  inner_join(., model_bmi) %>%
  inner_join(., model_disease) %>%
  unique() 


#missing_value_report #######################
# library(DataExplorer)
# 
# pdf(paste0(IncludeFigHere, "/lassoMissingness.pdf"), 10, 10)
# print(plot_missing(dflasso))
# dev.off()
 


dflasso <- dflasso %>%
  drop_na() %>%
  clean_names()



#UBH(Healthy_cross)=1, HBU(Unhealthy_cross)=0
dflasso$Label<- ifelse(
  dflasso$category== "UBH" ,1,0) 

dflasso <- dflasso %>%
  dplyr::select(-c(category,eid))


library(caret)

#non-zero variance
dflasso2 <-  preProcess(dflasso, method = "nzv") 
#All <- names(train.data)

#All2 <- names(final)





#library(DataExplorer)
#create_report(dflasso, output_dir = IncludeFigHere)


library(tidyverse)
library(glmnet)
library(lattice)
library(reshape2)


#train-test split #############################################################
set.seed(123)
## create a list of 80% of the rows in the original dataset we can use for training
training.samples <- dflasso$Label %>% 
  createDataPartition(p = 0.8, list = FALSE) #0.8

# use the remaining 80% of data to training and testing the models
train.data  <- predict( dflasso2, dflasso[training.samples, ]) #)dflasso[training.samples, ]
# select 20% of the data for validation
test.data <-  predict( dflasso2, dflasso[-training.samples, ]) ##dflasso[-training.samples, ]

# dimensions of dataset:
dim(dflasso)

# list types for each attribute
sapply(dflasso, class)

# summarize the class distribution: Class 0 =HBU(44.2%), Class 1 = UBH(55.8%)##########################
percentage <- prop.table(table(dflasso$Label)) * 100
cbind(freq=table(dflasso$Label), percentage=percentage)
percentage


# Compute AUC-ROC using function #######################################
library(plotROC)


ROCWithCI <- function(train_probsValCalc) {
  
  # train_probsValCalc <- MeanAdapted
  #train_probsValCalc$Label <- as.numeric((as.factor(train_probsValCalc$Label))) - 1
  #train_probsValCalc <- ValidationWPredictionLeucine
  
  obj <-
    pROC::roc(
      train_probsValCalc$Label,
      train_probsValCalc$Prediction,
      ci = TRUE,
      plot = FALSE
    )
  ciobj <-
    ci.se(obj, specificities = seq(0, 1, l = 25), method = "bootstrap")
  dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                       lower = ciobj[, 1],
                       upper = ciobj[, 3])
  
  basicplot <-
    ggplot(train_probsValCalc, aes(d = Label, m = Prediction)) +
    geom_roc(n.cuts = 50, labels = FALSE) +
    style_roc(
      ylab = "Sensitivity",
      xlab = "1 - Specificity",
      minor.breaks = c(seq(0, 0.1, by = 0.01), seq(0.9, 1, by = 0.01)),
      guide = TRUE
    )
  
  dat.ci2 <- dat.ci %>%
    mutate(x = 1 - x)
  
  
  Vald <- basicplot +
    #ggtitle(paste0(title,"-", ChosenRun)) +
    annotate(
      "text",
      x = .7,
      y = .25,
      label = paste("AUC =", round(calc_auc(basicplot)$AUC, 4)),
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = .7,
      y = .15,
      label = paste(gsub(
        "\\s*\\([^\\)]+\\)", "", capture.output(obj$ci)
      )),
      fontface = "bold"
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      color = "grey",
      linetype = "dashed"
    ) +
    geom_ribbon(
      data = dat.ci2,
      aes(x = x, ymin = lower, ymax = upper),
      inherit.aes = FALSE,
      fill = "steelblue",
      alpha = 0.2
    )
  
  return(Vald)
}




#########



set.seed(123)

# Define the number of  loop iterations
num_inner <- 1000

# Initialize a list to store AUC values
auc_values <- vector("list", length = num_inner)
Results <- list()

#lower the lambda, more stringent in selection
lambda_values <- c()

#number of effective coefficients selected
df_values <- c()

#  # Initialize a vector to store AUC values for the loop
inner_aucs <- vector("numeric", length = num_inner)
#  
# loop for train/validation split


library(caret)


df.lasso3  <- train.data





for (j in 1:num_inner) {
  
  print(j)
  # Split train data into train/validation
  inner_samples <- df.lasso3$Label %>% createDataPartition(p = 0.8, list = FALSE) #0.8
  train_inner <- df.lasso3[inner_samples, ]
  validation_inner <- df.lasso3[-inner_samples, ]
  
  # Predictor variables for loop
  x_inner <- model.matrix(Label ~ ., train_inner)[,-1]
  # Outcome variable for  loop
  y_inner <- train_inner$Label
  
  # Perform LASSO on the loop train data
  cv_inner <- cv.glmnet(x_inner, y_inner, alpha = 1, grouped = FALSE, nfolds = 20)
  model_inner <- glmnet(x_inner, y_inner, alpha = 1, lambda = cv_inner$lambda.min)
  Results[[j]] <- as.matrix(coef(model_inner))
  lambda_values <- c(lambda_values, cv_inner$lambda.min)
  df_values <- c(df_values, model_inner$df)

  # Predict on the loop validation data
  x_val_inner <- model.matrix(Label ~ ., validation_inner)[,-1]
  predictions_inner <- predict(model_inner, x_val_inner)
  
  # Convert predictions to a numeric vector
  predictions_inner <- as.numeric(predictions_inner)
  
  # Create binary indicator variable for actual class labels
  actual_inner <- validation_inner$Label #ifelse(validation_inner$Label > 0.5, 1, 0)
  
  # Ensure both levels (0 and 1) are present in actual_inner
 #if (sum(actual_inner == 0) == 0 || sum(actual_inner == 1) == 0) {
 #  # Skip this iteration if one of the levels is missing
 #  next
 #}
  
  # Compute AUC-ROC for the loop
  roc_obj_inner <- roc(actual_inner, predictions_inner)
  inner_aucs[j] <- auc(roc_obj_inner)
}


#Raw data of Coefficients of models from  loops ###############################
Coefficients <-  Results %>% 
  bind_cols() %>%
  add_column(Names = row.names(Results[[1]])) %>% 
  pivot_longer(-Names) 


#selected Coeff ################################################
SelectedCoeff <- Coefficients %>% 
  filter(value != 0) %>%
  group_by(Names) %>%
  dplyr::summarise(count=n()) %>%
  filter(Names != "(Intercept)")

SelectedCoeff90 <- Coefficients %>% 
  filter(value != 0) %>%
  group_by(Names) %>%
  dplyr::summarise(count=n()) %>% 
  filter(count > num_inner*0.9) %>%
  filter(Names != "(Intercept)")

#=========
# median_coeff_each_feature <- Coefficients %>% 
#   group_by(Names) %>%
#   dplyr::summarise(median=median(value)) %>% 
#   filter(Names != "(Intercept)")
# 
# mean_of_median_coeff_each_feature = abs(mean(median_coeff_each_feature$median))
# mean_of_median_coeff_each_feature
# 
# StatCoeff <- Coefficients %>% 
#   group_by(Names) %>%
#   dplyr::summarise(median=median(value)) %>%
#   filter(median > mean_of_median_coeff_each_feature) %>%
#   filter(Names != "(Intercept)")

library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(df.lasso3, as.integer))
# Plot the graph
pdf(paste0(IncludeFigHere, "/heatmap.pdf"), 35, 35)  



plot <- ggcorr(corr,
             method = c("pairwise", "spearman"),
             nbreaks = 6,
             hjust = 1,
             label = TRUE,
             label_size = 6,
             color = "grey50")

print(plot + theme(legend.position = "bottom"))
dev.off()




#=========

#Create a dataframe for the result of 1000 models in the loop ###############################
bagging <- data.frame(AUC = inner_aucs,lambda_values, df_values)


#group  together 
library(patchwork)


pdf(paste0(IncludeFigHere, "/bagging.pdf"), 15, 15)  
A1 <- ggplot(bagging, aes(x = lambda_values, y = AUC, color = df_values )) +
  geom_jitter() +
  geom_point() +
  labs(x = "lambda_values", y = "AUC", color = "number of selected coefficients") +
  theme_minimal()

A2 <- ggboxplot(Coefficients %>% filter(Names %in% SelectedCoeff90$Names),
                  x = "Names",
                  y = "value",
                  color = "black",
                  palette = "FC4E07",
                  sort.val = "desc",
                  sort.by.groups = FALSE,
                  x.text.angle = 90,
                  ylab = "coefficients",
                  legend.title = "Group",
                  rotate = TRUE,
                  ggtheme = theme_minimal()) +
  geom_hline(yintercept = 0, colour="red",linewidth = 0.5) + 
  coord_flip() 


A3 <-  ggplot(SelectedCoeff, aes(x = reorder(Names, count), weight = count, fill = ifelse(count > num_inner*0.9, "Red", "Blue"))) +
  geom_bar() +
  theme_minimal() +
  coord_flip() +
  xlab("LASSO Selected Coefficients Names")+
  guides(fill = "none")

#====

# A4 <- ggboxplot(Coefficients %>% filter(Names %in% StatCoeff$Names),
#                 x = "Names",
#                 y = "value",
#                 color = "black",
#                 palette = "FC4E07",
#                 sort.val = "desc",
#                 sort.by.groups = FALSE,
#                 x.text.angle = 90,
#                 ylab = "coefficients",
#                 legend.title = "Group",
#                 rotate = TRUE,
#                 ggtheme = theme_minimal()) +
#   geom_hline(yintercept = 0, colour="red",linewidth = 0.5) + 
#   coord_flip() 
# 
# 
# A5 <-  ggplot(StatCoeff, aes(x = reorder(Names, median), weight = median, fill = ifelse(median > mean_of_median_coeff_each_feature, "Red", "Blue"))) +
#   geom_bar() +
#   theme_minimal() +
#   coord_flip() +
#   xlab("LASSO Selected Coefficients Names")+
#   guides(fill = "none")
  

#====


print(A3 + A2 / A1)
# print(A5 + A4 / A1)


dev.off()

