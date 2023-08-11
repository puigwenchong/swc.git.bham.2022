#http://faculty.cas.usf.edu/mbrannick/regression/Logistic.html

mainDir <- "/rds/projects/g/gkoutosg-variant-prediction/MScData/Gwen"
NameRun <- "Final"
subDir <- paste0(sub('\\..*', '', NameRun), format(Sys.time(), '_%Y%m%d_%H%M'))
dir.create(file.path(mainDir, subDir))
IncludeFigHere <- file.path(mainDir, subDir)

setwd("/rds/projects/g/gkoutosg-variant-prediction/MScData/Gwen")

source("2507Part1loadingdata.R")
source("2507Part2EDAbaseline.R")
source("2507Part2EDAdisease.R")
source("2507Part2EDAsmoking.R")
source("2507Part2EDAbmi.R")   
source("2507Part2EDAexercise.R")
source("2507Part3FeatureSelection.R") 


 save(model_bmi,model_disease, model_smoking, file = paste0(IncludeFigHere, "/Models.RData" ))
 save(dflasso, file = paste0(IncludeFigHere, "/KeyResults.RData" ))
 save(SelectedCoeff,file = paste0(IncludeFigHere, "/SelectedFeatures.RData" ) )



#####################Final model#############################

IncludeFigHere <- "/rds/projects/g/gkoutosg-variant-prediction/MScData/Gwen/17July_20230717_1749"
# load(paste0(IncludeFigHere, "/SelectedFeatures.RData" ) )
# load(paste0(IncludeFigHere, "/KeyResults.RData" ) )
# load(paste0(IncludeFigHere, "/Models.RData" ) )
# 
#library(caret)
#train.data2 <-  preProcess(train.data, method = "nzv") 
#All <- names(train.data)
#final <- predict(train.data2, train.data)
#All2 <- names(final)

 lm3 <-  glm(Label ~ .,
             data=train.data %>% 
               dplyr::select(SelectedCoeff$Names[-1], Label),family = binomial)

 
#lm3$coefficients
dim(train.data)
dim(test.data)
#model_output <- tidy(lm3)

library(gt)
library(gtExtras)
library(broom)

# model_output <- model_output %>%
#   gt()

out_conf <- tidy(lm3, conf.int = TRUE)
# Result for train.data ##############################################
train_predictions <- predict(lm3, newdata = train.data %>% select(SelectedCoeff$Names[-1], Label), type="response")
train_predictions
train_actuals <- ifelse(train.data$Label > 0.5, 1, 0)
train_actuals

# Result for test.data ##############################################
test_predictions <- predict(lm3, newdata = test.data %>% select(SelectedCoeff$Names[-1], Label), type="response")
test_predictions
test_actuals <- ifelse(test.data$Label > 0.5, 1, 0)
test_actuals

#Plot the ROC curve for train and test data ############################################
TrainAUC <- data.frame(Label = train_actuals, Prediction = train_predictions)
TrainROCPlot <- ROCWithCI(TrainAUC)


TestAUC <- data.frame(Label = test_actuals, Prediction = test_predictions)
TestROCPlot <- ROCWithCI(TestAUC)

pdf(paste0(IncludeFigHere, "/TrainTestROCPlot.pdf"), 20, 10)
print(TrainROCPlot + TestROCPlot)
dev.off()

library(MLmetrics)

test.table_mat <- table(test.data$Label, test_predictions > 0.5)
test.table_mat

test_accuracy <- sum(diag(test.table_mat)) / sum(test.table_mat)
test_accuracy

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

test_precision <- precision(test.table_mat)
test_precision

test_recall <- recall(test.table_mat)
test_recall

test_f1 <- 2 * ((test_precision * test_recall) / (test_precision + test_recall))
test_f1

test_AUC <- AUC(test_predictions, test.data$Label)

TestResults <- data.frame(test_AUC, test_precision, test_recall, test_accuracy,test_f1)
TestResults <-round(TestResults, digits = 2)
TestResults
#################

train.table_mat <- table(train.data$Label, train_predictions > 0.5)
train.table_mat

train_accuracy <- sum(diag(train.table_mat)) / sum(train.table_mat)
train_accuracy

train_precision <- precision(train.table_mat)
train_precision

train_recall <- recall(train.table_mat)
train_recall

train_f1 <- 2 * ((train_precision * train_recall) / (train_precision + train_recall))
train_f1

train_AUC <- AUC(train_predictions, train.data$Label)

TrainResults <- data.frame(train_AUC, train_precision, train_recall, train_accuracy,train_f1)
TrainResults <-round(TrainResults, digits = 2)
TrainResults

#Venn diagram of  input data ######################################################



#Venn diagram of  input data ######################################################



library(ggvenn)

bmi_data <- model_bmi$eid
disease_data <- model_disease$eid
smoking_data <- model_smoking$eid
exercise_data <- model_exercise$eid


# Create a three-set Venn diagram
venn_data <- list(BMI = bmi_data, Smoking = smoking_data, Disease = disease_data, Exercise = exercise_data)

pdf(paste0(IncludeFigHere, "/modelVenn.pdf"), 10, 10)
print(ggvenn(venn_data))
dev.off()

