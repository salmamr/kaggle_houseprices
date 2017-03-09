# cleaning environment -----
rm(list = ls())

#package loading ----
library(dplyr)
library(aRpsDCA)
library(RCurl)
library(prettyR)
library(caret)
library(glmnet)
library(geosphere)
library(party)
library(mboost)
library(elasticnet)
library(rpart)
library(LiblineaR)
library(e1071)
library(ranger)
library(reshape2)
library(mice)
library(randomForest)
library(nnet)
library(kernlab)
library(brnn)
library(relaxo)
library(maps)
library(lubridate)
library(manipulate)
library(Boruta)
library(missForest)
library(caretEnsemble)

library("caret")
library("mlbench")
library("pROC")

library("mlbench")
library("randomForest")
library("nnet")
library("caTools")



# read input ----

#DATA.DIR = "C:\\Users\\melsaban\\Documents\\Kaggle\\HousePricing\\"


train.raw <- read.csv("train.csv") #,stringsAsFactors = FALSE)



# handle missing values with MICE package (or just drop NAs for now) ---- 
#train.raw_mids <- mice(train.raw)
#train.raw <- complete(train.raw_mids)

#train.raw = train.raw[complete.cases(train.raw),]

test.raw <- read.csv("test.csv") #, stringsAsFactors = FALSE)


# build models ----


# set.seed(5)
# train.raw_response <- train.raw[, "SalePrice"]
# train.raw_train_set <- createDataPartition(train.raw_response, p = 1, list = FALSE)
# head(train.raw_train_set)








glmnet_grid <- expand.grid(alpha = c(0, .1, .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl <- trainControl(method = "cv", number = 3)


# boruta_output <- Boruta(SalePrice~., data = train.raw, doTrace = 2)
# boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
# print(paste("contractor name", unique_contractor_names[contractor_index]))
# print(boruta_signif)  # significant variables
# plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance
# plot(boruta_output)

#filter some columns which lack many values ----
# train.raw.filtered = dplyr::select(train.raw, c(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature,
#                                                 -BsmtQual, -BsmtCond, -BsmtExposure, -BsmtFinType1, -BsmtFinType2, -LotFrontage,
#                                                 -MasVnrType, -MasVnrArea, -GarageType, -GarageYrBlt, -GarageFinish))
# 
# test.raw.filtered = dplyr::select(test.raw, c(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature,
#                                               -BsmtQual, -BsmtCond, -BsmtExposure, -BsmtFinType1, -BsmtFinType2, -LotFrontage,
#                                               -MasVnrType, -MasVnrArea, -GarageType, -GarageYrBlt, -GarageFinish))




train.raw.filtered = dplyr::select(train.raw, c(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature))

test.raw.filtered = dplyr::select(test.raw, c(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature))

#train.raw.filtered = train.raw
#test.raw.filtered = test.raw
#take complete cases ----
#train.raw_complete = train.raw.filtered[complete.cases(train.raw.filtered),]
#test.raw_complete = test.raw.filtered[complete.cases(test.raw.filtered),]


imputed_train <- missForest(train.raw.filtered)
imputed_test <- missForest(test.raw.filtered)

#dim(test.raw.filtered)
#dim(test.raw_complete)
# fitting single model with (random forest) ---- 



# model <- train(
#   SalePrice ~ .,
#   data = imputed_train$ximp,
#   method = "xgbTree")
# 
# 
# predicted <- predict(model, imputed_test$ximp)







training <- imputed_train
testing  <- imputed_test

training <- training$ximp
testing <- testing$ximp


my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  #classProbs=FALSE,
  index=createResample(training$SalePrice, 25)
  #summaryFunction=twoClassSummary
)


model_list_big <- caretList(
  SalePrice~., data=training,
  trControl=my_control,
  #metric="ROC",
  #methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=15)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10)),#, preProcess="pca")
    rf3=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=20))
    # gbm1=caretModelSpec(method="gbm", 
    #                     tuneGrid=data.frame(.mtry=15,n.trees = 100, 
    #                                         interaction.depth = 10, 
    #                                         shrinkage = 0.05, 
    #                                         n.minobsinnode = 10)),
    # gbm2=caretModelSpec(method="gbm", 
    #                     tuneGrid=data.frame(.mtry=10,n.trees = 200, 
    #                                         interaction.depth = 10, 
    #                                         shrinkage = 0.05, 
    #                                         n.minobsinnode = 10))
    
    #nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)


predictions_severalEnsembles <- as.data.frame(predict(model_list_big, newdata=testing))



##greedy ensemble ----

greedy_ensemble <- caretEnsemble(
  model_list_big, 
  #metric="ROC",
  trControl=trainControl(
    number=2
    #summaryFunction=twoClassSummary,
    #classProbs=TRUE
  ))
summary(greedy_ensemble)





#model_preds <- lapply(model_list, predict, newdata=testing, type="prob")
#model_preds <- lapply(model_preds, function(x) x[,"M"])
#model_preds <- data.frame(model_preds)
ens_preds_greedy <- predict(greedy_ensemble, newdata=testing)#, type="prob")




##stack of models ----



library("gbm")
gbm_ensemble <- caretStack(
  model_list_big,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  #metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final"
    #classProbs=TRUE,
    #summaryFunction=twoClassSummary
  )
)


ens_preds_stacking <- predict(gbm_ensemble, newdata=testing)#, type="prob")






# write submission file (single_model) ----


Kaggle_submission <- cbind(Id=test.raw.filtered$Id,SalePrice=predicted)
colnames(Kaggle_submission) <- c("Id","SalePrice")

write.csv(Kaggle_submission,file="Kaggle_submission_singleModel.csv",row.names=FALSE)



# write submission file (greedy ensemble) ----


Kaggle_submission <- cbind(Id=test.raw.filtered$Id,SalePrice=ens_preds_greedy)
colnames(Kaggle_submission) <- c("Id","SalePrice")

write.csv(Kaggle_submission,file="Kaggle_submission_ens_preds_greedy.csv",row.names=FALSE)


# write submission file (stacked ensemble) ----


Kaggle_submission <- cbind(Id=test.raw.filtered$Id,SalePrice=ens_preds_stacking)
colnames(Kaggle_submission) <- c("Id","SalePrice")

write.csv(Kaggle_submission,file="Kaggle_submission_ens_preds_stacking.csv",row.names=FALSE)