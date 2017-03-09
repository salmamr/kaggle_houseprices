

#alternative way of loading and installing libraries quickely ----
load.libraries <- c("Hmisc", "corrplot", "Amelia", "mice", "lattice", "missForest", 
                    "ggplot2", "caret", "mlr", "dplyr", "VIM", "sampling", "Boruta")
                    
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


#load used functions ----
antilog<-function(lx,base) 
{ 
  lbx<-lx/log(exp(1),base=base) 
  result<-exp(lbx) 
  result 
} 

#read files ----
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

describe(train)

train$Alley[is.na(train$Alley)] <- "None"
train$BsmtQual[is.na(train$BsmtQual)] <- "None"
train$BsmtCond[is.na(train$BsmtCond)] <- "None"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "None"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "None"
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "None"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "None"
train$GarageType[is.na(train$GarageType)] <- "None"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- "None"
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
train$GarageQual[is.na(train$GarageQual)] <- "None"
train$GarageCond[is.na(train$GarageCond)] <- "None"
train$PoolQC[is.na(train$PoolQC)] <- "None"
train$Fence[is.na(train$Fence)] <- "None"
train$MiscFeature[is.na(train$MiscFeature)] <- "None"


test$Alley[is.na(test$Alley)] <- "None"
test$BsmtQual[is.na(test$BsmtQual)] <- "None"
test$BsmtCond[is.na(test$BsmtCond)] <- "None"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "None"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "None"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "None"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "None"
test$GarageType[is.na(test$GarageType)] <- "None"
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- "None"
test$GarageFinish[is.na(test$GarageFinish)] <- "None"
test$GarageQual[is.na(test$GarageQual)] <- "None"
test$GarageCond[is.na(test$GarageCond)] <- "None"
test$PoolQC[is.na(test$PoolQC)] <- "None"
test$Fence[is.na(test$Fence)] <- "None"
test$MiscFeature[is.na(test$MiscFeature)] <- "None"

hist(train$SalePrice)

# train$Id <- NULL
# test$Id <- NULL


all <- rbind(train[,-81], test)
# all$Id <- NULL

md.pattern(all)
aggr_plot <- aggr(all,
                  col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(all, m=5, maxit=50, meth='pmm', seed=5)
summary(tempData)


xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)

completedData <- complete(tempData,1)

train.response <- as.data.frame(train[,c(1,81)])

completed.train <- inner_join(completedData, train.response, by = "Id")
completed.test <- completedData %>% dplyr::filter(!Id %in% completed.train$Id)

indx <- sapply(completed.train, is.character)
completed.train[indx] <- lapply(completed.train[indx], function(x) as.factor(x))

indx <- sapply(completed.test, is.character)
completed.test[indx] <- lapply(completed.test[indx], function(x) as.factor(x))

completed.train$SalePrice <- log(completed.train$SalePrice)
hist(completed.train$SalePrice)

#train and test for first submission ----
regr.task <- makeRegrTask(id = "train1",
                         data = as.data.frame(completed.train[, !colnames(completed.train) %in% c("Id")]),
                         target = "SalePrice")
regr.lrn <- makeLearner("regr.gbm",
                       par.vals = list(n.trees = 2000, interaction.depth = 5))
mod <- train(regr.lrn, regr.task)
pred <- predict(mod,  newdata = as.data.frame(completed.test[, !colnames(completed.test) %in% c("Id")]))

pred.antilog <- antilog(pred$data$response)

sample_submission <- list()
sample_submission$Id <- completed.test$Id
sample_submission$SalePrice <- pred.antilog
sample_submission <- data.frame(sample_submission)

write.csv(sample_submission,
          paste(getwd(), "sample_submission_1.csv", sep = "/"),
          row.names = FALSE)


#split train set and tune using best algorithm ----
set.seed(5)
data.boruta <- completed.train[complete.cases(completed.train), ]
boruta.train <- Boruta(SalePrice ~ .,
                       data = as.data.frame(data.boruta[, !colnames(data.boruta) %in% c("Id")]),
                       doTrace = 2)
mar.default = c(14,1,1,1)
par(mar = mar.default + c(0, 4, 0, 0))
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


# train important features only ----
 completed.train.temp <- completed.train %>% dplyr::select(-PoolArea,
                                                      -PoolQC,
                                                      -LotConfig,
                                                      -MoSold,
                                                      -X3SsnPorch,
                                                      -MiscVal,
                                                      -LowQualFinSF,
                                                      -YrSold,
                                                      -MiscFeature,
                                                      -LandSlope,
                                                      -Street,
                                                      -RoofMatl,
                                                      -Heating,
                                                      -BsmtFinSF2,
                                                      -SaleType,
                                                      -BsmtCond,
                                                      -BsmtHalfBath,
                                                      -ExterCond,
                                                      -Fence,
                                                      -EnclosedPorch,
                                                      -Alley,
                                                      -SaleCondition,
                                                      -Utilities)

  completed.test.temp <- completed.test %>% dplyr::select(-PoolArea,
                                                     -PoolQC,
                                                     -LotConfig,
                                                     -MoSold,
                                                     -X3SsnPorch,
                                                     -MiscVal,
                                                     -LowQualFinSF,
                                                     -YrSold,
                                                     -MiscFeature,
                                                     -LandSlope,
                                                     -Street,
                                                     -RoofMatl,
                                                     -Heating,
                                                     -BsmtFinSF2,
                                                     -SaleType,
                                                     -BsmtCond,
                                                     -BsmtHalfBath,
                                                     -ExterCond,
                                                     -Fence,
                                                     -EnclosedPorch,
                                                     -Alley,
                                                     -SaleCondition,
                                                     -Utilities)
  #train and test for second submission ----
  regr.task <- makeRegrTask(id = "train1",
                            data = as.data.frame(completed.train.temp[, !colnames(completed.train.temp) %in% c("Id")]),
                            target = "SalePrice")
  
  regr.lrn <- makeLearner("regr.gbm",
                          par.vals = list(n.trees = 2000, interaction.depth = 5))
 
  mod <- train(regr.lrn, regr.task)
  pred <- predict(mod,  newdata = as.data.frame(completed.test.temp[, !colnames(completed.test.temp) %in% c("Id")]))
  
  pred.antilog <- antilog(pred$data$response)
  
  sample_submission <- list()
  sample_submission$Id <- completed.test.temp$Id
  sample_submission$SalePrice <- pred.antilog
  sample_submission <- data.frame(sample_submission)
  
  write.csv(sample_submission,
            paste(getwd(), "sample_submission_2.csv", sep = "/"),
            row.names = FALSE)
  
  
  #train and test for second submission ----
  
  completed.train.temp_2 <- completed.train[complete.cases(completed.train), ]
  completed.test.temp_2 <- completed.test[complete.cases(completed.test), ]
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  set.seed(5)
  model <- caret::train(SalePrice ~.,
                  data = as.data.frame(completed.train.temp_2[, !colnames(completed.train.temp_2) %in% c("Id")]),
                  method = "brnn",
                  metric="RMSE",
                  tuneGrid = NULL,
                  trControl = control)
    
  predict <- predict(model,
                     as.data.frame(completed.test.temp_2[, !colnames(completed.test.temp_2) %in% c("Id")]))
  pred.antilog <- antilog(predict)
  
  sample_submission <- list()
  sample_submission$Id <- completed.test$Id
  sample_submission$SalePrice <- pred.antilog
  sample_submission <- data.frame(sample_submission)
  
  write.csv(sample_submission,
            paste(getwd(), "sample_submission_3.csv", sep = "/"),
            row.names = FALSE)
  
  sample <- strata(completed.train, 
                   size = 50,
                   method = "srswor")











