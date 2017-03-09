install.packages(c("Hmisc", "corrplot", "Amelia", "mice", "lattice", "missForest"))
install.packages(c("ggplot2", "caret", "mlr"))
install.packages("dplyr")
install.packages("VIM")

require(ggplot2)
require(caret)
require(mlr)
require(Hmisc)
require(corrplot)
require(Amelia)
require(mice)
require(ggplot2)
require(lattice)
require(missForest)
require(dplyr)
require(VIM)

#alternative way of loading and installing libraries quickely ----
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


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

new.train <- completedData %>% dplyr::filter(Id %in% train$Id) %>% bind_cols(train)





