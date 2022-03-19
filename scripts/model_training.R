#Install packages

#install.packages("xgboost")
#install.packages("caret")
install.packages("randomForest")
install.packages("ROCR")

#Import packages
library(dplyr)
library(xgboost)
library(tidyr)
library(ggplot2)
library(caret)
library(pROC)
library(randomForest)
library(rpart)
library(ROCR)
library(class)
`?`(knn)
##### UTIL FUNCTIONS ######

## DRAW CM FUNCTION ##

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, '1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, '0', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, '1', cex=1.2, srt=90)
  text(140, 335, '0', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

## DRAW CM FUNCTION ##

#Set work directory

setwd("/Users/egeoguzman/Desktop/NCI/MLDM")

#Read Data

full_dataset <- read.csv(file='/Users/egeoguzman/Desktop/NCI/MLDM/processed_data/full_dataset.csv', na.strings = '..',stringsAsFactors = FALSE)

names(full_dataset)

#Prepare Train Dataset

# Match Stats Dataset

drops <- c("X","Season", "DayNum","T1TeamID", "T2TeamID","T1Loc","NumOT","T1Score","T2Score","SEEDDIFF")
train_dataset <- full_dataset[ , !(names(full_dataset) %in% drops)]
names(train_dataset)

# Test - Train Split %10 / %90

trainSize <- round(nrow(train_dataset) * 0.8)
testSize <- nrow(train_dataset) - trainSize

set.seed(42)
training_indices <- sample(seq_len(nrow(train_dataset)),
                           size=trainSize)

trainSet <- train_dataset[training_indices, ]
testSet <- train_dataset[-training_indices, ]

trainSet[, c("T1FTR", "T2FTR", "T1ELO", "T2ELO")] <- sapply(trainSet[, c("T1FTR", "T2FTR", "T1ELO", "T2ELO")], as.numeric)
testSet[, c("T1FTR", "T2FTR", "T1ELO", "T2ELO")] <- sapply(testSet[, c("T1FTR", "T2FTR", "T1ELO", "T2ELO")], as.numeric)

#trainSet[,'T1WON']<-numeric(trainSet[,'T1WON'])
#testSet[,'T1WON']<-numeric(testSet[,'T1WON'])

trainSet <- trainSet %>% drop_na()
testSet <- testSet %>% drop_na()

names(trainSet)
nrow(trainSet)

names(testSet)
nrow(testSet)

dim(trainSet)
dim(testSet)

sapply(trainSet, typeof)
sapply(testSet, typeof)

sapply(trainSet, class)
sapply(testSet, class)

trainSet
testSet

# Seed Dataset

seed_diff_trainSet = full_dataset[, 58:59]
head(seed_diff_trainSet)
colSums(is.na(seed_diff_trainSet))

seed_trainSize <- round(nrow(seed_diff_trainSet) * 0.9)
seed_testSize <- nrow(seed_diff_trainSet) - seed_trainSize

set.seed(42)
seed_training_indices <- sample(seq_len(nrow(seed_diff_trainSet)),
                                size=seed_trainSize)

seed_diff_train = seed_diff_trainSet[seed_training_indices, ]
seed_diff_test = seed_diff_trainSet[-seed_training_indices, ]

seed_diff_train[, c("SEEDDIFF")] <- sapply(seed_diff_train[, c("SEEDDIFF")], as.numeric)
seed_diff_test[, c("SEEDDIFF")] <- sapply(seed_diff_test[, c("SEEDDIFF")], as.numeric)

seed_diff_train <- seed_diff_train %>% drop_na()
seed_diff_test <- seed_diff_test %>% drop_na()

seed_diff_train <- seed_diff_train[!grepl("NA", seed_diff_train$SEEDDIFF),]
seed_diff_test <- seed_diff_train[!grepl("NA", seed_diff_train$SEEDDIFF),]

dim(seed_diff_train)
dim(seed_diff_test)

sapply(seed_diff_train, typeof)
sapply(seed_diff_test, typeof)

sapply(seed_diff_train, class)
sapply(seed_diff_test, class)

seed_diff_train
seed_diff_test

# ELO Dataset

elo_diff_trainSet = full_dataset[, c(56,57,59)]
head(elo_diff_trainSet)
colSums(is.na(elo_diff_trainSet))

elo_trainSize <- round(nrow(elo_diff_trainSet) * 0.9)
elo_testSize <- nrow(elo_diff_trainSet) - elo_trainSize

set.seed(42)
elo_training_indices <- sample(seq_len(nrow(elo_diff_trainSet)),
                                size=elo_trainSize)

elo_diff_train = elo_diff_trainSet[elo_training_indices, ]
elo_diff_test = elo_diff_trainSet[-elo_training_indices, ]

elo_diff_train[, c("T1ELO")] <- sapply(elo_diff_train[, c("T1ELO")], as.numeric)
elo_diff_test[, c("T1ELO")] <- sapply(elo_diff_test[, c("T1ELO")], as.numeric)
elo_diff_train[, c("T2ELO")] <- sapply(elo_diff_train[, c("T2ELO")], as.numeric)
elo_diff_test[, c("T2ELO")] <- sapply(elo_diff_test[, c("T2ELO")], as.numeric)

elo_diff_train <- elo_diff_train %>% drop_na()
elo_diff_test <- elo_diff_test %>% drop_na()

dim(elo_diff_train)
dim(elo_diff_test)

sapply(elo_diff_train, typeof)
sapply(elo_diff_test, typeof)

sapply(elo_diff_train, class)
sapply(elo_diff_test, class)

elo_diff_train
elo_diff_test


#### XGBOOST ####

# Create training matrix
dtrain <- xgb.DMatrix(data = as.matrix(trainSet[, c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO")]), label = trainSet$T1WON) 
# Create test matrix
dtest <- xgb.DMatrix(data = as.matrix(testSet[, c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO")]), label = testSet$T1WON)

set.seed(12345)
bst_1 <- xgb.cv(data = dtrain, 
                nfold=5,
                nrounds = 2000, 
                eta=.1,
                
                verbose = 1, 
                print_every_n = 20, 
                early_stopping_rounds = 20,
)

set.seed(12345)
bst_mod_1 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.005, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)

set.seed(12345)
bst_mod_2 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.01, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)

set.seed(12345)
bst_mod_3<- xgb.cv(data = dtrain, # Set training data
                   
                   nfold=5,
                   
                   eta = 0.05, # Set learning rate
                   max.depth =  7, # Set max depth
                   min_child_weight = 10, # Set minimum number of samples in node to split
                   gamma = 0, # Set minimum loss reduction for split
                   subsample =  0.9, # Set proportion of training data to use in tree
                   colsample_bytree = 0.9, # Set number of variables to use in each tree
                   
                   nrounds = 100, # Set number of rounds
                   early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                   
                   verbose = 1, # 1 - Prints out fit
                   nthread = 1, # Set number of parallel threads
                   print_every_n = 20, # Prints out result every 20th iteration
)
set.seed(12345)
bst_mod_4 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.1, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)
set.seed(12345)
bst_mod_5 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold=5,
                    
                    eta = 0.3, # Set learning rate
                    max.depth =  7, # Set max depth
                    min_child_weight = 10, # Set minimum number of samples in node to split
                    gamma = 0, # Set minimum loss reduction for split
                    subsample =  0.9, # Set proportion of training data to use in tree
                    colsample_bytree = 0.9, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
)

# Extract results for model with eta = 0.005
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.005, nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- "eta"
# Extract results for model with eta = 0.01
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.01, nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- "eta"
# Extract results for model with eta = 0.05
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.05, nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- "eta"
# Extract results for model with eta = 0.1
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.1, nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- "eta"
# Extract results for model with eta = 0.3
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[,c("iter", "test_rmse_mean")], rep(0.3, nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- "eta"
# Join datasets
plot_data <- rbind.data.frame(pd1, pd2, pd3, pd4, pd5)
# Converty ETA to factor
plot_data$eta <- as.factor(plot_data$eta)
# Plot points

# Plot lines
g_7 <- ggplot(plot_data, aes(x = iter, y = test_rmse_mean, color = eta))+
  geom_smooth(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Number of Trees", title = "Error Rate v Number of Trees",
       y = "Error Rate", color = "Learning \n Rate")  # Set labels
g_7

set.seed(12345)
bst_final <- xgboost(data = dtrain, # Set training data
                     
                     booster = "gbtree",
                     objective = "binary:logistic",
                     eta = 0.05, # Set learning rate
                     max.depth =  7, # Set max depth
                     min_child_weight = 10, # Set minimum number of samples in node to split
                     gamma = 0, # Set minimum loss reduction for split
                     subsample =  0.9, # Set proportion of training data to use in tree
                     colsample_bytree = 0.9, # Set number of variables to use in each tree
                     
                     nrounds = 120, # Set number of rounds
                     early_stopping_rounds = 20, # Set number of rounds to stop at if there is no improvement
                     silent = 1,
                     verbose = 1, # 1 - Prints out fit
                     nthread = 1, # Set number of parallel threads
                     print_every_n = 20, # Prints out result every 20th iteration
                     showsd = T, 
                     stratified = T,
                     
)

xgb_test_preds <- predict(bst_final, dtest, type = "prob")
xgb_test_preds <- ifelse (xgb_test_preds > 0.5,1,0)
xgb_test_preds
summary(xgb_test_preds)
cm <- confusionMatrix(data=as.factor(xgb_test_preds), reference = as.factor(testSet$T1WON))
cm
draw_confusion_matrix(cm)
err <- mean(as.numeric(xgb_test_preds > 0.5) != testSet$T1WON)
print(paste("test-error=", err))

p1 <- predict(bst_final, dtest, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(testSet$T1WON, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')


imp_mat <- xgb.importance(model = bst_final)
# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat, top_n = 48)

saveRDS(bst_final, "models/xgboost.rds")

#### LOGISTIC REGRESSION ####

dtrain <- trainSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
dtest <- testSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
test_labels = dtest$T1WON
test_data = subset(dtest, select = -c(23))

lr_model <- glm(formula = T1WON ~ T1FGP+T13Pper+T1FTR+T1OR+T1DR+T1TO+T1Stl+T1Blk+T1PF+T1Ast+T1ELO+T2FGP+T23Pper+T2FTR+T2OR+T2DR+T2TO+T2Stl+T2Blk+T2PF+T2Ast+T2ELO,
           data = dtrain, 
           family = "binomial")

lr_test_preds <- predict(lr_model, test_data, type = "response")
summary(lr_test_preds)
lr_test_preds <- ifelse (lr_test_preds > 0.5,1,0)
cm <- confusionMatrix(data=as.factor(lr_test_preds), reference = as.factor(test_labels))
cm
draw_confusion_matrix(cm)
err <- mean(as.numeric(lr_test_preds > 0.5) != test_labels)
print(paste("test-error=", err))

p1 <- predict(lr_model, test_data, type = "response")
p1 <- p1[,2]
r <- multiclass.roc(testSet$T1WON, lr_test_preds, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

saveRDS(lr_model, "models/lr_model.rds")

#### LOGISTIC REGRESSION SEED DIFF ####

lr_seed_model <- glm( T1WON ~ SEEDDIFF,
           data = seed_diff_train, 
           family = "binomial")

results <- predict(lr_seed_model, seed_diff_test, type = "response")
summary(results)
results
err <- mean(as.numeric(results > 0.5) != seed_diff_test$T1WON)
print(paste0("Error:",err))
print(paste0("Accuracy:",1-err))

cm <- confusionMatrix(data=as.factor(as.numeric(results > 0.5)), reference = as.factor(seed_diff_test$T1WON))
draw_confusion_matrix(cm)

saveRDS(lr_seed_model, "models/lr_seed_model.rds")

#### LOGISTIC REGRESSION ELO RANK ####

lr_elo_model <- glm( T1WON ~ T1ELO+T2ELO,
            data = elo_diff_train, 
            family = "binomial")

results <- predict(lr_elo_model, elo_diff_test, type = "response")
summary(results)
results
err <- mean(as.numeric(results > 0.5) != elo_diff_test$T1WON)
print(paste0("Error:",err))
print(paste0("Accuracy:",1-err))

cm <- confusionMatrix(data=as.factor(as.numeric(results > 0.5)), reference = as.factor(elo_diff_test$T1WON))
draw_confusion_matrix(cm)

saveRDS(lr_elo_model, "models/lr_elo_model.rds")

#### KNN ####

dtrain <- trainSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
dtest <- testSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
#dtrain <- lapply(dtrain[, c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO","T1WON")], normalize)
test_labels = dtest$T1WON
train_labels = dtrain$T1WON

k1 <- round(sqrt(ncol(dtrain)))
k2 <- round(sqrt(nrow(dtrain)))

dtrain$T1WON <- factor(dtrain$T1WON)
dtest$T1WON <- factor(dtest$T1WON)

knn_model <- knn3(T1WON ~ ., data=dtrain, k = k2)
knn_model2 <- knn3(T1WON ~ ., data=dtrain, k = k1)
p1 = predict(knn_model, dtest, type = "prob")
p2 = predict(knn_model, dtest, type = "prob")

p1 <- p1[,2]
r <- multiclass.roc(dtest$T1WON, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

cm <- confusionMatrix(data=as.factor(as.numeric(p1 > 0.5)), reference = as.factor(dtest$T1WON))
cm
draw_confusion_matrix(cm)


saveRDS(knn_model, "models/knn_model.rds")
saveRDS(knn_model2, "models/knn_model2.rds")

### RANDOM FOREST #### 
dtrain <- trainSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
dtest <- testSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
test_data = subset(dtest, select = -c(23))



rf_model <- randomForest(as.factor(T1WON) ~T1FGP+T13Pper+T1FTR+T1OR+T1DR+T1TO+T1Stl+T1Blk+T1PF+T1Ast+T1ELO+T2FGP+T23Pper+T2FTR+T2OR+T2DR+T2TO+T2Stl+T2Blk+T2PF+T2Ast+T2ELO, data = dtrain, ntree = 128, mtry = 5, importance = TRUE)
rf_model

preds <- predict(rf_model,newdata = test_data , type="prob")
plot(rf_model)
plot(rf_model, print.thres = 0.5, type="S")

p1 <- predict(rf_model, test_data, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(dtest$T1WON, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

cm <- confusionMatrix(data=as.factor(as.numeric(p1 > 0.5)), reference = as.factor(dtest$T1WON))
draw_confusion_matrix(cm)

saveRDS(rf_model, "models/rf_model.rds")

### DECISION TREE ###
dtrain <- trainSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
dtest <- testSet[,c("T1FGP","T13Pper","T1FTR","T1OR","T1DR","T1TO","T1Stl","T1Blk","T1PF","T1Ast","T1ELO","T2FGP","T23Pper","T2FTR","T2OR","T2DR","T2TO","T2Stl","T2Blk","T2PF","T2Ast","T2ELO", "T1WON")]
test_labels = dtest$T1WON
test_data = subset(dtest, select = -c(23))


tree <- rpart(as.factor(T1WON) ~T1FGP+T13Pper+T1FTR+T1OR+T1DR+T1TO+T1Stl+T1Blk+T1PF+T1Ast+T1ELO+T2FGP+T23Pper+T2FTR+T2OR+T2DR+T2TO+T2Stl+T2Blk+T2PF+T2Ast+T2ELO, data = dtrain)
rpart.plot(tree)
plotcp(tree)


p1 <- predict(tree, test_data, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test_labels, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

cm <- confusionMatrix(data=as.factor(as.numeric(p1 > 0.5)), reference = as.factor(dtest$T1WON))
draw_confusion_matrix(cm)

saveRDS(tree, "models/dt_model.rds")
