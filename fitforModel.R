usePackage('caret')
LogLosSummary <- function (data, lev = NULL, model = NULL) {
        LogLos <- function(actual, pred, eps = 1e-15) {
                stopifnot(all(dim(actual) == dim(pred)))
                pred[pred < eps] <- eps
                pred[pred > 1 - eps] <- 1 - eps
                - sum(actual * log(pred)) / nrow(pred)
        }
        if (is.character(data$obs))
                data$obs <- factor(data$obs, levels = lev)
        pred <- data[, "pred"]
        obs <- data[, "obs"]
        isNA <- is.na(pred)
        pred <- pred[!isNA]
        obs <- obs[!isNA]
        data <- data[!isNA,]
        cls <- levels(obs)
        
        if (length(obs) + length(pred) == 0) {
                out <- rep(NA, 2)
        } else {
                pred <- factor(pred, levels = levels(obs))
                require("e1071")
                out <-
                        unlist(e1071::classAgreement(table(obs, pred)))[c("diag",                                                                                                                                                             "kappa")]
                
                probs <- data[, cls]
                actual <- model.matrix( ~ obs - 1)
                out2 <- LogLos(actual = actual, pred = probs)
        }
        out <- c(out, out2)
        names(out) <- c("Accuracy", "Kappa", "LogLoss")
        
        if (any(is.nan(out)))
                out[is.nan(out)] <- NA
        
        out
}

# ctrl <-
#         trainControl(..., classProbs = TRUE
#                      ,summaryFunction = LogLosSummary)
# 
# model <- train(
#         x = ...,
#         y = ...,
#         ....
#         distribution = "multinomial",
#         metric = "LogLoss",
#         maximize = FALSE,
#         tuneGrid = ...,
#         trControl = ctrl
# )





#1
control=trainControl(method="repeatedcv", 
                     number=5, 
                     repeats=10, 
                     classProbs=TRUE, 
                     savePredictions=TRUE,
                     summaryFunction = LogLosSummary)

#2 
model=train(target~.,method="",metric="LogLoss",trControl=control)





# load libraries
library(caret)
# load the dataset
data(iris)
# prepare resampling method
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(7)
fit <- train(Species~., data=iris, method="rpart", metric="logLoss", trControl=control)
# display results
print(fit)





DTTrain=read.csv('DTTrain.csv')
dim(DTTrain)
head(DTTrain)
# load libraries
library(caret)

# prepare resampling method
control <-
        trainControl(
                method = "cv",
                number = 5,
                #repeats = 5,
                
                classProbs = TRUE,
                summaryFunction = mnLogLoss
        )


set.seed(7)
fit <-
        train(
                target ~ .,
                data = DTTrain,
                method = "rpart",
                metric = "logLoss",
                trControl = control,
                tuneLength=20
        )

DTTrain=DTTrain[,-1]
head(DTTrain)

set.seed(60134)
fit2 <-
        train(
                target ~ .,
                data = DTTrain,
                method = "rf",
                metric = "logLoss",
                trControl = control,
                tuneLength=4,
                verbose=T
        )

save(fit2, file = "fit2.rda")

set.seed(919)
newFeat = data.frame(f1=rowSums(kNNTrain==0),
                     f2=rowSums(kNNTrain==1))
newFeatTest = data.frame(f1=rowSums(kNNTest==0),
                         f2=rowSums(kNNTest==1))
