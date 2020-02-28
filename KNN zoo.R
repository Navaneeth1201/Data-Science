library(caret)
library(pROC)
library(mlbench)
library(lattice)
zoo <- read.csv(file.choose())
str(zoo)
zoo1 <- zoo[,2:18]
str(zoo1)
set.seed(123)
ind <- sample(2,nrow(zoo1), replace = T, prob = c(0.7,0.3))
train <- zoo1[ind==1,]
test <- zoo1[ind==2,]
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,
trControl = trcontrol, preProc = c("center","scale"))
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$type)
