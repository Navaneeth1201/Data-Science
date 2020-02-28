library(caret)
library(pROC)
library(mlbench)
library(class)
library(lattice)
library(gmodels) 
glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
glass$type = as.factor(glass$Type)
str(glass)
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("RI","Na","Mg")])
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
glass_n<- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
summary(glass_n[c("RI","Na","Mg")])
View(glass_n)
set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]
#loop for different knn values
i=1                          
k.optm=1                    
for (i in 1:10){ 
  knn.mod <-  knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=i)
  k.optm[i] <- 100 * sum(glass_test_labels == knn.mod)/NROW(glass_test_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')  
}
glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=2)
table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels)
CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 

