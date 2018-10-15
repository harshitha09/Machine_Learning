## KNN and Clustering on iris data

data(iris)
View(iris)



train = iris[1:100,]
test = iris[101:nrow(iris),]
library(rpart)
library(caret)
model = rpart(Species~., data=train)
test$pred = predict(model, test, type="class")
confusionMatrix(test$pred, test$Species)
unique(iris$Species)

View(iris)
View(test)
library(rattle)
library(randomForest)
fancyRpartPlot(model)


?sample
rows_train = sample(seq(1, nrow(iris)), 100)

train = iris[rows_train,]
test = iris[-rows_train,]

model = randomForest(Species~., data=train)
test$pred = predict(model, test, type="class")

View(predict(model, test, type="prob"))
confusionMatrix(test$pred, test$Species)

library(dplyr)
test %>% filter(pred!='setosa',
                Species=='setosa') %>% nrow()


### KNN

set.seed(100)
rows_train = sample(seq(1, nrow(iris)), 100)
train = iris[rows_train,]
test = iris[-rows_train,]

dist(rbind(test[1,1:4], train[1, 1:4]))[1]

rows = c()
dists = c()     
for (i in seq(1, nrow(train))){
  curr_dist = dist(rbind(test[1,1:4],
                         train[i,1:4]))
  dists = append(dists, curr_dist)
  rows = append(rows, i)
}
result = data.frame(rows=rows, ed=dists)
result = result %>% arrange(dists)
View(result)
kneigh = head(result$rows,7)
View(train[kneigh,])



library(class)
library(caret)
pred_iris = knn(train %>% select(-Species),
                test %>% select(-Species),
                cl= train$Species,
                k=7)

confusionMatrix(pred_iris,
                test$Species)

## Covert categorical columns to numerical columns
hr$Attrition = as.integer(hr$Attrition)
dummy_obj = dummyVars(~., data=hr %>% select(-Over18))
hr_new = data.frame(predict(dummy_obj, newdata = hr))
View(hr_new)
set.seed(100)
rows_train = sample(seq(1, nrow(hr_new)), 1029) # 0.7*nrow(hr)=1029
train = hr_new[rows_train,]
test = hr_new[-rows_train,]

pred_hr = knn(train %>% select(-Attrition),
              test %>% select(-Attrition),
              cl= train$Attrition,
              k=7)
confusionMatrix(as.factor(pred_hr),
                as.factor(test$Attrition),
                positive="1")


### Clustering
iris_new = iris %>% select(-Species)

kmeans_model = kmeans(iris_new,  centers = 3)
iris$cluster = kmeans_model$cluster
View(iris)
