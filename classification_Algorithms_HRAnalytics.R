
## Classification
# Decision tree

hr = read.csv('E:/Machine Learning/My practice/Kaggle_AV projects/class datasets/datasets-master/HR Analytics.csv')
View(hr)

dim(hr)
install.packages("rpart")

library(rpart)

0.7*nrow(hr)

hr$Attrition = as.factor(hr$Attrition)
train = hr[1:1029,]
test = hr[1030:nrow(hr),]

model = rpart(Attrition~.,
              data=train,   
              control=rpart.control(maxdepth=3))   
fancyRpartPlot(model)
model
plot(model)
text(model)
install.packages('rattle')
library(rattle)

colnames(hr)


(train %>% filter(OverTime=='No') %>% nrow()) / nrow(train)


(train %>% filter(Attrition == 1) %>% nrow()) / nrow(train)


train %>% filter(OverTime == 'No') %>% nrow()


train %>% filter(OverTime == 'No', Attrition==0) %>% nrow()

650/726*100


train %>% filter(OverTime == 'Yes') %>% nrow()

train %>% filter(OverTime == 'Yes', Attrition==0) %>% nrow()

nrow(train)


train %>% filter(OverTime=='Yes',
                 MonthlyIncome>=3485,
                 Attrition==0) %>% nrow()



## Gini impurity

n = nrow(train)
nl = train %>% filter(OverTime=='No') %>% nrow()
nr = train %>% filter(OverTime=='Yes') %>% nrow()

## Gini impurity for left child
p0_left = (train %>%
             filter(OverTime=='No',Attrition==0) %>% 
             nrow()) / nl
p1_left = (train %>%
             filter(OverTime=='No',Attrition==1) %>% 
             nrow()) / nl         

p0_left + p1_left # This should be = 1

gi_left = 1 - p0_left^2 - p1_left^2
gi_left

## Gini impurity for right child
p0_right = (train %>%
              filter(OverTime=='Yes',Attrition==0) %>% 
              nrow()) / nr
p1_right = (train %>%
              filter(OverTime=='Yes',Attrition==1) %>% 
              nrow()) / nr        

p0_right + p1_right # This should be = 1

gi_right = 1 - p0_right^2 - p1_right^2
gi_right
## Overall gini impurity
# gi = (nleft/n*gileft) + (nright/n*giright)
gi = (nl/n*gi_left) + (nr/n*gi_right)
gi





## Gini impurity for numerical column


View(mis)


gis = c()
cuts = c()
train_subset = train %>% filter(OverTime=='Yes')
mis = sort(unique(train_subset$MonthlyIncome))
n = nrow(train_subset)
for (i in seq(1, (length(mis)-1))){
  cut = (mis[i] + mis[i+1]) / 2
  nl = train_subset %>% filter(MonthlyIncome < cut) %>% nrow()
  nr = train_subset %>% filter(MonthlyIncome > cut) %>% nrow()
  # GI Left
  p0_left = (train_subset %>%
               filter(MonthlyIncome < cut,Attrition==0) %>% 
               nrow()) / nl
  p1_left = (train_subset %>%
               filter(MonthlyIncome < cut ,Attrition==1) %>% 
               nrow()) / nl         
  p0_left + p1_left # This should be = 1
  gi_left = 1 - p0_left^2 - p1_left^2
  # GI Right
  p0_right = (train_subset %>%
                filter(MonthlyIncome > cut,Attrition==0) %>% 
                nrow()) / nr
  p1_right = (train_subset %>%
                filter(MonthlyIncome > cut,Attrition==1) %>% 
                nrow()) / nr        
  
  p0_right + p1_right # This should be = 1
  
  gi_right = 1 - p0_right^2 - p1_right^2
  
  gi = (nl/n*gi_left) + (nr/n*gi_right)
  
  gis = append(gis, gi)
  cuts = append(cuts, cut)
}
result = data.frame(cut=cuts, gi=gis)
View(result)


### Prediction
pred = predict(model, test, type = 'class')
View(pred)

test$pred = pred

accuracy =  sum(test$Attrition == test$pred) / nrow(test) * 100
accuracy


table(test$Attrition) / nrow(test) * 100
library(caret)
install.packages('caret')
confusionMatrix(test$pred, test$Attrition, positive="1")

test %>% filter(pred==0, Attrition==0) %>% nrow() # TN
test %>% filter(pred==1, Attrition==1) %>% nrow() # TP
test %>% filter(pred==1, Attrition==0) %>% nrow() # FP
test %>% filter(pred==0, Attrition==1) %>% nrow() # FN

16 / (49+16) 
nrow(test)

## Random_Forest

hr$Attrition = as.factor(hr$Attrition)
train = hr[1:1029,]
test = hr[1030:nrow(hr),]
model = rpart(Attrition~.,
              data=train)   
library(rattle)
library(caret)
fancyRpartPlot(model)

train_predict = predict(model, train, 
                        type='class')
confusionMatrix(train_predict, 
                train$Attrition,
                positive='1')

library(randomForest)
rf = randomForest(Attrition~.,
                  data=train,
                  mtry=6,
                  ntree=401)
test$predict = predict(rf, test, type='class')
train$predict = predict(rf, train, type='class')
confusionMatrix(train$predict,
                train$Attrition,
                positive="1")


probs = predict(rf, test, type='prob')
predict_new = ifelse(probs[,2]>0.3, 1, 0)
predict_new = as.factor(predict_new)
confusionMatrix(predict_new,
                test$Attrition,
                positive="1")
