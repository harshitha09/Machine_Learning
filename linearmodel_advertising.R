
# Simple linear regression for advertising dataset

library(dplyr)
library(ggplot2)
adv = read.csv('E:/Machine Learning/My practice/Kaggle_AV projects/class datasets/datasets-master/Advertising.csv')

View(adv)


ggplot(adv, aes(x=TV, y=sales))+geom_point() + 
  geom_smooth(method='lm')

cor(adv$radio, adv$sales)
View(cor(adv))

# Model = TV ~ Sales


dim(adv)
0.7*200

train = adv[1:140,]
test = adv[141:nrow(adv),]

# Fit a linear model
model = lm(sales~newspaper, data=train)
model

# Predict the values using test data
test$sales_predict = 0.04 * test$newspaper + 12.79

plot(test$sales, type='l')
lines(test$sales_predict, col='red')