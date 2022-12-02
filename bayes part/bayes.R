library(tidyverse)

data <- read_csv("heart.csv")
#设置随机数
set.seed(2021)

##将数据集分为80%的训练集和20%的验证集
select <- sample(1:nrow(data),nrow(data)*0.8)

train <- data[select,]

test <- data[-select,]

#解压做贝叶斯的R包
library(e1071)

nb_default <- naiveBayes(target ~ ., data=train)

#可以看一下模型的细节，可以看到类别型的数据，和数值型的数据是不一样的，前者仍然是传统的概率，后者的第一个字段为平均值，第二个字段为标准差。
nb_default

#贝叶斯对于验证集模型的预测
test.y_hat <- predict(nb_default, test, type="class")

#保留3位小数
test.y_hat_prob <- round(predict(nb_default, test, type="raw"),3)

cbind(Prediction=as.character(test.y_hat), test.y_hat_prob)  #可以看到，概率已经做了正规化处理。

#贝叶斯模型评估
accuracy.nb_default <- sum(test.y_hat==test$target) / length(test$target)

accuracy.nb_default ##贝叶斯模型的预测准确性为 0.8032787

##看一下数据的实际情况
agreement_KNN <- test.y_hat==test$target

agreement_KNN

#交叉表比较实际情况和贝叶斯模型预测的比较
table(test.y_hat, test$target, dnn=c("Prediction","Actual"))

#可以发现建立的贝叶斯模型有较高的预测效能，target实际为0，预测为0的有24个，为1的只有5个；实际为1的，预测为1的有25个，为0的有7个



