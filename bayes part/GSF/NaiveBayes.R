#朴素贝叶斯分类
#导入数据
heart <- read_csv("D:/Stat with R/Group project/R group project/heart.csv")
heart
#选取75%数据为训练集，25%数据为测试集
#构建训练集的下标集
library(caret)
ind <- createDataPartition(heart$target,times = 1,p=0.75,list = F)
heart_train <- heart[ind,]
heart_test <- heart[-ind,]
#e1071函数包使用
library(e1071)
nb.model <- naiveBayes(target~.,data = heart_train)
#预测结果
nb_predict <- predict(nb.model,newdata = heart_test)
#生成实际与预测交叉表和预测精度
nb.matrix <- table(actual=heart_test$target,predict=nb_predict)
nb_prediction <- sum(diag(nb.table))/sum(nb.table)

#分析结果
nb.matrix
nb_prediction

