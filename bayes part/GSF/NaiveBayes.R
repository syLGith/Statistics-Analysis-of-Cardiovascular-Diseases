#���ر�Ҷ˹����
#��������
heart <- read_csv("D:/Stat with R/Group project/R group project/heart.csv")
heart
#ѡȡ75%����Ϊѵ������25%����Ϊ���Լ�
#����ѵ�������±꼯
library(caret)
ind <- createDataPartition(heart$target,times = 1,p=0.75,list = F)
heart_train <- heart[ind,]
heart_test <- heart[-ind,]
#e1071������ʹ��
library(e1071)
nb.model <- naiveBayes(target~.,data = heart_train)
#Ԥ����
nb_predict <- predict(nb.model,newdata = heart_test)
#����ʵ����Ԥ�⽻�����Ԥ�⾫��
nb.matrix <- table(actual=heart_test$target,predict=nb_predict)
nb_prediction <- sum(diag(nb.table))/sum(nb.table)

#�������
nb.matrix
nb_prediction
