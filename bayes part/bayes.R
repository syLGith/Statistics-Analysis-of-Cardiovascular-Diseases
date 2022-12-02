library(tidyverse)

data <- read_csv("heart.csv")
#���������
set.seed(2021)

##�����ݼ���Ϊ80%��ѵ������20%����֤��
select <- sample(1:nrow(data),nrow(data)*0.8)

train <- data[select,]

test <- data[-select,]

#��ѹ����Ҷ˹��R��
library(e1071)

nb_default <- naiveBayes(target ~ ., data=train)

#���Կ�һ��ģ�͵�ϸ�ڣ����Կ�������͵����ݣ�����ֵ�͵������ǲ�һ���ģ�ǰ����Ȼ�Ǵ�ͳ�ĸ��ʣ����ߵĵ�һ���ֶ�Ϊƽ��ֵ���ڶ����ֶ�Ϊ��׼�
nb_default

#��Ҷ˹������֤��ģ�͵�Ԥ��
test.y_hat <- predict(nb_default, test, type="class")

#����3λС��
test.y_hat_prob <- round(predict(nb_default, test, type="raw"),3)

cbind(Prediction=as.character(test.y_hat), test.y_hat_prob)  #���Կ����������Ѿ��������滯������

#��Ҷ˹ģ������
accuracy.nb_default <- sum(test.y_hat==test$target) / length(test$target)

accuracy.nb_default ##��Ҷ˹ģ�͵�Ԥ��׼ȷ��Ϊ 0.8032787

##��һ�����ݵ�ʵ�����
agreement_KNN <- test.y_hat==test$target

agreement_KNN

#������Ƚ�ʵ������ͱ�Ҷ˹ģ��Ԥ��ıȽ�
table(test.y_hat, test$target, dnn=c("Prediction","Actual"))

#���Է��ֽ����ı�Ҷ˹ģ���нϸߵ�Ԥ��Ч�ܣ�targetʵ��Ϊ0��Ԥ��Ϊ0����24����Ϊ1��ֻ��5����ʵ��Ϊ1�ģ�Ԥ��Ϊ1����25����Ϊ0����7��


