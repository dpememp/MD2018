require(pacman)
p_load(tidyverse,caret,pROC)

s_predict <- read_csv("/home/adriano/Documents/MD/Trabalho-01/all-influence/predict.csv")
train <- read_csv("/home/adriano/Documents/MD/Trabalho-01/all-influence/train.csv")
tests <- read_csv("/home/adriano/Documents/MD/Trabalho-01/all-influence/test.csv")

inTraining <- createDataPartition(choice,p=.75)$Resample1

train3 <- train
  train3$A_posts <- NULL
  train3$B_posts <- NULL
  train3$A_mentions_sent <- NULL
  train3$B_mentions_sent <- NULL
  train3$A_retweets_sent <- NULL
  train3$B_retweets_sent <- NULL
  train3$A_following_count <-NULL
  train3$B_following_count <-NULL
  train3$A_network_feature_3 <- NULL
  train3$B_network_feature_3 <- NULL
  train3$A_network_feature_2 <- NULL
  train3$B_network_feature_2 <- NULL
train3$Choice <- as.factor(train3$Choice)
  
x <- ifelse(s_predict$Choice>0.5,1,0)
x <- as.factor(x)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

modC13 <- train(Choice ~ ., data = train3,
                method = "gbm",
                trControl = fitControl,
                verbose = FALSE)

prdC13 <- predict(modC13,tests,type = "prob")
prdCCC <- predict(modC13,tests)
  confusionMatrix(prdCCC,x)

rocRF <- plot.roc(x,predictor = prdC13[,2])
rocRF

data <- data_frame("Id" = seq.int(nrow(tests)),"Choice" = prdC13[,2])
write.table(data,file="~/Documents/MD/Trabalho-01/submit.csv",sep=',')
