rm(list = ls())
library(data.table)
library(rvest)
library(Metrics)
library(caret)
interim_data_path ="~/Desktop/STAT380/Coin_Flips1/project/volume/data/interim/"
model_path ="~/Desktop/STAT380/Coin_Flips1/project/volume/models/"
train = fread(file.path(interim_data_path,"train.csv"))
test = fread(file.path(interim_data_path,"test.csv"))
train_y = train$result
train[, result := train_y]
test<-data.table(test)
coin_model <- glm(result ~ ., data = train)
summary(coin_model)
saveRDS(coin_model, file = file.path(model_path,"coin_model.rds"))
test$result = predict(coin_model, newdata = test,type = "response")
submit = test[, .(result)]
tosubmit = submit[, Id := 1:nrow(submit)]
fwrite(tosubmit,file = file.path(model_path,"submit_coin.csv"))
logLoss(train_y, coin_model$fitted.values)