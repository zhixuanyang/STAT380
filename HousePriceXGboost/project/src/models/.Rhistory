library(data.table)
library(rvest)
library(Metrics)
library(caret)
interim_data_path ="~/Desktop/STAT380/ML_model/project/volume/data/interim/"
model_path ="~/Desktop/STAT380/ML_model/project/volume/models/"
train = fread(file.path(interim_data_path,"train.csv"))
test = fread(file.path(interim_data_path,"test.csv"))
train_y = train$SalePrice
dummies = dummyVars(SalePrice ~ ., data = train)
train = as.data.table(predict(dummies, newdata = train))
train[, SalePrice := train_y]
test<-data.table(test)
lm_model <- lm(SalePrice ~ ., data = train)
summary(lm_model)
saveRDS(dummies, file = file.path(model_path,"dummies.rds"))
saveRDS(lm_model, file = file.path(model_path,"lm_model.rds"))
test$SalePrice = predict(lm_model, newdata = test)
submit = test[, .(SalePrice)]
tosubmit = submit[, Id := 1:nrow(submit)]
fwrite(tosubmit, file = file.path(model_path,"submit_lm.csv"))
rmse(train_y, rep(mean(train_y), length(train_y)))
rmse(train_y, lm_model$fitted.values)
