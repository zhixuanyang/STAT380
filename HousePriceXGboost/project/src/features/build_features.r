rm(list = ls())
library("data.table")
raw_data_path = "~/Desktop/STAT380/HousePriceXGboost/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/HousePriceXGboost/project/volume/data/interim/"
train = fread(file.path(raw_data_path,'Stat_380_train.csv'))
test = fread(file.path(raw_data_path,'Stat_380_test.csv'))
test$SalePrice <- 0 
train_y = train$SalePrice
test_y = test$SalePrice
train = train[,2:18]
test = test[,2:18]
dummies = dummyVars(SalePrice ~., data = train)
train_x = predict(dummies,newdata = train)
test_x = predict(dummies,newdata = test)
save(train_x,train_y,test_x,train_y,file = '~/Desktop/STAT380/HousePriceXGboost/project/volume/data/interim/HousePrice.rdata')
