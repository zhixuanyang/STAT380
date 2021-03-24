library("data.table")
raw_data_path = "~/Desktop/STAT380/Coin_Flips1/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/Coin_Flips1/project/volume/data/interim/"
DT_train = fread(file.path(raw_data_path,"train_file.csv"))
DT_test = fread(file.path(raw_data_path,"test_file.csv"))
set.seed(460)
train = DT_train[,.(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,result)]
test = DT_test[,.(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)]
fwrite(train,'~/Desktop/STAT380/Coin_Flips1/project/volume/data/interim/train.csv')
fwrite(test,'~/Desktop/STAT380/Coin_Flips1/project/volume/data/interim/test.csv')
