library("data.table")
raw_data_path = "~/Desktop/STAT380/ML_model/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/ML_model/project/volume/data/interim/"
DT_train = fread(file.path(raw_data_path,"Stat_380_train.csv"))
DT_test = fread(file.path(raw_data_path,"Stat_380_test.csv"))
set.seed(460)
train = DT_train[,.(SalePrice,OverallQual,
                     OverallCond,LotArea,GrLivArea,YrSold,
                     BedroomAbvGr,FullBath,
                     TotRmsAbvGrd,YearBuilt,
                     HalfBath,TotalBsmtSF)]
test = DT_test[,.(OverallQual,OverallCond,
                   LotArea,GrLivArea,YrSold,
                   BedroomAbvGr,FullBath,
                   TotRmsAbvGrd,YearBuilt,
                   HalfBath,TotalBsmtSF)]
fwrite(train,'~/Desktop/STAT380/ML_model/project/volume/data/interim/train.csv')
fwrite(test,'~/Desktop/STAT380/ML_model/project/volume/data/interim/test.csv')
