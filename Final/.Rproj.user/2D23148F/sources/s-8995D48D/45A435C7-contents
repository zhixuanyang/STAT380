rm(list = ls())
library(data.table)
library(Rtsne)
library(ggplot2)
library(xgboost)
library(tm)
raw_data_path = "~/Desktop/STAT380/Final/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/Final/project/volume/data/interim/"
processed_data_path ="~/Desktop/STAT380/Final/project/volume/data/processed/"
train_data = fread(file.path(raw_data_path,'training_data.csv'))
test_data = fread(file.path(raw_data_path,'test_data.csv'))

original_text_train <- train_data$text
original_text_test <- test_data$text

train_data$code = NA
test_data$code = NA

train_data$code<-as.integer(train_data$code)
test_data$code<-as.integer(test_data$code)
case_function <- function (x){
  if (x$subredditcars==1){
    return (0)
  }
  else if(x$subredditCooking==1){
    return (1)
  }
  else if(x$subredditMachineLearning==1)
  {
    return (2)
  }
  else if(x$subredditmagicTCG==1)
  {
    return (3)
  }  
  else if(x$subredditpolitics==1)
  {
    return (4)
  }  
  else if(x$subredditReal_Estate==1)
  {
    return (5)
  }  
  else if(x$subredditscience==1)
  {
    return (6)
  }  
  else if(x$subredditStockMarket==1)
  {
    return (7)
  }  
  else if(x$subreddittravel==1)
  {
    return (8)
  }  
  else if(x$subredditvideogames==1)
  {
    return (9)
  }  
  else {
    return(NA)
  }
}
for (i in (1:nrow(train_data))){
  train_data[i]$code<-case_function(train_data[i])
}
fwrite(train_data,file.path(interim_data_path,'training_data.csv'))
fwrite(test_data,file.path(interim_data_path,'test_data.csv'))
