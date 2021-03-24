rm(list = ls())
library(caret)
library(data.table)
raw_data_path = "~/Desktop/STAT380/Identifying Species/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/Identifying Species/project/volume/data/interim/"
submission_path = "~/Desktop/STAT380/Identifying Species/project/volume/data/processed/"
set.seed(27)

data <- fread(file = file.path(raw_data_path,"Gene_data.csv"))
data[,locus_1 := as.factor(locus_1)]
data[,locus_2 := as.factor(locus_2)]
data[,locus_3 := as.factor(locus_3)]
data[,locus_4 := as.factor(locus_4)]
data[,locus_5 := as.factor(locus_5)]
data[,locus_6 := as.factor(locus_6)]
data[,locus_7 := as.factor(locus_7)]
data[,locus_8 := as.factor(locus_8)]
data[,locus_9 := as.factor(locus_9)]
data[,locus_10 := as.factor(locus_10)]
data[,locus_11 := as.factor(locus_11)]
data[,locus_12 := as.factor(locus_12)]
data[,locus_13 := as.factor(locus_13)]
data[,locus_14 := as.factor(locus_14)]
data[,locus_15 := as.factor(locus_15)]
data <- as.data.table(data)
fwrite(data,file = file.path(interim_data_path,"gene.csv"))
