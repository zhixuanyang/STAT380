library(data.table)
flights = fread('2008.csv')
flights
colnames(flights)

# Predict DepDelay based on some columns
# Possible useful predictors:
# Dest Distance Origin Month DayOfweek CRSDepTime DayOfWeek ArrTime UniqueCarrier

flights = flights[, .(DepDelay,Dest,Origin,Month,CRSDepTime,UniqueCarrier)]
flights[,mean(DepDelay]
flights[is.na(DepDelay)]
flights = flights[!is.na(DepDelay)]
flights[,mean(DepDelay)]
#divide the data into training and testing sets

idx = sample(1:nrow(flights),5000000)
train = flights[idx]
test = flights[-idx]

train[, Id := 1:nrow(train)]
test[,Id:= 1:nrow(test)]

train
test
colnames(train)

train
train[,pred.delay:=mean(DepDelay),.(Month,Origin)]
train
library(Metrics)
rmse(train$DepDelay,train$pred.delay)
#34.83

delay.vals = unique(train[,.(Origin,Month,pred.delay)])
delay.vals
setkey(delay.vals,Origin,Month)
delay.vals

setkey(test,Origin,Month)

merged = merge(test,delay.vals)
merged

rmse(merged$DepDelay,merged$pred.delay)

tosubmit = merged[,.(Id,pred.delay)]
tosubmit
setkey(tosubmit,Id)

tosubmit