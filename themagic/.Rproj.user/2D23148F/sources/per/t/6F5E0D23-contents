rm(list = ls())
library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)
interim_data_path = "~/Desktop/STAT380/themagic/project/volume/data/interim/"
model_path ="~/Desktop/STAT380/themagic/project/volume/models/"
submission_path = "~/Desktop/STAT380/themagicproject/volume/data/processed/"
raw_path = "~/Desktop/STAT380/themagic/project/volume/data/raw/"
train = fread(file.path(interim_data_path,"train_v1.csv"))
test = fread(file.path(interim_data_path,"test_v1.csv"))
example_sub = fread(file.path(raw_path,"example_submission.csv"))

test$future_price <- 0

year_num <- as.numeric(as.factor(year(as_date(train$current_date))))

# subset out only the columns to model
train
test

drops <-  c('id','future_date','current_date')
train <- train[, !drops, with = FALSE]
train
test <- test[, !drops, with = FALSE]
test
#save the response var because dummyVars will remove
train_y <- train$future_price
train_y

# work with dummies

dummies  <-  dummyVars(future_price ~ ., data = train)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

train <- data.table(train)
test <- data.table(test)



########################
# Use cross validation #
########################




train <- as.matrix(train)
train

?cv.glmnet

?glmnet

gl_model <- cv.glmnet(train, train_y, alpha = 1, family = "gaussian", foldid = year_num, nfolds = length(unique(year_num)))

plot(gl_model)

bestlam <- gl_model$lambda.min

log(bestlam)

####################################
# fit the model to all of the data #
####################################


#now fit the full model

#fit a logistic model
gl_model <- glmnet(train, train_y, alpha = 1,family="gaussian")

plot(gl_model)

#save model
saveRDS(gl_model,".gl_model.model")

test <- as.matrix(test)

#use the full model
pred <- predict(gl_model, s = bestlam, newx = test)

#########################
# make a submision file #
#########################


#our file needs to follow the example submission file format.
#we need the rows to be in the correct order

example_sub$future_price <- pred

example_sub

#now we can write out a submission
fwrite(example_sub,file.path(submission_path,"submit_17.csv")

       