rm(list = ls())
library(data.table)
library(rvest)
library(Metrics)
library(caret)
library(glmnet)
library(plotmo)
interim_data_path = "~/Desktop/STAT380/Midterm/project/volume/data/interim/"
model_path ="~/Desktop/STAT380/Midterm/project/volume/models/"
submission_path = "~/Desktop/STAT380/Midterm/project/volume/data/processed/"
raw_path = "~/Desktop/STAT380/Midterm/project/volume/data/raw/"
train = fread(file.path(interim_data_path,"train.csv"))
test = fread(file.path(interim_data_path,"test.csv"))
colnames(train)
drops<- c('team_1','team_2','Season','id_num','FTM','FGA3','FGM3','DayNum','PF','FTA','Loc')
train<-train[, !drops, with = FALSE]
test<-test[, !drops, with = FALSE]


select<-c('Result','DUN_dif','INC_dif','KPI_dif','SMS_dif',
          'BWE_dif',"Score","FGM",'FGA','DR',"OR","Ast","TO","Blk")
train<-train[, select, with = FALSE]
test<-test[, select, with = FALSE]
model <- glm(Result ~.,family=binomial(link='logit'),data=train)
a = summary(model)
b = anova(model)
print(a)
print(b)

model <- glm(Result ~.,family=binomial(link='logit'),data=train)

#dummies <- dummyVars(Result ~ ., data = train)
#train<-predict(dummies, newdata = train)
#test<-predict(dummies, newdata = test)

#train<-data.table(train)
#train$Result<-train_y
#test<-data.table(test)
#test$Result<-1
#glm_model<-glm(Result~.,family=binomial,data=train)
#train <- as.matrix(train)
#glm_model_cv <- cv.glmnet(train, train_y, alpha = 1, family = "binomial",nfolds = 10)
#bestlam <- glm_model_cv$lambda.min
#glm_model <- glmnet(train, train_y, alpha = 1,family="binomial")
#saveRDS(dummies,file = file.path(model_path,"glm.dummies"))
#saveRDS(glm_model,file = file.path(model_path,"glm.model"))
#test <- as.matrix(test)
pred <- predict(model,newdata = test,type = 'response')
#pred<-predict(glm_model,newdata = test,type = "response")
#test = fread(file.path(interim_data_path,"test.csv"))
#test$Result<-pred

sub<-fread(file = file.path(raw_path,'example_sub.csv'))
sub$result<-pred
fwrite(sub,file = file.path(submission_path,"submit.csv"))
#sub$order<-1:nrow(sub)
#teams<-data.table(matrix(unlist(strsplit(sub$id,"_")),ncol=4,byrow=T))
#setnames(teams,c("V1","V2","V3","V4"),c("Season","DayNum","team_1","team_2"))

#sub$Season <- teams$Season
#sub$DayNum <- teams$DayNum
#sub$team_1<-teams$team_1
#sub$team_2<-teams$team_2

#sub$Season<-as.character(sub$Season)
#sub$DayNum<-as.character(sub$DayNum)
#sub$team_1<-as.character(sub$team_1)
#sub$team_2<-as.character(sub$team_2)

#sub$result<-NULL
#test$Season<-as.character(test$Season)
#test$DayNum<-as.character(test$DayNum)
#test$team_1<-as.character(test$team_1)
#test$team_2<-as.character(test$team_2)
#submit<-merge(sub,test,all.x=T, by=c("Season","DayNum","team_1","team_2"))

#submit<-submit[order(order)]

#submit<-submit[,.(id,Result)]
#names(submit)[1]<-"id"
#names(submit)[2]<-"result"
#fwrite(submit,file = file.path(submission_path,"submit.csv"))
