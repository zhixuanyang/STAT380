rm(list = ls())
library(data.table)
library(rvest)
library(Metrics)
library(caret)
interim_data_path = "~/Desktop/STAT380/MarchMadnessR1/project/volume/data/interim/"
model_path ="~/Desktop/STAT380/MarchMadnessR1/project/volume/models/"
submission_path = "~/Desktop/STAT380/MarchMadnessR1/project/volume/data/processed/"
raw_path = "~/Desktop/STAT380/MarchMadnessR1/project/volume/data/raw/"
train = fread(file.path(interim_data_path,"train.csv"))
test = fread(file.path(interim_data_path,"test.csv"))

train_y<-train$Result

train$team_1<-NULL
train$team_2<-NULL
test$team_1<-NULL
test$team_2<-NULL

dummies <- dummyVars(Result ~ ., data = train)
train<-predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)

train<-data.table(train)
train$Result<-train_y
test<-data.table(test)

glm_model<-glm(Result~.,family=binomial,data=train)

summary(glm_model)

saveRDS(dummies,file = file.path(model_path,"glm.dummies"))
saveRDS(glm_model,file = file.path(model_path,"glm.model"))
pred<-predict(glm_model,newdata = test,type="response")
test = fread(file.path(interim_data_path,"test.csv"))
test$Result<-pred

sub<-fread(file = file.path(raw_path,'example_submission.csv'))
sub$order<-1:nrow(sub)
teams<-data.table(matrix(unlist(strsplit(sub$id,"_")),ncol=2,byrow=T))
setnames(teams,c("V1","V2"),c("team_1","team_2"))

sub$team_1<-teams$team_1
sub$team_2<-teams$team_2

sub$team_1<-as.character(sub$team_1)
sub$team_2<-as.character(sub$team_2)
test$team_1<-as.character(test$team_1)
test$team_2<-as.character(test$team_2)

sub$Result<-NULL

submit<-merge(sub,test,all.x=T, by=c("team_1","team_2"))

submit<-submit[order(order)]

submit<-submit[,.(id,Result)]

fwrite(submit,file = file.path(submission_path,"submit.csv"))
