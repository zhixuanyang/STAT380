rm(list = ls())
library(data.table)
library(rvest)
library(Metrics)
library(caret)
interim_data_path = "~/Desktop/STAT380/MarchMadnessR2/project/volume/data/interim/"
model_path ="~/Desktop/STAT380/MarchMadnessR2/project/volume/models/"
submission_path = "~/Desktop/STAT380/MarchMadnessR2/project/volume/data/processed/"
raw_path = "~/Desktop/STAT380/MarchMadnessR2/project/volume/data/raw/"
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

sub<-fread(file = file.path(raw_path,'MSampleSubmissionStage1_2020.csv'))
sub$order<-1:nrow(sub)
teams<-data.table(matrix(unlist(strsplit(sub$ID,"_")),ncol=3,byrow=T))
setnames(teams,c("V1","V2","V3"),c("Season","team_1","team_2"))

sub$Season <- teams$Season
sub$team_1<-teams$team_1
sub$team_2<-teams$team_2

sub$Season<-as.character(sub$Season)
test$Season<as.character(test$Season)
sub$team_1<-as.character(sub$team_1)
sub$team_2<-as.character(sub$team_2)
test$team_1<-as.character(test$team_1)
test$team_2<-as.character(test$team_2)

sub$Pred<-NULL

test$Season <-as.character(test$Season)
submit<-merge(sub,test,all.x=T, by=c("Season","team_1","team_2"))

submit<-submit[order(order)]

submit<-submit[,.(ID,Result)]
names(submit)[2]<-"Pred"
fwrite(submit,file = file.path(submission_path,"submit.csv"))
