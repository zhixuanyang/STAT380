rm(list = ls())
library("data.table")
set.seed(460)
raw_data_path = "~/Desktop/STAT380/MarchMadnessR1/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/MarchMadnessR1/project/volume/data/interim/"
rankings = fread(file.path(raw_data_path,"MasseyOrdinals_thru_2019_day_128.csv"))
RegularSeason = fread(file.path(raw_data_path,"RegularSeasonDetailedResults.csv"))
Tourney = fread(file.path(raw_data_path,"NCAATourneyDetailedResults.csv"))
test = fread(file.path(raw_data_path,"example_submission.csv"))
test<-data.table(matrix(unlist(strsplit(test$id,"_")),ncol = 2,byrow = TRUE))
setnames(test,c("V1","V2"),c("team_1","team_2"))
test$Season<-2019
test$DayNum<-133
test$Result<-0.5

train<-rbind(RegularSeason,Tourney)
train<-train[,.(WTeamID,LTeamID,Season,DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))

train$Result<-1

# make master data file

master<-rbind(train,test)
master$team_1<-as.character(master$team_1)
master$team_2<-as.character(master$team_2)

rankings$DayNum<-rankings$RankingDayNum+1

pom_ranks<-rankings[SystemName== 'LMC' ][,.(Season,DayNum,TeamID,OrdinalRank)]

setnames(pom_ranks,"TeamID","team_1")

pom_ranks$team_1<-as.character(pom_ranks$team_1)

setkey(master,Season,team_1,DayNum)
setkey(pom_ranks,Season,team_1,DayNum)

master<-pom_ranks[master,roll=T]
setnames(master,"OrdinalRank","team_1_POM")


setnames(pom_ranks,"team_1","team_2")
setkey(master,Season,team_2,DayNum)
setkey(pom_ranks,Season,team_2,DayNum)

master<-pom_ranks[master,roll=T]

setnames(master,"OrdinalRank","team_2_POM")

master$POM_dif<-master$team_2_POM-master$team_1_POM

master<-master[order(Season,DayNum)]

master<-master[,.(team_1,team_2,POM_dif,Result)]


master<-master[!is.na(master$POM_dif)]

test<-master[Result==0.5]
train<-master[Result==1]

rand_inx<-sample(1:nrow(train),nrow(train)*0.5)
train_a<-train[rand_inx,]
train_b<-train[!rand_inx,]

train_b$Result<-0
train_b$POM_dif<-train_b$POM_dif*-1

train<-rbind(train_a,train_b)

fwrite(train,file = file.path(interim_data_path,'train.csv'))
fwrite(test,file = file.path(interim_data_path,'test.csv'))
