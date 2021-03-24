rm(list = ls())
library("data.table")
set.seed(460)
raw_data_path = "~/Desktop/STAT380/MarchMadnessR2/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/MarchMadnessR2/project/volume/data/interim/"
rankings = fread(file.path(raw_data_path,"MMasseyOrdinals.csv"))
RegularSeason = fread(file.path(raw_data_path,"MRegularSeasonDetailedResults.csv"))
Tourney = fread(file.path(raw_data_path,"MNCAATourneyDetailedResults.csv"))
test = fread(file.path(raw_data_path,"MSampleSubmissionStage1_2020.csv"))
test<-data.table(matrix(unlist(strsplit(test$ID,"_")),ncol = 3,byrow = TRUE))
setnames(test,c("V1","V2","V3"),c("Season","team_1","team_2"))
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

#system_lst<-c("AP","BWE","POM","DOL","KPK","TRK","COL")
system_lst<-c("POM")
for(i in 1:length(system_lst)){
  system_rank<-rankings[SystemName==system_lst[i]][,.(Season,DayNum,TeamID,OrdinalRank)]
  setnames(system_rank,"TeamID","team_1")
  system_rank$team_1<-as.character(system_rank$team_1)
  system_rank$Season<-as.character(system_rank$Season)
  setkey(master,Season,team_1,DayNum)
  setkey(system_rank,Season,team_1,DayNum)
  master<-system_rank[master,roll=T]
  setnames(master,"OrdinalRank","team_1_rank")
  setnames(system_rank,"team_1","team_2")
  setkey(master,Season,team_2,DayNum)
  setkey(system_rank,Season,team_2,DayNum)
  master<-system_rank[master,roll=T]
  setnames(master,"OrdinalRank","team_2_rank")
  master$rank_dif<-master$team_2_rank-master$team_1_rank
  master$team_1_rank<-NULL
  master$team_2_rank<-NULL
  setnames(master,"rank_dif",paste0(system_lst[i],"_dif"))
}
master<-master[order(Season,DayNum)]
master<-master[,.(Season,team_1,team_2,POM_dif,Result)]
#AP_dif,BWE_dif, POM_dif,DOL_dif,KPK_dif,TRK_dif,COL_dif
#master<-master[!is.na(master$AP_dif)]
#master<-master[!is.na(master$BWE_dif)]
master<-master[!is.na(master$POM_dif)]
#master<-master[!is.na(master$DOL_dif)]
#master<-master[!is.na(master$KPK_dif)]
#master<-master[!is.na(master$TRK_dif)]
#master<-master[!is.na(master$COL_dif)]

test<-master[Result==0.5]
train<-master[Result==1]

rand_inx<-sample(1:nrow(train),nrow(train)*0.5)
train_a<-train[rand_inx,]
train_b<-train[!rand_inx,]
train_b$Result<-0
#train_b$AP_dif<-train_b$AP_dif*-1
#train_b$BWE_dif<-train_b$BWE_dif*-1
train_b$POM_dif<-train_b$POM_dif*-1
#train_b$DOL_dif<-train_b$DOL_dif*-1
#train_b$KPK_dif<-train_b$KPK_dif*-1
#train_b$TRK_dif<-train_b$TRK_dif*-1
#train_b$COL_dif<-train_b$COL_dif*-1

train<-rbind(train_a,train_b)
fwrite(train,file = file.path(interim_data_path,'train.csv'))
fwrite(test,file = file.path(interim_data_path,'test.csv'))
