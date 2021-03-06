rm(list = ls())
library("data.table")
library("tidyverse")
set.seed(460)
raw_data_path = "~/Desktop/STAT380/Midterm/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/Midterm/project/volume/data/interim/"
rankings = fread(file.path(raw_data_path,"massey.csv"))
a<-rankings[SystemName=='DUN']
RegularSeason = fread(file.path(raw_data_path,"season.csv"))
Tourney = fread(file.path(raw_data_path,"tourney.csv"))
test = fread(file.path(raw_data_path,"example_sub.csv"))
test<-data.table(matrix(unlist(strsplit(test$id,"_")),ncol = 4,byrow = TRUE))
setnames(test,c("V1","V2","V3","V4"),c("Season","DayNum","team_1","team_2"))
drops<- c('Season','DayNum')
#test<-test[, !drops, with = FALSE]
#test$Season<-2019
#test$DayNum<-120
test$id_num<-1:nrow(test)
test<-test[,.(id_num,team_1,team_2,Season,DayNum)]
test$Result<-0.5
RegularSeason<-RegularSeason[Season==2019]
train<-rbind(RegularSeason)
train<-train[,.(WTeamID,LTeamID,Season,DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))

train$Result<-1
train$id_num<-1:nrow(train)

master<-rbind(train,test)
master$team_1<-as.character(master$team_1)
master$team_2<-as.character(master$team_2)
master <- master[Season == 2019]
rankings$DayNum<-rankings$RankingDayNum+1

rankings <- rankings[Season == 2019]
a <-rankings[SystemName=="DUN"]
system_lst<-unique(rankings$SystemName)
system_lst<-system_lst[!system_lst =='AP']
system_lst<-system_lst[!system_lst =='USA']
system_lst<-system_lst[!system_lst =='DES']
system_lst<-system_lst[!system_lst =='BNT']
system_lst<-system_lst[!system_lst =='JJK']
#system_lst<-c("BWE","POM","DOL","KPK","TRK","COL","AWS","MAS")
test$Season<-as.character(test$Season)
train$Season<-as.character(train$Season)
master$Season<-as.character(master$Season)
master$DayNum<-as.character(master$DayNum)
for(i in 1:length(system_lst)){
  system_rank<-rankings[SystemName==system_lst[i]][,.(Season,DayNum,TeamID,OrdinalRank)]
  setnames(system_rank,"TeamID","team_1")
  system_rank$DayNum<-as.character(system_rank$DayNum)
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
#master<-master[,.(id_num,Season,DayNum,team_1,team_2,POM_dif,BWE_dif,DOL_dif, KPK_dif,TRK_dif,COL_dif,AWS_dif,MAS_dif,Result)]

for(i in 1:length(system_lst)){
  master<-master[!is.na(master[[paste0(system_lst[i],'_dif')]])]
}

#RegularSeason<-RegularSeason[Season==2019]
#Tourney<-Tourney[Season==2019]
all_games<-RegularSeason
Wstatsgames<-all_games[,.(Season,DayNum,WTeamID,WScore,WLoc,NumOT,WFGM,WFGA,WFGM3,WFGA3,WFTM,WFTA,WOR,WDR,WAst,WTO,WStl,WBlk,WPF)]
Lstatsgames<-all_games[,.(Season,DayNum,LTeamID,LScore,WLoc,NumOT,LFGM,LFGA,LFGM3,LFGA3,LFTM,LFTA,LOR,LDR,LAst,LTO,LStl,LBlk,LPF)]
setnames(Wstatsgames,c("WTeamID","WScore","WLoc","NumOT","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF"),
         c("TeamID","Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
setnames(Lstatsgames,c("LTeamID","LScore","WLoc","NumOT","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF"),
         c("TeamID","Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
master_stats<-rbind(Wstatsgames,Lstatsgames)
master_stats$Loc<-as.integer(as.factor(master_stats$Loc))
statsbyday<-NULL
for (i in 2:max(master_stats$DayNum)){
  submaster_stats<-master_stats[DayNum<i]
  teamstatbyday<-dcast(submaster_stats,TeamID+Season~.,mean,
                       value.var=c("Score","Loc","NumOT","FGM","FGA","FGM3",
                                   "FGA3","FTM","FTA","OR","DR","Ast","TO",
                                   "Stl","Blk","PF"))
  teamstatbyday$DayNum<-i
  statsbyday<-rbind(statsbyday,teamstatbyday)
}
#statsbyday<-statsbyday%>%distinct()
setnames(statsbyday,"TeamID","team_1")
master$Season<-as.character(master$Season)
statsbyday$Season<-as.character(statsbyday$Season)
statsbyday$team_1<-as.character(statsbyday$team_1)
statsbyday$DayNum<-as.character(statsbyday$DayNum)
setkey(statsbyday,Season,team_1,DayNum)
setkey(master,Season,team_1,DayNum)

master<-statsbyday[master,roll=T]

setnames(statsbyday,"team_1","team_2")
setnames(statsbyday,c("Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"),
         c("Score_2","Loc_2","NumOT_2","FGM_2","FGA_2","FGM3_2","FGA3_2","FTM_2","FTA_2","OR_2","DR_2","Ast_2","TO_2","Stl_2","Blk_2","PF_2"))
setkey(statsbyday,Season,team_2,DayNum)
setkey(master,Season,team_2,DayNum)

master<-statsbyday[master,roll=T]

master$Score<-master$Score_2-master$Score
master$Loc<-master$Loc_2-master$Loc
master$NumOT<-master$NumOT_2-master$NumOT
master$FGM<-master$FGM_2-master$FGM
master$FGA<-master$FGA_2-master$FGA
master$FGM3<-master$FGM3_2-master$FGM3
master$FGA3<-master$FGA3_2-master$FGA3
master$FTM<-master$FTM_2-master$FTM 
master$FTA<-master$FTA_2-master$FTA
master$OR<-master$OR_2-master$OR
master$DR<-master$DR_2-master$DR
master$Ast<-master$Ast_2-master$Ast 
master$TO<-master$TO_2-master$TO
master$Stl<-master$Stl_2-master$Stl
master$Blk<-master$Blk_2-master$Blk
master$PF<-master$PF_2-master$PF
master<-master[order(Season,DayNum)]
drops<-c("Score_2","Loc_2","NumOT_2","FGM_2","FGA_2","FGM3_2","FGA3_2","FTM_2","FTA_2","OR_2","DR_2","Ast_2","TO_2","Stl_2","Blk_2","PF_2")
master<-master[, !drops, with = FALSE]
#master<-master[,.(id_num,team_1,team_2,Score,Loc,FGM,FGA,FGM3,FGA3,FTM,FTA,OR,DR,Ast,TO,Stl,Blk,PF,
#                  POM_dif,BWE_dif,DOL_dif, KPK_dif,TRK_dif,COL_dif,AWS_dif,MAS_dif,Result,DayNum,Season)]
master<-master[!is.na(master$Score)]
master<-master[!is.na(master$Loc)]
master<-master[!is.na(master$FGM)]
master<-master[!is.na(master$FGA)]
master<-master[!is.na(master$FGM3)]
master<-master[!is.na(master$FGA3)]
master<-master[!is.na(master$FTM)]
master<-master[!is.na(master$FTA)]
master<-master[!is.na(master$OR)]
master<-master[!is.na(master$DR)]
master<-master[!is.na(master$Ast)]
master<-master[!is.na(master$TO)]
master<-master[!is.na(master$Stl)]
master<-master[!is.na(master$Blk)]
master<-master[!is.na(master$PF)]

#BS_data<-replicate(500,sample(c(1,0),nrow(master),replace=T))
#BS_data<-data.table(BS_data)
#setnames(BS_data,paste0("V",1:500),paste0("BS_",1:500))

#master<-cbind(master,BS_data)



test<-master[Result==0.5]
train<-master[Result==1]
test<-test[order(id_num)]
rand_inx<-sample(1:nrow(train),nrow(train)*0.5)
train_a<-train[rand_inx,]
train_b<-train[!rand_inx,]
train_b$Result<-0
for(i in 1:length(system_lst)){
  train_b[[paste0(system_lst[i],'_dif')]]<-train_b[[paste0(system_lst[i],'_dif')]]*-1
}
train_b$Score<-train_b$Score*-1
train_b$Loc<-train_b$Loc*-1
train_b$FGM<-train_b$FGM*-1
train_b$FGA<-train_b$FGA*-1
train_b$FGM3<-train_b$FGM3*-1
train_b$FGA3<-train_b$FGA3*-1
train_b$FTM<-train_b$FTM*-1
train_b$FTA<-train_b$FTA*-1
train_b$OR<-train_b$OR*-1
train_b$DR<-train_b$DR*-1
train_b$Ast<-train_b$Ast*-1
train_b$TO<-train_b$TO*-1
train_b$Stl<-train_b$Stl*-1
train_b$Blk<-train_b$Blk*-1
train_b$PF<-train_b$PF*-1
train<-rbind(train_a,train_b)
fwrite(train,file = file.path(interim_data_path,'train.csv'))
fwrite(test,file = file.path(interim_data_path,'test.csv'))
