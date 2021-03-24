rm(list = ls())
library(data.table)
library(Rtsne)
library(ClusterR)
library(xgboost)
library(ggplot2)
set.seed(1234)
raw_data_path = "~/Desktop/STAT380/Final/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/Final/project/volume/data/interim/"
processed_data_path ="~/Desktop/STAT380/Final/project/volume/data/processed/"
train_data = fread(file.path(interim_data_path,'training_data.csv'))
train_emb = fread(file.path(raw_data_path,'training_emb.csv'))
test_data = fread(file.path(interim_data_path,'test_data.csv'))
test_emb = fread(file.path(raw_data_path,'test_emb.csv'))

data = cbind(rbind(train_data[,.(id,text,code)],
                   test_data[,.(id,text,code)]),
             data.frame(lapply(rbind(train_emb,test_emb),
                               jitter,
                               factor = 0.000001)))
pca = prcomp(data[,4:515])
screeplot(pca)

pca_coords <- data.table(unclass(pca)$x)

dim(pca_coords)


tsne = Rtsne(pca_coords,
             dims = 3,
             perplexity = 150, 
             pca = FALSE) 
tsne_coords<-data.table(tsne$Y)

tsne_120 = Rtsne(pca_coords,
                 dims = 3,
                 perplexity = 120, 
                 pca = FALSE) 
tsne_coords_120<-data.table(tsne_120$Y)

tsne_100 = Rtsne(pca_coords,
             dims = 3,
             perplexity = 100, 
             pca = FALSE) 
tsne_coords_100<-data.table(tsne_100$Y)

tsne_70 = Rtsne(pca_coords,
                 dims = 3,
                 perplexity = 70, 
                 pca = FALSE) 
tsne_coords_70<-data.table(tsne_70$Y)

tsne_40 = Rtsne(pca_coords,
                dims = 3,
                perplexity = 40, 
                pca = FALSE) 
tsne_coords_40<-data.table(tsne_40$Y)


tsne_combined<-cbind(tsne_coords,
                     tsne_coords_120,
                     tsne_coords_100,
                     tsne_coords_70,
                     tsne_coords_40)
colnames(tsne_combined)<-c('v1_1','v2_1','v3_1',
                           'v1_2','v2_2','v3_2',
                           'v1_3','v2_3','v3_3',
                           'v1_4','v2_4','v3_4',
                           'v1_5','v2_5','v3_5')
idx = which(data$code < 10)
train_tsne = tsne_combined[idx]
test_tsne = tsne_combined[-idx]
dtrain<-xgb.DMatrix(as.matrix(train_tsne),
                    label = data$code[idx],
                    missing = NA)
dtest<-xgb.DMatrix(as.matrix(test_tsne),
                   missing=NA)

{
  param <- list(objective = 'multi:softprob',
                booster = 'gbtree',
                eval_metric = 'mlogloss',
                tree_method = 'hist',
                num_class = 10,
                max_depth           = 8,
                eta                 = 0.001,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                gamma               = 0.2,
                min_child_weight    = 1
  )
  
  time1 = proc.time()[3]
  XGBm.cv.1 <- xgb.cv(params        = param,
                      nfold         = 5,
                      nrounds       = 8000,  
                      missing       = NA,    
                      data          = dtrain, 
                      print_every_n = 10,
                      early_stopping_rounds = 300)
  time1 = proc.time()[3] - time1
  
  param <- list(objective = 'multi:softprob',
               booster = 'gbtree',
               eval_metric = 'mlogloss',
               tree_method = 'hist',
               num_class = 10,
               max_depth           = 9,
               eta                 = 0.001,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                gamma               = 0.2,
                min_child_weight    = 1
)
  
  time2 = proc.time()[3]
  XGBm.cv.2 <- xgb.cv(params        = param,
                      nfold         = 5,
                      nrounds       = 8000,  
                      missing       = NA,    
                      data          = dtrain, 
                      print_every_n = 10,
                      early_stopping_rounds = 300)
  time2 = proc.time()[3] - time2
  
  
  param <- list(objective = 'multi:softprob',
               booster = 'gbtree',
               eval_metric = 'mlogloss',
                tree_method = 'hist',
                num_class = 10,
                max_depth           = 11,
                eta                 = 0.001,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                gamma               = 0.2,
                min_child_weight    = 1
  )
  
  time3 = proc.time()[3]
  XGBm.cv.3 <- xgb.cv(params        = param,
                      nfold         = 5,
                     nrounds       = 8000,  
                      missing       = NA,    
                     data          = dtrain, 
                    print_every_n = 10,
                    early_stopping_rounds = 300)
  time3 = proc.time()[3] - time3
  
  param <- list(objective = 'multi:softprob',
                booster = 'gbtree',
                eval_metric = 'mlogloss',
                tree_method = 'hist',
               num_class = 10,
                max_depth           = 12,
                eta                 = 0.001,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                gamma               = 0.2,
                min_child_weight    = 1
 )
  
  
  time4 = proc.time()[3]
  XGBm.cv.4 <- xgb.cv(params        = param,
                     nfold         = 5,
                      nrounds       = 8000,  
                      missing       = NA,    
                      data          = dtrain, 
                      print_every_n = 10,
                     early_stopping_rounds = 300)
  time4 = proc.time()[3] - time4
  
  cat("depth = 8 time = ", time1, 
      "logloss = ", min(XGBm.cv.1$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.1$evaluation_log$test_mlogloss_mean), "\n")
  cat("depth = 9 time = ", time2, 
      "logloss = ", min(XGBm.cv.2$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.2$evaluation_log$test_mlogloss_mean), "\n")
  cat("depth =  11 time = ", time3, 
      "logloss = ", min(XGBm.cv.3$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.3$evaluation_log$test_mlogloss_mean), "\n")
  cat("depth =  12 time = ", time4, 
      "logloss = ", min(XGBm.cv.4$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.4$evaluation_log$test_mlogloss_mean), "\n")
}


{
  param <- list(objective = 'multi:softprob',
                booster = 'gbtree',
                eval_metric = 'mlogloss',
                tree_method = 'hist',
                num_class = 10,
                max_depth           = 8,
                eta                 = 0.001,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                gamma               = 0.2,
                min_child_weight    = 1
  )
  
  timeeta1 = proc.time()[3]
  XGBm.cv.eta1 <- xgb.cv(params        = param,
                         nfold         = 5,
                         nrounds       = 10000,  
                         missing       = NA,    
                         data          = dtrain, 
                         print_every_n = 1000,
                         early_stopping_rounds = 300)
  timeeta1 = proc.time()[3] - timeeta1
  
  param$eta = 0.01
  
  timeeta2 = proc.time()[3]
  XGBm.cv.eta2 <- xgb.cv(params        = param,
                         nfold         = 5,
                         nrounds       = 10000,  
                         missing       = NA,    
                         data          = dtrain, 
                         print_every_n = 1000,
                         early_stopping_rounds = 300)
  timeeta2 = proc.time()[3] - timeeta2
  
  param$eta = 0.1
  
  timeeta3 = proc.time()[3]
  XGBm.cv.eta3 <- xgb.cv(params        = param,
                         nfold         = 5,
                         nrounds       = 10000,  
                         missing       = NA,    
                         data          = dtrain, 
                         print_every_n = 1000,
                         early_stopping_rounds = 300)
  timeeta3 = proc.time()[3] - timeeta3
  
  param$eta = 1
  
  timeeta4 = proc.time()[3]
  XGBm.cv.eta4 <- xgb.cv(params        = param,
                         nfold         = 5,
                         nrounds       = 10000,  
                         missing       = NA,    
                         data          = dtrain, 
                         print_every_n = 1000,
                         early_stopping_rounds = 300)
  timeeta4 = proc.time()[3] - timeeta4

  cat("eta = 0.001 time = ", timeeta1, 
      "logloss = ", min(XGBm.cv.eta1$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.eta1$evaluation_log$test_mlogloss_mean), "\n")
  cat("eta = 0.01  time = ", timeeta2, 
      "logloss = ", min(XGBm.cv.eta2$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.eta2$evaluation_log$test_mlogloss_mean), "\n")
  cat("eta = 0.1   time = ", timeeta3, 
      "logloss = ", min(XGBm.cv.eta3$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.eta3$evaluation_log$test_mlogloss_mean), "\n")
  cat("eta = 1     time = ", timeeta4, 
      "logloss = ", min(XGBm.cv.eta4$evaluation_log$test_mlogloss_mean), 
      "idx = ", which.min(XGBm.cv.eta4$evaluation_log$test_mlogloss_mean), "\n")
}

{
  subsample = c(1.0, 0.95, 0.90, 0.85, 0.80)
  colsample_bytree = c(1.0, 0.95, 0.90, 0.85, 0.80)
  results = data.table(subsample = rep(0, 25),
                       colsample_bytree = rep(0, 25),
                       time             = rep(0, 25),
                       logloss             = rep(0, 25),
                       idx              = rep(0, 25))
  
  param <- list(objective = 'multi:softprob',
                booster = 'gbtree',
                eval_metric = 'mlogloss',
                tree_method = 'hist',
                num_class = 10,
                max_depth           = 8,
                eta                 = 0.001,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                gamma               = 0.2,
                min_child_weight    = 1
  )
  i = 1
  for (ss in subsample) {
    for (cs in colsample_bytree) {
      cat("\nss = ", ss, "cs = ", cs, "\n\n")
      param$subsample = ss
      param$colsample_bytree = cs
      time = proc.time()[3]
      XGBm.cv <- xgb.cv(params        = param,
                        nfold         = 5,
                        nrounds       = 10000,  
                        missing       = NA,    
                        data          = dtrain, 
                        print_every_n = 100,
                        early_stopping_rounds = 300)
      results$subsample[i] = ss
      results$colsample_bytree[i] = cs
      results$time[i] = proc.time()[3] - time
      results$logloss[i] = min(XGBm.cv$evaluation_log$test_mlogloss_mean)
      results$idx[i]  = which.min(XGBm.cv$evaluation_log$test_mlogloss_mean)
      print(results[1:i])
      i = i + 1
    }
  }
  print(results)
}



{
  gamma = c(0.2, 0.1, 0.05, 0.02, 0.01)
  min_child_weight = c(1)
  results = data.table(gamma            = rep(0, 5),
                       min_child_weight = rep(0, 5),
                       time             = rep(0, 5),
                       logloss             = rep(0, 5),
                       idx              = rep(0, 5))
  
  param <- list(objective = 'multi:softprob',
                booster = 'gbtree',
                eval_metric = 'mlogloss',
                tree_method = 'hist',
                num_class = 10,
                max_depth           = 8,
                eta                 = 0.001,
                subsample           = 0.85,
                colsample_bytree    = 0.80,
                gamma               = 0.2,
                min_child_weight    = 1
  )
  i = 1
  for (gam in gamma) {
    for (mcw in min_child_weight) {
      cat("\ngamma = ", gam, "min_child_weight = ", mcw, "\n\n")
      param$gamma = gam
      param$min_child_weight = mcw
      time = proc.time()[3]
      XGBm.cv <- xgb.cv(params        = param,
                        nfold         = 5,
                        nrounds       = 10000, 
                        missing       = NA,   
                        data          = dtrain, 
                        print_every_n = 100,
                        early_stopping_rounds = 300)  
      results$gamma[i] = gam               
      results$min_child_weight[i] = mcw         
      results$time[i] = proc.time()[3] - time  
      results$logloss[i] = min(XGBm.cv$evaluation_log$test_mlogloss_mean)
      results$idx[i]  = which.min(XGBm.cv$evaluation_log$test_mlogloss_mean)
      print(results[1:i])
      i = i + 1
    }
  }
  print(results)
}





param <- list(objective = 'multi:softprob',
              booster = 'gbtree',
              eval_metric = 'mlogloss',
              tree_method = 'hist',
              num_class = 10,
              max_depth           = 8,
              eta                 = 0.001,
              subsample           = 0.85,
              colsample_bytree    = 0.80,
              gamma               = 0.02,
              min_child_weight    = 1
)

watchlist<-list(train = dtrain)
fit<-xgb.train(params = param,
               nrounds = 5885,
               missing = NA,
               data = dtrain,
               watchlist = watchlist,
               print_every_n = 100)
pred = predict(fit,dtest)
sub = matrix(pred,ncol = 10,byrow=TRUE)


submission = cbind(test_data$id,as.data.table(sub))
example = fread(file.path(raw_data_path,'example_sub.csv'))
names(submission) = names(example)
fwrite(submission,file.path(processed_data_path,file = 'submission.csv'))


       