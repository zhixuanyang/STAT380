rm(list = ls())
library(data.table)
library(caret)
library(ggplot2)
library(ClusterR)
library(Rtsne)
raw_data_path = "~/Desktop/STAT380/Identifying Species/project/volume/data/raw/"
interim_data_path ="~/Desktop/STAT380/Identifying Species/project/volume/data/interim/"
submission_path = "~/Desktop/STAT380/Identifying Species/project/volume/data/processed/"
data <- fread(file = file.path(raw_data_path,'Gene_data.csv'))
data$id<-NULL
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
dummies <- dummyVars(~., data = data)
gene <- predict(dummies,newdata = data)
gene<-data.table(gene)
j_data<-data.frame(lapply(gene,jitter,factor = 1))
pca <- prcomp(j_data)
pca_true<-prcomp(gene)
pca_dt <- data.table(unclass(pca)$x)
pca_dt_true <- data.table(unclass(pca_true)$x)


tsne_pca <- Rtsne(pca_dt, pca = F)
tsne_data <- Rtsne(j_data, pca = T)
tsne_true <- Rtsne(pca_dt_true,pca=F)
tsne_raw <- Rtsne(gene,pca=T)


tsne_dt_pca <- data.table(tsne_pca$Y)
tsne_dt_data <- data.table(tsne_data$Y)
tsne_dt_true <- data.table(tsne_true$Y)
tsne_dt_raw <- data.table(tsne_raw$Y)





gmm_tsne_BIC <- Optimal_Clusters_GMM(tsne_dt_pca,
                                     max_clusters = 10,
                                     criterion = "BIC")
gmm_pca2_BIC <- Optimal_Clusters_GMM(pca_dt[, .(PC1, PC2)],
                                     max_clusters = 10,
                                     criterion = "BIC")
gmm_pca3_BIC <- Optimal_Clusters_GMM(pca_dt_true[, .(PC1, PC2)],
                                     max_clusters = 10,
                                     criterion = "BIC")
gmm_data_BIC <- Optimal_Clusters_GMM(j_data,
                                     max_clusters = 10,
                                     criterion = "BIC")
gmm_tsne_raw_BIC <- Optimal_Clusters_GMM(tsne_dt_raw,
                                    max_clusters = 10,
                                    criterion = "BIC")
gmm_tsne_true_BIC <- Optimal_Clusters_GMM(tsne_dt_true,
                                         max_clusters = 10,
                                         criterion = "BIC")
gmm_tsne_pca_BIC <- Optimal_Clusters_GMM(tsne_dt_data,
                                          max_clusters = 10,
                                          criterion = "BIC")

delta_tsne_pca_k <- gmm_tsne_pca_BIC[-1] - gmm_tsne_pca_BIC[-10]
delta_tsne_true_k <- gmm_tsne_true_BIC[-1] - gmm_tsne_true_BIC[-10]
delta_tsne_raw_k <- gmm_tsne_raw_BIC[-1] - gmm_tsne_raw_BIC[-10]
delta_data_k <- gmm_data_BIC[-1] - gmm_data_BIC[-10]
delta_pca2_k <- gmm_pca2_BIC[-1] - gmm_pca2_BIC[-10]
delta_pca3_k <- gmm_pca3_BIC[-1] - gmm_pca3_BIC[-10]
delta_tsne_k <- gmm_tsne_BIC[-1] - gmm_tsne_BIC[-10]


del_k_tab <- data.table(delta_tsne_k, 
                        delta_pca2_k,
                        delta_pca3_k,
                        delta_data_k, 
                        delta_tsne_pca_k,
                        delta_tsne_true_k,
                        delta_tsne_raw_k,
                        k = 2:10)

ggplot(del_k_tab, aes(x = k, y = -delta_tsne_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_pca2_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_pca3_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_tsne_pca_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_tsne_true_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_tsne_raw_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)



gmm <- GMM(pca_dt[, .(PC1, PC2)],3)
cluster_prob_data = predict_GMM(pca_dt[, .(PC1, PC2)], 
                                gmm$centroids, 
                                gmm$covariance_matrices, 
                                gmm$weights)$cluster_proba
cluster_prob_data = as.data.table(cluster_prob_data)
submission <- fread(file = file.path(raw_data_path,'example_sub.csv'))
submission$species1<-cluster_prob_data$V2
submission$species2<-cluster_prob_data$V3
submission$species3<-cluster_prob_data$V1
fwrite(submission,file = file.path(submission_path,"submission.csv"))
