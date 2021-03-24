rm(list = ls())
library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ggplot2)
library(ClusterR)

set.seed(3)

# load in data 
data<-fread("~/Desktop/STAT380/PCA Test/project/volume/data/raw/data.csv")

# we are not supposed to know the party of the individuals so we should hide this
party <- data$party
data$party <- NULL

j_data <- data.frame(lapply(data, jitter, factor = 0.01))

# do a pca
pca <- prcomp(j_data)

# use the unclass() function to get the data in PCA space
pca_dt <- data.table(unclass(pca)$x)

# remove it when doing the PCA
tsne_pca <- Rtsne(pca_dt, pca = F)  # do this, if you have already done PCA
tsne_data <- Rtsne(j_data, pca = T) # do this, if you have not run PCA
# need to use j_data, can't use data
# grab the x-y coodinrates for each observation
tsne_dt_pca <- data.table(tsne_pca$Y)
tsne_dt_data <- data.table(tsne_data$Y)

# Let's color by party to see how this separted parties
ggplot(tsne_dt_pca, aes(x = V1, y = V2)) + geom_point()
ggplot(tsne_dt_pca, aes(x = V1, y = V2, col = party)) + geom_point()
ggplot(tsne_dt_data, aes(x = V1, y = V2)) + geom_point()
ggplot(tsne_dt_data, aes(x = V1, y = V2, col = party)) + geom_point()
# Things to notice
# 1. The shapes are different, but that is because of random seeds
#    We can run Rtsne on the same data multiple times and the spearated
#    shape will look different each time
# 2. Unlike what we did on Monday, there is no clear visual distinction
#    between the two groups.  The clear distinction we were seeing was due
#    to the presence of the factor $party, which we shouldn't have had
# 3. Sometimes we can get a good idea of groupings based on the tsne plot,
#    sometimes we can't.  This is one of those times we can't

###################
# GMM Starts here #
###################

# use a gaussian mixture model to find optimal k and then get probability of
# membership for each row to each group

# this fits a gmm to the data for all k=1 to k= max_clusters, we then look for a
# major change in likelihood between k values

# I am going to run this first on tsne_dt_pca, which is the dataset 
# reduced to two dimensions

gmm_tsne_aic <- Optimal_Clusters_GMM(tsne_dt_pca,
                                     max_clusters = 10,
                                     criterion = "AIC")

# I am going to run this second on the first two principle components
# as found in pca_dt
gmm_pca2_aic <- Optimal_Clusters_GMM(pca_dt[, .(PC1, PC2)],
                                     max_clusters = 10,
                                     criterion = "AIC")

# I am going to run this third on the first three principle components
# as found in pca_dt (since the third one *might* be important)
gmm_pca3_aic <- Optimal_Clusters_GMM(pca_dt[, .(PC1, PC2, PC3)],
                                     max_clusters = 10,
                                     criterion = "AIC")

# I am going to run this last on the orginal data, since it has not
# been tampered with.  We are using full dimesions, but since there
# are only 15 columns, this isn't a problem (as opposed to having hundreds)
gmm_data_aic <- Optimal_Clusters_GMM(j_data,
                                     max_clusters = 10,
                                     criterion = "AIC")

# now we will look at the change in model fit between successive k values
delta_data_k <- gmm_data_aic[-1] - gmm_data_aic[-10]
delta_pca2_k <- gmm_pca2_aic[-1] - gmm_pca2_aic[-10]
delta_pca3_k <- gmm_pca3_aic[-1] - gmm_pca3_aic[-10]
delta_tsne_k <- gmm_tsne_aic[-1] - gmm_tsne_aic[-10]

# I'm going to make a plot so you can see the values, this part isnt necessary
del_k_tab <- data.table(delta_tsne_k, 
                        delta_pca2_k,
                        delta_pca3_k,
                        delta_data_k, k = 2:10)

# plot.  We are looking for the optimal number of clusters
ggplot(del_k_tab, aes(x = k, y = -delta_tsne_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_pca2_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_pca3_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

ggplot(del_k_tab, aes(x = k, y = -delta_data_k)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(aes(label = k), hjust = 0, vjust = -1)

opt_k <- 2

# now we run the model with our chosen k value
# gmm_pca <- GMM(pca_dt[, .(PC1, PC2)], opt_k)
gmm <- GMM(j_data, opt_k)
# the model gives a log-likelihood for each datapoint's membership to each
# cluster, me need to convert this log-likelihood into a probability

cluster_prob_data = predict_GMM(j_data, 
                                gmm$centroids, 
                                gmm$covariance_matrices, 
                                gmm$weights)$cluster_proba
cluster_prob_data = as.data.table(cluster_prob_data)
cluster_prob_data
# we can now plot to see what cluster 1 looks like

ggplot(tsne_dt_data, 
       aes(x = V1, y = V2, col = cluster_prob_data$V1)) +
  geom_point()


# I'm going to check out how well I did.
tab = table(round(cluster_prob_data$V1), party)
tab
(tab[1, 1] + tab[2, 2]) / 10000


# Let's see how well we would have done if we used tsne data with 2 clusters
gmm <- GMM(tsne_dt_pca, 2)
cluster_prob_data = predict_GMM(tsne_dt_pca, 
                                gmm$centroids, 
                                gmm$covariance_matrices, 
                                gmm$weights)$cluster_proba
cluster_prob_data = as.data.table(cluster_prob_data)
cluster_prob_data
# we can now plot to see what cluster 1 looks like

ggplot(tsne_dt_data, 
       aes(x = V1, y = V2, col = cluster_prob_data$V1)) +
  geom_point()


# I'm going to check out how well I did.
tab = table(round(cluster_prob_data$V1), party)
tab
# With 2 clusters on the tsne data, we did very bad.
# Moral: determining the "correct" number of clusters if you 
# don't know the number of clusters to start with is a hard problem
# and potentiall has many solutions


# Let's see what would happen with 4 clusters when applied to tsne data
gmm <- GMM(tsne_dt_pca, 4)
cluster_prob_data = predict_GMM(tsne_dt_pca, 
                                gmm$centroids, 
                                gmm$covariance_matrices, 
                                gmm$weights)$cluster_proba
cluster_prob_data = as.data.table(cluster_prob_data)
cluster_prob_data
cluster = rep("", 10000)
for (i in 1:10000) {  # I probably should use lapply or apply here, but I don't want to
  cluster[i] = names(which.max(cluster_prob_data[i]))
}
table(cluster, party)
ggplot(tsne_dt_data, 
       aes(x = V1, y = V2, col = cluster)) +
  geom_point()

ggplot(tsne_dt_data, 
       aes(x = V1, y = V2, col = party)) +
  geom_point()
# It looks like: Dem's who love cats, Dem's who hate cats, 
#                Rep's who love cats, Rep's who hat cats

# Let's see what would happen with 3 cluster applied to pca_data, one just the 
# first to components
gmm <- GMM(pca_dt[, .(PC1, PC2)], 3)
cluster_prob_data = predict_GMM(pca_dt[, .(PC1, PC2)], 
                                gmm$centroids, 
                                gmm$covariance_matrices, 
                                gmm$weights)$cluster_proba
cluster_prob_data = as.data.table(cluster_prob_data)
cluster_prob_data
cluster = rep("", 10000)
for (i in 1:10000) { # ditto above
  cluster[i] = names(which.max(cluster_prob_data[i]))
}
table(cluster, party)
cats = data$Cats
knitting = data$Knitting
table(cluster, party, cats, knitting)
ggplot(tsne_dt_data, 
       aes(x = V1, y = V2, col = cluster)) +
  geom_point()

ggplot(tsne_dt_data, 
       aes(x = V1, y = V2, col = party)) +
  geom_point()
# It looks like: People who hate cats and hate knitting
#                Democrates who love cats or love knitting or both
#                Repubs who love cats and or love knitting or both


# Let's go back to the correct solution given our knowledge that there
# are two polictical parties.
gmm <- GMM(j_data, 2)
cluster_prob_data = predict_GMM(j_data, 
                                gmm$centroids, 
                                gmm$covariance_matrices, 
                                gmm$weights)$cluster_proba
cluster_prob_data = as.data.table(cluster_prob_data)
cluster_prob_data
solution = data.table(Id = 1:10000,
                      Prob1 = cluster_prob_data$V1,
                      Prob2 = cluster_prob_data$V2)
solution = data.table(Id = 1:10000,
                      Prob1 = cluster_prob_data$V2,
                      Prob2 = cluster_prob_data$V1)
solution
fwrite(solution, file = "mysolution.csv")
# then submit this.
# Remmber that for your solution you may need to rearange the columns when creating
# 'solution'.  This is because observation 1 is species 1 and observation 2 is 
# species 2.  In this case, we have no such knowledge

# Questions?


       