library(data.table)
library(dplyr)
library(partykit)
library(ggplot2)
library(plyr)
library(cluster)
library(NbClust)
library(fpc)
library(dbscan)
library(clues)
library(factoextra)
library(purrr)

auto.kmeans <- function(data,maxclu=500,seed=1,nstart=20)

{ 
  wss <- rep(NA,maxclu)
  bss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart,iter.max = 1000)
    wss[i] <- model$tot.withinss
    bss[i] <- model$betweenss
  }
  plot(1:maxclu,	wss, type="b", 
       xlab="Number of Clusters",
       ylab="Aggregate Within Group SS")
}
fviz_nbclust(scale(data), FUN = hcut, method = "wss",k.max=50)
fviz_nbclust(scale(data), FUN = kmeans, method = "wss",k.max=50)


# dataset climate network
setwd("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes")

ClaimEmbeddings <- read.csv("Claims_MANE.csv",header = F)
ClaimAmountEmbeddings <- read.csv("ClaimAmount_MANE.csv",header=F)
PrecipEmbeddings <- read.csv("Precip_MANE.csv",header=F)
CreditEmbeddings <- read.csv("CreditScore_MANE.csv",header=F)
HouseEmbeddings <- read.csv("HouseAge_MANE.csv",header=F)

data_finally<- cbind(ClaimAmountEmbeddings,ClaimEmbeddings,PrecipEmbeddings,PrecipEmbeddings,CreditEmbeddings,HouseEmbeddings)
new <- dist(data_finally)
data<- as.matrix(new)
my.names <- paste("V",seq(1,90,1),sep="_")
colnames(data_finally) <- my.names
colnames(data_finally) 
dtable = as.data.table(data_finally)
#############################K-means###############################

auto.kmeans(data)

set.seed(1)
model1 <- kmeans(scale(data), centers = 8, nstart = 8)
table(model1$cluster)

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(scale(data), centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  #mean(ss[, 3])
}
k.values <- 2:50

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)


plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")



# Silhouette information
silkmeans<- silhouette(model1$cluster,scale(data))
fviz_silhouette(silkmeans)
#The summary of the silhouette analysis can be computed using the function summary.silhouette() as follow:
# Summary of silhouette analysis
si.sum <- summary(silkmeans)
# Average silhouette width of each cluster
si.sum$clus.avg.widths
# The total average (mean of all individual silhouette widths)
si.sum$avg.width
validkmeans <- cluster.stats(d = scale(data),clustering = model1$cluster)
valitda <- cluster.stats(d = scale(data),clustering = clstrs_t$membership)

##########################################################################################################
######################################## Hierarchical Clustering #########################################
##########################################################################################################

dtable = dist(data_finally)
fviz_nbclust(dtable, FUN = hcut, method = "wss",k.max=50)

set.seed(1)
model2 <- hclust(dist(data_finally))
plot(model2)

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  heir.res <- hclust(dist(data_finally))
  clust_heir <- cutree(heir.res,k=k)
  ss <- cluster::silhouette(clust_heir, dist(data_finally))
  mean(ss[, 3])
}
k.values <- 2:50

# extract avg silhouette for 2-15 clusters
avg_sil_values1 <- map_dbl(k.values, avg_sil)


plot(k.values, avg_sil_values1,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

model2 <- hclust(dist(data_finally))
plot(model2)
clust_heir <- cutree(model2,k=6)
rect.hclust(model2, k = 6, border = 2:6)
validheir <- cluster.stats(d = dist(data_finally),clustering = clust_heir)
valitda <- cluster.stats(d =  dist(data_finally),clustering = clstrs_t$membership)

###################### DBSCAN ########################################
dbscan::kNNdistplot(data, k =  30)
abline(h = 2.2, lty = 2)

res.db <- dbscan::dbscan(data, 2.2, 5)
#c1 <- adjustedRand(model1$cluster, res.db$cluster)
#c2 <- adjustedRand(clust_heir, res.db$cluster)
#c3 <- adjustedRand(clstrs$membership, res.db$cluster)

c4 <- adjustedRand(clstrs$membership, model1$cluster)
c5 <- adjustedRand(clstrs$membership, clust_heir)
c6 <- adjustedRand(model1$cluster, clust_heir)

comparisons <- rbind(c4,c5,c6)
rownames(comparisons) <- c("TDA--Kmeans","TDA--Hclust","Kmeans--Hclust")
write.csv(comparisons,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClusterComparisons_March28.csv")
plot(comparisons[,1],xlab = "comparisons",ylab = "Rand Index")

#clustering_results <- cbind(model1$cluster,clust_heir,res.db$cluster,clstrs$membership)
#colnames(clustering_results) <- c("Kmeans","Aggomerative","DBSCAN","TDA")

clustering_results <- cbind(clust_heir,clstrs$membership)
colnames(clustering_results) <- c("Aggomerative","TDA")

write.csv(clustering_results,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March29.csv")

###############################################################################################################
clustering_results <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March20.csv")

check <- cbind(clustering_results,colnames(precip_subset)[2:505])
write.csv(check,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March29.csv")

clustering_results <- as.data.frame(clustering_results)
clustering_results <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March29.csv")

matrices <- list(precipitation,
                 claimAmount,
                 ClaimNumber,
                HouseAge_corr,
CreditScore_corr,
IntrAmtandNumber,
IntrPrecipandAmt,
IntrPrecipandNumber,
IntrPrecipandHouse,
IntrPrecipandCredit,
IntrAmtandHouse,
IntrAmtandCredit,
IntrNumberandHouse,
IntrNumberandCredit,IntrHouseandCredit)

TDA_data = data.frame(A = c(seq(from = 1,to = length(unique(clustering_results$TDA)),by=1)))
                                                                                                            
for(m in 1:length(matrices))
{
  summary_stats <- c()
  for(j in 1:length(unique(clustering_results$TDA)))
    {
    med <-  median(matrices[[m]][which(clustering_results$TDA==j),which(clustering_results$TDA==j)])
    
      summary_stats <- c(summary_stats,med)
  }
  TDA_data <- cbind(TDA_data,summary_stats)
}


colnames(TDA_data) = c("No",
                       "precipitation",
                       "claimAmount",
                       "ClaimNumber",
                       "HouseAge_corr",
                       "CreditScore_corr",
                       "IntrAmtandNumber",
                       "IntrPrecipandAmt",
                       "IntrPrecipandNumber",
                       "IntrPrecipandHouse",
                       "IntrPrecipandCredit",
                       "IntrAmtandHouse",
                       "IntrAmtandCredit",
                       "IntrNumberandHouse",
                       "IntrNumberandCredit",
                        "IntrHouseandCredit")

TDA_data <- cbind(TDA_data,table(clustering_results$TDA))
write.csv(TDA_data,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\TDAClusters_March29.csv")

###########################################K means ####################################################


kmeans_data = data.frame(A = c(seq(from = 1,to = length(unique(clustering_results$Kmeans)),by=1)))

for(m in 1:length(matrices))
{
  summary_stats <- c()
  for(j in 1:length(unique(clustering_results$Kmeans)))
  {
    med <-  median(matrices[[m]][which(clustering_results$Kmeans==j),which(clustering_results$Kmeans==j)])
    summary_stats <- c(summary_stats,med)
  }
  kmeans_data <- cbind(kmeans_data,summary_stats)
}

colnames(kmeans_data) = c("No",
                       "precipitation",
                       "claimAmount",
                       "ClaimNumber",
                       "HouseAge_corr",
                       "CreditScore_corr",
                       "IntrAmtandNumber",
                       "IntrPrecipandAmt",
                       "IntrPrecipandNumber",
                       "IntrPrecipandHouse",
                       "IntrPrecipandCredit",
                       "IntrAmtandHouse",
                       "IntrAmtandCredit",
                       "IntrNumberandHouse",
                       "IntrNumberandCredit",
                       "IntrHouseandCredit")

kmeans_data <- cbind(kmeans_data,table(clustering_results$Kmeans))
write.csv(kmeans_data,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\KmeansClusters_March20_tr.csv")

#############################################DBSCAN######################################################

dbscan_data = data.frame(A = c(seq(from = 1,to = length(unique(clustering_results$DBSCAN)),by=1)))

for(m in 1:length(matrices))
{
  summary_stats <- c()
  for(j in 1:length(unique(clustering_results$DBSCAN)))
  {
    med <-  median(matrices[[m]][which(clustering_results$DBSCAN==j),which(clustering_results$DBSCAN==j)])
    summary_stats <- c(summary_stats,med)
  }
  dbscan_data <- cbind(dbscan_data,summary_stats)
}

colnames(dbscan_data) = c("No",
                       "precipitation",
                       "claimAmount",
                       "ClaimNumber",
                       "HouseAge_corr",
                       "CreditScore_corr",
                       "IntrAmtandNumber",
                       "IntrPrecipandAmt",
                       "IntrPrecipandNumber",
                       "IntrPrecipandHouse",
                       "IntrPrecipandCredit",
                       "IntrAmtandHouse",
                       "IntrAmtandCredit",
                       "IntrNumberandHouse",
                       "IntrNumberandCredit",
                       "IntrHouseandCredit")

dbscan_data <- cbind(dbscan_data,table(clustering_results$DBSCAN))
write.csv(dbscan_data,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\DBSCANClusters_March20_tr.csv")

###################################### Hierarchical ######################################################


h_data = data.frame(A = c(seq(from = 1,to = length(unique(clustering_results$Aggomerative)),by=1)))

for(m in 1:length(matrices))
{
  summary_stats <- c()
  for(j in 1:length(unique(clustering_results$Aggomerative)))
  {
    med <-  median(matrices[[m]][which(clustering_results$Aggomerative==j),which(clustering_results$Aggomerative==j)])
    summary_stats <- c(summary_stats,med)
  }
  h_data <- cbind(h_data,summary_stats)
}

colnames(h_data) = c("No",
                       "precipitation",
                       "claimAmount",
                       "ClaimNumber",
                       "HouseAge_corr",
                       "CreditScore_corr",
                       "IntrAmtandNumber",
                       "IntrPrecipandAmt",
                       "IntrPrecipandNumber",
                       "IntrPrecipandHouse",
                       "IntrPrecipandCredit",
                       "IntrAmtandHouse",
                       "IntrAmtandCredit",
                       "IntrNumberandHouse",
                       "IntrNumberandCredit",
                       "IntrHouseandCredit")

h_data <- cbind(h_data,table(clustering_results$Aggomerative))
write.csv(h_data,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HierarClusters_March29.csv")















v_data = data.frame(A = rep(1:505,times=505))

for(m in 1:length(matrices))
{
  {
    med <-  as.vector(matrices[[m]])
    
  }
  v_data = cbind(v_data,med)
}

colnames(v_data) = c("NodeNumbwe",
                     "precipitation",
                     "claimAmount",
                     "ClaimNumber",
                     "HouseAge_corr",
                     "CreditScore_corr",
                     "IntrAmtandNumber",
                     "IntrPrecipandAmt",
                     "IntrPrecipandNumber",
                     "IntrPrecipandHouse",
                     "IntrPrecipandCredit",
                     "IntrAmtandHouse",
                     "IntrAmtandCredit",
                     "IntrNumberandHouse",
                     "IntrNumberandCredit",
                     "IntrHouseandCredit")

final <- cbind(v_data,rep(clustering_results$Kmeans,times=505))
final <- cbind(final,rep(clustering_results$TDA,times=505))
final <- cbind(final,rep(clustering_results$Aggomerative,times=505))





boxplot(precipitation~rep(clustering_results$Kmeans,times = 505),data = final )

boxplot(claimAmount~rep(clustering_results$Kmeans,times = 505),data = final )
boxplot(precipitation~rep(clustering_results$TDA,times = 505),data = final )


corrfinal_dataset = c()                                                                                                               
for(m in 1:length(matrices)){
  j <- matrices[[m]]
  corrfinal_dataset <- cbind(corrfinal_dataset,as.vector(j[-1,-1]))
                             }
finalcorr <- cbind(corrfinal_dataset,rep(clustering_results$X.1,504))
finalcorr <- as.data.frame(finalcorr)
colnames(finalcorr) <-  c(                  "precipitation",
                                            "claimAmount",
                                            "ClaimNumber",
                                            "HouseAge_corr",
                                            "CreditScore_corr",
                                            "IntrAmtandNumber",
                                            "IntrPrecipandAmt",
                                            "IntrPrecipandNumber",
                                            "IntrPrecipandHouse",
                                            "IntrPrecipandCredit",
                                            "IntrAmtandHouse",
                                            "IntrAmtandCredit",
                                            "IntrNumberandHouse",
                                            "IntrNumberandCredit",
                                            "IntrHouseandCredit","FSA")

write.csv(finalcorr,"C:\\Users\\moniy\\Desktop\\coors.csv")

#####################################################################################################
model1$cluster

center =c()
total_wss_kmeans = 0
total_bss_kmeans = 0
test = c()

for(i in 1:8)
{
    indices <- which(model1$cluster == i,arr.ind = T) ## all the indices in the cluster
    wasser_0 <- wasser_dist_2[indices,indices] #subset dataset for those points
    index <- which.min(rowSums(wasser_0)) # find centroid of the cluster
    point_0 <- indices[index] #put centroid in point_0
    wss <- sum((wasser_0[index,])^2)
    wss <- min(rowSums(wasser_dist_2[indices,indices]))
    #print(wss)
    center <- c(center,point_0)
    #print(center)
    total_wss_kmeans = total_wss_kmeans + wss
    wasser_cen <- wasser_dist_2[center,center]
    total_bss_kmeans = sum(wasser_cen)/2
    test <- c(total_wss_kmeans,total_bss_kmeans,i)
#test <- c(cutoff_2,avg_sil,clstrs$no)
print(test)

  #summary <- rbind(test,summary)
}

wb_ratio_kmeans = test[1]/test[2]

###################################################################################################
model2$cluster

center_kmeans =c()
total_wss_kmeans = 0
total_bss_kmeans = 0
test = c()

for(i in 1:8)
{
  indices <- which(model1$cluster == i,arr.ind = T) ## all the indices in the cluster
  wasser_0 <- wasser_dist_2[indices,indices] #subset dataset for those points
  index <- which.min(rowSums(wasser_0)) # find centroid of the cluster
  point_0 <- indices[index] #put centroid in point_0
  wss <- sum((wasser_0[index,])^2)
  wss <- min(rowSums(wasser_dist_2[indices,indices]))
  #print(wss)
  center_kmeans <- c(center_kmeans,point_0)
  #print(center)
  total_wss_kmeans = total_wss_kmeans + wss
  #summary <- rbind(test,summary)
}
wasser_cen_kmeans <- wasser_dist_2[center_kmeans,center_kmeans]
total_bss_kmeans = sum(wasser_cen_kmeans)/2
test <- c(total_wss_kmeans,total_bss_kmeans,i)
#test <- c(cutoff_2,avg_sil,clstrs$no)
print(test)

wb_ratio_kmeans = test[1]/test[2]
wb_ratio_kmeans

##########################################################################


cluster_size_heir <- table(clust_heir)
center_heir =c()
total_wss_heir = 0
test = c()

for(i in 1:length(cluster_size_heir))
{
  if(cluster_size_heir[i] > 1)
  {
    indices <- which(clust_heir == i,arr.ind = T) ## all the indices in the cluster
    wasser_0 <- wasser_dist_2[indices,indices] #subset dataset for those points
    index <- which.min(rowSums(wasser_0)) # find centroid of the cluster
    point_0 <- indices[index] #put centroid in point_0
    wss <- sum((wasser_0[index,])^2)
    wss <- min(rowSums(wasser_dist_2[indices,indices]))
    
  }
  else
  {
    point_0 <- which(clstrs$membership == i,arr.ind = T)
    wss <- 0
    
  }
  center_heir <- c(center_heir,point_0)
  total_wss_heir = total_wss_heir + wss
  print(i)
}

wasser_cen_heir <- wasser_dist_2[center_heir,center_heir]
total_bss_heir = sum(wasser_cen_heir)/2

test <- c(total_wss_heir,total_bss_heir)
#test <- c(cutoff_2,avg_sil,clstrs$no)
print(test)
#summary <- rbind(test,summary)
wb_ratio_heir <- test[1]/test[2]
wb_ratio_heir


#####################################################################################################

center_kmeans =c()
total_wss_kmeans = 0
total_bss_kmeans = 0
test = c()

for(i in 1:8)
{
  indices <- which(model1$cluster == i,arr.ind = T) ## all the indices in the cluster
  wasser_0 <- wasser_dist_2[indices,indices] #subset dataset for those points
  index <- which.min(rowSums(wasser_0)) # find centroid of the cluster
  point_0 <- indices[index] #put centroid in point_0
  wss <- sum((wasser_0[index,])^2)
  wss <- min(rowSums(wasser_dist_2[indices,indices]))
  #print(wss)
  center_kmeans <- c(center_kmeans,point_0)
  #print(center)
  total_wss_kmeans = total_wss_kmeans + wss
  #summary <- rbind(test,summary)
}
wasser_cen_kmeans <- wasser_dist_2[center_kmeans,center_kmeans]
total_bss_kmeans = sum(wasser_cen_kmeans)/2
test <- c(total_wss_kmeans,total_bss_kmeans,i)
#test <- c(cutoff_2,avg_sil,clstrs$no)
print(test)

wb_ratio_kmeans = test[1]/test[2]
wb_ratio_kmeans



CreditScoreMeans = colMeans(Weekly_CreditScore)
PrecipMeans = colMeans(precip_subset)
ClaimAmount= colMeans(ClaimAmt)
NUmberClaims = colMeans(No_Claim)
HouseAge = colMeans(Weekly_HouseAge)

MeansofAll = cbind(CreditScoreMeans,PrecipMeans,ClaimAmount,NUmberClaims,HouseAge)
MeansofAll = cbind(MeansofAll,colnames(Weekly_CreditScore))

write.csv(MeansofAll,"MeansforBuildingClusters.csv")


