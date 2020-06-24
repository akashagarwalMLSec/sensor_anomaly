
### Run Matlab Code after this#########################
library(cluster)

# dataset climate network
setwd("C:/Users/akd130230//OneDrive/Climate Network AD/Monisha's Codes/MATLAB Codes")

#setwd("D:\\OneDrive\\Climate Network AD\\Monisha's Codes\\MATLAB Codes")
#setwd("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes")

ClaimEmbeddings <- read.csv("Claims_MANE30_Sep2.csv",header = F)
ClaimAmountEmbeddings <- read.csv("ClaimAmount_MANE30_Sep2.csv",header=F)
PrecipEmbeddings <- read.csv("Precip_MANE30_Sep2.csv",header=F)
CreditEmbeddings <- read.csv("CreditScore_MANE30_Sep2.csv",header=F)
HouseEmbeddings <- read.csv("HouseAge_MANE30_Sep2.csv",header=F)

dim(ClaimEmbeddings)
dim(ClaimAmountEmbeddings)

data_finally <- cbind(ClaimAmountEmbeddings,ClaimEmbeddings,PrecipEmbeddings,PrecipEmbeddings,CreditEmbeddings,HouseEmbeddings)
#data_finally <- data_finally[-1,]
dim(data_finally)


new <- dist(data_finally) # Euclidian Matrix
data<- as.matrix(new)



source('C:\\Users\\akd130230\\OneDrive\\Climate Network AD\\Monisha\'s Codes/R codes/TDA Clustering of Embeddings.R')


############################################

cap=2# dimension cap
nodes=nrow(data) # number of points
nNghb=30 # number of neighbors around each data point
delta=0.1 # step size for filtration
filt_len=30 # filtration length.
# When distances are transformed, delta=0.01 and filt_len=L 
#implies that we account for L% of variation in distances
#data <- as.matrix(data)
# Running Perseus
#betti_0=betti_1=c()
wasser_dist_0 = wasser_dist_1 =wasser_dist_2 = c()
Persistance =c()
index=c()
for (i in 1:nodes){ 
  
  ind=which(rank(data[i,],ties.method = 'first')<=nNghb) # indices of points around point i
  
  # writing data into file M.txt
  cat(length(ind),file='M.txt',append=F,sep = '\n')
  cat(paste(0,delta,filt_len,cap,sep = ' '),file = 'M.txt',append = T,sep = '\n') # threshold: 0, 0.1, 0.2,...,1
  cat(data[ind,ind],file='M.txt',append = T)
  
  system('perseusWin.exe distmat M.txt OutputFile') # for Windows
  print(i) 
  index=rbind(index,ind)
  betti_data=as.matrix(read.table('OutputFile_betti.txt'))
  # dim=0
  persist_data = as.matrix(read.table('OutputFile_0.txt'))
  persist_data[persist_data[,2] == -1, 2] = filt_len + 1
  persist_data = persist_data/(filt_len + 1)
  P = cbind(rep(0, nrow(persist_data)), persist_data)
  
  # dim=1
  if (file.info('OutputFile_1.txt')$size>0)
  { 
    persist_data = as.matrix(read.table('OutputFile_1.txt', blank.lines.skip = T))
    persist_data[persist_data[,2] == -1, 2] = filt_len + 1
    persist_data = persist_data/(filt_len + 1)
    P = rbind(P, cbind(rep(1, nrow(persist_data)), persist_data))
    
  }
  
  if (file.info('OutputFile_2.txt')$size>0)
  { 
    persist_data = as.matrix(read.table('OutputFile_2.txt', blank.lines.skip = T))
    persist_data[persist_data[,2] == -1, 2] = filt_len + 1
    persist_data = persist_data/(filt_len + 1)
    P = rbind(P, cbind(rep(2, nrow(persist_data)), persist_data))
    
  }
  
  Persistance[[i]] <- P
  
}

##################################################

#wasser_dist_0 = matrix(0,nodes,nodes)
#wasser_dist_1 = matrix(0,nodes,nodes)
wasser_dist_2 = matrix(0,nodes,nodes)

for(i in 1:nodes){
  for(j in 1:nodes){
    # wasser_dist_0[i,j] = wasserstein(Persistance[[i]], Persistance[[j]], dimension = 0)
    #wasser_dist_1[i,j] = wasserstein(Persistance[[i]], Persistance[[j]], dimension = 1)
    wasser_dist_2[i,j] = wasserstein(Persistance[[i]], Persistance[[j]], dimension = c(0,1))
    #wasser_dist_2[i,j] = wasserstein(Persistance[[i]], Persistance[[j]], dimension = 2)
  }
  print(i)
}



##################################################

bp_2=boxplot(as.vector(wasser_dist_2),pch=20)
quantile(wasser_dist_2,c(0.1,0.2,0.25,0.3,0.35,0.4))

##########################Elbow Plot##########################

summary <- data.frame(cutoff = double(),
                      WithinSumSquares= double(),
                      BetweenSumSquares= double(),
                      NoOfClusters = integer())
#summary1 <- data.frame(cutoff = double(),
 #                     AvgSilWidth= double(),
  #                    NoOfClusters = integer())


cutoff = seq(0.01,0.99,length.out=100)
test =c()
#avg_sil1 = c()
for (c in cutoff)
{
  cutoff_2 = c
  #cutoff_2 = 0.25
  ##############################################
  # Forming adjacency matrix
  A=matrix(0,ncol = nodes,nrow = nodes)
  for (i in 1:nodes)
  {
    index_2 = which(wasser_dist_2[i,]<=cutoff_2)
    A[i,index_2]=1
  }
  ##############################################
  
  # Clustering
  g=graph_from_adjacency_matrix(A,"directed") # form a graph from adj. matrix A
  clstrs=clusters(g)  # clusters are strongly connected components of adj. matrix A
  
  #ss <- cluster::silhouette(clstrs$membership, dist(data_finally))
  #avg_sil1 = c(avg_sil1,mean(ss[, 3]))
  center =c()
  total_wss = 0
  
  for(i in 1:clstrs$no)
  {
    if(clstrs$csize[i] > 1)
    {
      indices <- which(clstrs$membership == i,arr.ind = T) ## all the indices in the cluster
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
    center <- c(center,point_0)
    total_wss = total_wss + wss
  }
  
  wasser_cen <- wasser_dist_2[center,center]
  total_bss = sum(wasser_cen)/2
  
  test <- c(cutoff_2,total_wss,total_bss,clstrs$no)
  summary <- rbind(test,summary)
}

colnames(summary) <- c("cutOff","WithinSumSquares","BetweenSumSquares","NoOfClusters")

summary$wssBybss = summary$WithinSumSquares/summary$BetweenSumSquares
plot(summary$cutOff,summary$new)
plot(summary$NoOfClusters,summary$new)
plot(summary$cutOff,summary$WithinSumSquares)
ggplot(data = summary,aes(x=cutOff,y=WithinSumSquares))+geom_line()

plot(summary$NoOfClusters,summary$WithinSumSquares)
plot(summary$NoOfClusters,summary$cutOff)




##############################################################################################################
cutoff_2 = 0.11
##############################################
# Forming adjacency matrix

A=matrix(0,ncol = nodes,nrow = nodes)
for (i in 1:nodes)
{
  index_2 = which(wasser_dist_2[i,]<=cutoff_2)
  A[i,index_2]=1
}
##############################################

# Clustering
g=graph_from_adjacency_matrix(A,"directed") # form a graph from adj. matrix A
clstrs=clusters(g)  # clusters are strongly connected components of adj. matrix A
print(clstrs)

data_t<- as.data.frame(data)
clstrs_t <- clstrs

tdaclustered <- cbind(data_t,clstrs_t$membership)



########################## Hierarchical Clustering ###########################
#dtable = dist(data_finally)
fviz_nbclust(data, FUN = hcut, method = "wss",k.max=50)

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
clust_heir <- cutree(model2,k=10)
rect.hclust(model2, k = 10, border = 2:10)

validheir <- cluster.stats(d = dist(data_finally),clustering = clust_heir)

valitda <- cluster.stats(d =  dist(data_finally),clustering = clstrs_t$membership)

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


##############################################K Medoids################################################
ElbowPAM <- data.frame(WithinSumSquares= double(),
                      BetweenSumSquares= double(),
                      NoOfClusters = integer())
cutoff = seq(2,30,length.out=29)
test =c()
    
for (c in cutoff)
{

    # Clustering
      Kmedoids= pam(wasser_dist_2, k= c, diss = TRUE,medoids = NULL, stand = FALSE, cluster.only = FALSE,
                    do.swap = FALSE,
                    pamonce = FALSE, trace.lev = 0)
  
      center =c()
      total_wss = 0
      
      for(i in 1:c)
      {
        
          indices <- which(Kmedoids$clustering == i,arr.ind = T) ## all the indices in the cluster
          wasser_0 <- wasser_dist_2[indices,indices] #subset dataset for those points
          index <- which.min(rowSums(wasser_0)) # find centroid of the cluster
          point_0 <- indices[index] #put centroid in point_0
          wss <- sum((wasser_0[index,])^2)
          wss <- min(rowSums(wasser_dist_2[indices,indices]))
          center <- c(center,point_0)
          total_wss = total_wss + wss
      }
      
      wasser_cen <- wasser_dist_2[center,center]
      total_bss = sum(wasser_cen)/2
      
      test <- c(total_wss,total_bss,c)
      ElbowPAM <- rbind(test,ElbowPAM)
    }
    
    colnames(ElbowPAM) <- c("WithinSumSquares","BetweenSumSquares","NoOfClusters")
    
ElbowPAM$wssBybss = ElbowPAM$WithinSumSquares/ElbowPAM$BetweenSumSquares

plot(ElbowPAM$NoOfClusters,ElbowPAM$wssBybss)
plot(ElbowPAM$NoOfClusters,ElbowPAM$WithinSumSquares)

##########MOdel
Kmedoids= pam(wasser_dist_2, k= 10, diss = TRUE,medoids = NULL, stand = FALSE, cluster.only = FALSE,
              do.swap = FALSE,
              pamonce = FALSE, trace.lev = 0)



clustering_results <- cbind(clust_heir,clstrs$membership,Kmedoids$clustering)
colnames(clustering_results) <- c("Aggomerative","TDA","KMedoids")

###############################################################################################################
clustering_results <- cbind(clustering_results,colnames(precip_subset))
clustering_results <- as.data.frame(clustering_results)


#clustering_results <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_Sep2.csv")
write.csv(clustering_results,"ClustersSep2.csv")

