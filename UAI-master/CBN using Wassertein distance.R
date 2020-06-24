library(igraph)
library(randomcoloR)
library(multigraph)
library(TDA)

#rm(list=ls())
# dataset climate network
#setwd("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\Old")


setwd("D:\\OneDrive\\Climate Network AD\\Monisha's Codes\\MATLAB Codes\\Old")

ClaimEmbeddings <- read.csv("Claims_MANE_Mar20.csv",header = F)
ClaimAmountEmbeddings <- read.csv("ClaimAmount_MANE_mar20.csv",header=F)
PrecipEmbeddings <- read.csv("Precip_MANE_mar20.csv",header=F)
CreditEmbeddings <- read.csv("CreditScore_MANE_mar20.csv",header=F)
HouseEmbeddings <- read.csv("HouseAge_MANE_mar20.csv",header=F)


data_finally <- cbind(ClaimAmountEmbeddings,ClaimEmbeddings,PrecipEmbeddings,PrecipEmbeddings,CreditEmbeddings,HouseEmbeddings)
data_finally <- data_finally[-1,]
new <- dist(data_finally)
data<- as.matrix(new)

dim(data)


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

#par(mfrow = c(1, 2))
# Overall comparison
#bp_0=boxplot(as.vector(wasser_dist_0),pch=20)
#bp_1=boxplot(as.vector(wasser_dist_1),pch=20)
bp_2=boxplot(as.vector(wasser_dist_2),pch=20)

#cutoff_0=bp_0$stats[2,] # cutoff threshold for relative change in Betti-0 numbers. Can also set it manually
#cutoff_1=bp_1$stats[2,] # cutoff threshold for relative change in Betti-1 numbers. Can also set it manually

#quantile(wasser_dist_0,c(0.1,0.2,0.25,0.3,0.35,0.4))
#quantile(wasser_dist_1,c(0.1,0.2,0.25,0.3,0.35,0.4))
quantile(wasser_dist_2,c(0.1,0.2,0.25,0.3,0.35,0.4))

#cutoff_0 = quantile(wasser_dist_0,0.9)
#cutoff_1 = quantile(wasser_dist_1,0.3)
#cutoff_2 = quantile(wasser_dist_2,0.25)



##########################Elbow Plot##########################

summary <- data.frame(cutoff = double(),
                      WithinSumSquares= double(),
                      BetweenSumSquares= double(),
                      NoOfClusters = integer())
summary <- data.frame(cutoff = double(),
                      AvgSilWidth= double(),
                      NoOfClusters = integer())


cutoff = seq(0.01,0.49,length.out=50)
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
#test <- c(cutoff_2,avg_sil,clstrs$no)
#print(test)
summary <- rbind(test,summary)
}

colnames(summary) <- c("cutOff","WithinSumSquares","BetweenSumSquares","NoOfClusters")

summary$new = summary$WithinSumSquares/summary$BetweenSumSquares
plot(summary$cutOff,summary$new)
plot(summary$NoOfClusters,summary$new)
plot(summary$cutOff,summary$WithinSumSquares)
ggplot(data = summary,aes(x=cutOff,y=WithinSumSquares))+geom_line()

plot(summary$NoOfClusters,summary$WithinSumSquares)
plot(summary$NoOfClusters,summary$cutOff)
##############################################################################################################
cutoff_2 = 0.1
##############################################
# Forming adjacency matrix

A=matrix(0,ncol = nodes,nrow = nodes)
for (i in 1:nodes)
{
  #index_0=which(wasser_dist_0[i,]<=cutoff_0)
  #index_1=which(wasser_dist_1[i,]<=cutoff_1)
  # A[i,intersect(index_0,index_1)]=1
  index_2 = which(wasser_dist_2[i,]<=cutoff_2)
  A[i,index_2]=1
  #print(i) 
}
##############################################

# Clustering
g=graph_from_adjacency_matrix(A,"directed") # form a graph from adj. matrix A
clstrs=clusters(g)  # clusters are strongly connected components of adj. matrix A
print(clstrs)


#postalcodes<-read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\postalcodesordered.csv")

#data_final <- cbind(data_t,postalcodes)
#colnames(data_final)[507] <- "PostalCode"
#colnames(data_final)[506] <- "ClusterNumber"

#write.csv(data_final[,506:507],"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\TDAClusters.csv")


data_t <- as.data.frame(data)
clstrs_t <- clstrs

tdaclustered <- cbind(data_t,clstrs_t$membership)
write.csv(tdaclustered,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\TDAClusters_March28.csv")

##############################Silhoutte distance ###################

summary <- data.frame(cutoff = double(),
                      AvgSilWidth= double(),
                      NoOfClusters = integer())


cutoff = seq(0.01,0.5,length.out=50)
test1 =c()
avg_sil1 = c()
for (c in cutoff)
{
  cutoff_2 = c
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
  print(clstrs$no)
  ss <- cluster::silhouette(clstrs$membership, dist(data_finally))
  avg_sil1 = mean(ss[, 3])
  test1 <- c(cutoff_2,avg_sil1,clstrs$no)
  summary <- rbind(test1,summary)
}

colnames(summary) <- c("cutOff","Avg.Sil","NoOfClusters")
plot(summary$cutOff,summary$Avg.Sil)
