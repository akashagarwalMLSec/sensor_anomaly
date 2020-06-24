
library(igraph)
library(randomcoloR)

# dataset 'Smiley'

 setwd('C:\\Users\\moniy\\Desktop\\GelLab\\Data')
data=as.matrix(read.csv('C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NodeEmbeddings.csv',header=FALSE)) # load dataset

cap=1 # dimension cap
d=nrow(data) # number of points

eDist=as.matrix(dist(data)) # compute Euclidean distance between data points
nNghb=10 # number of neighbors around each data point

############################################
# Transforming distance matrix. This is an optional step.
all.dist=c()
for (i in 1:d){
  ind=which(rank(eDist[i,],ties.method = 'first')<=nNghb)
  ind=setdiff(ind,i)
  all.dist=c(all.dist,eDist[i,ind])
}
dist.cdf=ecdf(all.dist)
eDist=matrix(dist.cdf(eDist),nrow=d) # transform distances using empirical cdf. Now, distances are in [0,1]

#hist(all.dist)
#hist(dist.cdf(all.dist))

############################################

delta=0.01 # step size for filtration
filt_len=90 # filtration length.
# When distances are transformed, delta=0.01 and filt_len=L implies that we account for L% of variation in distances

# Running Perseus

betti_0=betti_1=c()

for (i in 1:d){
  
  ind=which(rank(eDist[i,],ties.method = 'first')<=nNghb) # indices of points around point i
  
  # writing data into file M.txt
  cat(length(ind),file='M.txt',append=F,sep = '\n')
  cat(paste(0,delta,filt_len,cap,sep = ' '),file = 'M.txt',append = T,sep = '\n') # threshold: 0, 0.1, 0.2,...,1
  cat(eDist[ind,ind],file='M.txt',append = T)
  
  system('perseusWin.exe distmat M.txt Moutput') # for Windows
  #system('./perseusMac distmat M.txt Moutput') # for Mac
  print(i) 
  
  betti_data=as.matrix(read.table('Moutput_betti.txt'))
  
  # filling in omitted Betti numbers
  betti_index=setdiff(0:filt_len,betti_data[,1])
  for (k in betti_index)
  {
    if (k<length(betti_data[,1]))
    {
      betti_data=rbind(betti_data[1:k,],betti_data[k,],betti_data[(k+1):length(betti_data[,1]),]);
      betti_index=betti_index+1
    } else
      betti_data=rbind(betti_data[1:k,],betti_data[k,])
  
  betti_0=rbind(betti_0,betti_data[,2])
  betti_1=rbind(betti_1,betti_data[,3])
  
}
##################################################

bettiDist_0=as.matrix(dist(betti_0)) # computing Euclidean distance between Betti-0 numbers
bettiDist_1=as.matrix(dist(betti_1)) # computing Euclidean distance between Betti-1 numbers

##################################################

# Computing relative change in Betti numbers
delta_betti_0=delta_betti_1=c()
index=c()
for (i in 1:d)
{
  ind=which(rank(eDist[i,],ties.method = 'first')<=nNghb)
  ind=ind[order(eDist[i,ind])] # rearrange indexes
  ind=ind[-1]
  
  delta_betti_0=rbind(delta_betti_0,bettiDist_0[i,ind]/norm(as.matrix(betti_0[i,]),type = 'f'))
  delta_betti_1=rbind(delta_betti_1,bettiDist_1[i,ind]/norm(as.matrix(betti_1[i,]),type = 'f'))
  
  index=rbind(index,ind)
  
  print(i)
}

##################################################

par(mfrow = c(1, 2))
# Overall comparison
bp_0=boxplot(as.vector(delta_betti_0),pch=20,ylab='Relative change in Betti-0')
bp_1=boxplot(as.vector(delta_betti_1),pch=20,ylab='Relative change in Betti-1')
par(mfrow = c(1, 1))

cutoff_0=bp_0$stats[5,] # cutoff threshold for relative change in Betti-0 numbers. Can also set it manually
cutoff_1=bp_1$stats[5,] # cutoff threshold for relative change in Betti-1 numbers. Can also set it manually

##############################################

# Forming adjacency matrix
A=matrix(0,ncol = d,nrow = d)
for (i in 1:d)
{
  index_0=which(delta_betti_0[i,]<=cutoff_0)
  index_1=which(delta_betti_1[i,]<=cutoff_1)
  
  A[i,index[i,intersect(index_0,index_1)]]=1
  
  print(i)
}

##############################################

# Clustering
g=graph_from_adjacency_matrix(A,mode='directed') # form a graph from adj. matrix A
clstrs=clusters(g,mode = 'strong') # clusters are strongly connected components of adj. matrix A
distinct.clrs=distinctColorPalette(clstrs$no)
clrs=distinct.clrs[clstrs$membership] # distinct colors for clusters
# plot(data,col=clrs,pch=20,xlab='x',ylab='y') # if data is 2D, plot data points with distinct colors for different clusters
print(clstrs) # print clustering result