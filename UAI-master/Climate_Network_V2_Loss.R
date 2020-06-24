

loss_D<-read.csv("C:/Users/akd130230/Dropbox/Climate Network AD/Data/LOSS_Daily22.csv",header=TRUE)

dim(loss_D) # 3652  505
summary(loss_D[,1])




#------------- Weakly data ----------------------
library(zoo)

#ncl_W<-apply(ncl_D, 2, function(x) rollapply(x, 7, sum, by = 7))
loss_W<-apply(loss_D, 2, function(x) rollapply(x, 7, sum, by = 7))


#dim(ncl_W)  # 521 505
#dim(loss_W)  # 521 505

#summary(ncl_W[,2])
#summary(rollapply(ncl_D[,2], 7, sum, by = 7))

##############################################################################
###################################################################################

library(ppcor)
library(igraph)


#mycorr1<-cor(loss_W) # correlation

Loss_mycorr<-pcor(loss_W,method = "spearman") # partial correlation Loss
#method = c("pearson", "kendall", "spearman")



Ecorr<-abs(Loss_mycorr$estimate)
corr.pvals <- Loss_mycorr$p.value

###----------------------------
pcorr0<-ifelse(corr.pvals<0.05, 1, 0)
diag(pcorr0)<-0


Ecorr0<-abs(Loss_mycorr$estimate)
Ecorr0<-ifelse(Ecorr0>0.15, 1, 0)
diag(Ecorr0)<-0


############################Correlation ############################################
#---- All connected weighted graph -----------------

g.Loss<- graph.adjacency(Ecorr, mode="undirected",weighted=TRUE,diag=FALSE)
E(g.Loss)$weight

summary(E(g.Loss)$weight)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000034 0.0773255 0.1624613 0.1869166 0.2719045 0.98657329

summary(g.Loss)
str(g.Loss)


igraph.options(vertex.size=30,  edge.arrow.size=0.5) #vertex.label=NA,
plot(g.Loss)



#V(g.Loss)$name<-1:505  #vertex labels  change-
#V(g.Loss)$name
#str(g.Loss)


node<-V(g.Loss);node #  505
edge<-E(g.Loss);edge # 127259
str(g.Loss)

Loss_degree<-degree(g.Loss)
summary(Loss_degree)
table(Loss_degree)

#------- edge weights ------------------------

edge_W<-cbind(get.edgelist(g.Loss) , round(E(g.Loss)$weight,5))
write.csv(edge_W,"C:/Users/akd130230/Dropbox/Climate Network AD/Data/edge_W_LOSS_ID_B2.csv")

node_W<-vertex_attr(g.Loss)
write.csv(node_W,"C:/Users/akd130230/Dropbox/Climate Network AD/Data/node_W_Loss_B2.csv")



######################### Network Visualization #############################

igraph.options(vertex.size=0.5,  edge.arrow.size=10) #vertex.label=NA,
plot(g.Loss,  main="ncl based network, weekly data, 521 PCode")



plot.igraph(g.Loss,vertex.label=V(g.Loss)$name,layout=layout.fruchterman.reingold, 
            edge.color="black",edge.width=E(g.Loss)$weight)


#--------------- with qgraph----------------

library("qgraph")
corMat <- cor_auto(loss_W)

#Graph_pcor <- qgraph(corMat, graph = "cor", layout = "spring",alpha = 0.05)
Graph_pcor <- qgraph(corMat, graph = "cor", layout = "spring")








#################################Corr - p.value ################################

pvals<-1-corr.pvals

g.Loss2<- graph.adjacency(pvals, mode="undirected",weighted=TRUE,diag=FALSE)
E(g.Loss2)$weight

summary(E(g.Loss2)$weight)

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000106 0.2396101 0.4804783 0.4842935 0.7249490 1.0000000 


summary(g.Loss2)
str(g.Loss2)

node<-V(g.Loss2);node #  505
edge<-E(g.Loss2);edge # 127259
str(g.Loss2)

Loss_degree2<-degree(g.Loss2)
summary(Loss_degree2)
table(Loss_degree2)




edge_W2<-cbind(get.edgelist(g.Loss2) ,round(E(g.Loss2)$weight,5))
write.csv(edge_W2,"C:/Users/akd130230/Dropbox/Climate Network AD/Data/edge_W_LOSS_ID_B2.csv")














####################################################################
#-------------Descriptive Statistics -----------------

node<-V(g.Loss);node #  505
edge<-E(g.Loss);edge # 63503
str(g.Loss)

Loss_degree<-degree(g.Loss)
summary(Loss_degree)
table(Loss_degree)

#Min. 1st  Qu.  Median    Mean   3rd Qu.   Max. 
# 0.0    13.0   354.0   251.5   361.0   367.0 


meanD_Loss<-mean(Loss_degree);meanD_Loss # 251.497
sdD1<-sd(Loss_degree) #122.3423

# second moment
moment2<-  var(Loss_degree)+meanD_Loss**2


degree_Loss_N<-degree.distribution(g.Loss)
plot(degree_Loss_N[2:length(degree_Loss_N)],type="l",lwd=2,ylab = "p(k)",xlab = "Number of edge (Degree), k",main = "Loss degree distribution",col="red")


degree_Loss<-degree_Loss_N*516 ;degree_Loss #Node 516
#---plot




#--------Poisson degree distribution---------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


Loss_degree_A=as.numeric(Loss_degree) 

library(MASS)
m1=fitdistr(Loss_degree_A,densfun = "Poisson");m1 #lambda: 257.0000000: Mean=lambda

m2=fitdistr(Loss_degree_A,densfun = "exponential");m2 # gamma:  0.003891051  : Mean=(1/gamma)=128

plot(dpois(x=1:373, lambda=257.0), type="b", ylab = "p(k)",xlab = "Number of edge (Degree), k",main = "Loss degree distribution",col="red")
lines(c(1:373),degree_Loss_N[2:374],type="o", col="black")
lines(dexp(x=1:286, rate=0.0078125000),type="o", col="blue")




legend("topright",  #cex=1.1, y.intersp=.9,
       c('Observed degree distribution ', 'Poisson degree distribution','Exponential degree distribution'),
       lwd=c(1,1,1), 
       lty=c(1,1,1),
       col=c('black',  'red','blue')) 


############################ Cumulative Degree Distribution  #######################################

cum_d_Loss<-degree_distribution(g.Loss, cumulative = TRUE) 
cum_d_Loss
length(cum_d_Loss) #374


plot(c(1:374),cum_d_Loss,type="o", col="black", ylab = "p(k)", xlab = "Degree")



#----------closeness ---------------
closeness1<-closeness(g.Loss) 

#----------betweenness ---------------
betweenness_Loss<-betweenness(g.Loss)
table(betweenness_Loss)
summary(betweenness_Loss) 

#Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#0.0     0.0   706.2    2235.8  2381.8   25510.6 

plot(betweenness_Loss)
hist(betweenness_Loss,xlab = "Betweenness", main="Histogram of Betweenness")
box() 


####################################################################################################
#####################################################################
#------------------- ncl Network ---------------------------------

ncl_W2<-ncl_W
ncl_W2<-ifelse(ncl_W2>quantile(ncl_W2,0.5), 1, 0)
diag(ncl_W2)<-0

g.ncl<- graph.adjacency(ncl_W2, "undirected")
summary(g.ncl)
str(g.ncl)


V(g.ncl)$name<-1:505  #vertex labels  change-
V(g.ncl)$name
str(g.ncl)

igraph.options(vertex.size=2,  edge.arrow.size=0.5) #vertex.label=NA,
plot(g.ncl,  main="ncl based network, weekly data, 521 PCode")




#-------------Descriptive Statistics -----------------

node<-V(g.ncl);node #  505
edge<-E(g.ncl);edge # 63503
str(g.ncl)



ncl_degree<-degree(g.ncl)
summary(ncl_degree)
table(ncl_degree)

#Min. 1st  Qu.  Median    Mean   3rd Qu.   Max. 
#  0.0   141.0   316.0   251.5   353.0   385.0 


meanD_Loss<-mean(Loss_degree);meanD_Loss # 251.497
sdD1<-sd(Loss_degree) #122.3423

# second moment
moment2<-  var(Loss_degree)+meanD_Loss**2


degree_ncl_N<-degree.distribution(g.ncl)
plot(degree_ncl_N[2:length(degree_ncl_N)],type="l",lwd=2,ylab = "p(k)",xlab = "Number of edge (Degree), k",main = "ncl degree distribution",col="red")


degree_Loss<-degree_Loss_N*505 ;degree_Loss #Node 505


#################################################################
#-----------------------------------------------
#corr.pvals <- mycorr1$p.value
#corr.pvals.adj <- p.adjust(corr.pvals, "BH")


pcorr0<-Loss_mycorr$p.value

pcorr<-ifelse(pcorr0<0.05, 1, 0)
diag(pcorr)<-0




#-----------------------------------------------------------------

p.pcorr<- graph.adjacency(pcorr, "undirected")

summary(p.pcorr)
str(p.pcorr)

V(p.pcorr)$name<-1:505 


igraph.options(vertex.size=2,  edge.arrow.size=0.5) #vertex.label=NA,
plot(p.pcorr,  main="ncl based network, monthly data, 100 PCode")



#------------------------------------------------------------------------------

