

setwd('C:\\Users\\moniy\\Desktop\\GelLab\\Data')

ncl_D<-read.csv("NCL_Daily22.csv",header=TRUE)

dim(ncl_D) #  3652  505
summary(ncl_D[,1])





#------------- Weekly data ----------------------
library(zoo)
ncl_W<-apply(ncl_D, 2, function(x) rollapply(x, 7, sum, by = 7))


dim(ncl_W)  # 521 505

##############################################################################
###################################################################################

library(ppcor)
library(igraph)



ncl_mycorr1<-abs(cor(ncl_W)) # correlation


######## column with missing columns #################
na.omit(t(ncl_W)) 

#N3A N8V K1P N6L L9Z M5E L4V 
#372 435  21 415 238 293 158
#M5L M5W N3A N8V K1P N6L M5H L9Z L5T M5C M4H L5S M5E L4V 
#303 311 382 446  22 425 301 242 177 298 282 176 299 159 

ncl_W2<-ncl_W[,-c(303, 311, 382, 446,  22, 425, 301 ,242, 177, 298, 282, 176, 299, 159 )]
na.omit(t(ncl_W2)) 


ncl_mycorr<-pcor(ncl_W2,method = "spearman") # partial correlation ncl

Ecorr<-abs(ncl_mycorr$estimate)
dim(Ecorr)

corr.pvals <- ncl_mycorr$p.value
dim(corr.pvals)




###----------------------------
pcorr0<-ifelse(corr.pvals<0.05, 1, 0)
diag(pcorr0)<-0


Ecorr0<-abs(Loss_mycorr$estimate)
Ecorr0<-ifelse(Ecorr0>0.15, 1, 0)
diag(Ecorr0)<-0


 

#############################Correlation ##################################################
#---- All connected weighted graph -----------------

g.ncl<- graph.adjacency(Ecorr, mode="undirected",weighted=TRUE,diag=FALSE)
E(g.ncl)$weight

summary(E(g.ncl)$weight)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000018 0.0821944 0.1726043 0.1977561 0.2878816 0.9353084 

summary(g.ncl)
str(g.ncl)


igraph.options(vertex.size=30,  edge.arrow.size=0.5) #vertex.label=NA,
plot(g.ncl)



#V(g.Loss)$name<-1:505  #vertex labels  change-
#V(g.Loss)$name
#str(g.Loss)


node<-V(g.ncl);node #  505
edge<-E(g.ncl);edge # 127259
str(g.ncl)

ncl_degree<-degree(g.ncl)
summary(ncl_degree)
table(ncl_degree)

#------- edge weights ------------------------

ncl_edge_W<-cbind(get.edgelist(g.ncl) , round(E(g.ncl)$weight,5))
write.csv(ncl_edge_W,"C:/Users/akd130230/Dropbox/Climate Network AD/Data/edge_WCorr_ncl_ID_B2.csv")



ncL_node_W<-vertex_attr(g.ncl)
write.csv(ncL_node_W,"C:/Users/akd130230/Dropbox/Climate Network AD/Data/node_WCorr_ncl_B2.csv")



#################################Corr - p.value ################################

pvals<-1-corr.pvals

g.ncl2<- graph.adjacency(pvals, mode="undirected",weighted=TRUE,diag=FALSE)
E(g.ncl2)$weight

summary(E(g.ncl2)$weight)

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000055 0.2542400 0.5065891 0.5035412 0.7533095 1.0000000 


summary(g.ncl2)
str(g.ncl2)

node<-V(g.ncl2);node #  505
edge<-E(g.ncl2);edge # 127260
str(g.ncl2)

ncl_degree2<-degree(g.ncl2)
summary(ncl_degree2)
table(ncl_degree2)




ncl_edge_W2<-cbind(get.edgelist(g.ncl2) ,round(E(g.ncl2)$weight,5))
write.csv(ncl_edge_W2,"C:/Users/akd130230/Dropbox/Climate Network AD/Data/edge_Wpval_ncl_ID_B2.csv")





######################### Network Visualization #############################

igraph.options(vertex.size=0.5,  edge.arrow.size=10) #vertex.label=NA,
plot(g.ncl2,  main="ncl based network, weekly data, 521 PCode")



plot.igraph(g.ncl,vertex.label=V(g.ncl)$name,layout=layout.fruchterman.reingold, 
            edge.color="black",edge.width=E(g.ncl)$weight)


#--------------- with qgraph----------------

library("qgraph")
corMat <- cor_auto(ncl_W)

#Graph_pcor <- qgraph(corMat, graph = "cor", layout = "spring",alpha = 0.05)
Graph_pcor <- qgraph(corMat, graph = "cor", layout = "spring")




####################################################################
#-------------Descriptive Statistics -----------------

node<-V(g.ncl);node #  505
edge<-E(g.ncl);edge # 63503
str(g.ncl)

ncl_degree<-degree(g.ncl)
summary(ncl_degree)
table(ncl_degree)

#Min. 1st  Qu.  Median    Mean   3rd Qu.   Max. 
# 0.0    13.0   354.0   251.5   361.0   367.0 


meanD_ncl<-mean(ncl_degree);meanD_Loss # 251.497
sdD_ncl<-sd(ncl_degree) #122.3423

# second moment
moment2<-  var(ncl_degree)+meanD_ncl**2


degree_ncl_N<-degree.distribution(g.ncl)
plot(degree_ncl_N[2:length(degree_ncl_N)],type="l",lwd=2,ylab = "p(k)",xlab = "Number of edge (Degree), k",main = "Loss degree distribution",col="red")


degree_Loss<-degree_ncl_N*516 ;degree_Loss #Node 516
#---plot

