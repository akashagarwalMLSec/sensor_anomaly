
#install.packages("TDA")
library(TDA)


X <- circleUnif(400)
plot(X)

Xlim <- c(-1.6, 1.6); Ylim <- c(-1.7, 1.7); by <- 0.065
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

distance <- distFct(X = X, Grid = Grid)

#-------------------------------------------
X<-new_Italy_network

X <- circleUnif(400)
h <- 0.3

band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100,
                       parallel = FALSE, alpha = 0.1, h = h)


DiagGrid <- gridDiag(
  X = X, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim), by = by,
  sublevel = FALSE, library = "Dionysus", location = TRUE,
  printProgress = FALSE)

#------------
par(mfrow=c(2,2))
plot(DiagGrid[["diagram"]], band = 2 * band[["width"]],
     main = "KDE Diagram")

plot(DiagGrid[["diagram"]], band = 2 * band[["width"]],
     main = "KDE Diagram",rotated = TRUE )


plot(DiagGrid[["diagram"]], rotated = TRUE, band = band[["width"]],
        main = "Rotated Diagram")

plot(DiagGrid[["diagram"]], barcode = TRUE, main = "Barcode")


################################### Power Grid  #############################################

country<-c("Italy","Germany","Spain","France")

library(igraph)
library(NetSwan)

#data11 <- read.csv("C:/Users/akd130230/Dropbox/Power Grid Network/Data from author/processed/Export_Output22.csv")
data11 <- read.csv("D:/02/Dropbox/Power Grid Network/Data from author/processed/Export_Output22.csv")

nodes_G2<-data11[,c(2,3,4)][data11[,5]=="Spain",]
length(nodes_G2$X)
ID_G2<-data11[,2][data11[,5]=="Spain"]

G2<-data11[(data11[,17] %in% ID_G2) & (data11[,18] %in% ID_G2)  , ] # or
G2_data_all<-G2[,c(16,17,18)]

G2_data_all_no_dup=unique(G2_data_all)
G2_data<-G2_data_all_no_dup[,c(2,3)]   # Edge

#------------Weight------------
dist<-G2_data_all_no_dup[,c(1)]
G2_weight<-dist

#------------------------------

G2_edge=data.matrix(G2_data)
G2_network=graph_from_edgelist(G2_edge,directed = F)

ed11<-c(as.vector(G2_edge[,1]),as.vector(G2_edge[,2]))
unique_ed11<-unique(ed11)
nodes_index=sort(unique_ed11)
m1<-max(nodes_index)
delete_nodes_index=setdiff(c(1:m1),nodes_index)
V(G2_network)$name=V(G2_network)
new_G2_network=delete_vertices(G2_network,delete_nodes_index)

length(V(new_G2_network));length(E(new_G2_network))


################################################################    

library(TDA)

g<-new_G2_network
weight1<-G2_weight #  node distance 

#-------------------------Weighted Adjacency Matrix ---------------

E(g)$weight <- weight1 
max(E(g)$weight) # Spain =2.760759; Germany= 2.202105

A1<- get.adjacency(g,attr="weight")
A2<-as.matrix(A1)
A2[A2==0]<-999
diag(A2)=0

#----------------------------------------------------------------
#The function ripsDiag computes the persistence diagram of the 
#Rips filtration built on top of a point cloud.

maxdimension <- 1 # max dimension of the homological features to be computed
                  # 1 for connected components and loops

maxscale <- 3     # maximum value of the rips filtration.



DiagRips22 <- ripsDiag(X = A2, maxdimension, maxscale, dist = "arbitrary",
                      library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = FALSE)
#plot(DiagRips22[["diagram"]])

#------------
D10<-DiagRips22$diagram
grid()
plot(D10, barcode = TRUE, main = "Barcode, Spanish grid")
#grid()
abline(h=462,lwd=1) # Germany 446; Spanish 473


axis(2,at=c(250,  560),tick = FALSE,
     labels=c(expression(paste(H[0])), expression(paste(H[1]))),las=1)

#-------------------------------------------

plot(DiagRips22[["diagram"]],  main = "KDE Diagram, Germany Power Grid") #, band = 2 * band[["width"]]



#plot(DiagRips22[["diagram"]], band = 2 * band[["width"]],     main = "KDE Diagram",rotated = TRUE )
#plot(DiagRips22[["diagram"]], rotated = TRUE, band = band[["width"]],   main = "Rotated Diagram")



-----------------------











