########################################################################################################
#                Construction of MultiLayer Graph using Claims, Losses, Precipitation Data
#                     Author: Monisha Yuvaraj
#                     Date: September 20th 2018
########################################################################################################                   

library("reshape2")
library("tidyverse")
library("ppcor")
library("igraph")
library("multigraph")
library("abind")
library("sqldf")
library(acepack)
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
library(igraph)
library(randomcoloR)
library(multigraph)
library(TDA)

rm(list = ls())

##Read in RData File which contains Claims and Loss Data

load("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\ONsouth.RData")

##Cast NBRCL_ columns into integer type
ONsouth$NBRCL_HAIL <- as.numeric(as.character(ONsouth$NBRCL_HAIL))    
ONsouth$NBRCL_WIND <- as.numeric(as.character(ONsouth$NBRCL_WIND))
ONsouth$NBRCL_WATER <- as.numeric(as.character(ONsouth$NBRCL_WATER))
ONsouth$NBRCL_SBU <- as.numeric(as.character(ONsouth$NBRCL_SBU))
ONsouth$CREDIT_SCORE_POSTAL_CODE_BASED <- as.numeric(as.character(ONsouth$CREDIT_SCORE_POSTAL_CODE_BASED))

##Replace NA's with 0 in all the NBRCL_ columns
ONsouth[c("NBRCL_SBU","NBRCL_HAIL","NBRCL_WATER","NBRCL_WIND")][is.na(ONsouth[c("NBRCL_SBU","NBRCL_HAIL","NBRCL_WATER","NBRCL_WIND")])] <- 0
ONsouth$AGE_OF_DWELLING[ONsouth$AGE_OF_DWELLING == 9999] <- NA
ONsouth$AGE_OF_DWELLING[ONsouth$AGE_OF_DWELLING < 0] <- NA

ONsouth$CREDIT_SCORE_POSTAL_CODE_BASED[ONsouth$CREDIT_SCORE_POSTAL_CODE_BASED == "NRQ"] <- NA

##Add all the different Claims amount and Numbers into one column.
ONsouth$TotAmtOfClaims = ONsouth$INC_SBU + ONsouth$INC_WIND + ONsouth$INC_WATER + ONsouth$INC_HAIL
ONsouth$TotNumberOfClaims <- ONsouth$NBRCL_HAIL + ONsouth$NBRCL_WATER + ONsouth$NBRCL_SBU + ONsouth$NBRCL_WIND


##Subset Data to only those rows that have Claims or Loss Data
Insurance_v1 <- ONsouth[which(ONsouth$TotAmtOfClaims > 0 & ONsouth$TotNumberOfClaims > 0),
                        c("FSA","CHANGE_EFFECTIVE_DATE","FUTURE_CHANGE_EFFECTIVE_DATE", "DOLYYYY","TotAmtOfClaims","TotNumberOfClaims","NBR_OF_LIVING_UNITS_CD")]

#Insurance_social <- ONsouth[which(ONsouth$TotAmtOfClaims > 0 & ONsouth$TotNumberOfClaims > 0),
 #                           c("FSA","DOLYYYY","CREDIT_SCORE_POSTAL_CODE_BASED","AGE_OF_DWELLING")]

AnamolousDates <- subset(Insurance_v1,!(DOLYYYY >= CHANGE_EFFECTIVE_DATE & DOLYYYY <= FUTURE_CHANGE_EFFECTIVE_DATE))
## The Insurance Claim was filed after the insurance lapsed. For our purposes this data can be included.There are 8 such cases

#################Check duplication in the dataset ######################
Insurance_v2 <- plyr::count(Insurance_v1,colnames(Insurance_v1))  ##frequency of each row & only unique rows
##9 rows were duplicated

Insurance_social_v2 <-  plyr::count(Insurance_social,colnames(Insurance_social))

count <- plyr::count(Insurance_v2,c("FSA","DOLYYYY"))
AnamolousDates <- count[which(count$freq > 1),]

DuplicatedDates <- ONsouth[which(ONsouth$FSA %in% AnamolousDates$FSA & ONsouth$DOLYYYY %in% AnamolousDates$DOLYYYY),]
DuplicatedDates <- DuplicatedDates[order(z$FSA,z$DOLYYYY),]
##3850(7.05%) claims have been filed in the same county on the same date


##############################Divide Claims and Number of claims by Number of units#################
Insurance_v2$AmtOfClaims <- Insurance_v2$TotAmtOfClaims/Insurance_v2$NBR_OF_LIVING_UNITS_CD
Insurance_v2$NoOfClaims <- Insurance_v2$TotNumberOfClaims/Insurance_v2$NBR_OF_LIVING_UNITS_CD

################################relationships between variables#################################
cor(x = Insurance_v2$AmtOfClaims,y=Insurance_v2$NoOfClaims,use = "pairwise.complete.obs")
##0.06

cor(x = ONsouth$TotAmtOfClaims,y=ONsouth$TotNumberOfClaims,use = "pairwise.complete.obs")
## 0.45

############################### Create Dataset to run time series correlation#######################

TotalAmt <- dcast(Insurance_v2, DOLYYYY ~ FSA, fun.aggregate = sum, value.var="AmtOfClaims")
TotalNoClaims <- dcast(Insurance_v2, DOLYYYY ~ FSA, fun.aggregate = sum, value.var="NoOfClaims")
#TotalHouses  <- dcast(Insurance_v2,DOLYYYY ~ FSA,fun.aggregate = sum,value.var = "NBR_OF_LIVING_UNITS_CD")

x <- aggregate(data = ONsouth, AGE_OF_DWELLING ~ DOLYYYY + FSA, mean, na.rm=T)
TotalHouseAge <- dcast(x,DOLYYYY ~ FSA,value.var = "AGE_OF_DWELLING",fun.aggregate = sum)

TotalCreditScore <- dcast(ONsouth, DOLYYYY ~ FSA, fun.aggregate = sum, value.var="CREDIT_SCORE_POSTAL_CODE_BASED")

# Generate Weekly(Starting Sunday Dates to cover 2002 - 2012)
Dates <- as.data.frame(seq(from = as.Date("2001-12-30"), to = as.Date("2011-12-31"), 1))
colnames(Dates) <- "Date"

## Join the data from Cleaned dataset to the Dates generated
Daily_ClaimAmt <- merge(x= Dates,y=TotalAmt,by.x = "Date",by.y="DOLYYYY",all.x = T)
Daily_ClaimAmt[is.na(Daily_ClaimAmt)] <- 0

##Aggregate the Claim amount by week 
Daily_ClaimAmt$week <- cut(Daily_ClaimAmt$Date,'week',start.on.monday = F)
Daily_ClaimAmt <- Daily_ClaimAmt[c(508,2:507)]
WeeklyClaimAmt <- aggregate(data=Daily_ClaimAmt,.~Daily_ClaimAmt$week, sum)

WeeklyClaimAmt <- subset(WeeklyClaimAmt, select=-c(L6G)) ## Drop Postal Code that has no Claims 


##############################################################################################################
## Join the data from Cleaned dataset to the Dates generated
Daily_No_Claim <- merge(x= Dates,y=TotalNoClaims,by.x = "Date",by.y="DOLYYYY",all.x = T)
Daily_No_Claim[is.na(Daily_No_Claim)] <- 0

##Aggregate the Claim amount by week 
Daily_No_Claim$week <- cut(Daily_No_Claim$Date,'week',start.on.monday = F)
Daily_No_Claim <- Daily_No_Claim[c(508,2:507)]
Weekly_No_Claim <- aggregate(data=Daily_No_Claim,.~Daily_No_Claim$week, sum)

Weekly_No_Claim <- subset(Weekly_No_Claim, select=-c(L6G)) ## Drop Postal Code that has no Claims 

##############################################################################################################
HouseAge <- read.csv("C:\\Users\\moniy\\Dropbox\\Climate Network\\Data\\HouseAge2.csv")
CreditScore <- read.csv("C:\\Users\\moniy\\Dropbox\\Climate Network\\Data\\CrScore2.csv")

############Check which colnmaes are there in House age and not in Credit Score#################

notpresent <- colnames(HouseAge)[!(colnames(HouseAge) %in% colnames(Weekly_No_Claim))]

HouseAge <- subset(HouseAge, select=-c(K1A,L5S,L5T,M4H,M5C,M5G,M5H,M5L,M5W,N6N,L6G))
CreditScore <- subset(CreditScore, select=-c(K1A,L5S,L5T,M4H,M5C,M5G,M5H,M5L,M5W,N6N,L6G))
HouseAge[HouseAge == "NaN"] <- NA
CreditScore[CreditScore == "NaN"] <- NA
##Aggregate the HouseAge by week 

Weekly_HouseAge <- HouseAge %>%
  group_by(week) %>%
  summarise_all(funs(mean)) %>%
  complete(week)

Weekly_HouseAge <- Weekly_HouseAge[-1,]
Weekly_HouseAge <- Weekly_HouseAge[-1]

Weekly_CreditScore <- CreditScore %>%
  group_by(week) %>%
  summarise_all(funs(mean)) %>%
  complete(week)


Weekly_CreditScore <- Weekly_CreditScore[-1,]
Weekly_CreditScore <- Weekly_CreditScore[-1]

#############################################################################################################
## Read the precipitation Data
precip <- read.csv("C:\\Users\\moniy\\Dropbox\\Climate Network\\Data\\weather_network_wideformat_v3.csv")

precip_subset <- subset(x= precip,subset =(as.Date(time) >= "2001-12-30" & as.Date(time) <= "2011-12-25" ))
precip_subset <- precip_subset[ , which(names(precip_subset) %in% colnames(WeeklyClaimAmt))]

#####################################Check the order of the columns #########################################

length(colnames(precip_subset)) #505
length(Weekly_No_Claim[3:507]) #505
length(WeeklyClaimAmt[3:507]) #505
length(Weekly_HouseAge) #505
length(Weekly_CreditScore) #505


colnames(HouseAge)[!(colnames(HouseAge) %in% colnames(Weekly_No_Claim))]
colnames(CreditScore)[!(colnames(CreditScore) %in% colnames(Weekly_No_Claim))]
colnames(precip_subset)[!(colnames(precip_subset) %in% colnames(Weekly_No_Claim))]
colnames(Weekly_No_Claim[3:507])[!(colnames(Weekly_No_Claim[3:507]) %in% colnames(precip_subset))]
colnames(Weekly_No_Claim[3:507])[!(colnames(Weekly_No_Claim[3:507]) %in% colnames(HouseAge))]



No_Claim <- Weekly_No_Claim[3:507] ##Removed Week number and Date column
No_Claim <- No_Claim[names(precip_subset)]
ClaimAmt <- WeeklyClaimAmt[3:507]##Removed Week number and Date column
ClaimAmt <- ClaimAmt[names(precip_subset)]
Weekly_HouseAge <- Weekly_HouseAge[names(precip_subset)]
Weekly_CreditScore <- Weekly_CreditScore[names(precip_subset)]

##############################################Transformed correlations ###############################

precip_subset <- subset(precip_subset, select=-c(N8V))
Weekly_HouseAge <- subset(Weekly_HouseAge, select=-c(N8V))
ClaimAmt <- subset(ClaimAmt, select=-c(N8V))
No_Claim <- subset(No_Claim, select=-c(N8V))
Weekly_CreditScore <- subset(Weekly_CreditScore, select=-c(N8V))



impute.mean <- function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x[!is.na(x) & !is.nan(x) & !is.infinite(x)]))

Weekly_HouseAge <- apply(Weekly_HouseAge, 2, impute.mean)
Weekly_HouseAge <- as.data.frame(Weekly_HouseAge)

Weekly_CreditScore <- apply(Weekly_CreditScore, 2, impute.mean)
Weekly_CreditScore <- as.data.frame(Weekly_CreditScore)

IntrAmtandNumber <- matrix(data=NA,ncol=505,nrow=505) ## Declare empty matrix
IntrPrecipandAmt <- matrix(data=NA,ncol=505,nrow=505)
IntrPrecipandNumber <- matrix(data=NA,ncol=505,nrow=505)
IntrPrecipandHouse <- matrix(data=NA,ncol=505,nrow=505)
IntrPrecipandCredit <- matrix(data=NA,ncol=505,nrow=505)
IntrAmtandHouse <- matrix(data=NA,ncol=505,nrow=505)
IntrAmtandCredit <- matrix(data=NA,ncol=505,nrow=505)
IntrNumberandHouse <- matrix(data=NA,ncol=505,nrow=505)
IntrNumberandCredit <- matrix(data=NA,ncol=505,nrow=505)
IntrHouseandCredit <- matrix(data=NA,ncol=505,nrow=505)
precipitation <- matrix(data=NA,ncol=505,nrow=505)
claimAmount <- matrix(data=NA,ncol=505,nrow=505)
ClaimNumber <- matrix(data=NA,ncol=505,nrow=505)
HouseAge_corr <- matrix(data=NA,ncol=505,nrow=505)
CreditScore_corr <- matrix(data=NA,ncol=505,nrow=505)

for(i in 2:505){
  for(j in 2:505){
    print(i)
    print(j)
    
    ace0=ace(x= as.numeric(unlist(precip_subset[i])),y= as.numeric(unlist(precip_subset[j])))
    precipitation[i,j] = cor(ace0$tx,ace0$ty)
    print("Pdone")
    
    ace1 = ace(x= as.numeric(unlist(ClaimAmt[i])),y= as.numeric(unlist(ClaimAmt[j])))
    claimAmount[i,j] = cor(ace1$tx,ace1$ty)
    print("Cdone")
    
    ace2 = ace(x= as.numeric(unlist(No_Claim[i])),y= as.numeric(unlist(No_Claim[j])))
    ClaimNumber[i,j] = cor(ace2$tx,ace2$ty)
    print("Ndone")
    
    ace3 = ace(x= as.numeric(unlist(Weekly_HouseAge[i])),y= as.numeric(unlist(Weekly_HouseAge[j])))
    HouseAge_corr[i,j] =cor(ace3$tx,ace3$ty)
    print("HouseDone")
    ace4 = ace(x= as.numeric(unlist(Weekly_CreditScore[i])),y= as.numeric(unlist(Weekly_CreditScore[j])))
    CreditScore_corr[i,j] = cor(ace4$tx,ace4$ty)
    print("creditdone")
    
    ace5 = ace(x= as.numeric(unlist(No_Claim[i])),y= as.numeric(unlist(ClaimAmt[j])))
    IntrAmtandNumber[i,j] = cor(ace5$tx,ace5$ty)
    print("1")
    ace6 = ace(x= as.numeric(unlist(precip_subset[i])),y= as.numeric(unlist(ClaimAmt[j])))
    IntrPrecipandAmt[i,j] = cor(ace6$tx,ace6$ty)
    print("21")
    ace7 = ace(x= as.numeric(unlist(precip_subset[i])),y= as.numeric(unlist(No_Claim[j])))
    IntrPrecipandNumber[i,j] = cor(ace7$tx,ace7$ty)
    print("22")
    ace8 = ace(x= as.numeric(unlist(precip_subset[i])),y=as.numeric(unlist(Weekly_HouseAge[j])))
    IntrPrecipandHouse[i,j] = cor(ace8$tx,ace8$ty)
    print("23")
    ace9 = ace(x= as.numeric(unlist(precip_subset[i])),y=as.numeric(unlist(Weekly_CreditScore[j])))
    IntrPrecipandCredit[i,j] = cor(ace9$tx,ace9$ty)
    print("24")
    ace10 = ace(x= as.numeric(unlist(ClaimAmt[i])),y=as.numeric(unlist(Weekly_HouseAge[j])))
    IntrAmtandHouse[i,j] = cor(ace10$tx,ace10$ty)
    print("25")
    ace11 = ace(x= as.numeric(unlist(ClaimAmt[i])),y=as.numeric(unlist(Weekly_CreditScore[j])))
    IntrAmtandCredit[i,j] = cor(ace11$tx,ace11$ty)
    print("26")
    ace12 = ace(x= as.numeric(unlist(No_Claim[i])),y=as.numeric(unlist(Weekly_HouseAge[j])))
    IntrNumberandHouse[i,j] = cor(ace12$tx,ace12$ty)
    print("27")
    ace13 = ace(x= as.numeric(unlist(No_Claim[i])),y=as.numeric(unlist(Weekly_CreditScore[j])))
    IntrNumberandCredit[i,j] = cor(ace13$tx,ace13$ty)
    print("28")
    ace14 = ace(x= as.numeric(unlist(Weekly_HouseAge[i])) ,y=as.numeric(unlist(Weekly_CreditScore[j])))
    IntrHouseandCredit[i,j] = cor(ace14$tx,ace14$ty)
    print("29")
  }
}

precipitation[is.na(precipitation)] <- 0
claimAmount[is.na(claimAmount)] <- 0
ClaimNumber[is.na(ClaimNumber)] <- 0
HouseAge_corr[is.na(HouseAge_corr)] <- 0
CreditScore_corr[is.na(CreditScore_corr)] <- 0
IntrAmtandNumber[is.na(IntrAmtandNumber)] <- 0
IntrPrecipandAmt[is.na(IntrPrecipandAmt)] <- 0
IntrPrecipandNumber[is.na(IntrPrecipandNumber)] <- 0
IntrPrecipandHouse[is.na(IntrPrecipandHouse)] <- 0
IntrPrecipandCredit[is.na(IntrPrecipandCredit)] <- 0
IntrAmtandHouse[is.na(IntrAmtandHouse)] <- 0
IntrAmtandCredit[is.na(IntrAmtandCredit)] <- 0
IntrNumberandHouse[is.na(IntrNumberandHouse)] <- 0
IntrNumberandCredit[is.na(IntrNumberandCredit)] <- 0
IntrHouseandCredit[is.na(IntrHouseandCredit)] <- 0

#write.csv(precipitation,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\Precipitationtransformed.csv")
#write.csv(claimAmount,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClaimsAmounttransformed.csv")
#write.csv(ClaimNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClaimsNumbertransformed.csv")
#write.csv(HouseAge_corr,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HouseAgetransformed.csv")
#write.csv(CreditScore_corr,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\CreditScoretransformed.csv")

#write.csv(IntrAmtandNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandNumbertransformed.csv")
#write.csv(IntrPrecipandAmt,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandAmttransformed.csv")
#write.csv(IntrPrecipandNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandNumbertransformed.csv")
#write.csv(IntrPrecipandHouse,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandHousetransformed.csv")
#write.csv(IntrPrecipandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandCredittransformed.csv")
#write.csv(IntrAmtandHouse,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandHousetransformed.csv")
#write.csv(IntrAmtandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandCredittransformed.csv")
#write.csv(IntrNumberandHouse,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\NumberandHousetransformed.csv")
#write.csv(IntrNumberandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\NumberandCredittransformed.csv")
#write.csv(IntrHouseandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HouseandCredittransformed.csv")

### Run Matlab Code after this#########################
# dataset climate network
setwd("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes")

ClaimEmbeddings <- read.csv("Claims_MANE.csv",header = F)
ClaimAmountEmbeddings <- read.csv("ClaimAmount_MANE.csv",header=F)
PrecipEmbeddings <- read.csv("Precip_MANE.csv",header=F)
CreditEmbeddings <- read.csv("CreditScore_MANE.csv",header=F)
HouseEmbeddings <- read.csv("HouseAge_MANE.csv",header=F)

data_finally <- cbind(ClaimAmountEmbeddings,ClaimEmbeddings,PrecipEmbeddings,PrecipEmbeddings,CreditEmbeddings,HouseEmbeddings)
data_finally <- data_finally[-1,]
new <- dist(data_finally)
data<- as.matrix(new)

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

data_t <- as.data.frame(data)
clstrs_t <- clstrs

tdaclustered <- cbind(data_t,clstrs_t$membership)

#########################################################Hierarchical Clustering ###############
# dataset climate network
setwd("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes")

ClaimEmbeddings <- read.csv("Claims_MANE.csv",header = F)
ClaimAmountEmbeddings <- read.csv("ClaimAmount_MANE.csv",header=F)
PrecipEmbeddings <- read.csv("Precip_MANE.csv",header=F)
CreditEmbeddings <- read.csv("CreditScore_MANE.csv",header=F)
HouseEmbeddings <- read.csv("HouseAge_MANE.csv",header=F)


data_finally<- cbind(ClaimAmountEmbeddings,ClaimEmbeddings,PrecipEmbeddings,PrecipEmbeddings,CreditEmbeddings,HouseEmbeddings)
#new <- dist(data_finally)
#data<- as.matrix(new)
my.names <- paste("V",seq(1,90,1),sep="_")
colnames(data_finally) <- my.names
colnames(data_finally) 
dtable = as.data.table(data_finally)


########################## Hierarchical Clustering ###########################
dtable = dist(data_finally)
fviz_nbclust(dtable, FUN = hcut, method = "wss",k.max=50)

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

clustering_results <- cbind(clust_heir,clstrs$membership)
colnames(clustering_results) <- c("Aggomerative","TDA")

###############################################################################################################
clustering_results <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March20.csv")

check <- cbind(clustering_results,colnames(precip_subset)[2:505])

write.csv(check,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March29.csv")


clustering_results <- as.data.frame(clustering_results)
clustering_results <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AllClusters_March29.csv")


