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

#a <-as.data.frame(colSums(No_Claim))
#b <- as.data.frame(colSums(ClaimAmt))
#c <- as.data.frame(colSums(Weekly_HouseAge))
#d <- as.data.frame(colSums(Weekly_CreditScore))
#e <- as.data.frame(colSums(precip_subset))

#data.plot <- cbind(a,b,c,d,e)
#write.csv(data.plot,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\plotprelim.csv")
#write.csv(postalcodes,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\postalcodesordered.csv")
################################Correlation Time Series########################################################

#ClaimNumber_Matrix <- data.matrix(No_Claim) 
#ClaimAmount_Matrix <- data.matrix(ClaimAmt)
#Precip_Matrix <- data.matrix(precip_subset)

#Correl_Claims<-pcor(ClaimAmount_Matrix,method = "pearson") # partial correlation Loss
#CorClaims<-abs(Correl_Claims$estimate)

#####################Same Process for Claims Number ######################################
#Correl_No_Claims<-pcor(ClaimNumber_Matrix,method = "pearson") # partial correlation Loss
#CorNoClaims<-abs(Correl_No_Claims$estimate)
#CorNoClaims[is.na(CorNoClaims)] <- 0

##################### Same Process for Precipitation ######################################
#Correl_Precip<-pcor(Precip_Matrix,method = "pearson") # partial correlation Loss
#CorPrecip<-abs(Correl_Precip$estimate)
#CorPrecip[is.na(CorPrecip)] <- 0

##################### Same Process for HouseAge ######################################
#Weekly_HouseAge[is.na(Weekly_HouseAge)] <- median(Weekly_HouseAge, na.rm=TRUE)
#Correl_HouseAge<-pcor(Weekly_HouseAge,method = "pearson") # partial correlation Loss
#CorHouseAge<-abs(Correl_HouseAge$estimate)
#CorHouseAge[is.na(CorHouseAge)] <- 0
##################### Same Process for Credit Score ######################################
#Weekly_CreditScore[is.na(Weekly_CreditScore)] <- 0
#Correl_CreditScore<-pcor(Weekly_CreditScore,method = "pearson") # partial correlation Loss
#CorCreditScore<-abs(Correl_CreditScore$estimate)
#CorCreditScore[is.na(CorCreditScore)] <- 0
############################Correlation Matrices for interlayer ##########################################

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

for(i in 1:505){
  for(j in 1:505){
    
    precipitation[i,j]=cor(x= precip_subset[i],y= precip_subset[j],use = "pairwise.complete.obs")
    claimAmount[i,j]=cor(x= ClaimAmt[i],y= ClaimAmt[j],use = "pairwise.complete.obs")
    ClaimNumber[i,j]=cor(x= No_Claim[i],y= No_Claim[j],use = "pairwise.complete.obs")
    HouseAge_corr[i,j]=cor(x= Weekly_HouseAge[i],y= Weekly_HouseAge[j],use = "pairwise.complete.obs")
    CreditScore_corr[i,j]=cor(x= Weekly_CreditScore[i],y= Weekly_CreditScore[j],use = "pairwise.complete.obs")
    
    
    IntrAmtandNumber[i,j]=cor(x= No_Claim[i],y= ClaimAmt[j],use = "pairwise.complete.obs")
    IntrPrecipandAmt[i,j]=cor(x= precip_subset[i],y= ClaimAmt[j],use = "pairwise.complete.obs")
    IntrPrecipandNumber[i,j]=cor(x= precip_subset[i],y= No_Claim[j],use = "pairwise.complete.obs")
    IntrPrecipandHouse[i,j] = cor(x= precip_subset[i],y=Weekly_HouseAge[j],use = "pairwise.complete.obs")
    IntrPrecipandCredit[i,j] = cor(x= precip_subset[i],y=Weekly_CreditScore[j],use = "pairwise.complete.obs")
    IntrAmtandHouse[i,j] = cor(x= ClaimAmt[i],y=Weekly_HouseAge[j],use = "pairwise.complete.obs")
    IntrAmtandCredit[i,j] = cor(x= ClaimAmt[i],y=Weekly_CreditScore[j],use = "pairwise.complete.obs")
    IntrNumberandHouse[i,j] = cor(x= No_Claim[i],y=Weekly_HouseAge[j],use = "pairwise.complete.obs")
    IntrNumberandCredit[i,j] = cor(x= No_Claim[i],y=Weekly_CreditScore[j],use = "pairwise.complete.obs")
    IntrHouseandCredit[i,j] = cor(x= Weekly_HouseAge[i],y=Weekly_CreditScore[j],use = "pairwise.complete.obs")
    
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


library(psych)

precip_dist <- cor2dist(precipitation)
claimamount_dist <- cor2dist(claimAmount)
ClaimNumber_dist <- cor2dist(ClaimNumber)
HouseAge_dist <- cor2dist(HouseAge_corr)
CreditScore_dist <- cor2dist(CreditScore_corr)
IntrAmtandNumber_dist <- cor2dist(IntrAmtandNumber)
IntrPrecipandAmt_dist<- cor2dist(IntrPrecipandAmt)
IntrPrecipandNumber_dist <- cor2dist(IntrPrecipandNumber)
IntrPrecipandHouse_dist <- cor2dist(IntrPrecipandHouse)
IntrPrecipandCredit_dist <- cor2dist(IntrPrecipandCredit)
IntrAmtandHouse_dist <- cor2dist(IntrAmtandHouse)
IntrAmtandCredit_dist <- cor2dist(IntrAmtandCredit)
IntrNumberandHouse_dist <- cor2dist(IntrNumberandHouse)
IntrNumberandCredit_dist <- cor2dist(IntrNumberandCredit)
IntrHouseandCredit_dist <- cor2dist(IntrHouseandCredit)







#precipitation_abs <- abs(precipitation)
#claimAmount_abs <- abs(claimAmount)
#ClaimNumber_abs <- abs(ClaimNumber)
#HouseAge_corrabs <- abs(HouseAge_corr)
#CreditScore_corrabs <- abs(CreditScore_corr)
#IntrAmtandNumber_abs <- abs(IntrAmtandNumber)
#IntrPrecipandAmt_abs <- abs(IntrPrecipandAmt)
#IntrPrecipandNumber_abs <- abs(IntrPrecipandNumber)
#IntrPrecipandHouse_abs <- abs(IntrPrecipandHouse)
#IntrPrecipandCredit_abs <- abs(IntrPrecipandCredit)
#IntrAmtandHouse_abs <- abs(IntrAmtandHouse)
#IntrAmtandCredit_abs <- abs(IntrAmtandCredit)
#IntrNumberandHouse_abs <- abs(IntrNumberandHouse)
#IntrNumberandCredit_abs <- abs(IntrNumberandCredit)
#IntrHouseandCredit_abs <- abs(IntrHouseandCredit)

####################Write them out#############################################
write.csv(precipitation,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\Precipitationnew.csv")
write.csv(claimAmount,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClaimsAmountnew.csv")
write.csv(ClaimNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClaimsNumbernew.csv")
write.csv(HouseAge_corr,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HouseAgenew.csv")
write.csv(CreditScore_corr,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\CreditScorenew.csv")

write.csv(IntrAmtandNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandNumbernew.csv")
write.csv(IntrPrecipandAmt,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandAmtnew.csv")
write.csv(IntrPrecipandNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandNumbernew.csv")
write.csv(IntrPrecipandHouse,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandHousenew.csv")
write.csv(IntrPrecipandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandCreditnew.csv")
write.csv(IntrAmtandHouse,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandHousenew.csv")
write.csv(IntrAmtandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandCreditnew.csv")
write.csv(IntrNumberandHouse,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\NumberandHousenew.csv")
write.csv(IntrNumberandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\NumberandCreditnew.csv")
write.csv(IntrHouseandCredit,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HouseandCreditnew.csv")

write.csv(precip_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\Precipitation.csv")
write.csv(claimamount_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClaimsAmount.csv")
write.csv(ClaimNumber_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\ClaimsNumber.csv")
write.csv(HouseAge_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HouseAge.csv")
write.csv(CreditScore_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\CreditScore.csv")

write.csv(IntrAmtandNumber_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandNumber.csv")
write.csv(IntrPrecipandAmt_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandAmt.csv")
write.csv(IntrPrecipandNumber_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandNumber.csv")
write.csv(IntrPrecipandHouse_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandHouse.csv")
write.csv(IntrPrecipandCredit_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\PrecipandCredit.csv")
write.csv(IntrAmtandHouse_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandHouse.csv")
write.csv(IntrAmtandCredit_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\AmtandCredit.csv")
write.csv(IntrNumberandHouse_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\NumberandHouse.csv")
write.csv(IntrNumberandCredit_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\NumberandCredit.csv")
write.csv(IntrHouseandCredit_dist,"C:\\Users\\moniy\\Desktop\\GelLab\\Climate Network\\Codes\\MATLAB Codes\\HouseandCredit.csv")








######### Construct All connected weighted graph #############################

g.NumberClaims<- graph.adjacency(CorNoClaims, mode="undirected",weighted=TRUE,diag=FALSE)
node<-V(g.NumberClaims)
# 505
edge<-E(g.NumberClaims);edge
#7276
mean(degree(g.NumberClaims)) #29


g.ClaimAmount<- graph.adjacency(CorClaims, mode="undirected",weighted=TRUE,diag=FALSE)
node<-V(g.ClaimAmount);node
# 505
edge<-E(g.ClaimAmount);edge
#13648
mean(degree(g.ClaimAmount)) #54


g.Precip<- graph.adjacency(CorPrecip, mode="undirected",weighted=TRUE,diag=FALSE)
node<-V(g.Precip);node
# 505
edge<-E(g.Precip);edge
#15694
mean(degree(g.Precip)) #62

g.HouseAge<- graph.adjacency(CorHouseAge, mode="undirected",weighted=TRUE,diag=FALSE)
node<-V(g.HouseAge);node
# 505
edge<-E(g.HouseAge);edge
#5212
mean(degree(g.HouseAge)) #20

g.CreditScore<- graph.adjacency(CorCreditScore, mode="undirected",weighted=TRUE,diag=FALSE)
node<-V(g.CreditScore);node
# 505
edge<-E(g.CreditScore);edge
#11839
mean(degree(g.CreditScore)) #47

###################### Write Out Edgelists ####################################################

edge_claims<-cbind(get.edgelist(g.NumberClaims) , round(E(g.NumberClaims)$weight,5))
write.csv(edge_W,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NumberClaims_EdgeList_NoAbsolute.csv")


edge_ClaimAmount<-cbind(get.edgelist(g.ClaimAmount) , round(E(g.ClaimAmount)$weight,5))
write.csv(edge_W,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimsAmt_EdgeList_Noabsolute.csv")

edge_Precip<-cbind(get.edgelist(g.Precip) , round(E(g.Precip)$weight,5))
write.csv(edge_W,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\Precip_EdgeList_Noabsolute.csv")

edge_HouseAge<-cbind(get.edgelist(g.HouseAge) , round(E(g.HouseAge)$weight,5))
write.csv(edge_W,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\HouseAge_EdgeList_Noabsolute.csv")


edge_CreditScore<-cbind(get.edgelist(g.CreditScore) , round(E(g.CreditScore)$weight,5))

node_W<-vertex_attr(g.Loss)
write.csv(node_W,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NodeList.csv")

nodes <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NodeList.csv")
######################### Write Out Adjacency Matrices ##########################################

write.csv(CorNoClaims,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NumberClaims_CorrMatrix.csv")
write.csv(CorClaims,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimsAmount_CorrMatrix.csv")
write.csv(CorPrecip,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\Precip_CorrMatrix.csv")

write.csv(CorCreditScore,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\CreditScore_CorrMatrix.csv")
write.csv(CorHouseAge,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\HouseAge_CorrMatrix.csv")

write.csv( IntrAmtandNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NumberClaims&Amt_CorrMatrix.csv")
write.csv(IntrPrecipandAmt,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimsAmount&precip_CorrMatrix.csv")
write.csv(IntrPrecipandNumber,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\Precip&ClaimNumber_CorrMatrix.csv")

CorNoClaims <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\NumberClaims_AdjMatrix.csv")
CorClaims <- read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimsAmount_AdjMatrix.csv")
CorPrecip <-read.csv("C:\\Users\\moniy\\Desktop\\GelLab\\Data\\Precip_AdjMatrix.csv")

#################################################################################################
#                             Building the Multigraph
#################################################################################################

d <- list(CorNoClaims, CorClaims,CorPrecip)
StackedMultiGraph <- abind(d, along = 3)

multigraph(StackedMultiGraph)

###################################################################################################
#                               Multivariate Multiple Regression
###################################################################################################

library(ecodist)


fit2 <- MRM(as.dist(CorPrecip) ~ as.dist(CorClaims) + as.dist(CorNoClaims),nperm =999,mrank=FALSE)


fit3 <- MRM(as.dist(CorPrecip) ~ as.dist(CorClaims) + as.dist(CorHouseAge) + as.dist(CorCreditScore)
            + as.dist(CorNoClaims),nperm =999,mrank=FALSE)

fit5 <- MRM(as.dist((1-cor(ClaimAmount_Matrix))/2) ~ as.dist((1 - cor(Precip_Matrix))/2) + 
              as.dist((1 - cor(HouseAge))/2) ,nperm = 999,mrank=T)

#####################################Check edgelists###################################################
edges_CreditScorev1 <- merge.data.frame(edge_CreditScore,nodes,by.x = 'V1',by.y='X')
edges_CreditScorev2 <- merge.data.frame(edges_CreditScorev1,nodes,by.x = 'V2',by.y='X')
edges_CreditScorev2 <- edges_CreditScorev2[,3:5]
edges_CreditScorev2 <- edges_CreditScorev2[,c(2,3,1)]
colnames(edges_CreditScorev2) <- c("V1","V2","V3") 
is.na(edges_CreditScorev2$V3) <- 0

edges_claimsv1 <- merge.data.frame(edge_claims,nodes,by.x = 'V1',by.y='X')
edges_claimsv2 <- merge.data.frame(edges_claimsv1,nodes,by.x = 'V2',by.y='X')
edges_claimsv2 <- edges_claimsv2[,3:5]
edges_claimsv2 <- edges_claimsv2[,c(2,3,1)]
colnames(edges_claimsv2) <- c("V1","V2","V3") 

edges_precipv1 <- merge.data.frame(edge_Precip,nodes,by.x = 'V1',by.y='X')
edges_precipv2 <- merge.data.frame(edges_precipv1,nodes,by.x = 'V2',by.y='X')
edges_precipv2 <- edges_precipv2[,3:5]
edges_precipv2 <- edges_precipv2[,c(2,3,1)]
colnames(edges_precipv2) <- c("V1","V2","V3") 


edges_ClaimAmountv2 <- as.data.frame(edge_ClaimAmount)
edges_HouseAge <- as.data.frame(edge_HouseAge)


edgelist <- rbind(edges_claimsv2,edges_ClaimAmountv2,edges_precipv2,edges_HouseAge,edges_CreditScorev2)
write.csv(edgelist,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\EdgeList_NoAbsolute_combinedv1.csv")

write.csv(edges_claimsv2,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimsEdgeList_NoAbsolute_combinedv1.csv")
write.csv(edges_ClaimAmountv2,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\ClaimAmountEdgeList_NoAbsolute_combinedv1.csv")
write.csv(edges_precipv2,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\PrecipEdgeList_NoAbsolute_combinedv1.csv")
write.csv(edges_HouseAge,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\HouseAgeEdgeList_NoAbsolute_combinedv1.csv")
write.csv(edges_CreditScorev2,"C:\\Users\\moniy\\Desktop\\GelLab\\Data\\CreditScoreEdgeList_NoAbsolute_combinedv1.csv")



######################Analysis of Clusters###############
clustering1 <- read.csv("C:\Users\moniy\Desktop\GelLab\Results")

rm(list=ls())

check <- as.data.frame(check)
colnames(check)[3] <- "FSA"
join_df <- merge.data.frame(x=Insurance_v2,y=check,by = "FSA")

avg_tda <- aggregate(cbind(TotAmtOfClaims,TotNumberOfClaims)~ TDA,data=join_df,mean)
avg_hier <- aggregate(cbind(TotAmtOfClaims,TotNumberOfClaims)~Aggomerative,data=join_df,FUN="mean")

house_means <- colMeans(HouseAge[,-1],na.rm=T)
houseage_df <- as.data.frame(cbind(colnames(HouseAge)[-1],house_means))
colnames(houseage_df)[1] <- "FSA"

credit_means <- colMeans(CreditScore[,-1],na.rm=T)
credit_df <- as.data.frame(cbind(colnames(CreditScore)[-1],credit_means))
colnames(credit_df)[1] <- "FSA"

precip_means <- colMeans(precip_subset[,-1],na.rm=T)
precip_df <- as.data.frame(cbind(colnames(precip_subset)[-1],precip_means))
colnames(precip_df)[1] <- "FSA"

fsa_clusters <- aggregate(cbind(Aggomerative,TDA)~FSA,data=join_df,mean)

join_df1 <- merge.data.frame(x= credit_df,y=houseage_df,by ="FSA")
join_df2 <- merge.data.frame(x=join_df1,y=precip_df,by="FSA")
join_df3 <- merge.data.frame(x=join_df2,y=fsa_clusters,by="FSA")


avg_tda1 <- aggregate(cbind(credit_means,house_means,precip_means)~ TDA,data=join_df3,mean)
avg_heir1 <- aggregate(cbind(credit_means,house_means,precip_means)~ Aggomerative,data=join_df3,mean)


join_res1 <- merge.data.frame(x= avg_heir1,y=avg_hier,by ="Aggomerative")
join_res2 <- merge.data.frame(x=avg_tda1,y=avg_tda,by="TDA")

avgs <- colMeans(as.numeric(join_df3[,-1]))

write.csv(join_res1,"C:\\Users\\moniy\\Desktop\\ProfileHierClusters.csv")
write.csv(join_res2,"C:\\Users\\moniy\\Desktop\\ProfileTDAClusters.csv")
write.csv(join_df3,"C:\\Users\\moniy\\Desktop\\AvgDataperNode.csv")
write.csv(join_df,"C:\\Users\\moniy\\Desktop\\MoreAvgdataperNode.csv")

 
plot_data <- read.csv("C:\\Users\\moniy\\Desktop\\plotdata.csv")

par(mforw=c(3,2))

hist(plot_data$colSums.No_Claim.,probability = T)
hist(plot_data$colSums.ClaimAmt.,probability = T)
hist(plot_data$colSums.Weekly_HouseAge.,probability = T)
hist(plot_data$colSums.Weekly_CreditScore.,probability = T)
hist(plot_data$colSums.precip_subset.)

qplot((plot_data$colSums.ClaimAmt)/520,
      geom="histogram",
      binwidth=500,
      xlab = "Total Claim Amount",
      ylab = "Frequency",
      fill = I("blue"))

