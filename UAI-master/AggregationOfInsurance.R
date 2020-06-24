###############################################################################
##     Title : Code to aggregate insurabce data for analysis of clusters
##                     Author : Monisha Yuvaraj
##                      Date: 18th July 2018
###############################################################################

library(plyr)
library(tibble)

#########################Observations on uncleaned data#######################
## 1. DOLYYY not useful
## 2. Dwelling type = 74.8% missing values
## 3. marital status client 61.7% missing values
## 4. mitigation has 98.4% missing values
## 5. nbrcl_sbu = 97.33% missing
## 6. nbrcl_water = 97.33% missing
## 7. nbrcl_wind = 97.33% missing
## 8. nbrcl_hail = 97.33% missing
## 9. NBRCL columns are highly correlated to the inc i.e, claim amount column
###############################################################################


###########################################################################
##                      Missing Value treatment                          ##
###########################################################################
ONsouth$AGE_OF_CLIENT[ONsouth$AGE_OF_CLIENT== 999] <- NA
ONsouth$AGE_OF_CLIENT[ONsouth$AGE_OF_CLIENT < 0] <- NA

ONsouth$AGE_OF_DWELLING[ONsouth$AGE_OF_DWELLING == 9999] <- NA
ONsouth$AGE_OF_DWELLING[ONsouth$AGE_OF_DWELLING < 0] <- NA

ONsouth$CREDIT_SCORE[ONsouth$CREDIT_SCORE == "NRQ"] <- NA
ONsouth$CREDIT_SCORE[ONsouth$CREDIT_SCORE == "000"] <- NA

ONsouth$ROOF_MATERIAL[ONsouth$ROOF_MATERIAL == "UNKN"] <- NA
ONsouth$ROOF_MATERIAL[ONsouth$ROOF_MATERIAL == ""] <- NA

ONsouth$CREDIT_SCORE_POSTAL_CODE_BASED[ONsouth$CREDIT_SCORE_POSTAL_CODE_BASED == "."] <- NA

input$NBRCL_HAIL[input$NBRCL_HAIL == "."] <- NA
input$NBRCL_SBU[input$NBRCL_SBU == "."] <- NA
input$NBRCL_WATER[input$NBRCL_WATER == "."] <- NA
input$NBRCL_WIND[input$NBRCL_WIND == "."] <- NA

input$NBRCL_HAIL <- as.numeric(levels(input$NBRCL_HAIL))[as.integer(input$NBRCL_HAIL)]
input$NBRCL_SBU<- as.numeric(levels(input$NBRCL_SBU))[as.integer(input$NBRCL_SBU)]
input$NBRCL_WATER <- as.numeric(levels(input$NBRCL_WATER))[as.integer(input$NBRCL_WATER)]
input$NBRCL_WIND <- as.numeric(levels(input$NBRCL_WIND))[as.integer(input$NBRCL_WIND)]

input$Total_loss_amt <- input$INC_HAIL+ input$INC_SBU+ input$INC_WATER + input$INC_WIND
input$Total_loss_numbers <- input$NBRCL_WIND+ input$NBRCL_SBU+ input$NBRCL_WATER + input$NBRCL_HAIL

############################################################################
##                  Aggregate to FSA Code Level                           ##
############################################################################

##count number of unique values in factor type columns
count_df<- aggregate(cbind(ROOF_MATERIAL,AUXLRY_HEATING_SYSTEM_CD,PRIMARY_HEATING_SYSTEM_CD)~Cluster.Labels,
                     input, function(x) length(unique(x)))

#change data type for column as its a integer column stored as factor
input$CREDIT_SCORE_POSTAL_CODE_BASED <- as.numeric(as.character(input$CREDIT_SCORE_POSTAL_CODE_BASED))

#add vakues for numeric columns that can later be average when cluster level results are needed 
mean_df <- aggregate(cbind(Total_loss_amt,Total_loss_numbers,INC_HAIL,INC_SBU,INC_WATER,INC_WIND,AGE_OF_DWELLING,CREDIT_SCORE_POSTAL_CODE_BASED,BUILDING_LMT_OF_INS_AMT,Elevation)~Cluster.Labels,input, mean)


#calculate proportions of factor level data per FSA code
prop_roof <- rownames_to_column(as.data.frame.matrix(table(input$Cluster.Labels,input$ROOF_MATERIAL)),"Cluster.Labels")
prop_aux <- rownames_to_column(as.data.frame.matrix(table(input$Cluster.Labels,input$AUXLRY_HEATING_SYSTEM_CD)),"Cluster.Labels")
prop_pri <- rownames_to_column(as.data.frame.matrix(table(input$Cluster.Labels,input$PRIMARY_HEATING_SYSTEM_CD)),"Cluster.Labels")


##################################################################################
##                   Merge all the above datasets                               ##
##################################################################################

roof_aux <- merge.data.frame(x=prop_aux,y=prop_pri,by="Cluster.Labels",suffixes = c(".aux_heat",".pri_heat"))
roof_aux_pri <- merge.data.frame(x=roof_aux,y=prop_roof,by="Cluster.Labels")
prop <- subset(roof_aux_pri, rowSums(roof_aux_pri[,2:43])>0 )

sum_prop <- merge.data.frame(x= mean_df,y=prop,by="Cluster.Labels",all.x=TRUE)
final<- merge.data.frame(x=sum_prop,y=count_df,by="Cluster.Labels",all.x = TRUE)


write.csv(file="AggregatedCluster_V3.csv",x=final)

