library(plyr)
library(dplyr)

sampleList <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/sampleList.csv')%>%
  select(-X)%>%
  plyr::rename(c('BCGAttribute'='attLevel'))

#----------------------------------Taxa metric functions---------------------------------------------------------
# Number of Total Taxa
totTax <- function(sample){nrow(unique(sample))}
# Number of Total Individuals (for later calculations)
totInd <- function(sample){sum(sample$Count)}
# Number of Attribute I&II Taxa
T_12 <- function(sample){sum(sample$attLevel %in% c(1,2))}
# % Attribute 1,2,&3 Taxa
P_123 <- function(sample){(sum(sample$attLevel %in% c(1,2,3))/totTax(sample))*100}
# % Attribute 1,2,&3 Individuals
PI_123 <- function(sample){
  df <- filter(sample,attLevel %in% c(1,2,3))
  (sum(df$Count)/totInd(sample))*100}
# % Atribute 5 Individuals
PI_5 <- function(sample){
  df <- filter(sample,attLevel %in% c(5))
  (sum(df$Count)/totInd(sample))*100}
# % Atribute 5&6t Individuals
PI_56t <- function(sample){
  df <- filter(sample,attLevel %in% c('5','6t'))
  (sum(df$Count)/totInd(sample))*100}
# Number of Attribute 6 Taxa
T_6 <- function(sample){sum(sample$attLevel %in% c('6i','6m','6t'))}
# Number of Attribute 6t Taxa
T_6t <- function(sample){sum(sample$attLevel %in% c('6t'))}
# Number of Darter Taxa
T_darter <- function(sample){sum(filter(sample, grepl('darter',CommonName))$Count)}
# Number of native Brook Trout individuals
N_bTrout <- function(sample){sum(filter(sample, grepl('brook trout',CommonName))$Count)}
# % Most Dominant Attribute 5&6t individuals
P_Dom56t <- function(sample){(max(filter(sample,attLevel %in% c('5','6t'))$Count)/totInd(sample))*100}
# Number of Attribute 1,2,&3 taxa
T_123 <- function(sample){sum(sample$attLevel %in% c(1,2,3))}
# % Attribute 5&6 taxa
P_56 <- function(sample){(sum(sample$attLevel %in% c('5','6i','6m','6t'))/totTax(sample))*100}
## ---------------------------------------------------------------------------------------------------------
# Master metric that outputs dataframe of all results
masterMetric <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),T_12=T_12(sample),P_123=P_123(sample)
             ,PI_123=PI_123(sample),PI_5=PI_5(sample),PI_56t=PI_56t(sample),T_6=T_6(sample)
             ,T_6t=T_6t(sample),T_darter=T_darter(sample),N_bTrout=N_bTrout(sample),P_Dom56t=P_Dom56t(sample)
             ,T_123=T_123(sample),P_56=P_56(sample))}

# BCG Fuzzy Membership logic
fuzzyMembership <- function(metric,low,high){metric/(high-low)-low/(high-low)}

fmFinal <- function(x){
  if(x>=1){return(1)}else(y=x)
  if(y<=0){return(0)}else(return(x))
}
## ---------------------------- Level Rules ---------------------------------------------------------------------
# BCG level logic, Other Medium/Large
otherMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,10,20)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,55,65)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,3)))}
otherMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,9,19)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)))}
otherMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,7,15)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,75,85)))}
otherMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,16,26)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)))}

# BCG level logic, Other Small
otherSmall_BCGlevel2alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,16)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,10,20)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,55,65)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,3))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherSmall_BCGlevel2alt2 <- function(test){
  data.frame(fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,10,20))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,3))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)),fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
otherSmall_BCGlevel3alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,13)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,5,15)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)),fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,1,4)))}
otherSmall_BCGlevel3alt2 <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,5,15))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)),fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
otherSmall_BCGlevel4alt1 <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,80,90)))}
otherSmall_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,13,23)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,1,4)))}
otherSmall_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)))}

# BCG level logic, Above Falls Medium/Large
aboveFallsMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,15)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,2,5))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,14)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,15,25))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,35,45))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)))}

# BCG level logic, starting at Above Falls Small
aboveFallsSmall_BCGlevel2alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,8,12)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,2,5))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsSmall_BCGlevel2alt2 <- function(test){
  data.frame(fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,2,5)),fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1))
             ,fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
aboveFallsSmall_BCGlevel3alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,11)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,50,60))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsSmall_BCGlevel3alt2 <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,50,60)),fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4))
             ,fm_N_bTrout=fmFinal(fuzzyMembership(test$N_bTrout,0,3)))}
aboveFallsSmall_BCGlevel4alt1 <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)))}
aboveFallsSmall_BCGlevel4alt2 <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)))}
aboveFallsSmall_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)))}




### Other Medium/Large BCG Model
OtherMedLargeModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(otherMedLarge_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(otherMedLarge_BCGlevel4alt1(metricResults)),min(otherMedLarge_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=min(otherMedLarge_BCGlevel3(metricResults))
                            ,BCGlevel2=min(otherMedLarge_BCGlevel2(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}


### Other Small BCG Model
OtherSmallModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(otherSmall_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(otherSmall_BCGlevel4alt1(metricResults)),min(otherSmall_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=max(min(otherSmall_BCGlevel3alt1(metricResults)),min(otherSmall_BCGlevel3alt2(metricResults)))
                            ,BCGlevel2=max(min(otherSmall_BCGlevel2alt1(metricResults)),min(otherSmall_BCGlevel2alt2(metricResults))))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}

### AboveFalls Medium/Large BCG Model
AboveFallsMedLargeModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsMedLarge_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(aboveFallsMedLarge_BCGlevel4alt1(metricResults)),min(aboveFallsMedLarge_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=min(aboveFallsMedLarge_BCGlevel3(metricResults))
                            ,BCGlevel2=min(aboveFallsMedLarge_BCGlevel2(metricResults)))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}

### Above Falls Small BCG Model
AboveFallsSmallModel <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsSmall_BCGlevel5(metricResults))
                            ,BCGlevel4=max(min(aboveFallsSmall_BCGlevel4alt1(metricResults)),min(aboveFallsSmall_BCGlevel4alt2(metricResults)))
                            ,BCGlevel3=max(min(aboveFallsSmall_BCGlevel3alt1(metricResults)),min(aboveFallsSmall_BCGlevel3alt2(metricResults)))
                            ,BCGlevel2=max(min(aboveFallsSmall_BCGlevel2alt1(metricResults)),min(aboveFallsSmall_BCGlevel2alt2(metricResults))))%>%
    mutate(Level1=0,Level2=min(1-Level1,BCGlevel2),Level3=min(1-sum(Level1,Level2),BCGlevel3)
           ,Level4=min(1-sum(Level1,Level2,Level3),BCGlevel4)
           ,Level5=min(1-sum(Level1,Level2,Level3,Level4),BCGlevel5)
           ,Level6=1-sum(Level1,Level2,Level3,Level4,Level5))
  placehold<-sort(levelresult[6:11],TRUE)[2] # pick second highest BCG level, can't be done in mutate statement
  final <- data.frame(SampleName=metricResults[1]
                      ,nominalTier=colnames(levelresult[,6:11])[apply(levelresult[6:11],1,which.max)]
                      ,nominalMembership=apply(levelresult[,6:11],1,max)
                      ,secondMembership=placehold[1,]
                      ,runnerupTier=ifelse(placehold[1,]==0,"",colnames(placehold)))%>%
    mutate(close=ifelse(nominalMembership-secondMembership<0.1,"tie"
                        ,ifelse(nominalMembership-secondMembership<0.2,"yes","")))
  return(final)
}

#-------------------------------------------------------------------------------------------------------
### Put actual data in
splits <- split(sampleList,sampleList$SampleName,drop=T)%>% #Split dataframe into multiple df's in a list
  lapply(function(x) x[!(names(x) %in% c('SampleName'))])

options(digits = 3)

result <- data.frame(matrix(NA, ncol = 7, nrow = 1))
names(result) <- c('SampleName','nominalTier','nominalMembership','secondMembership','runnerupTier'
                   ,'close','Model')

for(i in 1:length(splits)){
  print(i)
  samplename <- names(splits)[i]
  sampleathand <- data.frame(splits[[i]])
  if(splits[[i]]$SubBasin[1] %in% c('New','UNew','MNew','Green')){
    if(splits[[i]]$Catchment[1] < 10){
      result<- rbind(result,cbind(AboveFallsSmallModel(samplename,sampleathand),Model='AboveFalls,Small'))
    }else{
      result<- rbind(result,cbind(AboveFallsMedLargeModel(samplename,sampleathand),Model='AboveFalls,MediumLarge'))
    }
  }else{
    if(splits[[i]]$Catchment[1] < 10){
      result<- rbind(result,cbind(OtherSmallModel(samplename,sampleathand),Model='Other,Small'))
    }else{
      result<- rbind(result,cbind(OtherMedLargeModel(samplename,sampleathand),Model='Other,MediumLarge'))
    }
  }
  result<-filter(result,!(SampleName=='NA'))
}




# Bring in real results
realresults <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/AboveOtherModelResults.csv')%>%
  plyr::rename(c('ExerciseID'='SampleName'))

realresults$SampleName <- as.character(realresults$SampleName)

realresults<- as.data.frame(sapply(realresults, function(x) gsub('Samp0','Sample',x)))

together3 <- join(result,realresults,by='SampleName')

#write.csv(together,'together.csv')




# --------------------------------- DIFFERENT TEST DATASET ------------------------------------
# run Lou's hw1 taxa list through code
library(readxl)
# Step1 in excel, take out all spaces in column names
hw1 <- read_excel('C:/HardDriveBackup/R/BCG/Lou/abovefalls_hw1.xlsx',sheet='Sheet1')%>%
  select(c(ExerciseID,Basin,DrArea_mi2,CommonName,ScientificName,Count,BCGAttribute)) %>% #get just the variables I need
  plyr::rename(c("ExerciseID"="SampleName","DrArea_mi2"="Catchment"
                 ,"BCGAttribute"="attLevel")) %>% #rename to script variables 
  mutate(SubBasin=revalue(Basin,c('Upper New'='UNew','Middle New'='MNew','Lower New'='LNew'
                                      ,'Greenbrier'='Green')))
  


#louHW1 <- split(hw1,hw1$SampleName,drop=T)%>% #Split dataframe into multiple df's in a list
 # lapply(function(x) x[!(names(x) %in% c('SampleName'))])


testfunction <- function(dfInCorrectFormat){
  splits <-split(dfInCorrectFormat,dfInCorrectFormat$SampleName,drop=T)%>% #Split dataframe into multiple df's in a list
    lapply(function(x) x[!(names(x) %in% c('SampleName'))])
  
  result <- data.frame(matrix(NA, ncol = 7, nrow = 1))
  names(result) <- c('SampleName','nominalTier','nominalMembership','secondMembership','runnerupTier'
                     ,'close','Model')
  
  for(i in 1:length(splits)){
    print(i)
    samplename <- names(splits)[i]
    sampleathand <- data.frame(splits[[i]])
    if(splits[[i]]$SubBasin[1] %in% c('New','UNew','MNew','LNew','Green')){
      if(splits[[i]]$Catchment[1] < 10){
        result<- rbind(result,cbind(AboveFallsSmallModel(samplename,sampleathand),Model='AboveFalls,Small'))
      }else{
        result<- rbind(result,cbind(AboveFallsMedLargeModel(samplename,sampleathand),Model='AboveFalls,MediumLarge'))
      }
    }else{
      if(splits[[i]]$Catchment[1] < 10){
        result<- rbind(result,cbind(OtherSmallModel(samplename,sampleathand),Model='Other,Small'))
      }else{
        result<- rbind(result,cbind(OtherMedLargeModel(samplename,sampleathand),Model='Other,MediumLarge'))
      }
    }
    result<-filter(result,!(SampleName=='NA'))
  }
  
  return(result)
  
}


result <- testfunction(hw1)

result<- as.data.frame(sapply(result, function(x) gsub('Samp0','Sample',x)))

together <- join(result,realresults,by='SampleName')



# for later playing with making a function to detect column class
hw1<- as.data.frame(sapply(hw1, function(x) gsub('Samp0','Sample',x)))
hw1$SampleName <- as.character(hw1$SampleName)

hw2 <- hw1 %>% mutate_each(funs(type.convert(as.character(.))))

