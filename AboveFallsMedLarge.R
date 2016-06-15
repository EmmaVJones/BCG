library(plyr)
library(dplyr)
library(readxl)



# First run dataManagement.R to get taxa lists


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

# Master metric that outputs dataframe of all results
masterMetric <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),T_12(sample),P_123(sample),PI_123(sample),PI_5(sample),PI_56t(sample)
             ,T_6(sample),T_6t(sample),T_darter(sample),N_bTrout(sample),P_Dom56t(sample),T_123(sample),P_56(sample))}

# BCG Fuzzy Membership logic
fuzzyMembership <- function(metric,low,high){metric/(high-low)-low/(high-low)}

fmFinal <- function(x){
  if(x>=1){return(1)}else(y=x)
  if(y<=0){return(0)}else(return(x))
}

# BCG level logic, starting at Other Medium/Large
aboveFallsMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,15)),fm_T_12=fmFinal(fuzzyMembership(test$T_12.sample.,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123.sample.,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5.sample.,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6.sample.,2,5))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t.sample.,0,1)))}

aboveFallsMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,14)),fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,15,25))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123.sample.,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5.sample.,35,45))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t.sample.,1,4)))}

aboveFallsMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_T_123=fmFinal(fuzzyMembership(test$T_123.sample.,1,2))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,65,75)))}
aboveFallsMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,3,7))
             ,fm_P_56=fmFinal(1-fuzzyMembership(test$P_56.sample.,60,70)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,65,75)))}
       
aboveFallsMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)))}


# Other Medium/Large BCG Model, will need logic prior to this step to route user to this model/function
#  based on watershed size, basin parameters
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

AboveFallsMedLargeResults <- rbind(AboveFallsMedLargeModel("Sample062",samp062_att),AboveFallsMedLargeModel("Sample049",samp049_att)
                              ,AboveFallsMedLargeModel("Sample048",samp048_att),AboveFallsMedLargeModel("Sample047",samp047_att)
                              ,AboveFallsMedLargeModel("Sample192",samp192_att))


## for testing purposes, to make att list identical to excel
samp192_att <- mutate(samp192,attLevel=BCGAttribute)%>%
  select(-c(Family,BCGAttribute))
