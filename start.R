## This script:
#   - attaches taxa lists together (early versions)
#   - calculates taxa metrics relevant to BCG levels
#   - calculates fuzzy membership to arrive at a site's BCG level(s)
# Created by Emma Jones
# Last updated 06/13/2016

library(plyr)
library(dplyr)
library(readxl)

#Bring in Taxa lists (Lou and Jason versions) & test taxa list (sample212)
taxaAttL <- read_excel('C:/HardDriveBackup/R/BCG/Lou/emma_example_TaxaAttributeList_EVJ.xlsx',sheet='Lou')%>%
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
taxaAttJ <- read_excel('C:/HardDriveBackup/R/BCG/Lou/emma_example_TaxaAttributeList_EVJ.xlsx',sheet='Jason')%>%
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))




#Bring in test taxa list (sample 212)
samp212 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/Sample212taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))

# Combine tables
taxaAttL <- select(taxaAttL,c(CommonName,ScientificName,Gauley))
taxaAtt <- join(taxaAttJ,taxaAttL,by=c('CommonName','ScientificName'))

# Link Sample Common Name & Scientific Name to BCG Attribute (depending on Basin, which is just a taxa list version right now)
samp212_att <- join(samp212,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp212_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions

fix(samp212_att) #for testing purposes, changed telescope shiner to 6m

#-------------------------------------------------------------------------------------------------------------
## Taxa metric functions for all levels
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
T_56 <- function(sample){(sum(sample$attLevel %in% c('5','6i','6m','6t'))/totTax(sample))*100}


# Test area
totTax(samp212_att)
T_12(samp212_att)
P_123(samp212_att)
PI_123(samp212_att)
PI_5(samp212_att)#for later
PI_56t(samp212_att)
T_6(samp212_att) 
T_6t(samp212_att) # for later
T_darter(samp212_att) #for later
N_bTrout(samp212_att) # for later
P_Dom56t(samp212_att) # for later
T_123(samp212_att) # for later
T_56(samp212_att) # for later
#------------------------------------------------------------------------------------------------------


# Master metric that outputs dataframe of all results
masterMetric <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),T_12(sample),P_123(sample),PI_123(sample),PI_5(sample),PI_56t(sample)
             ,T_6(sample),T_6t(sample),T_darter(sample),N_bTrout(sample),P_Dom56t(sample),T_123(sample),T_56(sample))}
test <- masterMetric('Sample212',samp212_att)

# BCG Fuzzy Membership logic, need 2 functions bc 1- situation?
fuzzyMembership <- function(metric,low,high){metric/(high-low)-low/(high-low)}
  
fmFinal <- function(x){
  if(x>=1){return(1)}else(y=x)
  if(y<=0){return(0)}else(return(x))
}

  
# BCG level logic, starting at Other Medium/Large
otherMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,10,20)),fm_T_12=fmFinal(fuzzyMembership(test$T_12.sample.,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123.sample.,15,20))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,55,65)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6.sample.,1,3)))}
otherMedLarge_BCGlevel2(test)
min(otherMedLarge_BCGlevel2(test))

otherMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,9,19)),fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123.sample.,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,65,75))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t.sample.,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6.sample.,1,5)))}
otherMedLarge_BCGlevel3(test)
min(otherMedLarge_BCGlevel3(test))

otherMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,7,15)),fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,0,1))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t.sample.,75,85)))}
otherMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,16,26)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t.sample.,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter.sample.,2,5)))}
otherMedLarge_BCGlevel4alt1(test)
otherMedLarge_BCGlevel4alt2(test)
max(min(otherMedLarge_BCGlevel4alt1(test)),min(otherMedLarge_BCGlevel4alt2(test)))

otherMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)))}
otherMedLarge_BCGlevel5(test)
min(otherMedLarge_BCGlevel5(test))


# Try to write something that flows through above steps (functions) to go from level 5 -> level 2 
allTogetherNow <- function(taxaListFromOneSite){
  metricResults <- masterMetric('Sample212',samp212_att)
  X1 <- metricResults[1]
  if(min(otherMedLarge_BCGlevel5(metricResults))==1){
    print(paste('Level 5 Membership: ',min(otherMedLarge_BCGlevel5(metricResults)),sep=' '))
    if(max(min(otherMedLarge_BCGlevel4alt1(metricResults)),min(otherMedLarge_BCGlevel4alt2(metricResults)))==1){
      print(paste('Level 4 Membership:',max(min(otherMedLarge_BCGlevel4alt1(metricResults)),min(otherMedLarge_BCGlevel4alt2(metricResults))),sep=' '))
      if(min(otherMedLarge_BCGlevel3(metricResults))>=0.5){
        print(paste('Level 3 Membership: ', min(otherMedLarge_BCGlevel3(metricResults)),sep=' '))
        if(min(otherMedLarge_BCGlevel2(metricResults))>=0.5){
          print(paste('Level 2 Membership: ',min(otherMedLarge_BCGlevel2(metricResults)),sep=' '))}
      }else(print(paste('Level 3 Membership: ',min(otherMedLarge_BCGlevel3(metricResults)),sep=' ')))
    }else(print(paste('Level 4 Membership: ',max(min(otherMedLarge_BCGlevel4alt1(metricResults)),min(otherMedLarge_BCGlevel4alt2(metricResults))),sep=' ')))
    }else(print(paste('Level 5 Membership: ',min(otherMedLarge_BCGlevel5(metricResults)),sep=' ')))
}
allTogetherNow(test)
# kinda works, need to get it out output station name on each line

# Try adply for multiple sites
metricResults <- masterMetric('Sample212',samp212_att)
dataframes <- list(metricResults,test) # test dataframe, just duplicated the same sample set
adply(dataframes,1,allTogetherNow)# kinda works, need to get it out output station name on each line

