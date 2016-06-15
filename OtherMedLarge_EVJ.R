library(plyr)
library(dplyr)
library(readxl)

#------------------------------------------- Data Management-----------------------------------------------
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
samp212 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample212taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 214)
samp214 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample214taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 217)
samp217 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample217taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 371)
samp371 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample371taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 393)
samp393 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample393taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 605)
samp605 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample605taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 615)
samp615 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample615taxaList.csv') %>%
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
samp214_att <- join(samp214,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp214_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp217_att <- join(samp217,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp217_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp371_att <- join(samp371,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp371_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp393_att <- join(samp393,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp393_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp405_att <- join(samp405,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp405_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp605_att <- join(samp605,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp605_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp615_att <- join(samp615,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp615_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
#---------------------------------------- End Data Management -------------------------------------------------


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
T_56 <- function(sample){(sum(sample$attLevel %in% c('5','6i','6m','6t'))/totTax(sample))*100}

# Master metric that outputs dataframe of all results
masterMetric <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),T_12(sample),P_123(sample),PI_123(sample),PI_5(sample),PI_56t(sample)
             ,T_6(sample),T_6t(sample),T_darter(sample),N_bTrout(sample),P_Dom56t(sample),T_123(sample),T_56(sample))}

# BCG Fuzzy Membership logic
fuzzyMembership <- function(metric,low,high){metric/(high-low)-low/(high-low)}

fmFinal <- function(x){
  if(x>=1){return(1)}else(y=x)
  if(y<=0){return(0)}else(return(x))
}

# BCG level logic, starting at Other Medium/Large
otherMedLarge_BCGlevel2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,10,20)),fm_T_12=fmFinal(fuzzyMembership(test$T_12.sample.,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123.sample.,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,55,65)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6.sample.,1,3)))}
# call all BCG level functions like below
#otherMedLarge_BCGlevel2(test)
#min(otherMedLarge_BCGlevel2(test3[1,]))

otherMedLarge_BCGlevel3 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,9,19)),fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123.sample.,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,65,75))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t.sample.,25,35)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6.sample.,1,5)))}

otherMedLarge_BCGlevel4alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,7,15)),fm_P_123=fmFinal(fuzzyMembership(test$P_123.sample.,0,1))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t.sample.,75,85)))}
otherMedLarge_BCGlevel4alt2 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,16,26)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t.sample.,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter.sample.,2,5)))}

otherMedLarge_BCGlevel5 <- function(test){data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)))}

# Other Medium/Large BCG Model, will need logic prior to this step to route user to this model/function
#  based on watershed size, basin parameters
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

OtherMedLargeResults <- rbind(OtherMedLargeModel("Sample212",samp212_att),OtherMedLargeModel("Sample371",samp371_att)
                              ,OtherMedLargeModel("Sample217",samp217_att),OtherMedLargeModel("Sample405",samp405_att)
                              ,OtherMedLargeModel("Sample393",samp393_att),OtherMedLargeModel("Sample605",samp605_att)
                              ,OtherMedLargeModel("Sample615",samp615_att))
