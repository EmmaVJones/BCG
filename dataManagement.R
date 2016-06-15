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
#Bring in test taxa list (sample 360)
samp360 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample360taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 362)
samp362 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample362taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 363)
samp363 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample363taxaList.csv') %>%
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
#Bring in test taxa list (sample 405)
samp405 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample405taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 427)
samp427 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample427taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 477)
samp477 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample477taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 600)
samp600 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample600taxaList.csv') %>%
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
### ABOVE FALLS SITES
#Bring in test taxa list (sample 009)
samp009 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample009taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 062)
samp062 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample062taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 049)
samp049 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample049taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 048)
samp048 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample048taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 047)
samp047 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample047taxaList.csv') %>%
  select(-BCGAttribute) %>%#drop BCG attribute info so can link to lookup table of taxa Attributes by basin
  mutate(lCommonName=tolower(CommonName))%>% #make all common names lowercase
  select(-CommonName)%>%#remove old CommonName column
  plyr::rename(c('lCommonName'='CommonName'))
#Bring in test taxa list (sample 192)
samp192 <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/Sample192taxaList.csv') %>%
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
samp360_att <- join(samp360,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp360_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp362_att <- join(samp362,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp362_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp363_att <- join(samp363,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp363_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp371_att <- join(samp371,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp371_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp393_att <- join(samp393,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp393_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp405_att <- join(samp405,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp405_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp427_att <- join(samp427,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp427_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp477_att <- join(samp477,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp477_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp600_att <- join(samp600,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp600_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp605_att <- join(samp605,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp605_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp615_att <- join(samp615,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp615_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
## ABOVE FALLS SITES
samp009_att <- join(samp009,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp009_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp062_att <- join(samp062,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp062_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp049_att <- join(samp049,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp049_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp048_att <- join(samp048,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp048_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp047_att <- join(samp047,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp047_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
samp192_att <- join(samp192,taxaAtt,by=c('CommonName','ScientificName'))%>%
  select(c(CommonName,ScientificName,Count,UpperNew))
colnames(samp192_att) <- c('CommonName','ScientificName','Count','attLevel') #rename basin to generic name for functions
