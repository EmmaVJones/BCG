sampleList <- read.csv('C:/HardDriveBackup/R/BCG/FiguringItOut/taxaLists/sampleList.csv')%>%
  select(-X)%>%
  plyr::rename(c('BCGAttribute'='attLevel'))#%>%
  #select(-c(SubBasin,Catchment)) #just for testing purposes


splits <- split(sampleList,sampleList$SampleName,drop=T)%>%
  lapply(function(x) x[!(names(x) %in% c('SampleName'))])

options(digits = 3)

result <- data.frame(matrix(NA, ncol = 7, nrow = 1))
names(result) <- c('SampleName','nominalTier','nominalMembership','secondMembership','runnerupTier'
                   ,'close','Model')

for(i in 1:length(splits)){
  print(i)
  samplename <- names(splits)[i]
  sampleathand <- data.frame(splits[[i]])
  if(splits[[i]]$SubBasin[1] %in% c('New','UNew')){
    if(splits[[i]]$Catchment[1] < 10){
      result<- rbind(result,cbind(OtherSmallModel(samplename,sampleathand),Model='AboveFalls,Small'))
    }else{
      result<- rbind(result,cbind(OtherMedLargeModel(samplename,sampleathand),Model='Above,MediumLarge'))
    }
    result<-filter(result,!(SampleName=='NA'))
  }
}








# Works
result <- data.frame(matrix(NA, ncol = 6, nrow = 1))
names(result) <- c('SampleName', 'nominalTier', 'nominalMembership', 'secondMembership', 'runnerupTier', 'close')

for(i in 1:length(splits)){
  print(i)
  samplename <- names(splits)[i]
  sampleathand <- data.frame(splits[[i]])
  result<- rbind(result,OtherMedLargeModel(samplename,sampleathand)) %>%
    filter(!(SampleName=='NA'))
}
  








# for when doing model selection  
for(i in 1:length(sampleList)){
  if(sampleList$SubBasin)
    