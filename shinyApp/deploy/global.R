suppressMessages({
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(leaflet)
  library(DT)
  library(rgdal)
  library(raster)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(shiny)
  library(leaflet)
  library(raster)
  library(rgdal)
  library(plyr)
  library(dplyr)
  library(readxl)
})

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
P_Dom56t <- function(sample){(
  ifelse(nrow(filter(sample,attLevel %in% c('5','6t')))==0,0,max(filter(sample,attLevel %in% c('5','6t'))$Count))/totInd(sample))*100}
# Number of Attribute 1,2,&3 taxa
T_123 <- function(sample){sum(sample$attLevel %in% c(1,2,3))}
# % Attribute 5&6 taxa
P_56 <- function(sample){(sum(sample$attLevel %in% c('5','6i','6m','6t'))/totTax(sample))*100}
## ---------------------------------------------------------------------------------------------------------
# Master metric that outputs dataframe of all results
masterMetric <- function(sampleName,sample){
  data.frame(SampleName=sampleName,totTax=totTax(sample),totInd=totInd(sample),T_12=T_12(sample),P_123=P_123(sample)
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

# BCG level logic, Other Medium/Large, NO BROOK TROUT
otherMedLarge_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,12,22)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,20,30))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,50,60)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,0,2)))}
otherMedLarge_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,9,19)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,60,70))
             ,fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)))}
otherMedLarge_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,14)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,75,85)))}
otherMedLarge_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,15,25)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherMedLarge_BCGlevel5alt1_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,9)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}
otherMedLarge_BCGlevel5alt2_NOBKT <- function(test){
  data.frame(fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1)))}

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

# BCG level logic, Other Small, NO BROOK TROUT
otherSmall_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,16)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,10,20)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,50,60)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,0,2))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,2,5)))}
otherSmall_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,13)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,4,10))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,5,15)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,60,70))
             ,fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,5)),fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,0,3)))}
otherSmall_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,0,1)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,85,95)))}
otherSmall_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,13,23)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,25,35))
             ,fm_T_darter=fmFinal(fuzzyMembership(test$T_darter,1,4)))}
otherSmall_BCGlevel5_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,6)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}

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

# BCG level logic, Above Falls Medium/Large, NO BROOK TROUT
aboveFallsMedLarge_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,5,15)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,4))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsMedLarge_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,14)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,15,25))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,35,45))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsMedLarge_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,6,10)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70)),fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,65,75)))}
aboveFallsMedLarge_BCGlevel5_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}

# BCG level logic, Above Falls Small
aboveFallsSmall_BCGlevel2alt1 <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,12)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
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

# BCG level logic, Above Falls Small, NO BROOK TROUT
aboveFallsSmall_BCGlevel2_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,4,12)),fm_T_12=fmFinal(fuzzyMembership(test$T_12,0,1))
             ,fm_P_123=fmFinal(fuzzyMembership(test$P_123,20,30)),fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,25,35))
             ,fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,30,40)),fm_T_6=fmFinal(1-fuzzyMembership(test$T_6,1,4))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,0,1)))}
aboveFallsSmall_BCGlevel3_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,3,9)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,5,15))
             ,fm_PI_123=fmFinal(fuzzyMembership(test$PI_123,15,25)),fm_PI_5=fmFinal(1-fuzzyMembership(test$PI_5,50,60))
             ,fm_T_6t=fmFinal(1-fuzzyMembership(test$T_6t,1,4)))}
aboveFallsSmall_BCGlevel4alt1_NOBKT <- function(test){
  data.frame(fm_T_123=fmFinal(fuzzyMembership(test$T_123,1,2)),fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,45,55)))}
aboveFallsSmall_BCGlevel4alt2_NOBKT <- function(test){
  data.frame(fm_P_123=fmFinal(fuzzyMembership(test$P_123,3,7)),fm_P_56=fmFinal(1-fuzzyMembership(test$P_56,60,70))
             ,fm_PI_56t=fmFinal(1-fuzzyMembership(test$PI_56t,70,80)),fm_P_Dom56t=fmFinal(1-fuzzyMembership(test$P_Dom56t,45,55)))}
aboveFallsSmall_BCGlevel5_NOBKT <- function(test){
  data.frame(fm_totTax=fmFinal(fuzzyMembership(test$totTax,2,5)),fm_totInd=fmFinal(fuzzyMembership(test$totInd,50,60)))}


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


### Other Medium/Large BCG Model, NO BROOK TROUT
OtherMedLargeModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=max(min(otherMedLarge_BCGlevel5alt1_NOBKT(metricResults)),min(otherMedLarge_BCGlevel5alt2_NOBKT(metricResults)))
                            ,BCGlevel4=max(min(otherMedLarge_BCGlevel4alt1_NOBKT(metricResults)),min(otherMedLarge_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(otherMedLarge_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(otherMedLarge_BCGlevel2_NOBKT(metricResults)))%>%
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

### Other Small BCG Model, NO BROOK TROUT
OtherSmallModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(otherSmall_BCGlevel5_NOBKT(metricResults))
                            ,BCGlevel4=max(min(otherSmall_BCGlevel4alt1_NOBKT(metricResults)),min(otherSmall_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(otherSmall_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(otherSmall_BCGlevel2_NOBKT(metricResults)))%>%
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


### AboveFalls Medium/Large BCG Model, NO BROOK TROUT
AboveFallsMedLargeModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsMedLarge_BCGlevel5_NOBKT(metricResults))
                            ,BCGlevel4=max(min(aboveFallsMedLarge_BCGlevel4alt1_NOBKT(metricResults)),min(aboveFallsMedLarge_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(aboveFallsMedLarge_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(aboveFallsMedLarge_BCGlevel2_NOBKT(metricResults)))%>%
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


### Above Falls Small BCG Model, NO BROOK TROUT
AboveFallsSmallModel_NOBKT <- function(sampleName,taxaListFromOneSite){
  metricResults <- masterMetric(sampleName,taxaListFromOneSite)
  levelresult <- data.frame(SampleName=metricResults[1]
                            ,BCGlevel5=min(aboveFallsSmall_BCGlevel5_NOBKT(metricResults))
                            ,BCGlevel4=max(min(aboveFallsSmall_BCGlevel4alt1_NOBKT(metricResults)),min(aboveFallsSmall_BCGlevel4alt2_NOBKT(metricResults)))
                            ,BCGlevel3=min(aboveFallsSmall_BCGlevel3_NOBKT(metricResults))
                            ,BCGlevel2=min(aboveFallsSmall_BCGlevel2_NOBKT(metricResults)))%>%
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

### GIS COMPONENT

# Final BCG Model
BCG_Model_GIS <- function(dfInCorrectFormat){
  # split input dataframe by sampples, return list of dataframes
  splits <-split(dfInCorrectFormat,dfInCorrectFormat$SampleName,drop=T)%>% 
    lapply(function(x) x[!(names(x) %in% c('SampleName'))]) # remove SampleName column, housekeeping
  
  # establish blank dataframe to store data
  result <- data.frame(matrix(NA, ncol = 9, nrow = 1))
  names(result) <- c('SampleName','Catchment','nominalTier','nominalMembership','secondMembership','runnerupTier'
                     ,'close','Model','Comment')
  
  for(i in 1:length(splits)){ #loop through each dataframe in the list of dataframes and do:
    #### Get attribute data in
    att <- readRDS('www/attributes.RDS')
    splits[[i]] <- join(splits[[i]],att,by=c('ScientificName','CommonName','Subbasin_short'))
    # basic housekeeping
    print(i)
    samplename <- names(splits)[i]
    sampleathand <- data.frame(splits[[i]])
    
    # Taxa list QA, check to make sure no NA's for attributes after joined to correct Subbasin
    comment1 <- if(sum(is.na(sampleathand$attLevel))>0){
      NArows <- filter(sampleathand, is.na(attLevel))
      commonNames <- paste(as.character(NArows$CommonName),sep=', ',collapse=', ')
      paste(commonNames,'not attributed in the',NArows$Subbasin,'Subbasin',sep=' ')[1]
    }else(' ') #no taxa attribute list problems
    
    # Model decisions based on Subbasin and Catchment size (mi2)
    if(splits[[i]]$Subbasin_short[1] %in% c('UNew','MNew_WV','MNew_VA','LNew','Greenbrier','Gauley')){
      if(splits[[i]]$Catchment[1] < 5){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel(samplename,sampleathand),Model='AboveFalls,Small',Comment=comment1))
        }else(result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel_NOBKT(samplename,sampleathand),Model='AboveFalls,Small,NOBKT',Comment=comment1)))
      }else if(splits[[i]]$Catchment[1] > 5 & splits[[i]]$Catchment[1] < 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel(samplename,sampleathand),Model='AboveFalls,Small',Comment=comment1)
                          ,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel(samplename,sampleathand),Model='AboveFalls,MediumLarge',Comment=comment1))
        }else(result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsSmallModel_NOBKT(samplename,sampleathand),Model='AboveFalls,Small,NOBKT',Comment=comment1)
                              ,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel_NOBKT(samplename,sampleathand),Model='AboveFalls,MediumLarge,NOBKT',Comment=comment1)))
      }else if(splits[[i]]$Catchment[1] > 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel(samplename,sampleathand),Model='AboveFalls,MediumLarge',Comment=comment1))
        }else(result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],AboveFallsMedLargeModel_NOBKT(samplename,sampleathand),Model='AboveFalls,MediumLarge,NOBKT',Comment=comment1)))
      }
    }else{
      if(splits[[i]]$Catchment[1] < 5){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],OtherSmallModel(samplename,sampleathand),Model='Other,Small',Comment=comment1))
        }else(result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],OtherSmallModel_NOBKT(samplename,sampleathand),Model='Other,Small,NOBKT',Comment=comment1)))
      }else if(splits[[i]]$Catchment[1] > 5 & splits[[i]]$Catchment[1] < 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],OtherSmallModel(samplename,sampleathand),Model='Other,Small',Comment=comment1)
                          ,cbind(Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel(samplename,sampleathand),Model='Other,MediumLarge',Comment=comment1))
        }else(result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],OtherSmallModel_NOBKT(samplename,sampleathand),Model='Other,Small,NOBKT',Comment=comment1)
                              ,cbind(Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel_NOBKT(samplename,sampleathand),Model='Other,MediumLarge,NOBKT',Comment=comment1)))
      }else if(splits[[i]]$Catchment[1] > 15){
        if(any(splits[[i]]$CommonName %in% c('brook trout'))){
          result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel(samplename,sampleathand),Model='Other,MediumLarge',Comment=comment1))
        }else(result <- rbind(result,cbind(Catchment=splits[[i]]$Catchment[1],OtherMedLargeModel_NOBKT(samplename,sampleathand),Model='Other,MediumLarge,NOBKT',Comment=comment1)))
      }
    }
    result<-filter(result,!(SampleName=='NA'))
  }
  return(result)
}
