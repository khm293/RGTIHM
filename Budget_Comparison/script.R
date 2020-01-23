####### Script to Compare Water Budget Components Between RGTIHM Scenarios ######
####### written by Katie Markovich, January 2020 ########

# load libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library()

#read in base scenario, first set working directory
setwd('L:/projects/Mesilla_RG109K5/RGTIHM/Model_Versions/Revised_Release_09242018/NMWSC_Review/RGTIHM/output/')
FMP_names=read.table('FDS.out',nrow=1)
FMP_names=FMP_names[1,2:17]
FMP_base=read.table('FDS.out', skip =1, stringsAsFactors = FALSE)
names(FMP_base)=lapply(FMP_names[1,], as.character)

SFR_names=read.table('hyd_RGTIHM.otf', nrow=1)
SFR_names=SFR_names[1,2:196]
SFR_base=read.table('hyd_RGTIHM.otf', skip=1, stringsAsFactors = FALSE)
names(SFR_base)=lapply(SFR_names[1,], as.character)

#read in new scenario, first set working directory
setwd('L:/projects/Mesilla_RG109K5/RGTIHM/Model_Versions/RGTIHM_TXCIR/TR_C_2_loose_newEXE/output/')
FMP_names=read.table('FDS.out',nrow=1)
FMP_names=FMP_names[1,2:17]
FMP_new=read.table('FDS.out', skip =1, stringsAsFactors = FALSE)
names(FMP_new)=lapply(FMP_names[1,], as.character)

SFR_names=read.table('hyd_RGTIHM.otf', nrow=1)
SFR_names=SFR_names[1,2:196]
SFR_new=read.table('hyd_RGTIHM.otf', skip=1, stringsAsFactors = FALSE)
names(SFR_new)=lapply(SFR_names[1,], as.character)


#create percent diff matrix
FMP_percent_diff=matrix(NA, nrow=127516, ncol=3)
FMP_percent_diff=as.data.frame(FMP_percent_diff)
names(FMP_percent_diff)=c('Q_fin', 'TFDR_fin', 'NR_SWD_fin')

#loop through scenario results, and calculate percent diffs
for (i in 1:127516){
  FMP_percent_diff[i,1]= ((FMP_base[i,13]-FMP_new[i,13])/(FMP_base[i,13]+FMP_new[i,13]))*100
  FMP_percent_diff[i,2]= ((FMP_base[i,9]-FMP_new[i,9])/(FMP_base[i,9]+FMP_new[i,9]))*100
  FMP_percent_diff[i,3]= ((FMP_base[i,10]-FMP_new[i,10])/(FMP_base[i,10]+FMP_new[i,10]))*100
}

FMP_percent_diff[FMP_percent_diff == 'NaN'] =0

par(mfrow=c(1,3))
d=density(FMP_percent_diff$Q_fin)
plot(d,type='l', main=c('Q-Final'), xlab=c('Percent Difference'), xlim=c(-110,110))
abline(v=mean(FMP_percent_diff$Q_fin),col="blue")
d=density(FMP_percent_diff$TFDR_fin)
plot(d,type='l', main=c('TFDR-Final'), xlab=c('Percent Difference'))
abline(v=mean(FMP_percent_diff$TFDR_fin),col="blue")
d=density(FMP_percent_diff$NR_SWD_fin)
plot(d,type='l', main=c('NR-SWD-Final'), xlab=c('Percent Difference'))
abline(v=mean(FMP_percent_diff$NR_SWD_fin),col="blue")


