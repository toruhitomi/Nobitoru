####################################################################################################
#  time2aoristic()         
#
# *DESCRIPTION:
# R function for converting categorical relative chronology (e.g. "pottery phase A", "LBA" etc.) into an
# aoristic database with a user defined temporal resolution.
#
# *REQUIRED DATA AND INPUT PARAMETERS
# 
# data ==> Raw input data. The row corresponds to each event, and one of the column should define
# the categorical relative chronology e.g. :
# data.frame(id=1:10,phases=sample(c("phaseA","phaseB","phaseC"),10,replace=TRUE))
#
# colphase ==> Column number in <data> where the categorical relative chronology is stored
# 
# timeData ==> Three column data.frame, with the categorical relative chronologies and their START and END dates e.g.:
# data.frame(phase=c("phaseA","phaseB","phaseC"),start=c(-3410,-4560,-2720),end=c(-3300,-4490,-2570))
#
# resolution ==> resolution of the timeblocks (e.g. 100)
#
# minDate ==> the start  date of the eariest time-block
#
# maxDate ==> the start  date of the lates time-block
#
# *NOTE
# Date should refer to years, for BP and BC they should have negative values (e.g. 3400 cal BP is -3400)
#
# *OUTPUT
# The output is a single data.frame where <data> has new columns (with column names refering to the start date of each block)
# and aoristic weights (probability of existence) is computed. More details on the method can be found in:
#
# Ratcliffe, J. H., 2000, Aoristic analysis: the spatial interpretation of unspecifed temporal events, Inernational Journal of Geographical Information Science,  14, 669-679.
# Crema, E. R., In press, Modelling Temporal Uncertainty in Archaeological Analysis, Journal of Archaeological Method and Theory3  
#
#
#
#
# Author:  Enrico R. Crema
# email:   e.crema@ucl.ac.uk
# Version: 1.0
# 14/03/2012
####################################################################################################


time2aoristic<-function(data,timeData,resolution=100,minDate=NA,maxDate=NA,colphase)
{
if (is.na(minDate)) {minDate=min(timeData[,2:3])-resolution}
if (is.na(maxDate)) {maxDate=max(timeData[,2:3])+resolution}
timeBlocks<-seq(minDate,maxDate,resolution)  

tmpColnames<-colnames(timeData)
timeData<-cbind(timeData,matrix(NA,nrow=dim(timeData)[1],ncol=length(timeBlocks)))

for (x in 1:dim(timeData)[1])
  {
    timespan=timeData[x,3]-timeData[x,2] #compute time-span
    pp=1/timespan #compute probability of existence in a single year

    for (p in 1:length(timeBlocks))
      {
     start=timeData[x,2]
     end=timeData[x,3]
    if (timeBlocks[p]<start&(timeBlocks[p]+resolution)<=start) {timeData[x,p+3]=0} 
    if (timeBlocks[p]<start&(timeBlocks[p]+resolution)>start) {timeData[x,p+3]=pp*(resolution-abs(timeBlocks[p]-start))} 
    if (timeBlocks[p]>=start&timeBlocks[p]+resolution<=end){timeData[x,p+3]=pp*resolution} 
    if ((timeBlocks[p]>start)&(timeBlocks[p]<end)&timeBlocks[p]+resolution>=end){timeData[x,p+3]=pp*abs(timeBlocks[p]-end)} 
    if (timeBlocks[p]>=end) {timeData[x,p+3]=0}
    if (timeBlocks[p]<=start&(timeBlocks[p]+resolution)>=end){timeData[x,p+3]=1}
      }
  }

colnames(timeData)<-c(tmpColnames,timeBlocks)
res<-merge(x=data,y=timeData,by.x=colphase,by.y=1)
return(res)
}
