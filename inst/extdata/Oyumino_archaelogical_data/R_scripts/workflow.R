###################################################################################################
##########################   Example workflows for using time2aoristic()  #########################
###################################################################################################

# The following provides a series of examples for using the function time2aoristic()
# creator: E.R.Crema
# email: e.crema@ucl.ac.uk
# email2: enrico.crema@gmail.com


###################################################################################################
##############################   example using asn artificial dataset  ############################
###################################################################################################

timeData<-data.frame(phase=c("phaseA","phaseB","phaseC"),start=c(-3410,-4560,-2720),end=c(-3300,-4490,-2570))
data<-data.frame(id=1:10,phases=sample(c("phaseA","phaseB","phaseC"),10,replace=TRUE))
minDate=-5000
maxDate=-2000
resolution=100
colphase=2
source("./time2aoristic.R")


# timeData contains the name of the phases along with possible initial and ending dates 
print(timeData)
# data contains the events with one column (column 2 in this case) specifying the pottery phases
print(data)

#Compute the function time2aoristic() :
# data ==> raw data to be converted. Requires a column specifying the nominal time-phase (e.g. "phaseA")
# colphase ==> number of the column specifying the nominal time-phase (e.g. 2)
# resolution ==>Resolution of the time-blocks (e.g 100)
# minDate ==> START of the first time block (e.g. -5000)
# maxDate ==> START of the last time block (e.g.-2000)

res<-time2aoristic(data=data,timeData=timeData,resolution=100,minDate=-5000,maxDate=-2000,colphase=2)

print(res)

#export data as .csv
write.csv(res,"res.csv")


###################################################################################################
##############################   example using the provided dataset  ##############################
###################################################################################################

#The workflow provides the sequence of command necessary to recreate the file <adata.csv.> which can
#be find along with the code.

#import data and R script
pithouse<-read.csv("../pithousedata/pithousedata.csv")
potphase<-read.csv("../potphase/potphase.csv")
source("time2aoristic.R")

#trim unecessary information (consider only columns 1, 4, and 5)
potphasetrimmed<-potphase[,c(1,4,5)]

#create aoristic data with resolution of 100yrs, and interval from 5500 to 3300
adata<-time2aoristic(data=pithouse,timeData=potphasetrimmed,resolution=100,minDate=-5500,maxDate=-3300,colphase=6)

#export data after trimming uneccessary fields
adata<-adata[,c(2,15:36)]
write.csv(adata,row.names=FALSE,"adata.csv")
