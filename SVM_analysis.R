## SVM data analysis
install.packages("eegAnalysis")
install.packages("eegR")
install.packages("R.matlab")
library(eegAnalysis)
library("R.matlab")
dat=readMat('/Users/dannem/Desktop/ERP_s03dc_not_cleaned.mat')
dat=dat$output.same.uw
# B=randEEG(n.classes = 120, n.rec = 16, n.channels = 64, n.signals = 512,
# ar="default", ma="default", order="default",vars = c(1:120))
count=1
datEEG=matrix(data=NA,nrow=120*16*512,ncol=64)
classes.Id=matrix(data=NA,nrow=120*16*512,ncol=1)
rec.Id=matrix(data=NA,nrow=120*16*512,ncol=1)
for (i in 1:120){
  for (b in 1:16){
    for (j in 1:512){
      datEEG[count,]=dat[,i,b,j]
      classes.Id[count]=i
      rec.Id[count]=b
      count=count+1
    }
  }
}
B$data=datEEG
B$classes.Id=classes.Id
B$rec.Id=rec.Id
# features<-easyFeatures()
# x<-FeatureEEG(B$data,B$classes.Id,B$rec.Id,features="example",
#              Alpha=0.05, AlphaCorr=0.9,minacc=0.8,fast=FALSE)
