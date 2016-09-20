rm(list=ls())
install.packages("reshape")
library(reshape)
fam=read.csv("/Users/dannem/Dropbox/Data/IO_statistics/fam_IO_obj_test.csv")
fam_all <- fam[,1:4]
fam_L <- fam[,5:8]
summary(fam_all)
summary(fam_L)
unf=read.csv("/Users/dannem/Dropbox/Data/IO_statistics/unf_IO_obj_test.csv")
summary(unf)
unf_all <- unf[,1:4]
unf_L <- unf[,5:8]
summary(unf_all)
summary(unf_L)
fam_all$subnum <- 1:nrow(fam_all)
fam_L$subnum <- 1:nrow(fam_L)
unf_all$subnum <- 1:nrow(unf_all)
unf_L$subnum <- 1:nrow(unf_L)


# aggregating data
fam_all <- melt(fam_all,id="subnum")
fam_L <- melt(fam_L,id="subnum")
unf_all <- melt(unf_all,id="subnum")
unf_L <- melt(unf_L,id="subnum")
fam_all$conf <- substr(fam_all$variable,13,16)
fam_L$conf <- substr(fam_L$variable,13,16)
unf_all$conf <- substr(unf_all$variable,13,16)
unf_L$conf <- substr(unf_L$variable,13,16)
for (i in 1:nrow(fam_all)){
  a <- nchar(as.character(fam_all$variable[i]))-3
  b <- nchar(as.character(fam_all$variable[i]))
fam_all$ims[i] <- substr(fam_all$variable[i],a,b)
fam_L$ims[i] <- substr(fam_L$variable[i],a,b)
}
for (i in 1:nrow(unf_all)){
  a <- nchar(as.character(unf_all$variable[i]))-3
  b <- nchar(as.character(unf_all$variable[i]))
  unf_all$ims[i] <- substr(unf_all$variable[i],a,b)
  unf_L$ims[i] <- substr(unf_L$variable[i],a,b)
}
unf_L$conf <- gsub("hap_", "happ", unf_L$conf)
unf_all$conf <- gsub("hap_", "happ", unf_all$conf)
fam_L$conf <- gsub("hap_", "happ", fam_L$conf)
fam_all$conf <- gsub("hap_", "happ", fam_all$conf)
unf_all$type <- ifelse((unf_all$conf == "happ") & 
                         (unf_all$ims == "happ"), "within",
                       ifelse((unf_all$conf=="neut") & 
                                (unf_all$ims=="neut"), "within","between"))
unf_L$type <- ifelse((unf_L$conf == "happ") & 
                         (unf_L$ims == "happ"), "within",
                       ifelse((unf_L$conf=="neut") & 
                                (unf_L$ims=="neut"), "within","between"))
fam_all$type <- ifelse((fam_all$conf == "happ") & 
                         (fam_all$ims == "happ"), "within",
                       ifelse((fam_all$conf=="neut") & 
                                (fam_all$ims=="neut"), "within","between"))   

fam_L$type <- ifelse((fam_L$conf == "happ") & 
                         (fam_L$ims == "happ"), "within",
                       ifelse((fam_L$conf=="neut") & 
                                (fam_L$ims=="neut"), "within","between"))          
levels(unf_all$conf)=c("neut","happ") # changing default levels, so the baseline is neutral
levels(unf_L$conf)=c("neut","happ")
levels(fam_all$conf)=c("neut","happ")
levels(fam_L$conf)=c("neut","happ")
# Anova
##installing necessary packages
#install.packages("ez")
#install.packages("lsr")
#loading required libraries
library(ez)
library(lsr)


################################
### REPEATED MEASURES ANOVAS ###
################################
#ANOVAs that include repeated measures factors
mixed_anova_fam_all <- ezANOVA(data = fam_all,                
                       dv = value,                
                       wid = subnum,           
                       #between = group,        
                       within = .(conf, ims),    #within subjects variables to include in the model         
                       detailed = T) 
print(mixed_anova_fam_all)
mixed_anova_fam_L <- ezANOVA(data = fam_L,                
                       dv = value,                
                       wid = subnum,           
                       #between = group,        
                       within = .(conf, ims),    #within subjects variables to include in the model         
                       detailed = T) 
print(mixed_anova_fam_L)
mixed_anova_unf_all <- ezANOVA(data = unf_all,                
                       dv = value,                
                       wid = subnum,           
                       #between = group,        
                       within = .(conf, ims),    #within subjects variables to include in the model         
                       detailed = T) 
print(mixed_anova_unf_all)
mixed_anova_unf_L <- ezANOVA(data = unf_L,                
                       dv = value,                
                       wid = subnum,           
                       #between = group,        
                       within = .(conf, ims),    #within subjects variables to include in the model         
                       detailed = T) 
print(mixed_anova_unf_L)
par(mfrow=c(2,2))
boxplot(value~conf*ims, data=unf_all, 
        col=(c("gold","darkgreen")), 
        xaxt = "n", 
        main="Unfamiliar faces - all channels", xlab="Face space and emotion type", ylab="Objective reconstruction accuracy")
axis(1, at=1:4, labels=c("h space-h ims","n space - h ims","h space - n ims","n space-n ims"))
boxplot(value~conf*ims, data=unf_L, 
        col=(c("gold","darkgreen")), 
        xaxt = "n", 
        main="Unfamiliar faces - L channel", xlab="Face space and emotion type", ylab="Objective reconstruction accuracy")
axis(1, at=1:4, labels=c("h space-h ims","n space - h ims","h space - n ims","n space-n ims"))
boxplot(value~conf*ims, data=fam_all, 
        col=(c("gold","darkgreen")), 
        xaxt = "n", 
        main="Famous faces - all channels", xlab="Face space and emotion type", ylab="Objective reconstruction accuracy")
axis(1, at=1:4, labels=c("h space-h ims","n space - h ims","h space - n ims","n space-n ims"))
boxplot(value~conf*ims, data=fam_L, 
        col=(c("gold","darkgreen")), 
        xaxt = "n", 
        main="Famous faces - L channel", xlab="Face space and emotion type", ylab="Objective reconstruction accuracy")
axis(1, at=1:4, labels=c("h space h ims","n space - h ims","h space - n ims","n space-n ims"))

par(mfrow=c(2,2))
mm = tapply(unf_all$value, list(unf_all$type, unf_all$conf), mean)
graph2 = barplot(mm, beside=T, ylim=c(0,1.3), space=c(.1,.8),
                 main="Search Task Errors", xlab="hours of food deprivation",
                 ylab="mean number of errors", legend =T, axis.lty=0.8,
                 col=c("darkseagreen4","deepskyblue4"))
superpose.eb = function (x, y, ebl, ebu = ebl, length = 0.08, ...){
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
         length = length, ...)
}
sd_unf_all = tapply(unf_all$value, list(unf_all$conf, unf_all$type), sd)
temp=sd_unf_all
superpose.eb(x=graph2, y=mm, ebl=temp, col="black", lwd=2)

