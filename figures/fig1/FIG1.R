#code to reproduce Figure 1 of 
#"Greenspace exposure conducive for the resilience of public sentiment during the COVID-19 pandemic"
#author: Liuyi Song

library(ggplot2)
library(patchwork)
library(scales)
library(tidyverse)

#Fig.1A
#read the data
SI<-read.table('./SI_all.csv',header=TRUE)
#Convert date format
SI$Date<-as.Date(SI$Date,"%m/%d/%Y")
SI$Date
#Fig1A
fig_1A<-ggplot(SI, aes(x=Date)) + 
  geom_line(data=SI, aes(y=Wuhan),color="#CC0000",size=0.8)+
  geom_line(data=SI, aes(y=Beijing),color="#92D050",size=0.5)+
  geom_line(data=SI, aes(y=Shanghai),color="#FFC000",size=0.5)+
  geom_line(data=SI, aes(y=Shenzhen),color="#5B9BD5",size=0.5)+
  geom_line(data=SI, aes(y=rescale(Confirmed,c(0,1))),color="#0D0D03",linetype = "dashed",size=0.8) +
  scale_y_continuous(limit=c(0,1),breaks=seq(0, 1, 0.5),sec.axis = sec_axis(~ .*3000, breaks=seq(0, 3000, 1500),name = "Confirmed"))+
  theme_bw()+theme(axis.title=element_blank(),panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  geom_hline(aes(yintercept=0.65),col="grey",size=1,linetype="dashed")+
  geom_vline(xintercept=as.Date("2019-12-31"),col="red",linetype="dashed") +
  geom_vline(xintercept=as.Date("2020-01-23"),col="red",linetype="dashed")
fig_1A

