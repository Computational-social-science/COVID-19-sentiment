#code to reproduce Figure 2 of 
#"Greenspace exposure conducive for the resilience of public sentiment during the COVID-19 pandemic"
#author: Liuyi Song

library(ggplot2)
library(patchwork)
library(scales)
library(tidyverse)

#Fig.2A
#read the data
NDVI<-read.table('./NDVI_change.csv',header=TRUE)
fig2A <- ggplot(NDVI, aes(x=City, y=Change,fill=Area)) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+coord_flip()+
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm"),
  ) +theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("Urban"="#D9D9D9","Rural"="#9DC3E6")) + 
  theme(legend.title=element_blank(),legend.position = c(0.9,0.15)) + guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.text=element_text(size=20))
fig2A


#Fig.2B
Building<-read.table('./Building_change.csv',header=TRUE)
fig2B <- ggplot(Building, aes(x=City, y=Change,fill=Area)) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+coord_flip()+
  theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20),legend.position = 'none')+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("Urban"="#D9D9D9","Rural"="#9DC3E6"))
fig2B


#Fig.2C
Sidewalk<-read.table('./Sidewalk_change.csv',header=TRUE)
fig2C <- ggplot(Sidewalk, aes(x=City, y=Change,fill=Area)) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+coord_flip()+
  theme_bw()+theme(axis.text.y = element_blank(),panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20),legend.position = 'none')+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("Urban"="#D9D9D9","Rural"="#9DC3E6"))
fig2C


#Fig.2D
#read the exposure by day
NDVI<-read.table('./NDVI_daily_Wuhan.csv',header=TRUE)
NDVI$date<-as.Date(NDVI$date,"%m/%d/%Y")
span_loess <- 0.12
fig_2D<-ggplot() + 
  geom_smooth(data=NDVI, aes(x=date,y=Urban),col="#FF9933",span=span_loess,se=F,lwd=0.6*1.2)+
  geom_smooth(data=NDVI, aes(x=date,y=Rural),col="#2E75B6",span=span_loess,se=F,lwd=0.6*1.2) +
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm"),
  ) +theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous() + 
  geom_vline(xintercept=as.Date("2020-01-23"),col="red",linetype="dashed") +
  scale_fill_manual(values=c("Urban"="#FF9933","Rural"="#2E75B6")) + 
  theme(legend.title=element_blank(),legend.position = c(0.1,0.2)) + guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.text=element_text(size=20))+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"),axis.text=element_text(size=20))
fig_2D


#Fig.2E
Building<-read.table('./Building_daily_Wuhan.csv',header=TRUE)
Building$date<-as.Date(Building$date,"%m/%d/%Y")
span_loess <- 0.12
fig_2E<-ggplot() + 
  geom_smooth(data=Building, aes(x=date,y=Urban),col="#FF9933",span=span_loess,se=F,lwd=0.6*1.2)+
  geom_smooth(data=Building, aes(x=date,y=Rural),col="#2E75B6",span=span_loess,se=F,lwd=0.6*1.2) +
  theme_bw()+theme(legend.position = 'none',panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(breaks=seq(0, 15, 5)) + 
  geom_vline(xintercept=as.Date("2020-01-23"),col="red",linetype="dashed")
fig_2E


#Fig.2F
Sidewalk<-read.table('./Sidewalk_daily_Wuhan.csv',header=TRUE)
Sidewalk$date<-as.Date(Sidewalk$date,"%m/%d/%Y")
span_loess <- 0.12
fig_2F<-ggplot() + 
  geom_smooth(data=Sidewalk, aes(x=date,y=Urban),col="#FF9933",span=span_loess,se=F,lwd=0.6*1.2)+ylim(0,1500)+
  geom_smooth(data=Sidewalk, aes(x=date,y=Rural),col="#2E75B6",span=span_loess,se=F,lwd=0.6*1.2) +
  theme_bw()+theme(legend.position = 'none',panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_y_continuous(breaks=seq(0, 900, 300)) + 
  geom_vline(xintercept=as.Date("2020-01-23"),col="red",linetype="dashed")
fig_2F


#Fig.2G1
NDVI<-read.table('./NDVI_week_Wuhan.csv',header=TRUE)
NDVI$date<-c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")
NDVI$date <- factor(NDVI$date,levels=c("Mon","Tues","Wed","Thur","Fri","Sat","Sun"))
fig_G1<-ggplot()  +
  geom_line(data=NDVI,aes(x=date,y=Urban),color="#FF9933",group=1,lwd=0.6*1.2)+
  geom_line(data=NDVI,aes(x=date,y=Rural),color="#2E75B6",group=1,lwd=0.6*1.2) +
  geom_point(data=NDVI, aes(x=date,y=Urban),shape=15,size=3,color="#F4B183")+
  geom_point(data=NDVI, aes(x=date,y=Rural),shape=15,size=3,color="#4472C4")+
  theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  scale_y_continuous(limits = c(0,900),breaks = seq(0,900,300))+
  theme(axis.text.y = element_text(color="black"),axis.text.x = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)
fig_G1


#Fig.2G2
Building<-read.table('./Building_week_Wuhan.csv',header=TRUE)
Building$date<-c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")
Building$date <- factor(Building$date,levels=c("Mon","Tues","Wed","Thur","Fri","Sat","Sun"))
fig_G2<-ggplot() + 
  geom_line(data=Building,aes(x=date,y=Urban),color="#FF9933",group=1,lwd=0.6*1.2)+
  geom_line(data=Building,aes(x=date,y=Rural),color="#2E75B6",group=1,lwd=0.6*1.2) +
  geom_point(data=Building, aes(x=date,y=Urban),shape=15,size=3,color="#F4B183")+
  geom_point(data=Building, aes(x=date,y=Rural),shape=15,size=3,color="#4472C4")+
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm"),
  ) +theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  scale_y_continuous(limits = c(0,12),breaks = seq(0,12,4))+
  theme(axis.text.y = element_text(color="black"),axis.text.x = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)
fig_G2


#Fig.2G3
Sidewalk<-read.table('./Sidewalk_week_Wuhan.csv',header=TRUE)
Sidewalk$date<-c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")
Sidewalk$date <- factor(Sidewalk$date,levels=c("Mon","Tues","Wed","Thur","Fri","Sat","Sun"))
cols <- c("Urban"="#FF9933","Rural"="#2E75B6")
fig_G3<-ggplot() + ylim(0,600)+
  geom_line(data=Sidewalk,aes(x=date,y=Urban),color="#FF9933",group=1,lwd=0.6*1.2)+
  geom_line(data=Sidewalk,aes(x=date,y=Rural),color="#2E75B6",group=1,lwd=0.6*1.2) +
  geom_point(data=Sidewalk, aes(x=date,y=Urban),shape=15,size=3,color="#F4B183")+
  geom_point(data=Sidewalk, aes(x=date,y=Rural),shape=15,size=3,color="#4472C4")+
  theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_text(color="black"),axis.text.x = element_text(color="black"),axis.text=element_text(size=20),legend.position = 'none')+
  xlab(NULL)+ylab(NULL)
fig_G3
