#code to reproduce Figure 1 of 
#"Greenspace exposure conducive for the resilience of public sentiment during the COVID-19 pandemic"
#author: Liuyi Song

library(ggplot2)

#Fig.3A
Q<-read.table('./Q_Wuhan_urban.csv',header=TRUE)
Q$GEFs <- factor(Q$GEFs,levels = c("NDVI","Building", "Sidewalk"))
Q$Period <- factor(Q$Period,levels = c("S3","S2"),labels = c("After","Before"))
fig3A<-ggplot(data=Q,aes(x=Period,y=q))+ylim(0,0.5)+
  geom_col(aes(fill=GEFs),position = position_dodge(0.6),width = 0.6)+
  coord_flip ()+
  theme_minimal()+
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm"),
  ) +theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.x = element_text(color="black"),axis.text.y=element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+ 
  theme(legend.title=element_blank(),legend.position = c(0.8,0.8)) + guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.text=element_text(size=20))+
  scale_fill_manual(values=c("NDVI" = "#C0F2CC", "Building" = "#CCCCCC", "Sidewalk" = "#FBC074"))
fig3A


#Fig.3B
Q<-read.table('./Q_Beijing_urban.csv',header=TRUE)
Q$GEFs <- factor(Q$GEFs,levels = c("NDVI","Building", "Sidewalk"))
Q$Period <- factor(Q$Period,levels = c("S3","S2"),labels = c("After","Before"))
fig3B<-ggplot(data=Q,aes(x=Period,y=q))+ylim(0,0.5)+
  geom_col(aes(fill=GEFs),position = position_dodge(0.6),width = 0.6)+
  coord_flip ()+
  theme_minimal()+
  theme_bw()+theme(legend.position = 'none',panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("NDVI" = "#C0F2CC", "Building" = "#CCCCCC", "Sidewalk" = "#FBC074"))
fig3B


#Fig.3C
Q<-read.table('./Q_Wuhan_rural.csv',header=TRUE)
Q$GEFs <- factor(Q$GEFs,levels = c("NDVI","Building", "Sidewalk"))
Q$Period <- factor(Q$Period,levels = c("S3","S2"),labels = c("After","Before"))
fig3C<-ggplot(data=Q,aes(x=Period,y=q))+ylim(0,0.5)+
  geom_col(aes(fill=GEFs),position = position_dodge(0.6),width = 0.6)+
  coord_flip ()+
  theme_minimal()+
  theme_bw()+theme(legend.position = 'none',panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("NDVI" = "#C0F2CC", "Building" = "#CCCCCC", "Sidewalk" = "#FBC074"))
fig3C


#Fig.3D
Q<-read.table('./Q_beijing_rural.csv',header=TRUE)
Q$GEFs <- factor(Q$GEFs,levels = c("NDVI","Building", "Sidewalk"))
Q$Period <- factor(Q$Period,levels = c("S3","S2"),labels = c("After","Before"))
fig3D<-ggplot(data=Q,aes(x=Period,y=q))+ylim(0,0.5)+
  geom_col(aes(fill=GEFs),position = position_dodge(0.6),width = 0.6)+
  coord_flip ()+
  theme_minimal()+
  theme_bw()+theme(legend.position = 'none',panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("NDVI" = "#C0F2CC", "Building" = "#CCCCCC", "Sidewalk" = "#FBC074"))
fig3D


#Fig.3E
NDVI<-read.table('./NDVI_Qchange.csv',header=TRUE)
fig3E <- ggplot(NDVI, aes(x=City, y=Change,fill=Area)) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+coord_flip()+
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm"),
  ) +theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.x = element_text(color="black"),axis.text.y=element_text(color="black"),axis.text=element_text(size=20))+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("Urban"="#D9D9D9","Rural"="#DF7B87")) + 
  theme(legend.title=element_blank(),legend.position = c(0.5,0.5)) + guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.text=element_text(size=20))
fig3E

#Fig.3F
Building<-read.table('F:./Building_Qchange.csv',header=TRUE)
fig3F <- ggplot(Building, aes(x=City, y=Change,fill=Area)) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+coord_flip()+
  theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20),legend.position = 'none')+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("Urban"="#D9D9D9","Rural"="#DF7B87"))
fig3F


#Fig.3G
Sidewalk<-read.table('./Sidewalk_Qchange.csv',header=TRUE)
fig3G <- ggplot(Sidewalk, aes(x=City, y=Change,fill=Area)) + 
  geom_bar(stat="identity",position="dodge",width=0.5)+coord_flip()+
  theme_bw()+theme(panel.border = element_blank(),panel.grid.major.x=element_blank(),panel.grid.minor=element_blank(),plot.title = element_text(hjust = 0.5),axis.line.x=element_line(linetype=1,color="black",size=1))+
  theme(axis.text.y = element_blank(),axis.text.x = element_text(color="black"),axis.text=element_text(size=20),legend.position = 'none')+
  xlab(NULL)+ylab(NULL)+
  scale_fill_manual(values=c("Urban"="#D9D9D9","Rural"="#DF7B87"))
fig3G


