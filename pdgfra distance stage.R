install.packages("tidydistanceerse")
library(readxl)
library(ggplot2)
library("gridExtra")
library("cowplot")
library(Cairo)
library(readxl)
library(ggpubr)
library(ggsignif)
library(readr)
library(ggbeeswarm)
library(tidydistanceerse)
mydata <- read_excel("/Users/guoshuyu/Desktop/pdgfra distance.xlsx")

p1 <- ggplot(mydata,aes(mydata$diameter,mydata$distance))+
  geom_point(size=5,alpha=0.6)+  #aes(color = mydata$shuxing)
  #geom_distanceline(xintercept = c(35,65,85),linetype = "dotted",size = 0.5)+
  #geom_hline(yintercept = 25,linetype = "longdash",size = 1.0)+
  #scale_color_manual(distancealues = c('blue','magenta','green','gray'))+
  geom_smooth(method = 'loess',color = 'red3',fill='red3',se = T,size=3,linetype=0,alpha=0.6)+  #se = F
  
  
  theme_bw()+theme(legend.position = "none",
                   #axis.ticks.x= element_blank(),
                   #axis.text.x = element_blank(),
                   #axis.title=element_text(size=14,family="serif"),
                   # axis.text.x = element_text(face="bold", color="black", size=14),
                   # axis.text.y = element_text(face="bold", color="black", size=14) 
                   axis.text.x=element_text(margin=margin(30,30,30,30)),
                   axis.text.y=element_text(margin=margin(30,30,30,30)))+
  labs(x = NULL,y ='Distance(norm.)')+
  coord_cartesian(ylim = c(0,30),xlim=c(10,100),expand = F)+
  # xlab(NULL)
  #  ylab('Cadistanceity diameter(??m)')
 # xlim(0,110)+
 # ylim(0,30)
  #theme_bw()+
  #theme(panel.grid=element_blank(),
  # axis.title=element_text(size=14,family="serif"),
  # legend.position="top")+
  #scale_x_continuous(breaks =seq(0,128,32))
  scale_x_continuous(breaks=c(0,35,65,85,100), 
                     labels = c('0','35\nE3.5','65\nE3.75','85\nE4.0','100\nE4.25'))

#labels = c('32\nE3.50','64\nE3.75','96\nE4.00','128\nE4.25')
#plot(density(rnorm(1000)),cex.axis=3,cex.lab=3)

#plot (font.lab = 5)



p1

#ggsadistancee("p1.TIFF", p1)#width = 7, height = 3.15,dpi = 600







mydata$dose <- as.factor(mydata$dose)
mydata$distance <- as.numeric(mydata$distance)
#mydata2 <- mydata[mydata$dose == 'cn' | mydata$dose == 'ln',]
#mydata3 <- mydata[mydata$dose == 'cb' | mydata$dose == 'lb',]  

#with(mydata2,tapply(distancealue, dose, shapiro.test))  
#wilcox.test(distancealue~dose,data = mydata2,exact = F,
           # alternatidistancee = 't')  


#with(mydata3,tapply(distancealue, dose, shapiro.test))  
#wilcox.test(distancealue~dose,data = mydata3,exact = F,
          #  alternatidistancee = 't')    


#comp <- list(c('cb','lb'),c('cn','ln')) 
dat1<-subset(mydata, dose=="1")

dat2<-subset(mydata, dose=="2")
dat3<-subset(mydata, dose=="3")
dat4<-subset(mydata, dose=="4")
dat12=rbind(dat1, dat2)
dat13=rbind(dat1, dat3)
dat14=rbind(dat1, dat4)
shapiro.test(dat1$distance)

shapiro.test(dat2$distance)
shapiro.test(dat3$distance)
shapiro.test(dat4$distance)
bartlett.test(dat12$distance,dat12$dose) ##其实我感觉做一个就行
bartlett.test(dat13$distance,dat13$dose)
bartlett.test(dat14$distance,dat14$dose)

#kruskal.test(distancealue~dose,mydata)
t.test(distance~dose,dat12,distancear.equal=TRUE)
t.test(distance~dose,dat13,distancear.equal=TRUE)
t.test(distance~dose,dat14,distancear.equal=FALSE)
#kruskal.test(distance~dose,dat13)
p2 <- ggplot(mydata,aes(dose,distance,color = dose)) +
  geom_boxplot(fill = 'white',width = 0.8,
               color = c('slateblue1','slateblue2','slateblue3','slateblue4'), #width = 4
               size=0.4,alpha=0.6)+
  geom_beeswarm(cex = 3,size = 5,alpha=0.6 )+   #shape = 21,fill = 'white',cex = 1.3,size = 1
  scale_color_manual(distancealues = c('slateblue1','slateblue2','slateblue3','slateblue4'))+
  #scale_x_discrete(limits=c('cb','lb','cn','ln'),
                  # labels = c('-\nβ-ca','+\nβ-ca','-\nnanog','+\nnanog'))+
  labs(x = 'stage', y ='Distance(norm.)')+
  scale_x_discrete(limits=c('1','2','3','4'),
                   labels = c('??','??','??','??'))+
  coord_cartesian(xlim = c(1,4),ylim = c(0,30),)+ #expand = F
 # geom_signif(comparisons = comp,
    #          y_position = c(275,275),
         #     annotations =c('***',"***"),color = 'black')+
  theme_bw()+
  theme(#panel.grid=element_blank(),
    axis.title=element_text(size=14,family="serif",color = 'black'),
    # axis.text.x = element_text(face="bold", color="black", size=14),
    # axis.text.y = element_text(face="bold", color="black", size=14) ,
    legend.position="none",
    axis.text.x=element_text(margin=margin(30,30,30,30)),
    axis.text.y=element_text(margin=margin(30,30,30,30))) 
p2

p4 <- plot_grid(p1,p2,ncol = 1, nrow = 2)

p4
library(export)
graph2ppt(file="pdgfra distance",width =8, height =10)

#ggsadistancee("p4.TIFF",width = 6, height = 8, p4)#width = 7, height = 3.15,dpi = 600

