library(readxl)
library(ggsignif)
library(readr)
library(ggbeeswarm)
library(tidyverse)
mydata <- read_csv("/Users/guoshuyu/Desktop/显著性差异P值/Shape index embyro.csv")
#mydata$min <- 60/mydata$min
dat1<-subset(mydata, dose=="c")

dat2<-subset(mydata, dose=="t")
shapiro.test(dat1$index)

shapiro.test(dat2$index)
bartlett.test(mydata$index,mydata$dose)

#kruskal.test(value~dose,mydata)
t.test(index~dose,mydata,var.equal=FALSE)
mydata$dose <- as.factor(mydata$dose)
with(mydata,tapply(index, dose, shapiro.test))
kruskal.test(index~dose,mydata)
with(mydata,pairwise.wilcox.test(index,dose,exact = F))

comp <- list(c('c','t')) 

p2 <- ggplot(mydata,aes(dose,index,color = dose)) +
  geom_boxplot(fill = 'white',width = 0.8,
               color = c('black','red3'),
               size=0.4,alpha=0.6 )+  #width = 3
  geom_beeswarm(cex = 3,size = 5,alpha=0.6)+   #shap=21,cex = 1.3,size = 1
  scale_color_manual(values = c('black','red3'))+
  scale_x_discrete(limits=c('c','t'),
                   labels = c('Befor','After'))+
  labs(x = NULL,y ='Shape index')+
  coord_cartesian(xlim = c(-0,3),ylim = c(3,5),expand = F)+  #xlim = c(-0.1,5),ylim = c(0,300)
  # geom_signif(comparisons = comp, y_position = c(0.55), color = 'black')+    #annotations =c('**'),
  
  theme_bw()+
  theme(#panel.grid=element_blank(),
    axis.title=element_text(size=14,family="serif",color = 'black'),
    legend.position="none",
    axis.text.x=element_text(margin=margin(60,60,60,60)),
    axis.text.y=element_text(margin=margin(60,60,60,60)))
#axis.text.x = element_text(size=14,family="serif",color = 'black' ),

p2

library(export)
graph2ppt(file="Shape index embyro",width =8, height =8)


#ggsave("p1.TIFF", width = 6, height = 8,p2)  #dpi = 600 #width = 6, height = 6,


