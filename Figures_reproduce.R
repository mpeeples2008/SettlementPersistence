library(ggplot2)
library(ggridges)
library(lemon)
library(tidyverse)
library(ggpubr)

npp_CI <- read.csv(file="npp_CI.csv")
npp_SW1 <- read.csv(file="npp_SW1.csv")
npp_SW2 <- read.csv(file="npp_SW2.csv")
npp_SE <- read.csv(file="npp_SE.csv")
npp_YA <- read.csv(file="npp_YA.csv")
npp_FC <- read.csv(file="npp_FC.csv")
npp_BM <- read.csv(file="npp_BM.csv")
npp_SV <- read.csv(file="npp_SV.csv")
npp_dat <- rbind(npp_CI,npp_SW1,npp_SW2,npp_SE,npp_YA,npp_FC,npp_BM,npp_SV)
pop_dat <- read.csv('Occupation_spans.csv',header=T)

## Figure 2

## Shown at end of document as it relies on objects created in other figure calls


## Figure 3
pop_agg <- aggregate(data = pop_dat, Estimated.Total.Span ~ Region, median)
pop_dat$Region <- factor(pop_dat$Region, levels = pop_agg$Region[c(4,3,6,1,2,7,5)])

ggplot(pop_dat, aes(x=Estimated.Total.Span, fill=Region)) +
  scale_fill_viridis_d(option = 'turbo', direction = -1, name = "Region") + 
  geom_histogram(binwidth=100) +
  xlab("Estimated Total Occupation Span (Years)") +
  ylab("Site Count") +
  theme_bw() + theme(axis.line=element_line(),legend.position="none") +
  scale_x_continuous(limits=c(0,3200),breaks=c(0,500,1000,1500,2000,2500,3000)) +
  facet_rep_wrap(~ Region, ncol=2, scales = "free_y", repeat.tick.labels = "all") +
  geom_vline(data=pop_agg, aes(xintercept=(Estimated.Total.Span)), linetype = 2, size = 1.2, color='gray') 


## Figure 4
ggplot(npp_dat, aes(npp, 
                    reorder(region, -npp, FUN = median), 
                    fill = reorder(region, npp, FUN = median))) +
  geom_density_ridges(alpha = .65) +
  scale_fill_viridis_d(option = 'turbo', direction = -1, name = "Region") + 
  ggtitle('Regional NPP distributions') +
  labs(y = '', x = expression(Potential~net~primary~producivity~(g/m^2))) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size=20))

## Figure 5
npp_c <- npp_dat %>%
  group_by(region)%>% 
  summarise(Median=median(npp),Mean=mean(npp),IQ_high=quantile(npp,0.75),IQ_low=quantile(npp,0.25),
            IQRange=quantile(npp,0.75)-quantile(npp,0.25),
            TRange=range(npp)[2]-range(npp)[1])

pop_c <- pop_dat %>%
  group_by(Region)%>% 
  summarise(PMedian=median(Estimated.Total.Span),PMean=mean(Estimated.Total.Span),
            PIQ_high=quantile(Estimated.Total.Span,0.75),PIQ_low=quantile(Estimated.Total.Span,0.25),
            PIQRange=quantile(Estimated.Total.Span,0.75)-quantile(Estimated.Total.Span,0.25))

all_c <- cbind(npp_c,pop_c[c(4,5,2,1,7,3,6),]) # order objects so thtye are the same and combine

ggplot(data = all_c,aes(x = Median,y = PMedian, color=reorder(region, Median))) + 
  geom_point(size=5) + 
  geom_errorbar(aes(ymin = PIQ_low,ymax = PIQ_high)) + 
  geom_errorbarh(aes(xmin = IQ_low,xmax = IQ_high)) +
  scale_color_viridis_d(option = 'turbo', direction = -1, name = "Region") + 
  ggtitle('NPP vs. Settlement Persistence with Interquartile Range Error Bars') +
  theme_bw() +
  theme() +
  labs(y = 'Settlement persistence (Years)', x = expression(Potential~net~primary~producivity~(g/m^2))) 


## Figure 6
load("RegData.RData")
SV_mat$Cat <- cut(SV_mat$Peak.Pop, breaks=c(0,100,250,1000,5000,100000), labels = c('0-99','100-249', '250-999', '1000-4999', '5000+'), include.lowest = TRUE)
BOM_mat$Cat <- cut(BOM_mat$Peak.Pop, breaks=c(0,100,250,1000,5000,100000), labels = c('0-99','100-249', '250-999', '1000-4999', '5000+'), include.lowest = TRUE)
YU_mat$Cat <- cut(YU_mat$Peak.Pop, breaks=c(0,100,250,1000,5000,100000), labels = c('0-99','100-249', '250-999', '1000-4999', '5000+'), include.lowest = TRUE)
SW_mat$Cat <- cut(SW_mat$Peak.Pop, breaks=c(0,100,250,1000,5000,100000), labels = c('0-99','100-249', '250-999', '1000-4999', '5000+'), include.lowest = TRUE)

g1 <- ggplot(subset(SV_mat, !is.na(Cat)), aes(x=Cat, y=Estimated.Total.Span)) +
  geom_boxplot(fill="#660000",color='gray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence (Years)") +
  ggtitle("Santa Valley") +
  theme_bw()

g2 <- ggplot(subset(BOM_mat, !is.na(Cat)), aes(x=Cat, y=Estimated.Total.Span)) +
  geom_boxplot(fill="#a2fb3d",color='gray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence (Years)") +
  ggtitle("Basin of Mexico") +
  theme_bw()

g3 <- ggplot(subset(YU_mat, !is.na(Cat)), aes(x=Cat, y=Estimated.Total.Span)) +
  geom_boxplot(fill="#4586fa",color='gray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence (Years)") +
  ggtitle("Yautepec Valley") +
  theme_bw()

g4 <- ggplot(subset(SW_mat, !is.na(Cat)), aes(x=Cat, y=Estimated.Total.Span)) +
  geom_boxplot(fill="#f9ba39",color='darkgray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence (Years)") +
  ggtitle("U.S. Southwest") +
  theme_bw()


ggarrange(g1,g2,g3,g4,nrow=2,ncol=2)


## Figure 7
SW_mat2 <- SW_mat[which(SW_mat$End < 1500),]
SW_mat2$PF <- SW_mat2$Estimated.Total.Span/(max(SW_mat2$End)-SW_mat2$Begin)
SW_mat2$Cat2 <- cut(SW_mat2$Peak.Pop, breaks=c(0,50,100,250,500,100000), labels = c('0-49','50-99', '100-249', '250-499', '500+'), include.lowest = TRUE)

ggplot(data = SW_mat2,aes(x = Begin,y = Peak.Pop)) + 
  geom_point() + 
  geom_smooth(method="auto") +
  ggtitle('Southwest U.S.') +
  theme_bw() +
  theme(text = element_text(size=16)) +
  scale_x_continuous(limits = c(500, 1500)) +
  labs(y = 'Peak Population of Settlement', x = 'Earliest Date A.D.') 

## Figure 8
ggplot(subset(SW_mat2, !is.na(Cat)), aes(x=Cat, y=PF)) +
  geom_boxplot(fill="#f9ba39",color='darkgray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence as Proportion of Possible") +
  ggtitle("U.S. Southwest") +
  theme_bw()  

## Figure 9
BOM_mat2 <- BOM_mat[which(rowSums(BOM_mat[,3:9])>0),]

ggplot(subset(BOM_mat2, !is.na(Cat)), aes(x=Cat, y=Estimated.Total.Span)) +
  geom_boxplot(fill="#a2fb3d",color='gray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence (Years)") +
  ggtitle("Basin of Mexico") +
  theme_bw()


## Figure 2 Code

## Figure 2
ngroup <- 8
BOM_res <- NULL
for (i in 1:ngroup) {
  temp <- BOM_mat[which(BOM_mat[,i+2]>0),]
  BOM_res[i] <- mean(temp$Estimated.Total.Span,na.rm=T)
}
BOM_per <- cbind(BOM_res,c(540,700,200,300,500,300,250,370))

ngroup <- 12
YU_res <- NULL
for (i in 1:ngroup) {
  temp <- YU_mat[which(YU_mat[,i+2]>0),]
  YU_res[i] <- mean(temp$Estimated.Total.Span,na.rm=T)
}
YU_per <- cbind(YU_res,c(400,600,400,300,100,150,150,250,300,150,140,80))

ngroup <- 10
SV_res <- NULL
for (i in 1:ngroup) {
  temp <- SV_mat[which(SV_mat[,i+2]>0),]
  SV_res[i] <- mean(temp$Estimated.Total.Span,na.rm=T)
}
SV_per <- cbind(SV_res,c(1200,1300,400,200,400,450,150,200,370,62))


ngroup <- 11
CI_res <- NULL
for (i in 1:ngroup) {
  temp <- CI_mat[which(CI_mat[,i+2]>0),]
  CI_res[i] <- mean(temp$Estimated.Total.Span,na.rm=T)
}
CI_per <- cbind(CI_res,c(600,400,280,270,170,230,320,130,200,200,700))


site_dat <- rbind(cbind(BOM_per,rep("Basin of Mexico",nrow(BOM_per))),
                  cbind(YU_per,rep("Yautepec Valley",nrow(YU_per))),
                  cbind(CI_per,rep("Central Italy",nrow(CI_per))),
                  cbind(SV_per,rep("Santa Valley",nrow(SV_per))))

site_dat <- as.data.frame(site_dat)
colnames(site_dat) <- c("MeanLength","PerLength","Region")
site_dat$MeanLength <- as.numeric(site_dat$MeanLength)
site_dat$PerLength <- as.numeric(site_dat$PerLength)
site_dat$Region <- as.factor(site_dat$Region)


ggplot(data=site_dat) +
  geom_point(aes(x=MeanLength,y=PerLength, col=Region), size=3, alpha=0.5) +
  geom_smooth(aes(x=MeanLength,y=PerLength, col=Region, fill=Region)) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  xlab("Mean Estimated Occupation Length for All Sites that Intersect with Phase") +
  ylab("Phase Length") +
  theme_bw()


## supplemental figures

library(plotrix)

cerplot <- function(x,interval=200,ymult=4,year.size=NULL,type.size=NULL) { 
  
  xrange <- c(floor(min(x$Begin)/100)*100-interval,ceiling(max(x$End)/100)*100
              +interval)
  par(oma=c(0.5,1,0.5,1))
  plot(0, xlim=c(0, (xrange[2]-xrange[1])+0.5), ylim=c(0, dim(x)[1]+0.5), axes=FALSE, type="n", xlab="", ylab="",main=x$Site[1])
  
  x <- x[order(x$Begin),]
  z <- x$Count/sum(x$Count)
  years <- seq(xrange[1],xrange[2],by=interval)-xrange[1]
  y1 <- seq(1:dim(x)[1])-((z/2)*ymult) 
  y2 <- seq(1:dim(x)[1])+((z/2)*ymult)
  x1 <- x$Begin-(xrange[1]-1)
  x2 <- x$End-(xrange[1]-1)
  if(length(type.size)==0) {type.size <- (100-(ceiling(dim(x)[1]/5)*5))/100}
  if(length(year.size)==0) {year.size <- ((100-(ceiling(length(years)/5)*5))/100)+0.2}
  
  for(i in 1:length(years)) {
    abline(v=years[i],lty=2,lwd=0.5,col='gray')}
  for (i in 1:dim(x)[1]) {
    rect(x1[i],y1[i],x2[i],y2[i],col=col1[round(x$Count[i]*100)],border='black')}
  
  par(xpd=T)
  staxlab(1,years,years+xrange[1],cex=year.size)
  text(x1-0.1,seq(1:dim(x)[1]),labels=x$Type,pos=2,cex=type.size)
  par(xpd=F)}


col1 <- heat.colors(100, rev=T)

x <- read.csv("Ceramic_plot_test.csv",header=T)

x$Count <- as.numeric(c(colSums(YU_o)/nrow(YU_o),colSums(BOM_o)/nrow(BOM_o),colSums(CI_o)/nrow(CI_o),colSums(SV_o)/nrow(SV_o)))


by(x,x$Site,cerplot)

