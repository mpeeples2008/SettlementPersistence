library(ggplot2)
library(ggridges)
library(lemon)
library(tidyverse)
library(ggpubr)


npp_dat <- read.csv(file="npp_dat.csv")
pop_dat <- read.csv('Occupation_spans.csv',header=T)

## Figure 1




## Figure 2
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


## Figure 3
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

## Figure 4
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

all_c <- cbind(npp_c,pop_c[c(4,5,2,1,7,3,6),-1]) # order objects so thtye are the same and combine

ggplot(data = all_c,aes(x = Median,y = PMedian, color=reorder(region, Median))) + 
  geom_point(size=5) + 
  geom_errorbar(aes(ymin = PIQ_low,ymax = PIQ_high)) + 
  geom_errorbarh(aes(xmin = IQ_low,xmax = IQ_high)) +
  scale_color_viridis_d(option = 'turbo', direction = -1, name = "Region") + 
  ggtitle('NPP vs. Settlement Persistence with Interquartile Range Error Bars') +
  theme_bw() +
  theme() +
  labs(y = 'Settlement persistence (Years)', x = expression(Potential~net~primary~producivity~(g/m^2))) 


## Figure 5
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


## Figure 6
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
  labs(y = 'Peak Population of Settlement', x = 'Earliest Date C.E.') 

## Figure 7
gsw <- ggplot(subset(SW_mat2, !is.na(Cat)), aes(x=Cat, y=PF)) +
  geom_boxplot(fill="#f9ba39",color='darkgray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence as Proportion of Possible") +
  ggtitle("U.S. Southwest") +
  theme_bw()  

BOM_mat2 <- BOM_mat[which(rowSums(BOM_mat[,3:9])>0),]
gbom <- ggplot(subset(BOM_mat2, !is.na(Cat)), aes(x=Cat, y=Estimated.Total.Span)) +
  geom_boxplot(fill="#a2fb3d",color='gray') +
  xlab("Site Size Class") +
  ylab("Settlement Persistence (Years)") +
  ggtitle("Basin of Mexico") +
  theme_bw()

ggarrange(gsw,gbom,ncol=2,nrow=1)


