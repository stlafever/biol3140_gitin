<<<<<<< HEAD
###pseed.wide data tibble
#Load data sets
library(ggplot2)
=======
>>>>>>> 2ce9062446372db3c236bc9f67faef400c921c6e
library(tidyverse)
library(features)

#load data
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
<<<<<<< HEAD
#Merging data
=======

#combine pseed and speeds table
>>>>>>> 2ce9062446372db3c236bc9f67faef400c921c6e
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))

#combine pseed2 with pseed.bl
pseed2 <- pseed2%>%
<<<<<<< HEAD
  left_join(pseed.bl,by="fish")%>%
  print()
#Mutating to specific speed
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
#Plots
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()
#Writing function to find max amplitude
library(features)
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
fget(f1)
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()
pseed2%>%
  summarize(n=length(unique(date)))
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
=======
  left_join(pseed.bl, by="fish")

#compute new bl.s column with specific speed
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)

#create custom function to find max amplitude for each fin during each oscillation
find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
>>>>>>> 2ce9062446372db3c236bc9f67faef400c921c6e
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

#create tibble of peak amplitudes for each oscillation
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter


#create amplitude column for left and right fins and new column for sum
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
<<<<<<< HEAD
  mutate(amp.sum=L+R)%>%
  print() 
###Compute mean maximum
pseed.sum.max <- pseed.wide %>%
  group_by(date,fish)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.sum.max%>%
  ggplot(aes(x=bl.s,y=amp.sum))+geom_point()+geom_smooth(method="lm")
pseed.sum.max %>%
  group_by(fish, bl.s) %>%
  summarize(amp.sum.mean=mean(amp.sum)) %>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")
amp.sum.aov <- aov(amp.sum~bl.s,pseed.sum.max)
summary(amp.sum.aov)
getwd()
=======
  mutate(amp.sum=L+R)

#create tibble of peak amplitudes for each oscillation
pseed.sum.max <- pseed.wide%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) #new filter
pseed.sum.max

#standard error function
standard_error <- function(x) sd(x)/sqrt(length(x))

#add standard error column to pseed.sum.max
pseed.sum.max <- pseed.sum.max %>%
  group_by(fish, bl.s)%>%
  mutate(amp.sum.se=standard_error(amp.sum))

#compute means for each speed and each fish
pseed.sum.max <- pseed.sum.max %>%
  group_by(fish, bl.s) %>%
  mutate(amp.sum.mean=mean(amp.sum))

#plot pseed.sum.max
pseed.sum.max%>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), colour="black", width=.1)

#read in new data file
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#join new data file with pseed.sum.max
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate, by=NULL)

#plot met.rate vs amp.sum.mean
pseed.sum.max %>%
  ggplot(aes(x=amp.sum.mean, y=met.rate,col=fish))+geom_point()+geom_smooth(method="lm")
>>>>>>> 2ce9062446372db3c236bc9f67faef400c921c6e
