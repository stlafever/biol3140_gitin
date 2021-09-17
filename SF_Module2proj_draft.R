setwd("~/Documents/Org Bio/biol3140_gitin/Module_2")
library(tidyverse)
library(ggplot2)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#add the speed in m.s and cm.s from the speeds data set to pseed2 
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

#add the body length of each fish to pseed2
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

#use speed (cm.s) and bl to make new column w/ bl.s
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#plot of amp.bl at different bl.s for one fin of one exp
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

library(features)

exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)

#f2 is same as f1 but with y*100 to see neg values
#saved important data about the amp.bl v frame plot to f2
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

#just looking at the negative critical points (peaks) of exp1 in a tibble
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

#same thing but a plot, change the color when "peak" is true (when a frame exists in the peaks column of f.tib)
##CHECK IF THIS ^^ IS RIGHT
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

#stores f as the critical points and curvature of xy plot, filters out only neg curvatures(peaks), rounds them to integers so they match the frames
find.peaks <- function(x, y, mult=100){
  f <- fget(features(x= x, y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

#makes plot of amp.bl v frame for first three experiments
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl, alpha=peak, col=peak))+geom_point()+facet_grid(date~fin)

#create tibble with max amplitude of each oscillation
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T)

#plot max specific amp v specific speed for all data
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

#test to see of Pr(>F) is lower than 0.05 to show significant correlation
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

#find mean for each speed for each fish and plot
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s, y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#make separate columns for left and right fin
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#make new tibble w only frames that have max sum amplitude and plot
pseed.sum.max <- pseed.wide%>%
  group_by(fish, date)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.sum.max%>%
  ggplot(aes(x=bl.s,y=amp.sum))+geom_point()+geom_smooth(method="lm")

#plot mean sum of max fin amplitudes for each speed for each fish
pseed.sum.max %>%
  group_by(fish, bl.s) %>%
  summarize(amp.sum.mean=mean(amp.sum)) %>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")
amp.sum.aov <-  aov(amp.sum~bl.s,pseed.sum.max)
summary(amp.sum.aov)

#add new column with the mean maximum for each speed for each fish
pseed.sum.max <- pseed.sum.max%>%
  group_by(fish, bl.s)%>%
  mutate(amp.sum.mean=mean(amp.sum))



#computing SEM
  #group based on fish and speed and save as an object
  #run sd on that object
  #find number of observations for each speed and fish
  #divide by sqrt of number of obs

#idk wtf is going on:)
find.sem <- function(s, n){
  sem <- s/sqrt(n)
}

#these add columns for sample size and standard deviation - is the function supposed to skip these steps like huh
#number of observations for each mean
test.n2 <- pseed.sum.max%>%
  group_by(fish, bl.s)%>%
  mutate(n.obs=n())
#standard deviation for each mean
test.n2 <- test.n2%>%
  group_by(fish, bl.s)%>%
  mutate(st.dev=sd(amp.sum))

  
