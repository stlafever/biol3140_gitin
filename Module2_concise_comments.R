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

library(features)
#stores f as the critical points and curvature of xy plot, filters out only neg curvatures(peaks), rounds them to integers so they match the frames
find.peaks <- function(x, y, mult=100){
  f <- fget(features(x= x, y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

#create tibble with max amplitude of each oscillation
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T)

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
#[CPK] but you repeat this below
pseed.sum.max %>%
  group_by(fish, bl.s) %>%
  summarize(amp.sum.mean=mean(amp.sum)) %>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")

#add new column with the mean maximum for each speed for each fish
# pseed.sum.max <- pseed.sum.max%>%
#   group_by(fish, bl.s)%>%
#   mutate(amp.sum.mean=mean(amp.sum))

#standard error of the mean function
find.sem <- function(x){
  sd(x)/sqrt(length(x))
}

#[CPK] you could have just done this mutate along with the mean . . . 

pseed.sum.max <- pseed.sum.max%>%
  group_by(fish, bl.s)%>%
  mutate(amp.sum.mean=mean(amp.sum),amp.sum.se=find.sem(amp.sum))

# #add new column for se
# pseed.sum.max <- pseed.sum.max%>%
#   group_by(fish, bl.s)%>%
#   mutate(amp.sum.se=find.sem(amp.sum))

#[CPK] dont' we wont to again summarize by fish and speed again

#load and merge metabolic data
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate)%>%
  group_by(fish, bl.s)%>%
  mutate(amp.sum.mean=mean(amp.sum),
         amp.sum.se=find.sem(amp.sum),
         met.mean=mean(met.rate),
         met.se=find.sem(met.rate))


#plot metabolic data - this is definitely wrong
#[CPK] why would you have error bars that reflect met.rate +/- amp SEM?, try this.
pseed.sum.max%>%
  ggplot(aes(x=amp.sum.mean,y=met.mean, col=fish))+geom_errorbar(aes(ymin=met.mean-met.se, ymax=met.mean+met.se), width=.05)+geom_point()+geom_smooth(method="lm")

#[CPK] good work, just think more about the goal of the operations. 10/10

