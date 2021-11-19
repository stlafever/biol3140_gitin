library(tidyverse)
library(features)

#load data
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#combine pseed and speeds table
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))

#combine pseed2 with pseed.bl
pseed2 <- pseed2%>%
  left_join(pseed.bl, by="fish")

#compute new bl.s column with specific speed
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

#create custom function to find max amplitude for each fin during each oscillation
find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
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