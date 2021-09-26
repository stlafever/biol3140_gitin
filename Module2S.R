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
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (rember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store resutls in `f` and compute the features for the x-y relationship, wrep in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that rturns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

#create tibble of peak amplitudes for each oscillation
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter
pseed.max

#compute means for each speed and each fish
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl))

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
pseed.sum.max  %>% head()

#compute means for each speed and each fish
pseed.sum.max <- pseed.sum.max %>%
  group_by(fish, bl.s) %>%
  mutate(mean.max=mean(amp.sum))

pseed.sum.max %>% head()

#plot pseed.max
pseed.sum.max%>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=mean.max-amp.sum.se, ymax=mean.max+amp.sum.se), colour="black", width=.1)

#read in new data file
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#join new data file with pseed.sum.max
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate, by=NULL)
pseed.sum.max %>% head()

#plot new pseed.sum.max tibble
pseed.sum.max %>%
  ggplot(aes(x=met.rate,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")
