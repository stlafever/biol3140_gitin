library(ggplot2)
library(tidyverse)
setwd("C:\\Users\\lafev\\Desktop\\Organismal Biology\\scales\\biol3140_gitin_scales")
#line creating dat variable with scales info
dat <- read.csv("scales.csv")
head(dat)
#line showing class of variables
sapply(dat,class)
#line showing dimension of dataset
dim(dat)
#create species variable
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
#summary of number of scales punctured for each species
species_n <- dat %>%
  group_by(species) %>%
  summarise(n = n())
species_n
#summary of number of specimens sampled for each species
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")
#pdf file 
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()