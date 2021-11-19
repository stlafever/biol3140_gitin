#My comments begin wit [CPK]

library(ggplot2)
library(tidyverse)
#line creating dat variable with scales info
# [CPK] I can't run this code without a scales.csv file in the repo/project directory!!!!
dat <- read.csv("scales.csv")

# [CPK] from here on, some lines you don't need.  I know the dim of the data and what the first few lines look like, etc. Please stick to addressing the tasks.


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

#Grade=10/10 ("Spot on" for both rubric items)
#Well done.
