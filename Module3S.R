library(tidyverse) # Rember to load your libraries!
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

#load data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

#left join so we know which ecomorph data was taken from
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()
#na.omit() omits rows where there are missing values

#remove any unique or close to unique values
anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)
#we use log to see data as proportions


anole.log.lm  <- lm(HTotal~SVL,anole.log)
anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))
anole.log %>% head()

#read in anole.tree
anole.tree <- read.tree("anole.tre")

#lm accounting for perch height
anole.log.lm.ph  <- lm(HTotal~SVL+PH,anole.log)

#lm accounting for perch diameter
anole.log.lm.pd <- lm(HTotal~SVL+ArbPD, anole.log)

#add column with anole.log.lm.ph residuals
anole.log <- anole.log %>%
  mutate(res.ph=residuals(anole.log.lm.ph))

#add column with anole.log.lm.pd residuals
anole.log <- anole.log %>%
  mutate(res.pd=residuals(anole.log.lm.pd))

anole.log %>% head()

#plot perch height residuals vs Ecomorphs
anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res.ph)) +geom_boxplot()

#plot perch diameter residuals vs Ecomorphs
anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res.pd)) +geom_boxplot()

#PGLS under BM, w perchh height
pgls.BM1 <- gls(HTotal~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, w perch diameter
pgls.BM2 <- gls(HTotal~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, w perch height and diameter
pgls.BM3 <- gls(HTotal~SVL+PH+ArbPD, correlation = corBrownian(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

anole.aic <- AICc(pgls.BM1, pgls.BM2, pgls.BM3) %>% print()

anole.aicw <- aicw(anole.aic$AICc)
print(anole.aicw)
#perch diameter is a significant predictor of hindlimb length