library(tidyverse) 
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
#Q1: establish anole.log
anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

#read in anole.tree
anole.tree <- read.tree("anole.tre")

#lm accounting for perch height
anole.log.lm.ph  <- lm(HTotal~SVL+PH,anole.log)

#lm accounting for perch diameter
anole.log.lm.pd <- lm(HTotal~SVL+ArbPD, anole.log)


#Q2: Construct two simple linear models
#add column with anole.log.lm.ph residuals
anole.log <- anole.log %>%
  mutate(res.ph=residuals(anole.log.lm.ph))
#add column with anole.log.lm.pd residuals
anole.log <- anole.log %>%
  mutate(res.pd=residuals(anole.log.lm.pd))

#Q3: Plot residuals of linear models against covariates
#plot perch height residuals vs Ecomorphs
anole.log%>%
  ggplot(aes(x=PH,y=res.ph)) +geom_point()+geom_smooth(method="lm")
#plot perch diameter residuals vs Ecomorphs
anole.log%>%
  ggplot(aes(x=ArbPD,y=res.pd)) + geom_point()+geom_smooth(method="lm")

#Q4: Construct phylogenetic least squares models of the relationships
#PGLS under BM, w perchh height
pgls.BM1 <- gls(HTotal~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
#PGLS under BM, w perch diameter
pgls.BM2 <- gls(HTotal~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
#PGLS under BM, w perch height and diameter
pgls.BM3 <- gls(HTotal~SVL+PH+ArbPD, correlation = corBrownian(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#Q5: Assess the three models using AICc and AICw
anole.aic <- AICc(pgls.BM0,pgls.BM1, pgls.BM2, pgls.BM3) %>% print()
anole.aicw <- aicw(anole.aic$AICc)
print(anole.aicw)
#perch diameter is a significant predictor of hindlimb length (delta AIC ~ 10) but perch height is not

#Add phylogenetic residuals to anole.log
anole.log <- anole.log %>% mutate(phylo.res1=residuals(pgls.BM1))
anole.log <- anole.log %>% mutate(phylo.res2=residuals(pgls.BM2))
anole.log <- anole.log %>% mutate(phylo.res3=residuals(pgls.BM3))
anole.log %>% head()

#Q6: Plot effect of perch diameter of effect perch diameter on hindlimb residuals
#I used the residual of the model that just took into account perch diameter because
#while the model that took both into account was slightly better, the difference was small
#and it used more parameters
anole.log%>%
  dplyr::select(ArbPD,res.pd,phylo.res2)%>%
  pivot_longer(cols=c("res.pd","phylo.res2"))%>%
  print%>%
  ggplot(aes(x=ArbPD,y=value)) +geom_point() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")+geom_smooth(method="lm")