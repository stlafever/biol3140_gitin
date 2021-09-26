#load library
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

#merge anole with anole.eco and filtering out incomplete data
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in% c("U","CH"))%>%
  na.omit()

#log transformation of data to proportional representations
anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#visualizing data
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")

#linear model to predict HTotal
anole.lm <- lm(HTotal~SVL,anole2)
coef(anole.lm)
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")

#creating tibble with predictions
SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)
pred.lm <-tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2)))
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")
summary(anole.lm)

#non-linear model to predict HTotal
anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data = anole2)
summary(anole.allo)

#AICc from the MuMIn package
anole.aic <- AICc(anole.lm,anole.allo)

#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)
print(anole.aicw) #lower value indicates better fit

#likelihood
logLik(anole.lm)
logLik(anole.allo) #higher likelihood = modelthatfitsbest

#effect of ecomorph on the hindlimb-SVL
anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")

#model with Ecomorph2 variable (assessing the effect of a categorical variable)
anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
summary(anole.log.eco.lm)
anova(anole.log.eco.lm)

#model with the added Ecomorph2 parameter
anole.log.lm  <- lm(HTotal~SVL,anole.log)
anova(anole.log.lm)
anole.log.aic <- AICc(anole.log.lm,anole.log.eco.lm)
aicw(anole.log.aic$AICc)

#compute the residuals based on our global anole.log.lm model
anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))
#dot plot
anole.log%>%
  ggplot(aes(Ecomorph2,res))+geom_point()
#boxplot
p.eco <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res)) +geom_boxplot()
p.eco+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

#phylogeny
anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

#PGLS under BM, no ecomorph
pgls.BM1 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
#PGLS under BM, w ecomorph
pgls.BM2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
#PGLS under OU, no ecomorph
pgls.OU1 <- gls(HTotal ~SVL, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
#PGLS under OU, w, ecomorph
pgls.OU2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#seeing which model fits the data best
anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.OU1,pgls.OU2)
aicw(anole.phylo.aic$AICc)
anova(pgls.BM2)

#plotting more with less 
anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")

#model considering phylogeny
anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM2))
p.eco.phylo <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=phylo.res)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)
print(p.eco.phylo)

#plotting corrected residuals against uncorrected
anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")


###Project Report
#1) Combine the code above so that you can establish the anole.log data tibble.

#2) Linear models
anole.log.lm.pd <- lm(HTotal~SVL+ArbPD, anole.log)
anole.log.lm.ph  <- lm(HTotal~SVL+PH,anole.log)
anole.log.lm.pd%>%
  ggplot(aes(SVL+ArbPD,HTotal))+geom_point()+geom_smooth(method="lm")
anole.log.lm.ph%>%
  ggplot(aes(SVL+PH,HTotal))+geom_point()+geom_smooth(method="lm")

#3) Plot residuals of linear models
anole.log <- anole.log %>%
  mutate(res.pd=residuals(anole.log.lm.pd))
anole.log <- anole.log %>%
  mutate(res.ph=residuals(anole.log.lm.ph))
anole.log%>%
  ggplot(aes(x=ArbPD,y=res.pd)) + geom_point()#+geom_smooth(method="lm")
anole.log%>%
  ggplot(aes(x=PH,y=res.ph)) +geom_point()#+geom_smooth(method="lm")

#4) Construct phylogenetic least squares models
pgls.BM1 <- gls(HTotal~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM2 <- gls(HTotal~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM3 <- gls(HTotal~SVL+PH+ArbPD, correlation = corBrownian(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#5) Assess with AICc and AICw
anole.aicc <- AICc(pgls.BM1, pgls.BM2, pgls.BM3) %>% print()
anole.aicw <- aicw(anole.aic$AICc)
print(anole.aicw)

#6) Produce plot that visualizes effect of covariants on hindlimb residuals
