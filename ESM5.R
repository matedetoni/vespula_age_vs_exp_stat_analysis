### R Script for the statistical analysis of "Older, but not wiser: the effects of age and experience in Vespula nest defence behaviour at the colony level" ###

### Study by Detoni, M.; Adams, C.I.M.; Johnson, S.L. & Jandt, J.M.; script by Detoni, M. ###

## Loading required statistical packages:
# "ggpubr" allows the visualization of data density and graphic normality checking:
library(ggpubr)
# "lme4" creates GLMs, LMMs and NLMs:
library(lme4)
# "lmerTest" estimates p-values and significance level for fixed effects in LMMs in "lme4":
library(lmerTest)
# "emmeans" allows post-hoc analysis on model ANOVAs:
library(emmeans)
# "MASS" allows estimating DFs from individual effects in GLMMs:
library(MASS)

## Reading dataset files:
# Dataset "defence", which includes observations of first, middle and last weeks:
defence <- read.csv("ESM4.csv")

## Drawing Q-Q plots of variables  to  graphically verify the normality of their distribution:
ggqqplot(defence$rsp)
ggqqplot(defence$actv)
ggqqplot(defence$humid)
ggqqplot(defence$temp)
ggqqplot(defence$delay)

## 1) Looking for correlations between covariates that might be later used in the aggression models:

# LMM 1: Can temperature ("temp") be predicted by week ("humid")?
lmm1 <- lmer(temp ~ wk + (1 | id), data = defence)
summary (lmm1)
anova(lmm1)
# Week is a singifficant predictor of temperature (p<0.01)

# LMM 2: Can humididity ("humid") be predicted by week ("wk")?
lmm2 <- lmer(humid ~ wk + (1 | id), data = defence)
summary(lmm2)
anova(lmm2)
# Week ("wk") is not a significant predictor of humidity ("humid") (p > 0.05).
# Week ("wk") may be used in the model as a proxy for temperature ("temp"), but not for humidity ("humid").

# GLMM 1A: Can a colony's pre-disturbance foraging activity ("actv") be predicted by week ("wk"), treatment ("trt"), their interaction factor ("wk*trt") or humidity ("humid")?
glmm1a <- glmer(actv ~ wk*trt + humid + (1 | id), data = defence, family = poisson(link="log"))
summary(glmm1a)
anova(glmm1a)
# Estimating DFs for fixed effects in a sepparate model:
summary(glmmPQL(actv ~ wk*trt + humid, random = ~ 1 | id, family = poisson(link="log"), data = defence))
# Calclulating p-values for the model's ANOVA output (https://www.socscistatistics.com/pvalues/fdistribution.aspx): p(wk*trt) = 0.04.
# Interaction term is non-significant after a Bonferroni correction (see GLMM 2). Dropping the term from the model. 
# GLMM 1B: Can a colony's pre-disturbance foraging activity ("actv") be predicted by week ("wk"), treatment ("trt"), or humidity ("humid")?
glmm1b <- glmer(actv ~ wk + trt + humid + (1 | id), data = defence, family = poisson(link="log"))
summary(glmm1b)
anova(glmm1b)
# Estimating DFs for fixed effects in a sepparate model:
summary(glmmPQL(actv ~ wk + trt + humid, random = ~ 1 | id, family = poisson(link="log"), data = defence))
# Calclulating p-values for the model's ANOVA output (https://www.socscistatistics.com/pvalues/fdistribution.aspx): p(wk) < 0.001; p(trt) = 0.274; p(humid) = 0.0102.
# Pre-disturbance foraging activity ("actv") can be predicted by week ("wk") and  humidity ("humid"), even after a Bonferroni correction.  Treatment ("trt") is not a significant predictor.
# Post hoc Estimated Marginal means to analyse the interaction of pre-disturbance activity ("actv") with week ("wk"):
emmeans(glmm1b, pairwise ~ wk, adjust="Bonferroni")
# All weeks differ singificantly from each other.

#GLMM 2: Can a colony's pre-disturbance foraging activity ("actv") be predicted by temperature ("temp") or treatment ("trt")?
glmm2 <- glmer(actv ~ trt + temp + (1 | id), data = defence, family = poisson(link="log"))
summary(glmm2)
anova(glmm2)
# Estimating DFs for fixed effects in a sepparate model:
summary(glmmPQL(actv ~ trt + temp, random = ~ 1 | id, family = poisson(link="log"), data = defence))
# Calclulating p-values for the model's ANOVA output (https://www.socscistatistics.com/pvalues/fdistribution.aspx): p(trt) = 0.274; p(temp) < 0.00001.
# Temperature ("temp"), but not treatment ("trt"), is a predictor of pre-disturbance colony activity.

## 2) Looking for predictors of colony nest defence behaviour:

# LMM 3A: Can defence flight duration ("rsp") be predicted by week ("wk"), treatment ("trt"), their interaction ("wk*trt"), or humidity ("humid")?
lmm3a <- lmer(rsp ~ wk*trt + humid + (1 | id), data = defence)
summary(lmm3a)
anova(lmm3a)
# Interaction term (wk*trt) and humidity ("humid") were not significant (p > 0.05). Dropping both from the model.
# LMM 3B: an defence flight duration ("rsp") be predicted by week ("wk") or treatment ("trt")?
lmm3b <- lmer(rsp ~ wk + trt  + humid + (1 | id), data = defence)
summary(lmm3b)
anova(lmm3b)
# Week ("wk", p = 0.03809), but not treatment ("trt"), is a predictor of defence flight duration.
# Post hoc Estimated Marginal Means to analyze the interaction of defence flight duration ("rsp") with week("wk"):
emmeans(lmm3b, pairwise ~ wk, adjust = "Bonferroni")
# First week is significantly different from last week. Other relationships are non-significant.

# LMM 4: Can defence flight duration ("rsp") be predicted by activity ("actv")?
lmm4 <- lmer(rsp ~ actv + (1 | id), data = defence)
summary(lmm4)
anova(lmm4)
# Activity ("actv") is not a predictor of defence flight duration ("rsp") (p > 0.05).

# GLMM 3A: can target strikes ("stk") be predicted by colony age ("wk"), treatment ("trt"), their interaction (wk*trt), or humidity ("humid")?
glmm3a <- glmer(stk ~ wk*trt + humid + (1 | id), family = poisson(link = "log"),  data = defence)
summary(glmm3a)
anova(glmm3a)
# Estimating DFs for fixed effects in a sepparate model:
summary(glmmPQL(stk ~ wk*trt + humid, random = ~ 1 | id, family = poisson(link="log"), data = defence))
# Calculating p-values for the model's ANOVA output (https://www.socscistatistics.com/pvalues/fdistribution.aspx): p(wk*trt) = 1.
# The interaction between week and treatment ("wk*trt") is not a significant predictor of target strikes ("stk"). Dropping the term from the model.
glmm3b <- glmer(stk ~ wk + trt + (1 | id), family = poisson(link = "log"),  data = defence)
summary(glmm3b)
anova(glmm3b)
# Estimating DFs for fixed effects in a separate model:
summary(glmmPQL(stk ~ wk + trt + humid, random = ~ 1 | id, family = poisson(link="log"), data = defence))
# Calculating p-values for the model's ANOVA output (https://www.socscistatistics.com/pvalues/fdistribution.aspx): p(wk)=0.072; p(trt)=0.184; p(humid)=0.374.
# None of the covariates are significant predictors of target strikes ("stk").

# GLMM 4: can target strikes ("stk") be predicted by pre-disturbance foraging activity ("actv")?
glmm4 <- glmer(stk ~ actv + (1 | id), family = poisson(link = "log"),  data = defence)
summary(glmm4)
anova(glmm4)
# Estimating DFs for fixed effects in a sepparate model:
summary(glmmPQL(stk ~ actv, random = ~ 1 | id, family = poisson(link="log"), data = defence))
# Calculating p-values for the model's ANOVA output (https://www.socscistatistics.com/pvalues/fdistribution.aspx): p(actv) < 0.0001.
# Activity is a significant predictor of target strikes (p < 0.05).

# LM 1a: Can defence flight duration at the last trial ("rsp") be predicted by treatment ("trt"), total nest area ("narea"), or their interaction ("narea*trt") measured after nest excavation?
lm1a <- lm(rsp ~ narea*trt,  data = defence)
summary(lm1a)
anova(lm1a)
# Interaction term ("narea*trt") is non-significant. Dropping the term from the model.
# LM 1B: Can defence flight duration at the last trial ("rsp") be predicted by treatment ("trt") total nest area ("narea") measured after nest excavation?
lm1b <- lm(rsp ~ narea + trt,  data = defence)
summary(lm1b)
anova(lm1b)
# Neither treatment ("trt") nor nest size ("narea") were could predict defence flight duration at the last trial.

####
