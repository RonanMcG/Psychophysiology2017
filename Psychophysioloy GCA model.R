# install and load required packages #
install.packages("gridExtra")
install.packages("multcomp")
install.packages("car")
install.packages("lme4")
install.packages("reshape")
install.packages("ggplot2")
install.packages("stringr")
install.packages("reshape2")
install.packages("plyr")
install.packages("dplyr")
install.packages("Hmisc")

library(gridExtra)
library(ggplot2)
library(lme4)
library(car)
library(multcomp)
library(reshape)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(Hmisc)

## Create new data frame for speech processing (after baseline) only (Note, 2 = 2 '500 ms', i.e., 1 sec)
BlockStudyMergedNew <- subset(BlockStudyMerged, Second>2)
BlockStudyMergedNew$SecondNew <- BlockStudyMergedNew$Second-2
BlockStudyMergedNew$Second <- BlockStudyMergedNew$SecondNew
BlockStudyMergedNew$SecondNew <- NULL

## changing labels for easier coding ##
BlockStudy <- BlockStudyMergedNew
BlockStudy$PupilSize <- BlockStudy$PupilBaseCorrect
BlockStudy$PupilBaseCorrect <- NULL

# changing variables to factors for GCA format
BlockStudy$ConditionFactor <- as.factor(BlockStudy$Condition)
BlockStudy$Condition <- NULL
BlockStudy$Condition <- BlockStudy$ConditionFactor
BlockStudy$ConditionFactor <- NULL

BlockStudy$HalfFactor <- as.factor(BlockStudy$Half)
BlockStudy$Half <- NULL
BlockStudy$Half <- BlockStudy$HalfFactor
BlockStudy$HalfFactor <- NULL


## Add polynomial terms to data frame ##
BlockStudyGCA <- BlockStudy
t <- poly(unique(BlockStudyGCA$Second), 3) 
BlockStudyGCA[,paste("ot", 1:3, sep="")] <- t[BlockStudyGCA$Second, 1:3] # adds linear, quadratic, and cubic polynomial terms

# An alternative optimizer. Try 'bobyqa' if model produces convergence warnings #
library(nloptr)
defaultControl <- list(algorithm="NLOPT_LN_BOBYQA",xtol_rel=1e-6,maxeval=1e5)
nloptwrap2 <- function(fn,par,lower,upper,control=list(),...) {
  for (n in names(defaultControl)) 
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
    res <- nloptr(x0=par,eval_f=fn,lb=lower,ub=upper,opts=control,...)
    with(res,list(par=solution,
                  fval=objective,
                  feval=iterations,
                  conv=if (status>0) 0 else status,
                  message=message))
}

## Run GCA model ##
m.fullCubic <- lmer(PupilSize ~ (ot1+ot2+ot3) * Condition * Half + (ot1+ot2+ot3|Subject) + 
                      (ot1+ot2+ot3|Subject:Condition:Half) + (ot1+ot2+ot3|Trial:Condition:Half), 
                    data=BlockStudyGCA, control=lmerControl(optimizer="nloptwrap2"), REML=TRUE)

# Obtain Model coefficients #
coefs.full <- as.data.frame(coef(summary(m.fullCubic)))
coefs.full$p <- format.pval(2*(1-pnorm(abs(coefs.full[,"t value"]))))

## Run submodels to clarify interaction ##
BlockStudyFirstHalf <- subset(BlockStudyGCA, Half=="1st")
BlockStudySecondHalf <- subset(BlockStudyGCA, Half=="2nd")

m.fullCubicFirstHalf <- lmer(PupilSize ~ (ot1+ot2+ot3) * Condition + 
                               (ot1+ot2+ot3|Subject) + (ot1+ot2+ot3|Subject:Condition) + 
                               (ot1+ot2+ot3|Trial:Condition), 
                             data=BlockStudyFirstHalf, control=lmerControl(optimizer="bobyqa"), REML=TRUE)
m.fullCubicSecondHalf <- lmer(PupilSize ~ (ot1+ot2+ot3) * Condition + 
                                (ot1+ot2+ot3|Subject) + (ot1+ot2+ot3|Subject:Condition) + 
                                (ot1+ot2+ot3|Trial:Condition), data=BlockStudySecondHalf, 
                              control=lmerControl(optimizer="bobyqa"), REML=TRUE)
coefs.FirstHalf <- as.data.frame(coef(summary(m.fullCubicFirstHalf)))
coefs.FirstHalf$p <- format.pval(2*(1-pnorm(abs(coefs.FirstHalf[,"t value"])))) # add p value using the normal distribution for approximation ##
coefs.SecondHalf <- as.data.frame(coef(summary(m.fullCubicSecondHalf)))
coefs.SecondHalf$p <- format.pval(2*(1-pnorm(abs(coefs.SecondHalf[,"t value"])))) # add p value using the normal distribution for approximation ##

# Correlation analyses using GCA metrics from Mirman (2014) #

## Retrieve random effects for individual differences in pupil response ##
ranef(m.fullCubic)

## calculating linear (interaction) effect sizes for each subject ##
re.id <- colsplit(row.names(ranef(m.fullCubic)$"Subject:Condition:Half"), ":", c("Subject", "Condition", "Half"))
RandomEffects <- data.frame(re.id, ranef(m.fullCubic)$"Subject:Condition:Half") # Creating dataframe with random effects (from p.132-133 Mirman GCA book)
RandomEffectsSecondHalf <- subset(RandomEffects, Half=="2nd")
PupilLinearEffectSizes <- ddply(RandomEffectsSecondHalf, .(Subject), 
                                summarize, LinearEffect = ot1[Condition=="Hard"] - ot1[Condition=="Easy"])

# Enter PupilLinearEffectSizes into correlation analysis with other variables (e.g., self-reported fatigue) #