# Mixed Models
# Thurs 28 January

# Clear R's Brain
rm(list=ls())

# Load required libraries
library(ggplot2)
library(dplyr)
library(lme4)
library(pbkrtest)
library(gridExtra)

# This code reads in the data file soay_mass_data.csv
SoayMassData <- read.csv("~/University/Level 4/APS405 Stats/DataSets/soay_mass_data.csv")
SoayMassData

# Plot total litter mass versus mother's age
ggplot(SoayMassData, aes(x=Ag, y=logM))+
  geom_point(size=4, alpha=0.5, col="steelblue")+
  labs(x="Maternal age (years)", y="Total mass(kg - log scale")+
  theme_classic(base_size=15)

# Make a plot that groups them by individual sheep
# Need to mutate data to rename Id as a factor
SoayMassData <- mutate(SoayMassData, Id=factor(Id))

plot1<-ggplot(SoayMassData, aes(x=Ag, y=logM, colour= Id))+
  geom_point(size=4, alpha=0.5)+
  labs(x="Maternal age (years)", y="Total mass(kg - log scale")+
  facet_wrap(~Id, ncol=5)+
  geom_line()+
  geom_smooth(method=lm, se=FALSE, linetype="dashed")+
  theme_classic(base_size=15)

# An alternative graph...
plot2<-ggplot(SoayMassData, aes(x=Ag, y=logM, colour= Id))+
  geom_point(size=4, alpha=0.5)+
  labs(x="Maternal age (years)", y="Total mass(kg - log scale")+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic(base_size=15)

# Puts them together nicely
grid.arrange(plot1, plot2, ncol=2)

# Divide the data into groups by Id and runs a linear model on each of them
# pulls out the confidence interval and plots it
ci<-confint(lmList(logM~Ag|Id, data=SoayMassData), pooled=TRUE)
plot3<- plot(ci, order=1)
plot3

# The intercepts and gradients of the lines in the left, are plotted on the right.
# Can see an average trend, fitness increases with age.
# Very strong negative correlation between early and later reproduction:
#       Individuals who invest heavily early on suffer later in life,
#       Individuals who invest little early on do better later on in life.
grid.arrange(plot2, plot3, ncol=2)

# Make the full model
offMassMod1 <- lmer(logM ~ Ag + (1 + Ag | Id), data=SoayMassData)

summary(offMassMod1)
# Here's how you make diagnostic plots for Mixed Models
diagnostics <- fortify(offMassMod1)
diagnostics

# Residuals vs fitted plot:
p1<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  theme_classic()
p1

# Q-Q plot
p2<-ggplot(diagnostics, aes(sample= .scresid))+
  stat_qq()+
  geom_abline()+
  theme_classic()
p2

# Resids vs Fits divided up by Id
p3<- ggplot(diagnostics, aes(x= .fitted, y= .resid))+
  geom_point()+
  geom_hline(yintercept= 0, linetype= "dashed")+
  facet_wrap(~Id, ncol=5)+
  theme_classic()
p3

# Put them altogether
grid.arrange(p1, p2, p3, ncol=2)

# lme4 Useful functions
# Fitted values of 'fixed' component
fixef(offMassMod1)

# Random effects (predictors)
ranef(offMassMod1)

# Coefficients (fixed + random)
coef(offMassMod1)

#######
# Testing fixed effects
#######
# Is the fixed effect of age significant?
model1 <- lmer(logM ~ 1 + Ag + (1 + Ag | Id), SoayMassData)
model2 <- lmer(logM ~ 1 +      (1 + Ag | Id), SoayMassData)

# Using a likelihood ratio test
# Defaults to ML based test
# Fixed Effects Comparison
anova(model1, model2)
# yes it is.. because p<0.001

#######
# Testing "random" terms
#######
# Is the among-individual age-slope variation significant?
model1 <- lmer(logM ~ 1 + Ag + (1 + Ag | Id), SoayMassData)
model3 <- lmer(logM ~ 1 + Ag + (1      | Id), SoayMassData) # This is like having multiple parallel lines with different intercepts

# Refit=FALSE PREVENTS ML because random test
anova(model1, model3, refit=FALSE)

# Is the correlation term significant?
model1 <- lmer(logM ~ 1 + Ag + (1 + Ag | Id), SoayMassData)
model4 <- lmer(logM ~ 1 + Ag + (1 + Ag || Id), SoayMassData)
# Two pipes || mean estimate variance of intercepts and slopes independently of each other
# eg: correlation=0
anova(model1, model4, refit=FALSE)
# This proves that sheep that invest more when they're young suffer later on,
# and that those who invest little early on do better later on. 

PBmodcomp(model1, model2, nsim=100)


