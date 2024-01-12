library(here)

raw = read.csv(here::here("00-data", "a-raw", "Crawfish-eHf-age.csv"))
str(raw)
head(raw)
## linear mixed effects models for pluton 
## magmatic zircons eHf vs U/Pb age

#### Clean workspace ####
rm(list=ls())

#### Load required packages ####
library(readxl)
library(ggplot2)
library(GGally)
library(carData)
library(stats)
library(car)
library(dplyr)
library(tidyr)
library(lme4)
library(nlme)
library(effects)
library(ggplot2)
library(tidyverse) #y - pipe (%>%)
library(cowplot) #y - cowplot
library(MuMIn)
library(r2glmm)
library(here) #y - here


## Download data and read into R    ##

# Check whether a folder named data exists in the current working directory
if(dir.exists("data")) {
  
  # If it already exists, do nothing.
  
} else {
  
  # If it doesn't exits, make a new folder named data
  dir.create("data")
  
}

## load various required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(carData)
library(effects)
library(stats)
library(car)
library(nlme)
library(lme4)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(MuMIn)
library(r2glmm)

## Geochemical trends through time within the Crawfish
## and Krestof Island plutons

##eHf vs. U/Pb age

# Read downloaded file into dataframes
sbb <- read.csv("data/Crawfish-Ycn-age.csv")

d <- read.csv("data/Crawfish-Ycn-age.csv", stringsAsFactors = TRUE, header = TRUE)
head(d)
attach(d)

d$sample<- factor(d$sample)
d$pluton<- factor(d$pluton)
d$age_Ma<- as.numeric(d$age_Ma)
d$log10_age <- log10(d$age_Ma)
d$log10_age<- as.numeric(d$log10_age)
d$age_Ma_error<- as.numeric(d$age_Ma_error)
d$Y_cn<- as.numeric(d$Y_cn)

### set custom theme for plots ###

custom_theme <- theme_cowplot()
custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))

theme_set(custom_theme)

###Step 1: Check for Outliers###
#Cleveland dotplots for all continuous variables. Each panel corresponds to a variable. 
#The x-axes show the values of a variable and the y-axes the observations, 
op<- par(mfrow=c(3,1),mar=c(3,3,3,1))

dotchart(d$age_Ma,main="U/Pb age (Ma)")
dotchart(d$e_Hf_t,main="zircon eHf")
dotchart(d$e_Hf_t_error,main="zircon eHf error")

#Check for normality
hist(age_Ma)
shapiro.test(age_Ma)

hist(log(age_Ma))
shapiro.test(log(age_Ma))

hist(log10(age_Ma))
shapiro.test(log10(age_Ma))

hist(Y_cn)
shapiro.test(Y_cn)

model <- lm(Y_cn ~ age_Ma, data = d)
model

summary(model)

## Visualize the data ##

vis <- ggplot(d, aes(x=age_Ma, y=e_Hf_t)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=e_Hf_t-e_Hf_t_error, ymax=e_Hf_t+e_Hf_t_error)) +
  geom_smooth(method = lm, fill="lightgray")

print(vis) + scale_x_reverse()



# Read downloaded file into dataframes
sbb <- read.csv("data/sbb-eHf-UPb-pluton-zircons.csv")

d <- read.csv("data/sbb-eHf-UPb-pluton-zircons.csv", stringsAsFactors = TRUE, header = TRUE)
head(d)
attach(d)

d$zircon_sample<- factor(d$zircon_sample)
d$rock_sample<- factor(d$rock_sample)
d$pluton<- factor(d$pluton)
d$age_Ma<- as.numeric(d$age_Ma)
d$log10_age <- log10(d$age_Ma)
d$log10_age<- as.numeric(d$log10_age)
d$e_Hf_t<- as.numeric(d$e_Hf_t)
d$e_Hf_t_error<- as.numeric(d$e_Hf_t_error)

### set custom theme for plots ###

custom_theme <- theme_cowplot()
custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))

theme_set(custom_theme)

###Step 1: Check for Outliers###
#Cleveland dotplots for all continuous variables. Each panel corresponds to a variable. 
#The x-axes show the values of a variable and the y-axes the observations, 
op<- par(mfrow=c(3,1),mar=c(3,3,3,1))

dotchart(d$age_Ma,main="U/Pb age (Ma)")
dotchart(d$e_Hf_t,main="zircon eHf")
dotchart(d$e_Hf_t_error,main="zircon eHf error")

#Check for normality
hist(age_Ma)
shapiro.test(age_Ma)

hist(log(age_Ma))
shapiro.test(log(age_Ma))

hist(log10(age_Ma))
shapiro.test(log10(age_Ma))

hist(e_Hf_t)
shapiro.test(e_Hf_t)


## Visualize the data ##

vis <- ggplot(d, aes(x=age_Ma, y=e_Hf_t)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=e_Hf_t-e_Hf_t_error, ymax=e_Hf_t+e_Hf_t_error)) +
  geom_smooth(fill="lightgray")

print(vis) + scale_x_reverse()

### re-set custom theme for plots ###

library(cowplot)

custom_theme <- theme_cowplot()
custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))

theme_set(custom_theme)


######## SBB plutons eHf magmatic zircons ########
### nlme mixed effects models ###
## comparing different fixed effects structures first ###
model1a<-lme(e_Hf_t ~  log10(age_Ma) + I((log10(age_Ma))^2), random = ~1|pluton  ,method="ML",data=d)  
summary(model1a)
model1b<-lme(e_Hf_t ~  log10(age_Ma) , random = ~1|pluton  ,method="ML",data=d)
summary(model1b)

anova(model1a,model1b)

##Extract r2 value
summary(model1a)
r2 <- r2beta(model=model1a,partial=TRUE,method='sgv')
r2

##Plot model residuals
plot(model1a)
plot(model1a,e_Hf_t~fitted(.))
qqnorm(model1a,~resid(.)|pluton)

##create model predictor variable
d$predicted_e_Hf_t <- predict(model1a, type = "response")

## plot up final lme model
lme <- ggplot(d, aes(x=age_Ma, y=e_Hf_t)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=e_Hf_t-e_Hf_t_error, ymax=e_Hf_t+e_Hf_t_error)) +
  stat_smooth(data=d, aes(age_Ma,(predicted_e_Hf_t)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lme) + scale_x_reverse()

### Effects plot for final lme model
plot(allEffects(model1a))

### Using lmer mixed effects model rather than nlme

#### Comparing different random effects structures
##random intercept
M1 <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton), data = d, REML = TRUE)
summary(M1)

##random slopes
M2 <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (0 + log10_age | pluton) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M2)

M2a <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (0 + log10_age | pluton), data = d, REML = TRUE)
summary(M2a)

M2b <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M2b)

## random slopes and intercepts (slope & intercept calculated together at the grouping level)
M3 <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 + log10_age | pluton) + (1 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M3)

M3a <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 + log10_age | pluton), data = d, REML = TRUE)
summary(M3a)

M3b <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M3b)

## random slopes and intercepts (slope & intercept calculated independently)
M4 <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton) + (0 + log10_age | pluton) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M4)

M4a <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton) + (0 + log10_age | pluton), data = d, REML = TRUE)
summary(M4a)

M4b <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M4b)

anova(M1,M2,M2a,M2b,M3,M3a,M3b,M4,M4a,M4b)
anova(M2a,M4)
### lmer mixed effects models ###
mod1a <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1|pluton), data = d)
summary(mod1a)

##extract coefficients
coefsmod1a <- data.frame(coef(summary(mod1a)))

# use normal distribution to approximate p-value
coefsmod1a$p.z <- 2 * (1 - pnorm(abs(coefsmod1a$t.value)))
coefsmod1a

##Extract r2 value
summary(mod1a)
r2 <- r2beta(model=mod1a,partial=TRUE,method='sgv')
r2

##PLOT res.
plot(mod1a)
plot(mod1a,e_Hf_t~fitted(.))

##create model predictor variable
d$predicted_e_Hf_t2 <- predict(mod1a, type = "response")

## plot up final lme model
lmer <- ggplot(d, aes(x=age_Ma, y=e_Hf_t)) + geom_point(aes(colour = as.factor(rock_sample))) + 
  geom_errorbar(aes(ymin=e_Hf_t-e_Hf_t_error, ymax=e_Hf_t+e_Hf_t_error)) +
  stat_smooth(data=d, aes(age_Ma,(predicted_e_Hf_t2)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lmer) + scale_x_reverse()

## model optimization using LmerTest ###
library(lmerTest)

(aov <- anova(mod1a))

step_result <- step(mod1a)
step_result

final_model <- get_model(step_result)
final_model

summary(final_model)

d$predicted_e_Hf_t <- predict(final_model, type = "response")

fin <- ggplot(d, aes(x=age_Ma, y=e_Hf_t)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=e_Hf_t-e_Hf_t_error, ymax=e_Hf_t+e_Hf_t_error)) +
  stat_smooth(data=d, aes(age_Ma,(predicted_e_Hf_t)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin) + scale_x_reverse()

(aov <- anova(final_model))
summary(final_model)
r2 <- r2beta(model=final_model,partial=TRUE,method='sgv')
r2

fin2 <- ggplot(d, aes(x=age_Ma, y=e_Hf_t)) + geom_point(aes(colour = as.factor(rock_sample))) + 
  stat_smooth(data=d, aes(age_Ma,(predicted_e_Hf_t)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin2) + scale_x_reverse()

##extract coefficients for final optimized model
coefsfinal_model <- data.frame(coef(summary(final_model)))

# use normal distribution to approximate p-value for final optimized model
coefsfinal_model$p.z <- 2 * (1 - pnorm(abs(coefsfinal_model$t.value)))
coefsfinal_model

######## eHf sedimentary zircons versus 
######## U/Pb pluton ages of SBB units ########
######## CPW sediment detrital zircons ########

# Read downloaded file into dataframes
sbb2 <- read.csv("data/sbb-eHf-UPb-sediment-zircons-540Ma.csv")

d <- read.csv("data/sbb-eHf-UPb-sediment-zircons-540Ma.csv", stringsAsFactors = TRUE, header = TRUE)
head(d)
attach(d)

## denoting data type within matrix
d$zircon_sample<- factor(d$zircon_sample)
d$rock_sample<- factor(d$rock_sample)
d$formation<- factor(d$formation)
d$location<- factor(d$location)
d$age_Ma_plutons<- as.numeric(d$age_Ma_plutons)
d$log10_age_plutons <- log10(d$age_Ma_plutons)
d$log10_age_plutons<- as.numeric(d$log10_age_plutons)
d$age_Ma_plutons_error<- as.numeric(d$age_Ma_plutons_error)
d$age_Ma_seds<- as.numeric(d$age_Ma_seds)
d$e_Hf_50Ma<- as.numeric(d$e_Hf_50Ma)
d$e_Hf_50Ma_error<- as.numeric(d$e_Hf_50Ma_error)

### set custom theme for plots ###

library(cowplot)

custom_theme <- theme_cowplot()
custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))

theme_set(custom_theme)


#Check for normality
hist(log10(age_Ma_plutons))
shapiro.test(log(age_Ma_plutons))

hist(log10(e_Hf_50Ma + 100))
shapiro.test(e_Hf_50Ma)

## Plot the data ##

y <- ggplot(d, aes(x=age_Ma_seds, y=e_Hf_50Ma)) + geom_point(aes(colour = as.factor(location))) + 
  geom_errorbar(aes(ymin=e_Hf_50Ma-e_Hf_50Ma_error, ymax=e_Hf_50Ma+e_Hf_50Ma_error)) +
  geom_smooth(fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(y) + scale_x_reverse()

z <- ggplot(d, aes(x=age_Ma_plutons, y=e_Hf_50Ma)) + geom_point(aes(colour = as.factor(location))) + 
  geom_errorbar(aes(ymin=e_Hf_50Ma-e_Hf_50Ma_error, ymax=e_Hf_50Ma+e_Hf_50Ma_error)) +
  geom_smooth(fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(z) + scale_x_reverse()


### nlme mixed effects models ###
## comparing different fixed effects structures first ###
### lme mixed effects models ###

model1a<-lme(e_Hf_50Ma ~  log10(age_Ma_plutons) , random = ~1|formation  ,method="ML",data=d)  
summary(model1a)
model1b<-lme(e_Hf_50Ma ~  log10(age_Ma_plutons) + I((log10(age_Ma_plutons))^2) , random = ~1|formation  ,method="ML",data=d) 
summary(model1b)
model1c<-lme(e_Hf_50Ma ~  log10(age_Ma_plutons) + I((log10(age_Ma_plutons))^2) + I((log10(age_Ma_plutons))^3) , random = ~1|formation  ,method="ML",data=d)
summary(model1c)


anova(model1a,model1b) 
anova(model1a,model1c)

##Extract r2 value
summary(model1a)
r2 <- r2beta(model=model1a,partial=TRUE,method='sgv')
r2

##Plot model residuals
plot(model1a)
plot(model1a,e_Hf_50Ma~fitted(.))
qqnorm(model1a,~resid(.)|formation)

##create model predictor variable
d$predicted_e_Hf_50Ma <- predict(model1a, type = "response")

## plot up final lme model
lme <- ggplot(d, aes(x=age_Ma_plutons, y=e_Hf_50Ma)) + geom_point(aes(colour = as.factor(location))) + 
  geom_errorbar(aes(ymin=e_Hf_50Ma-e_Hf_50Ma_error, ymax=e_Hf_50Ma+e_Hf_50Ma_error)) +
  stat_smooth(data=d, aes(age_Ma_plutons,(predicted_e_Hf_50Ma)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lme) + scale_x_reverse()


### lmer mixed effects models ###
mod1a <- lmer(e_Hf_50Ma ~ log10(age_Ma_plutons) + (1|formation), data = d)
summary(mod1a)

##extract coefficients
coefsmod1a <- data.frame(coef(summary(mod1a)))

# use normal distribution to approximate p-value
coefsmod1a$p.z <- 2 * (1 - pnorm(abs(coefsmod1a$t.value)))
coefsmod1a

##Extract r2 value
summary(mod1a)
r2 <- r2beta(model=mod1a,partial=TRUE,method='sgv')
r2

##PLOT res.
plot(mod1a)
plot(mod1a,e_Hf_50Ma~fitted(.))

##create model predictor variable
d$predicted_e_Hf_50Ma2 <- predict(mod1a, type = "response")

## plot up final lme model
lmer <- ggplot(d, aes(x=age_Ma_plutons, y=e_Hf_50Ma)) + geom_point(aes(colour = as.factor(location))) + 
  geom_errorbar(aes(ymin=e_Hf_50Ma-e_Hf_50Ma_error, ymax=e_Hf_50Ma+e_Hf_50Ma_error)) +
  stat_smooth(data=d, aes(age_Ma_plutons,(predicted_e_Hf_50Ma2)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lmer) + scale_x_reverse()

### Final regression with LmerTest
library(lmerTest)

(aov <- anova(mod1a))

step_result <- step(mod1a)
step_result

final_model <- get_model(step_result)
final_model

summary(final_model)

d$predicted_e_Hf_50Ma3 <- predict(final_model, type = "response")

fin <- ggplot(d, aes(x=age_Ma_plutons, y=e_Hf_50Ma)) + geom_point(aes(colour = as.factor(location))) + 
  geom_errorbar(aes(ymin=e_Hf_50Ma-e_Hf_50Ma_error, ymax=e_Hf_50Ma+e_Hf_50Ma_error)) +
  stat_smooth(data=d, aes(age_Ma_plutons,(predicted_e_Hf_50Ma3)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin) + scale_x_reverse()

(aov <- anova(final_model))
summary(final_model)
r2 <- r2beta(model=final_model,partial=TRUE,method='sgv')
r2

fin2 <- ggplot(d, aes(x=age_Ma_plutons, y=e_Hf_50Ma)) + geom_point(aes(colour = as.factor(location))) + 
  stat_smooth(data=d, aes(age_Ma_plutons,(predicted_e_Hf_50Ma3)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin2) + scale_x_reverse() + ylim(-3,18) + xlim(65,45)

##extract coefficients for final optimized model
coefsfinal_model <- data.frame(coef(summary(final_model)))

# use normal distribution to approximate p-value for final optimized model
coefsfinal_model$p.z <- 2 * (1 - pnorm(abs(coefsfinal_model$t.value)))
coefsfinal_model



##################################

######## SBB plutons U/Pb zircon age versus 
######### distance from Sanak Island ########
##### CONCORDANT U/PB AGES ONLY #########

# Read downloaded file into dataframes
sbb3 <- read.csv("data/sbb-age-distance-zircons-final.csv")

d <- read.csv("data/western-sbb-age-distance-zircons-final.csv", stringsAsFactors = TRUE, header = TRUE)
head(d)
attach(d)


## denoting data type within matrix
d$sample<- factor(d$sample)
d$pluton<- factor(d$pluton)
d$method<- factor(d$method)
d$age_Ma<- as.numeric(d$age_Ma)
d$age_Ma_error<- as.numeric(d$age_Ma_error)
d$distance<- as.numeric(d$distance)

### set custom theme for plots ###

library(cowplot)

custom_theme <- theme_cowplot()
custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))

theme_set(custom_theme)


#Checking for normality
hist(age_Ma)
shapiro.test(age_Ma)

hist(distance)
shapiro.test(distance)

## Plot the data ##

y <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  geom_smooth(fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(y) 



### nlme mixed effects models ###
##### comparing different fixed effects structures first ###
model1a<-lme(age_Ma ~  distance + I((distance)^2) + I((distance)^3), random = ~1|pluton  ,method="ML",data=d)  
summary(model1a)
model1b<-lme(age_Ma ~  distance + I((distance)^2), random = ~1|pluton  ,method="ML",data=d) 
summary(model1b)
model1c<-lme(age_Ma ~  distance, random = ~1|pluton  ,method="ML",data=d)
summary(model1c)


anova(model1a,model1b) 
anova(model1b,model1c)

##Extract r2 value
summary(model1c)
r2 <- r2beta(model=model1c,partial=TRUE,method='sgv')
r2

##Plot model residuals
plot(model1c)
plot(model1c,age_Ma~fitted(.))
qqnorm(model1c,~resid(.)|pluton)

##create model predictor variable
d$predicted_age_Ma <- predict(model1c, type = "response")

## plot up final lme model
lme <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  stat_smooth(data=d, aes(distance,(predicted_age_Ma)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lme)

#### Comparing different random effects structures
##random intercept
M1 <- lmer(age_Ma ~  distance + (1 | pluton), data = d, REML = TRUE)
summary(M1)

##random slopes
M2 <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = d, REML = TRUE)
summary(M2)

M2a <- lmer(age_Ma ~  distance + (1 + distance | pluton), data = d, REML = TRUE)
summary(M2a)

M2b <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M2b)

## random slopes and intercepts (slope & intercept calculated together at the grouping level)
M3 <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 + log10_age | pluton) + (1 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M3)

M3a <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 + log10_age | pluton), data = d, REML = TRUE)
summary(M3a)

M3b <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M3b)

## random slopes and intercepts (slope & intercept calculated independently)
M4 <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton) + (0 + log10_age | pluton) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M4)

M4a <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton) + (0 + log10_age | pluton), data = d, REML = TRUE)
summary(M4a)

M4b <- lmer(e_Hf_t ~  log10_age + I((log10_age)^2) + (1 | pluton) + (0 + I((log10_age)^2) | pluton), data = d, REML = TRUE)
summary(M4b)

anova(M1,M2,M2a,M2b,M3,M3a,M3b,M4,M4a,M4b)
anova(M1,M2)

### lmer mixed effects models ###
mod1a <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = d)
summary(mod1a)

##extract coefficients
coefsmod1a <- data.frame(coef(summary(mod1a)))

# use normal distribution to approximate p-value
coefsmod1a$p.z <- 2 * (1 - pnorm(abs(coefsmod1a$t.value)))
coefsmod1a

##Extract r2 value
summary(mod1a)
r2 <- r2beta(model=mod1a,partial=TRUE,method='sgv')
r2

##PLOT res.
plot(mod1a)
plot(mod1a,age_Ma~fitted(.))

##create model predictor variable
d$predicted_age_Ma2 <- predict(mod1a, type = "response")

## plot up final lme model
lmer <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  stat_smooth(data=d, aes(distance,(predicted_age_Ma2)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lmer)

## model optimization using LmerTest ###
library(lmerTest)

(aov <- anova(mod1a))

step_result <- step(mod1a)
step_result

final_model <- get_model(step_result)
final_model

summary(final_model)

d$predicted_age_Ma3 <- predict(final_model, type = "response")

fin <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(pluton))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  stat_smooth(data=d, aes(distance,(predicted_age_Ma3)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin)

(aov <- anova(final_model))
summary(final_model)
r2 <- r2beta(model=final_model,partial=TRUE,method='sgv')
r2

fin2 <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(pluton))) + 
  stat_smooth(data=d, aes(distance,(predicted_age_Ma3)), method = "lm", formula = y ~ x, fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin2)

##extract coefficients for final optimized model
coefsfinal_model <- data.frame(coef(summary(final_model)))

# use normal distribution to approximate p-value for final optimized model
coefsfinal_model$p.z <- 2 * (1 - pnorm(abs(coefsfinal_model$t.value)))
coefsfinal_model

##################################

######## SBB plutons U/Pb zircon age versus 
######### distance from Sanak Island ########
########## ALL AGES!!!  #########

# Read downloaded file into dataframes
sbb4 <- read.csv("data/sbb-age-distance-all-samples.csv")

d <- read.csv("data/sbb-age-distance-all-samples.csv", stringsAsFactors = TRUE, header = TRUE)
head(d)
attach(d)


## denoting data type within matrix
d$sample<- factor(d$sample)
d$pluton<- factor(d$pluton)
d$method<- factor(d$method)
d$age_Ma<- as.numeric(d$age_Ma)
d$log10_age <- log10(d$age_Ma)
d$log10_age<- as.numeric(d$log10_age)
d$age_Ma_error<- as.numeric(d$age_Ma_error)
d$distance<- as.numeric(d$distance)

### set custom theme for plots ###

library(cowplot)

custom_theme <- theme_cowplot()
custom_theme <- custom_theme + theme_bw() + theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))

theme_set(custom_theme)


#Checking for normality
hist(log10(age_Ma))
shapiro.test(log10(age_Ma))

hist(distance)
shapiro.test(distance)

## Plot the data ##

y <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(method))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  geom_smooth(fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(y) 



### nlme mixed effects models ###
##### comparing different fixed effects structures first ###
model1a<-lme(log10(age_Ma) ~  distance + I((distance)^2) + I((distance)^3), random = ~1|method  ,method="ML",data=d)  
summary(model1a)
model1b<-lme(log10(age_Ma) ~  distance, random = ~1|method  ,method="ML",data=d) 
summary(model1b)
model1c<-lme(log10(age_Ma) ~  distance + I((distance)^2), random = ~1|method  ,method="ML",data=d)
summary(model1c)


anova(model1a,model1b) 
anova(model1a,model1c)

##Extract r2 value
summary(model1c)
r2 <- r2beta(model=model1c,partial=TRUE,method='sgv')
r2

##Plot model residuals
plot(model1c)
plot(model1c,age_Ma~fitted(.))
qqnorm(model1c,~resid(.)|pluton)

##create model predictor variable
d$predicted_age_Ma <- predict(model1c, type = "response")

## plot up final lme model
lme <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(method))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  stat_smooth(data=d, aes(distance,(10^predicted_age_Ma)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lme)

### lmer mixed effects models ###
mod1a <- lmer(log10(age_Ma) ~  distance + I((distance)^2) + (1|pluton), data = d)
summary(mod1a)

##extract coefficients
coefsmod1a <- data.frame(coef(summary(mod1a)))

# use normal distribution to approximate p-value
coefsmod1a$p.z <- 2 * (1 - pnorm(abs(coefsmod1a$t.value)))
coefsmod1a

##Extract r2 value
summary(mod1a)
r2 <- r2beta(model=mod1a,partial=TRUE,method='sgv')
r2

##PLOT res.
plot(mod1a)
plot(mod1a,age_Ma~fitted(.))

##create model predictor variable
d$predicted_age_Ma2 <- predict(mod1a, type = "response")

## plot up final lme model
lmer <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(method))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  stat_smooth(data=d, aes(distance,(10^predicted_age_Ma2)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(lmer)

## model optimization using LmerTest ###
library(lmerTest)

(aov <- anova(mod1a))

step_result <- step(mod1a)
step_result

final_model <- get_model(step_result)
final_model

summary(final_model)

d$predicted_age_Ma3 <- predict(final_model, type = "response")

fin <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(method))) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error)) +
  stat_smooth(data=d, aes(distance,(10^predicted_age_Ma3)), method = "lm", formula = y ~ x + I(x^2), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin)

(aov <- anova(final_model))
summary(final_model)
r2 <- r2beta(model=final_model,partial=TRUE,method='sgv')
r2

fin2 <- ggplot(d, aes(x=distance, y=age_Ma)) + geom_point(aes(colour = as.factor(method))) + 
  stat_smooth(data=d, aes(distance,(10^predicted_age_Ma3)), method = "lm", formula = y ~ x + I(x^3), fill = "lightgray", colour = 'gray', alpha = 3/10, size=0.4)

print(fin2)

##extract coefficients for final optimized model
coefsfinal_model <- data.frame(coef(summary(final_model)))

# use normal distribution to approximate p-value for final optimized model
coefsfinal_model$p.z <- 2 * (1 - pnorm(abs(coefsfinal_model$t.value)))
coefsfinal_model



