

# Tim Slattery, 2019

rm(list=ls())

# load/ install packages:
if('lme4' %in% rownames(installed.packages())==FALSE){
  install.packages('lme4'); library(lme4)} else{ library(lme4)}

if('reshape' %in% rownames(installed.packages())==FALSE){
  install.packages('reshape'); library(reshape)} else{ library(reshape)}

if('ggplot2' %in% rownames(installed.packages())==FALSE){
  install.packages('ggplot2'); library(ggplot2)} else{ library(ggplot2)}

if('MASS' %in% rownames(installed.packages())==FALSE){
  install.packages('MASS'); library(MASS)} else{ library(MASS)}

if('car' %in% rownames(installed.packages())==FALSE){
  install.packages('car'); library(car)} else{ library(car)}

if('effects' %in% rownames(installed.packages())==FALSE){
  install.packages('effects'); library(effects)} else{ library(effects)}


# Load and prepare data files:
dat <- read.csv("data/OZdata.csv", na.strings = "na")

get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
dat$subject<- get_num(dat$subject)
dat$subject<- as.factor(dat$subject)

dat$condition<- factor(dat$condition)
levels(dat$condition)<- c("Normal","Bold")
dat$item <- factor(dat$item)

dat$FixType<- factor(dat$FixType, levels = c("intra-line", "line-final", "undersweep", "accurate-sweep"))
contrasts(dat$FixType)


# TEMPORARY!!!
dat<- subset(dat, FixType != "oops")
table(dat$FixType)
#dat$FixType <- factor(dat$FixType)
#levels(dat$FixType) <- c("Standard","Line final","Accurate-sweep","Undersweep")

# remove outliers:
out<- which(dat$fixduration >= 1000)
cat(paste(round((length(out)/nrow(dat))*100, 3)), "% of fixations removed as outliers")
dat<- dat[-out, ]


# Comprehension accuracy:

# Note: questions 1-5 are from "Dorothy" and questions 6-10 are from "Tiktok".
q <- read.csv("data/OZquest.csv", na.strings = "na")
q$condition<- factor(q$condition)
levels(q$condition)<- c("Normal","Bold")
contrasts(q$condition)

q$subject<- as.factor(q$subject)
q$item<- as.factor(q$item)

DesQ<- melt(q, id=c('subject', 'item', 'condition'), 
                measure=c("accuracy") , na.rm=TRUE)

mQ<- cast(DesQ, condition ~ variable
              , function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))
# subject average accuracy:
mQ2<- cast(DesQ, subject ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

# Accuracy model:
if(!file.exists("Models/GM1.Rda")){
  # model does not converge with a random slope for items.
  GM1<- glmer(accuracy ~ condition + (condition|subject)+ (1|item), family= binomial, data=q)
  save(GM1, file= "Models/GM1.Rda")
  round(coef(summary(GM1)),2)
}else{
  load("Models/GM1.Rda")
  round(coef(summary(GM1)),2)
}


#### Fixation type:
DesType<- melt(dat, id=c('subject', 'item', 'condition', 'FixType'), 
            measure=c('fixduration') , na.rm=TRUE)

mType<- cast(DesType, condition+FixType ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

mType$fixduration_SD<- round(mType$fixduration_SD)



# Fixation type x Text type model:
(!file.exists("Models/LM1.Rda")){
  LM1<- lmer(log(fixduration)~ condition * FixType + (condition|subject)+ (condition|item), data= dat)
  save(LM1, file= "Models/LM1.Rda")
  summary(LM1)
}else{
  load("Models/LM1.Rda")
  summary(LM1)
}
round(coef(summary(LM1)),3)



#####

# Landing profile analysis:

# remove the first line from each item: there is no return sweep there because participants start reading
# with a gaze box as the start of the sentence (line 0 in the dataset).
dat2<- subset(dat, line!=0)

# Take only first fixation on each line:
dat2<- subset(dat2, FixType== "undersweep" | FixType== "accurate-sweep")


# Code landing position from the start of each line:
dat2$lineStartLand<- dat2$currentX- dat2$StartLineX 

# Return sweep saccade length:
dat2$sacc_len<- abs(dat2$priorX - dat2$currentX)

# center saccade length to improve model scaling:
dat2$sacc_lenCntr<- scale(dat2$sacc_len)

# Model:
contrasts(dat2$condition)
dat2$undersweep<- as.factor(dat2$undersweep)
contrasts(dat2$undersweep)

LM2<- lmer(lineStartLand~ condition*undersweep +sacc_len +sacc_len:condition+ (condition|subject)+ (condition|item), data= dat2)
summary(LM2)

plot(effect('condition', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")

plot(effect('condition:undersweep', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")


plot(effect('condition:sacc_len', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")



LM3<- lmer(lineStartLand~ condition*sacc_len*undersweep+ (condition|subject)+ (condition|item), data= dat2)
summary(LM3)


LM3<- lmer(lineStartLand~ condition*Len1 + sacc_len+ (condition|subject)+ (condition|item), data= dat2)
summary(LM3)


GM2<- glmer(undersweep ~ condition + (condition|subject)+ (condition|item), family= binomial, data= dat2)
summary(GM2)

plot(effect('condition', GM2), main= "Effect of bolding on undersweep probability",
     ylab= "Probability of undersweep")
















p2 <- qplot(fixduration, data = p, facets = FixType ~ ., linetype = condition, geom = "density", xlim = c(0,500)) + xlab("Fixation Duration")
p2
ggsave(file = "Psycho17fixtype.pdf", dpi = 1200)

fixtype.model = lmer(data = p, fixduration ~ FixType * condition + (1 + FixType + condition| subject) + (1 + FixType + condition| item))
summary(fixtype.model)

pdf(file = "FixTypeEffects.pdf", width = 9, height = 7)
plot(effect("FixType", fixtype.model),main = NULL, grid = TRUE, colors = "black", xlab = " ", ylim = c(120,300), ylab = "Fixation Duration")
dev.off()

p <- p[p$lineinitial == 1,]
str(p)
p3 <- qplot(Rserror, data = p, linetype = condition, geom = "density", xlim = c(0,20)) + xlab("Return-Sweep Error")
p3
ggsave(file = "OZRSerror.pdf", dpi = 1200)

p4 <- qplot(currentX, data = p, linetype = condition, geom = "density", xlim = c(0,30)) + xlab("Return-Sweep Landing Site")
p4
ggsave(file = "RSLand.pdf", dpi = 1200)

Undersweep.model = glmer(data = p, Undersweep ~ condition + (1 + condition | subject) + (1 + condition | item), family = binomial(link = "logit"))
summary(Undersweep.model)

pdf(file = "UndersweepEffects.pdf", width = 7, height = 7)
plot(effect("condition", Undersweep.model),main = NULL, grid = TRUE, colors = "black", xlab = "Line Initial Word", ylab = "Undersweep Likelihood")
dev.off()

#Return sweep error in characters
lmRSE = lmer(data = p, Rserror ~ condition + (1 + condition | subject) + (1 + condition | item))
summary(lmRSE)

q <- read.csv("TimOz.csv", na.strings = "na")
q <- q[q$Line1stFix == 1,]
#q <- q[q$Undersweep2 == 0,]
str(q)

q$item <- factor(q$item)
q$condition <- factor(q$condition)
levels(q$condition) <- c("Normal","Bold")

q3 <- qplot(RS_error, data = q, linetype = condition, geom = "density", xlim = c(0,25)) + xlab("Return-Sweep Error")
q3
ggsave(file = "OZRSerror2.pdf", dpi = 1200)

Undersweep2.model = glmer(data = q, Undersweep2 ~ condition + (1 + condition | subject) + (1 + condition | item), family = binomial(link = "logit"))
summary(Undersweep2.model)

pdf(file = "Undersweep2Effects.pdf", width = 7, height = 7)
plot(effect("condition", Undersweep2.model), main = NULL, grid = TRUE, colors = "black", xlab = "Line Initial Word", ylab = "Undersweep Likelihood")
dev.off()

#Return sweep error in characters
lmRSE2 = lmer(data = q, RS_error ~ condition + (1 + condition | subject) + (1 + condition | item))
summary(lmRSE2)

lmLand = lmer(data = q, lineland ~ condition * Length + (1 + condition | subject) + (1 + condition | item))
summary(lmLand)

q <- q[q$wordnum == 2,]
q4 <- qplot(X4OVP, data = q, linetype = condition, geom = "density", xlim = c(0,1)) + xlab("Landing Position Percent")
q4
ggsave(file = "OZRSerror2.pdf", dpi = 1200)