

# Martin R. Vasilev & Tim Slattery, 2018 

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

# remove outliers:
out<- which(dat$fixduration >= 1000)
cat(paste(round((length(out)/nrow(dat))*100, 3)), "% of fixations removed as outliers")
dat<- dat[-out, ]


# subset data for fixation type analysis:

# Note: In this experiment, there were some lines that are very short (i.e, even containing a single short word).
# This is a natural consequence of this type of text that was used in the study. As a result of this, there are 
# cases where such lines were fixated only once. Because in such case the fixation is both an accurate sweep and 
# a line-final fixation, we exclude them from this particular analysis. Such fixations accounted for 0.22% of all
# fixations.
table(dat$FixType)

dat1<- subset(dat, FixType!= "both")
dat1$FixType<- droplevels(dat1$FixType)
table(dat1$FixType) # good

# change contrast coding so that intra-line (i.e., typical fixation) is the baseline:
contrasts(dat1$FixType)
dat1$FixType<- factor(dat1$FixType, levels = c("intra-line", "line-final", "accurate-sweep", "under-sweep"))
contrasts(dat1$FixType)


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
DesType<- melt(dat1, id=c('subject', 'item', 'condition', 'FixType'), 
            measure=c('fixduration') , na.rm=TRUE)

mType<- cast(DesType, condition+FixType ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

mType$fixduration_SD<- round(mType$fixduration_SD)

# Plot:
# Dplot<- ggplot(dat1, aes(x= fixduration, group= condition, linetype= condition, color= condition,
#                          size= condition)) + xlim(0, 600)+
#               theme_bw(24)+ geom_density(size=1.3) + scale_color_manual(values=c("#E69F00", "#56B4E9"))+
#               theme(legend.position="bottom")+ facet_grid(rows = vars(FixType))+ 
#               theme(strip.text.x = element_text(size = 22, face="bold",family="serif"),
#               strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
#               legend.key = element_rect(colour = "#000000", size=1))+
#               xlab("Fixation duration")+ ylab("Probability density")
# Dplot
# 
# ggsave(Dplot, filename = "Plots/FixbyType.pdf", width = 10, height = 10)
# #+
#   geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
#              linetype="dashed")


p <- ggplot(dat1, aes(x=condition, y=fixduration, fill= condition)) + 
    geom_violin(weight= 2, alpha= 0.2) + geom_boxplot(width=0.25, outlier.color= "#777777")+ theme_bw(22) +
    scale_fill_brewer(palette="Dark2")+ scale_color_brewer(palette="Dark2")+
    theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
          axis.line = element_line(colour = "black", size=1),
          panel.border = element_rect(colour = "black", size=1, fill = NA),
          legend.position="none", plot.title = element_text(hjust = 0.5))+facet_grid(.~ FixType) + 
    theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
          strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
          legend.key = element_rect(colour = "#000000", size=1),
          plot.title = element_text(size = 20))+
    xlab("Condition")+ ylab("Fixation duration") +ggtitle("Fixation type"); p
ggsave(p, filename = "Plots/FixbyType.pdf", width = 16, height = 10)
  
  
  

contrasts(dat1$condition)
contrasts(dat1$FixType)
# Fixation type x Text type model:
if(!file.exists("Models/LM1.Rda")){
  LM1<- lmer(log(fixduration)~ condition * FixType + (condition|subject)+ (condition|item), data= dat1)
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
dat2<- subset(dat2, FixType== "under-sweep" | FixType== "accurate-sweep")


# Code landing position from the start of each line:
dat2$lineStartLand<- dat2$currentX- dat2$StartLineX 

# Return sweep saccade length:
dat2$sacc_len<- abs(dat2$priorX - dat2$currentX)

# center saccade length to improve model scaling:
dat2$sacc_lenC<- scale(dat2$sacc_len)


# scale word length:
dat2$Len1C<- scale(dat2$Len1)
dat2$Len2C<- scale(dat2$Len2)


# Model: new line landing position
contrasts(dat2$condition)
dat2$undersweep<- as.factor(dat2$undersweep)
contrasts(dat2$undersweep)


LM2<- lmer(lineStartLand~ condition*sacc_lenC+ (condition|subject)+ (condition|item),
           data= dat2)

summary(LM2)

plot(effect('condition', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")

plot(effect('condition:undersweep', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")


plot(effect('condition:sacc_lenCntr', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position", xlab= "Returns sweep saccade length (centred at 0)")



# LM3<- lmer(Rserror~ condition*sacc_lenCntr*undersweep+ (condition|subject)+ (condition|item), data= dat2)
# summary(LM3)
# 
# 
# LM3<- lmer(lineStartLand~ Len1C*Len2C*condition+ (condition|subject)+ (condition|item), data= dat2)
# summary(LM3)


###### Return sweep probability: 

if(!file.exists("Models/GM2.Rda")){
  GM2<- glmer(undersweep ~ condition * sacc_lenC + (condition|subject)+ (1|item),
              family= binomial, data= dat2)
  save(GM2, file= "Models/GM2.Rda")
}else{
  load("Models/GM2.Rda")
}

summary(GM2)

plot(effect('condition', GM2), main= "Effect of bolding on undersweep probability",
     ylab= "Probability of undersweep")

plot(effect('condition:sacc_lenCntr', GM2), main= "Effect of bolding on undersweep probability",
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