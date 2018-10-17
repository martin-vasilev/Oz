

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
#dat <- read.csv("OZ32fixdist.csv", na.strings = "na")


get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
dat$subject<- get_num(dat$subject)
dat$subject<- as.factor(dat$subject)

dat$condition<- factor(dat$condition)
levels(dat$condition)<- c("Normal","Bold")
dat$item <- factor(dat$item)

# remove outliers:
out<- which(dat$fixduration >= 800)
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
p <- ggplot(dat1, aes(x=condition, y=fixduration, fill= condition)) + 
    geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
            outlier.size= 1, outlier.shape=8, coef= 4)+
  geom_violin(weight= 2, alpha= 0.3, scale= "count") + theme_bw(22) + 
    scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
    theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
          axis.line = element_line(colour = "black", size=1),
          panel.border = element_rect(colour = "black", size=1, fill = NA),
          legend.position="none", plot.title = element_text(hjust = 0.5))+facet_grid(.~ FixType) + 
    theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
          strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
          legend.key = element_rect(colour = "#000000", size=1),
          plot.title = element_text(size = 20))+
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800))+
  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
    xlab("Condition")+ ylab("Fixation duration") +ggtitle("Fixation type"); p
ggsave(p, filename = "Plots/FixbyType.pdf", width = 12, height = 7)
ggsave(p, filename = "Plots/FixbyType.png", width = 12, height = 7, dpi= 300)
  

# Model:
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
dat2<- subset(dat2, FixType== "under-sweep" | FixType== "accurate-sweep" | FixType== "both")
dat2$FixType<- droplevels(dat2$FixType)
dat2$FixType<- as.character(dat2$FixType)
table(dat2$FixType)

# recode single fixations on a line occuring on short lines (see above)
# these are considered accurate sweeps since readers did not make any more fixations on the line
# and have presumably processed its contents
for(i in 1:nrow(dat2)){
  if(dat2$FixType[i]== "both"){
    dat2$FixType[i]= "accurate-sweep"
    dat2$undersweep[i]=1
  }
}

table(dat2$FixType)

# Code landing position from the start of each line:
dat2$lineStartLand<- dat2$currentX- dat2$StartLineX 

# Return sweep saccade length:
dat2$sacc_len<- abs(dat2$priorX - dat2$currentX)

# center saccade length to improve model scaling:
dat2$sacc_lenC<- scale(dat2$sacc_len)


# scale word length:
dat2$Len1C<- scale(dat2$Len1)
dat2$Len2C<- scale(dat2$Len2)

# Launch Site:
# here, we need to substract the empty region before the start of a line to get the launch site
# from the beginning of the text margin:
dat2$launch<- abs(dat2$priorX- dat2$StartLineX)


# Model: return sweep launch site as a function of experimental condition:

if(!file.exists("Models/LM2.Rda")){
  LSM<- lmer(launch~ condition+ (condition|subject)+ (condition|item), REML= T,
             data= dat2)
  save(LSM, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}
summary(LSM)


# Model: new line landing position
contrasts(dat2$condition)
dat2$undersweep<- as.factor(dat2$undersweep)
contrasts(dat2$undersweep)

if(!file.exists("Models/LM2.Rda")){
  LM2<- lmer(lineStartLand~ condition*sacc_lenC*Len1C*Len2C+ (condition|subject)+ (condition|item),
             data= dat2)
  save(LM2, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}

# launch site instead of sacc_len
# subtract priorX - line start (next line)

summary(LM2)
round(coef(summary(LM2)),3)

plot(effect('condition', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")


plot(effect('sacc_lenC', LM2))
plot(effect('sacc_lenC:Len2C', LM2))

plot(effect('sacc_lenC:Len1C', LM2))

plot(effect('condition:sacc_lenCntr', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position", xlab= "Returns sweep saccade length (centred at 0)")



# LM3<- lmer(lineStartLand~ condition*sacc_lenC+ Len1C+ Len1C:condition+ (condition|subject)+ (condition|item),
#            data= dat2)
# summary(LM3)


###### Return sweep probability: 

if(!file.exists("Models/GM2.Rda")){
  GM2<- glmer(undersweep ~ condition * sacc_lenC  + (condition||subject)+ (condition||item),
              family= binomial, data= dat2)
  save(GM2, file= "Models/GM2.Rda")
}else{
  load("Models/GM2.Rda")
}

GM2<- glmer(undersweep ~ condition *scale(lineStartLand)  + (1|subject)+ (1|item),
            family= binomial, data= dat2)
# Landing position+ launch site

summary(GM2)
round(coef(summary(GM2)),3)

plot(effect('sacc_lenC', GM2))

plot(effect('condition', GM2), main= "Effect of bolding on undersweep probability",
     ylab= "Probability of undersweep")

plot(effect('condition:sacc_lenC', GM2), main= "Effect of bolding on undersweep probability",
     ylab= "Probability of undersweep")




### Word-level analysis (on first word on the line):

library(readr)
FD <- read_csv("data/OZword_data.csv")

get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
FD$subject<- get_num(FD$subject)

# Take only the first word on a line:
FD<- subset(FD, wordnum==2) # num 1 is empty space before line


# get file names:
d<- list.files("Experiment/DorothyText")
n<- get_num(d)
d<- d[order(n, d)]
d<- paste("Experiment/DorothyText/",d, sep= '')

t<- list.files("Experiment/TikTokText")
n<- get_num(t)
t<- t[order(n, t)]
t<- paste("Experiment/TikTokText/",t, sep= '')

files<- c(d, t)

item<- NULL
line<- NULL
word<- NULL
curr_item= NULL

for(i in 1:length(files)){ # for each text page..
  text<- readLines(files[i])
  
  for(j in 1:length(text)){ # for each line in text
    string<- unlist(strsplit(text[j], " "))
    word_string<- gsub("#", "", string[1])
    
    curr_item<- get_num(files[i])
    line<- c(line, j)
    word<- c(word, word_string)
    item<- c(item, curr_item)
  }
  
}

wb<- data.frame(item, line, word)
wb$word<- as.character(wb$word)
wb$length<- nchar(wb$word)
wb$word_clean<- wb$word
wb$item[161:nrow(wb)]<-wb$item[161:nrow(wb)] +25
#wb$word_clean<- tolower(wb$word_clean)

# remove quotation marks, etc.:
for(i in 1:nrow(wb)){
  if(substr(wb$word_clean[i], 1, 1)== "'"){
    wb$word_clean[i]<- substr(wb$word_clean[i], 2, nchar(wb$word_clean[i]))
  }
  
  if(is.element(substr(wb$word_clean[i], nchar(wb$word_clean[i]), nchar(wb$word_clean[i])), c("'", ",", ".", "!", ";", ":"))){
    wb$word_clean[i]<- substr(wb$word_clean[i], 1, nchar(wb$word_clean[i])-1)
  }
  if(is.element(substr(wb$word_clean[i], nchar(wb$word_clean[i]), nchar(wb$word_clean[i])), c(",", ".", "!"))){
    wb$word_clean[i]<- substr(wb$word_clean[i], 1, nchar(wb$word_clean[i])-1)
  }
  
  if(!is.element(wb$word_clean[i], c("I", "I'm"))){
    wb$word_clean[i]<- tolower(wb$word_clean[i])
  }
}


## find lexical frequencies
library(readr)
lex <- read_table2("data/SUBTLEX-UK.txt")
wb$Zipf<- NA
wb$freq<-NA
for(i in 1:nrow(wb)){
  a<- which(lex$Spelling== wb$word_clean[i])
  if(length(a)>0){
    wb$Zipf[i]<- lex$`LogFreq(Zipf)`[a]
    wb$freq[i]<- lex$FreqCount[a]
  }
}
 

sprintf("%f percent of words are not entries in the database", 100*(length(which(is.na(wb$Zipf)))/ nrow(wb)))

FD$freq<- NA
FD$Zipf<- NA
FD$word_clean<- NA
for(i in 1:nrow(FD)){
  a<- which(wb$item== FD$item[i] & wb$line== (FD$line[i]+1))
  
  if(length(a)>0){
    FD$freq[i]<- wb$freq[a]
    FD$Zipf[i]<- wb$Zipf[a]
    FD$word_clean[i]<- wb$word_clean[a]
  }
}

FD$subject<- as.factor(FD$subject)

FD$condition<- factor(FD$condition)
levels(FD$condition)<- c("Normal","Bold")
FD$item <- factor(FD$item)

contrasts(FD$condition)

FD$word_len<- nchar(FD$word_clean)
FD$AltGaze<- as.numeric(FD$AltGaze)
FD$logFreq<- log(FD$freq)
FD$word_lenC<- scale(FD$word_len)
FD$TotalTime<- as.numeric(FD$TotalTime)

# Model:
library(lme4)

if(!file.exists("Models/WM1.Rda")){
  WM1<- lmer(log(AltGaze)~ condition*logFreq*word_lenC + (condition|subject)+ (condition|item),
             REML=T, data= FD)
  save(WM1, file= "Models/WM1.Rda")
}else{
  load("Models/WM1.Rda")
}
summary(WM1)
round(coef(summary(WM1)),3)
write.csv(round(coef(summary(WM1)),3), "Models/WM1.csv")

if(!file.exists("Models/WM2.Rda")){
  WM2<- lmer(log(TotalTime)~ condition*logFreq*word_lenC + (condition|subject)+ (condition|item),
             REML=T, data= FD)
  save(WM2, file= "Models/WM2.Rda")
}else{
  load("Models/WM2.Rda")
}

summary(WM2)
round(coef(summary(WM2)),3)
write.csv(round(coef(summary(WM2)),3), "Models/WM2.csv")
      

# separate model;
# launch site: predicted by condition
















######################################################################################################################




FitGM<- subset(dat2, !is.na(sacc_len))
FitGM$GM2<- fitted(GM2)

a<- effect('condition', GM2)
DesGM<- melt(FitGM, id=c('subject', 'item', 'condition', 'FixType'), 
               measure=c('GM2') , na.rm=TRUE)

mGM<- cast(DesGM, condition+subject ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


df<- data.frame(condition= c("Normal", "Bold"), mean= c(0.6305651, 0.5694246), lower= c(0.5496077, 0.4919586),
                upper= c(0.7047869, 0.6436351))
df$condition<- factor(df$condition, levels= c("Normal", "Bold"))
mGM<- as.data.frame(mGM)
mGM$SE<- (mGM$GM2_SD/sqrt(35))*1.96 # 35 items per sub
mGM$upper<- mGM$GM2_M- mGM$SE
mGM$lower<- mGM$GM2_M- mGM$SE
mGM$SE<- NULL; mGM$GM2_SD<- NULL
colnames(mGM)<- c("condition", "subject", "mean", "upper", "lower")

GMplot<- ggplot(df, aes(x= condition, y= mean, group=1, ymax = upper, ymin= lower)) +
         scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
         geom_line(size=1.2, color= "darkred")+ theme_classic(22)+ xlab("Condition")+ ylab("p(Return sweep)")+ 
         geom_ribbon(alpha=0.10, colour=NA, fill= "darkred")+ theme(legend.position = "none")
GMplot<- GMplot + geom_jitter(data= mGM, aes(color= condition, shape= condition), size=3, width = 0.2)+
         scale_x_discrete(expand = c(0.1,0.1)); GMplot




######################################################################################################################

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