

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
#dat <- read.csv("OZ32fixdist.csv", na.strings = "na")


get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
dat$subject<- get_num(dat$subject)
dat$subject<- as.factor(dat$subject)

dat$condition<- factor(dat$condition)
levels(dat$condition)<- c("Normal","Bold")
dat$item <- factor(dat$item)

# remove outliers:
out<- which(dat$fixduration >= 1000)
cat(paste(round((length(out)/nrow(dat))*100, 3)), "% of fixations removed as outliers")
outliers<- dat[out,]
a<-table(outliers$FixType)

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


# recalculate intra-line fixations to include only first-pass ones:

datNew<- NULL
dat1$secPass<- NA
currLine= 0

for(i in 1:length(unique(dat1$subject))){
  n<- subset(dat1, subject== i)
  
  nitems<- as.numeric(as.character(unique(n$item)))
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    currLine= 0
    
    for(k in 1:nrow(m)){
      if(m$line[k]>currLine){
        currLine= m$line[k]
      }
      if(m$line[k]< currLine){
        m$secPass[k]<- 1
      }else{
        m$secPass[k]<- 0
      }
    }
    datNew<- rbind(datNew, m)
  }
}

datNew<- subset(datNew, secPass==0)
datNew$secPass<- NULL
dat1<- datNew; rm(datNew)


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
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
    xlab("Condition")+ ylab("Fixation duration") +ggtitle("Fixation type"); p
ggsave(p, filename = "Plots/FixbyType.pdf", width = 12, height = 8)
ggsave(p, filename = "Plots/FixbyType.png", width = 12, height = 8, dpi= 300)
  

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

# Launch Site:
# here, we need to substract the empty region before the start of a line to get the launch site
# from the beginning of the text margin:
dat2$launch<- abs(dat2$priorX- dat2$StartLineX)
dat2$launchC<- scale(dat2$launch)
dat2$lineStartLandC<- scale(dat2$lineStartLand)



#### Descriptive statistics
DesSacc<- melt(dat2, id=c('subject', 'item', 'condition', 'FixType'), 
               measure=c('launch', 'lineStartLand', 'undersweep') , na.rm=TRUE)

mSacc<- cast(DesSacc, condition ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
write.csv2(mSacc, 'Sacc_descr.csv')



# Model: return sweep launch site as a function of experimental condition:

if(!file.exists("Models/LSM.Rda")){
  LSM<- lmer(launch~ condition+ (condition|subject)+ (condition|item), REML= T,
             data= dat2)
  save(LSM, file= "Models/LSM.Rda")
}else{
  load("Models/LSM.Rda")
}
summary(LSM)


# Model: new line landing position
contrasts(dat2$condition)
#dat2$undersweep<- as.factor(dat2$undersweep)
#contrasts(dat2$undersweep)

if(!file.exists("Models/LM2.Rda")){
  LM2<- lmer(lineStartLand~ condition*launchC*Len1C+ (condition|subject)+ (condition|item),
             data= dat2)
  save(LM2, file= "Models/LM2.Rda")
}else{
  load("Models/LM2.Rda")
}

summary(LM2)
round(coef(summary(LM2)),3)

write.csv2(round(coef(summary(LM2)),3), "Models/LM2.csv")


effect('condition', LM2)

plot(effect('condition', LM2), ylab= "Landing position (number of characters the from line start)",
     main= "Effect of bolding on return sweep landing position")

plot(effect('launchC', LM2))

plot(effect('launchC', LM2))
plot(effect('launchC:Len1C', LM2))


#### Interaction plot:
Int<- effect('launchC:Len1C', LM2)
Int<- as.data.frame(Int)

QWL<- c(-1.8330659, -0.9770474, -0.1210288,  0.7349897,  3.7310546)
WL<- c(2, 4, 6, 8, 15)

Int$WL<- 0
Int$WL[1:5]<- WL[1]
Int$WL[6:10]<- WL[2]
Int$WL[11:15]<- WL[3]
Int$WL[16:20]<- WL[4]
Int$WL[21:25]<- WL[5]

LP<- c(0, 19, 39, 58, 77)
Int$LP<- rep(LP,5)

# scatter3D(Int$launchC, Int$fit, Int$WL, pch = 18, cex = 2, 
#           theta = 20, phi = 20, ticktype = "detailed",
#           xlab = "Launch", ylab = "Land. pos", zlab = "word length")#,  
#           # surf = list(x = x.pred, y = y.pred, z = z.pred,  
#           #             facets = NA, fit = fitpoints), main = "mtcars")
# CI= list(Int$upper, Int$lower)
lines2D(Int$LP, Int$fit, colvar = Int$WL, pch = 16,  bty ="n", cex = 1.5, lwd=2.5,
          type ="b", ylab= "Landing position (from start of new line)",
          xlab= "Launch position", clab = c("Word 1", "length"))


W1<- subset(Int, WL== 2)
W2<- subset(Int, WL== 4)
W3<- subset(Int, WL== 6)
W4<- subset(Int, WL== 8)
W5<- subset(Int, WL== 15)

##################
pdf("Plots/Inter_plot.pdf", width = 8, height = 7.5)
#png("Plots/Inter_plot.png", width = 2400, height = 2000, res = 300)
# par(mar = rep(2, 4))

plot(W1$LP, W1$fit, col= "burlywood3" , pch= 16, cex= 3, ylim= c(4,8.5), family= "serif",
     xlab= "Launch position (char.)", ylab=  "Landing position (char.)",
     cex.lab=1.5, cex.axis= 1.3)
lines(W1$LP, W1$fit, col= "burlywood3", lwd=3)
text(W1$LP, W1$fit+0.004, "2", font= 2, col= "white")

###
points(W2$LP, W2$fit, col= "darkorange" , pch= 16, cex= 3)
lines(W2$LP, W2$fit, col= "darkorange", lwd=3)
text(W2$LP, W2$fit+0.004, "4", font= 2, col= "white")

###
points(W3$LP, W3$fit, col= "darkgreen" , pch= 16, cex= 3)
lines(W3$LP, W3$fit, col= "darkgreen", lwd=3)
text(W3$LP, W3$fit+0.004, "6", font= 2, col= "white")

###
points(W4$LP, W4$fit, col= "darkblue" , pch= 16, cex= 3)
lines(W4$LP, W4$fit, col= "darkblue", lwd=3)
text(W4$LP, W4$fit+0.004, "8", font= 2, col= "white")

###
points(W5$LP, W5$fit, col= "darkorchid" , pch= 16, cex= 3)
lines(W5$LP, W5$fit, col= "darkorchid", lwd=3)
text(W5$LP, W5$fit+0.004, "15", font= 2, col= "white")

legend(65, 5.6, legend=c("2", "4", "6", "8", "15"), lwd=3,
       col=c("burlywood3", "darkorange", "darkgreen", "darkblue", "darkorchid"), lty= rep(1, 5), cex=1,
       title= "Word length", bty = "n")

dev.off()


#### Re-run landing position model, but include only accurate sweeps:

dat2_acc<- subset(dat2, FixType== "accurate-sweep")

if(!file.exists("Models/LM2v2.Rda")){
  LM2v2<- lmer(lineStartLand~ condition*launchC*Len1C+ (condition|subject)+ (condition|item),
             data= dat2_acc)
  save(LM2v2, file= "Models/LM2v2.Rda")
}else{
  load("Models/LM2v2.Rda")
}

summary(LM2v2)
round(coef(summary(LM2v2)),3)

write.csv2(round(coef(summary(LM2v2)),3), "Models/LM2v2.csv")




###### Return sweep probability:
if(!file.exists("Models/GM2.Rda")){
  GM2<- glmer(undersweep ~ condition*launchC + (condition|subject)+ (1|item),
              family= binomial, data= dat2)
  save(GM2, file= "Models/GM2.Rda")
}else{
  load("Models/GM2.Rda")
}

summary(GM2)
round(coef(summary(GM2)),3)
write.csv(round(coef(summary(GM2)),3), "Models/GM2.csv")

effect('condition ', GM2)


plot(effect('launchC:Len1C', GM2))

# main effects:
plot(effect('launchC', GM2))
plot(effect('condition ', GM2))

GInter<- as.data.frame(effect('condition:launchC:lineStartLandC ', GM2))

LP<- c(0, 58, 66, 69, 77)
GInter$LP<- NA
GInter$LP[which(GInter$launchC== -3.00)]<- LP[1]
GInter$LP[which(GInter$launchC== -2.00)]<- LP[2]
GInter$LP[which(GInter$launchC== -0.90)]<- LP[3]
GInter$LP[which(GInter$launchC== 0.03)]<- LP[4]
GInter$LP[which(GInter$launchC== 1.00)]<- LP[5]

LNDP<- c(-5, 4, 6, 9, 55) 
GInter$LND<- NA
GInter$LND[which(GInter$lineStartLandC== -3.0)]<- LNDP[1]
GInter$LND[which(GInter$lineStartLandC== 0.7)]<- LNDP[2]
GInter$LND[which(GInter$lineStartLandC== 4.0)]<- LNDP[3]
GInter$LND[which(GInter$lineStartLandC== 7.0)]<- LNDP[4]
GInter$LND[which(GInter$lineStartLandC== 10.0)]<- LNDP[5]

NL1<- subset(GInter, condition== "Normal" & LP== 0)
NL2<- subset(GInter, condition== "Normal" & LP== 58)
NL3<- subset(GInter, condition== "Normal" & LP== 66)
NL4<- subset(GInter, condition== "Normal" & LP== 69)
NL5<- subset(GInter, condition== "Normal" & LP== 77)

BL1<- subset(GInter, condition== "Bold" & LP== 0)
BL2<- subset(GInter, condition== "Bold" & LP== 58)
BL3<- subset(GInter, condition== "Bold" & LP== 66)
BL4<- subset(GInter, condition== "Bold" & LP== 69)
BL5<- subset(GInter, condition== "Bold" & LP== 77)

#### Plot:

layout(mat = matrix(c(1,2),nrow = 1,ncol = 2,byrow = TRUE))#,heights = c(0.5, 0.5, 0.5))
#par(mar=c(4,4,3,4))

plot(NL1$LND, NL1$fit, col= "burlywood3" , pch= 16, cex= 3, ylim= c(0, 1), xlim= c(-7, 11), family= "serif",
     xlab= "Landing position from beginning of line (char.)", ylab=  "Probability of under-sweep fixation",
     cex.lab=1.5, cex.axis= 1.3, main= "Normal")
lines(NL1$LND, NL1$fit, col= "burlywood3", lwd=3)
text(NL1$LND, NL1$fit+0.004, "0", font= 2, col= "white")

###
points(NL2$LND, NL2$fit, col= "darkorange" , pch= 16, cex= 3)
lines(NL2$LND, NL2$fit, col= "darkorange", lwd=3)
text(NL2$LND, NL2$fit+0.004, "58", font= 2, col= "white")

###
points(NL3$LND, NL3$fit, col= "darkgreen" , pch= 16, cex= 3)
lines(NL3$LND, NL3$fit, col= "darkgreen", lwd=3)
text(NL3$LND, NL3$fit+0.004, "66", font= 2, col= "white")

###
points(NL4$LND, NL4$fit, col= "darkblue" , pch= 16, cex= 3)
lines(NL4$LND, NL4$fit, col= "darkblue", lwd=3)
text(NL4$LND, NL4$fit+0.004, "69", font= 2, col= "white")

###
points(NL5$LND, NL5$fit, col= "darkorchid" , pch= 16, cex= 3)
lines(NL5$LND, NL5$fit, col= "darkorchid", lwd=3)
text(NL5$LND, NL5$fit+0.004, "77", font= 2, col= "white")

######
# Bold

plot(BL1$LND, BL1$fit, col= "burlywood3" , pch= 16, cex= 3, ylim= c(0, 1), xlim= c(-7, 11), family= "serif",
     xlab= "Landing position from beginning of line (char.)", ylab=  "Probability of under-sweep fixation",
     cex.lab=1.5, cex.axis= 1.3, main= "Bold")
lines(BL1$LND, BL1$fit, col= "burlywood3", lwd=3)
text(BL1$LND, BL1$fit+0.004, "0", font= 2, col= "white")

###
points(BL2$LND, BL2$fit, col= "darkorange" , pch= 16, cex= 3)
lines(BL2$LND, BL2$fit, col= "darkorange", lwd=3)
text(BL2$LND, BL2$fit+0.004, "58", font= 2, col= "white")

###
points(BL3$LND, BL3$fit, col= "darkgreen" , pch= 16, cex= 3)
lines(BL3$LND, BL3$fit, col= "darkgreen", lwd=3)
text(BL3$LND, BL3$fit+0.004, "66", font= 2, col= "white")

###
points(BL4$LND, BL4$fit, col= "darkblue" , pch= 16, cex= 3)
lines(BL4$LND, BL4$fit, col= "darkblue", lwd=3)
text(BL4$LND, BL4$fit+0.004, "69", font= 2, col= "white")

###
points(BL5$LND, BL5$fit, col= "darkorchid" , pch= 16, cex= 3)
lines(BL5$LND, BL5$fit, col= "darkorchid", lwd=3)
text(BL5$LND, BL5$fit+0.004, "77", font= 2, col= "white")


legend(3.5, 0.35, legend=c("0", "58", "66", "69", "77"), lwd=3,
       col=c("burlywood3", "darkorange", "darkgreen", "darkblue", "darkorchid"), lty= rep(1, 5), cex=1,
       title= "Launch Position", bty = "n")


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
      

############ Plot landing position densities by subject:


# Plot:
p <- ggplot(dat2, aes(x=condition, y= lineStartLand, fill= subject)) + 
  # geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
  #              outlier.size= 1, outlier.shape=8, coef= 4)+
  geom_violin(weight= 2, alpha= 0.3, scale= "count") + 
  theme_bw(22) + 
 # scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="none", plot.title = element_text(hjust = 0.5))+#facet_grid(.~ subject) + 
  theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
        strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
        legend.key = element_rect(colour = "#000000", size=1),
        plot.title = element_text(size = 20), legend.position="right")+
  # scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  #stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("Condition")+ ylab("Landing position (char)") +
  ggtitle("Landing position distributions by subject"); p
ggsave(p, filename = "Plots/Land_pos_by_sub.pdf", width = 12, height = 8)


###############################################################################################################################

# Compare landing position after corrective saccade to an accurate-returnsweep saccade:

dat1$corrective<- NA

for(i in 1:nrow(dat1)){
  if(dat1$FixType[i]== "under-sweep"){
    dat1$corrective[i+1]<- "corrective"
  }
  if(dat1$FixType[i]== "accurate-sweep"){
    dat1$corrective[i]= "accurate-sweep"
  }
}

dat3<- subset(dat1, !is.na(corrective))

# Code landing position from the start of each line:
dat3$lineStartLand<- dat3$currentX- dat3$StartLineX 


Des<- melt(dat3, id=c('subject', 'item', 'condition', "corrective"), 
            measure=c("lineStartLand") , na.rm=TRUE)

mS<- cast(Des, corrective ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))


# Plot:
p <- ggplot(dat3, aes(x=corrective, y=lineStartLand, fill= corrective)) + 
  geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
               outlier.size= 1, outlier.shape=8, coef= 4)+
  geom_violin(weight= 2, alpha= 0.3, scale= "count") + theme_bw(22) + 
  scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  theme(strip.text.x = element_text(size = 20,  face="bold",family="serif"),
        strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
        legend.key = element_rect(colour = "#000000", size=1),
        plot.title = element_text(size = 20))+
#  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("Saccade type")+ ylab("Landing position (char.)"); p
ggsave(p, filename = "Plots/corrective.pdf", width = 8, height = 8)
ggsave(p, filename = "Plots/corrective.png", width = 8, height = 8, dpi= 300)



#######################################################################################################################

# New plots (for social media):

DesLand<- melt(dat2, id=c('subject', 'item', 'condition'), 
               measure=c('lineStartLand') , na.rm=TRUE)

mLand<- cast(DesLand, condition+subject ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

# Plot:
p <- ggplot(mLand, aes(x=condition, y=lineStartLand_M, fill= condition, group= condition)) + 
#  geom_violin(weight= 2, alpha= 0.3, scale= "count") +
  theme_bw(22) + geom_jitter(mapping = aes(fill= condition), height= 0, width=0.1, size= 2, shape=21,
                             color= "#4c5159", alpha= 1)+
  geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
               outlier.size= 1, outlier.shape=8, coef= 4, alpha=0.4, notch = T, notchwidth = 0.5)+
  scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="none", plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14))+
#  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("First word on a line")+ ylab("Landing position (letters)"); p
#ggsave(p, filename = "Plots/FixbyType.pdf", width = 12, height = 8)
ggsave(p, filename = "Plots/Land_pos.png", width = 8, height = 8, dpi= 300)




DesCorr<- melt(dat2, id=c('subject', 'item', 'condition'), 
               measure=c('undersweep') , na.rm=TRUE)

mCorr<- cast(DesCorr, condition+subject ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

# Plot:
p <- ggplot(mCorr, aes(x=condition, y=undersweep_M, fill= condition, group= condition)) + 
  #  geom_violin(weight= 2, alpha= 0.3, scale= "count") +
  theme_bw(22) + geom_jitter(mapping = aes(fill= condition), height= 0, width=0.1, size= 2, shape=21,
                             color= "#4c5159", alpha= 1)+
  geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
               outlier.size= 1, outlier.shape=8, coef= 4, alpha=0.4, notch = T, notchwidth = 0.5)+
  scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="none", plot.title = element_text(hjust = 0.5))+
#  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14))+
  #  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("First word on a line")+ ylab("Undersweep probability"); p
#ggsave(p, filename = "Plots/FixbyType.pdf", width = 12, height = 8)
ggsave(p, filename = "Plots/Undersweep_prob.png", width = 8, height = 8, dpi= 300)




####### Landing position by Line length:

DesLen<- melt(subset(dat2, Len1<14), id=c('subject', 'item', 'condition', 'Len1'), 
               measure=c('lineStartLand') , na.rm=TRUE)

mLen<- cast(DesLen, subject+condition+Len1 ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))



p <- ggplot(mLen, aes(x=Len1, y= lineStartLand_M, fill= condition, group= condition)) + 
  #  geom_violin(weight= 2, alpha= 0.3, scale= "count") +
  theme_bw(22) + geom_jitter(mapping = aes(fill= condition), height= 0, width=0.2, size= 2, shape=21,
                             color= "#4c5159", alpha= 1)+
  # geom_boxplot(width=0.25, outlier.color = "#4c5159", #outlier.color= "#777777", 
  #              outlier.size= 1, outlier.shape=8, coef= 4, alpha=0.4, notch = T, notchwidth = 0.5)+
  scale_fill_brewer(palette="Accent")+ scale_color_brewer(palette="Accent")+
  theme(panel.grid = element_line(colour = "#ededed", size=0.5), 
        axis.line = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        legend.position="bottom", plot.title = element_text(hjust = 0.5))+ 
  geom_abline(intercept =  coef(summary(LM2))[1,1], slope = coef(summary(LM2))[4,1], col= "blue", size= 1.2, alpha=0.5)+
  annotate("text", label = paste("b= ", round(coef(summary(LM2))[4,1],3),
                                 ", SE= ", round(coef(summary(LM2))[4,2],3),
                                 ", t= ", round(coef(summary(LM2))[4,3],4), sep= ''), 
           x = 7, y = 18, size = 4.5, colour = "blue", alpha= 0.7)+
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20))+
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14))+
  #  stat_summary(fun.y=mean, geom="point", shape=16, color= "darkred", size=2)+
  xlab("Length of first word on the line (letters)")+ ylab("Landing position (letters)"); p


ggsave(p, filename = "Plots/Land_x_Len.png", width = 12, height = 8, dpi= 300)

######################################################################################################################



# 
# FitGM<- subset(dat2, !is.na(sacc_len))
# FitGM$GM2<- fitted(GM2)
# 
# a<- effect('condition', GM2)
# DesGM<- melt(FitGM, id=c('subject', 'item', 'condition', 'FixType'), 
#                measure=c('GM2') , na.rm=TRUE)
# 
# mGM<- cast(DesGM, condition+subject ~ variable
#              , function(x) c(M=signif(mean(x),3)
#                              , SD= sd(x) ))
# 
# 
# df<- data.frame(condition= c("Normal", "Bold"), mean= c(0.6305651, 0.5694246), lower= c(0.5496077, 0.4919586),
#                 upper= c(0.7047869, 0.6436351))
# df$condition<- factor(df$condition, levels= c("Normal", "Bold"))
# mGM<- as.data.frame(mGM)
# mGM$SE<- (mGM$GM2_SD/sqrt(35))*1.96 # 35 items per sub
# mGM$upper<- mGM$GM2_M- mGM$SE
# mGM$lower<- mGM$GM2_M- mGM$SE
# mGM$SE<- NULL; mGM$GM2_SD<- NULL
# colnames(mGM)<- c("condition", "subject", "mean", "upper", "lower")
# 
# GMplot<- ggplot(df, aes(x= condition, y= mean, group=1, ymax = upper, ymin= lower)) +
#          scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
#          geom_line(size=1.2, color= "darkred")+ theme_classic(22)+ xlab("Condition")+ ylab("p(Return sweep)")+ 
#          geom_ribbon(alpha=0.10, colour=NA, fill= "darkred")+ theme(legend.position = "none")
# GMplot<- GMplot + geom_jitter(data= mGM, aes(color= condition, shape= condition), size=3, width = 0.2)+
#          scale_x_discrete(expand = c(0.1,0.1)); GMplot
# 
# 


######################################################################################################################
# 
# p2 <- qplot(fixduration, data = p, facets = FixType ~ ., linetype = condition, geom = "density", xlim = c(0,500)) + xlab("Fixation Duration")
# p2
# ggsave(file = "Psycho17fixtype.pdf", dpi = 1200)
# 
# fixtype.model = lmer(data = p, fixduration ~ FixType * condition + (1 + FixType + condition| subject) + (1 + FixType + condition| item))
# summary(fixtype.model)
# 
# pdf(file = "FixTypeEffects.pdf", width = 9, height = 7)
# plot(effect("FixType", fixtype.model),main = NULL, grid = TRUE, colors = "black", xlab = " ", ylim = c(120,300), ylab = "Fixation Duration")
# dev.off()
# 
# p <- p[p$lineinitial == 1,]
# str(p)
# p3 <- qplot(Rserror, data = p, linetype = condition, geom = "density", xlim = c(0,20)) + xlab("Return-Sweep Error")
# p3
# ggsave(file = "OZRSerror.pdf", dpi = 1200)
# 
# p4 <- qplot(currentX, data = p, linetype = condition, geom = "density", xlim = c(0,30)) + xlab("Return-Sweep Landing Site")
# p4
# ggsave(file = "RSLand.pdf", dpi = 1200)
# 
# Undersweep.model = glmer(data = p, Undersweep ~ condition + (1 + condition | subject) + (1 + condition | item), family = binomial(link = "logit"))
# summary(Undersweep.model)
# 
# pdf(file = "UndersweepEffects.pdf", width = 7, height = 7)
# plot(effect("condition", Undersweep.model),main = NULL, grid = TRUE, colors = "black", xlab = "Line Initial Word", ylab = "Undersweep Likelihood")
# dev.off()
# 
# #Return sweep error in characters
# lmRSE = lmer(data = p, Rserror ~ condition + (1 + condition | subject) + (1 + condition | item))
# summary(lmRSE)
# 
# q <- read.csv("TimOz.csv", na.strings = "na")
# q <- q[q$Line1stFix == 1,]
# #q <- q[q$Undersweep2 == 0,]
# str(q)
# 
# q$item <- factor(q$item)
# q$condition <- factor(q$condition)
# levels(q$condition) <- c("Normal","Bold")
# 
# q3 <- qplot(RS_error, data = q, linetype = condition, geom = "density", xlim = c(0,25)) + xlab("Return-Sweep Error")
# q3
# ggsave(file = "OZRSerror2.pdf", dpi = 1200)
# 
# Undersweep2.model = glmer(data = q, Undersweep2 ~ condition + (1 + condition | subject) + (1 + condition | item), family = binomial(link = "logit"))
# summary(Undersweep2.model)
# 
# pdf(file = "Undersweep2Effects.pdf", width = 7, height = 7)
# plot(effect("condition", Undersweep2.model), main = NULL, grid = TRUE, colors = "black", xlab = "Line Initial Word", ylab = "Undersweep Likelihood")
# dev.off()
# 
# #Return sweep error in characters
# lmRSE2 = lmer(data = q, RS_error ~ condition + (1 + condition | subject) + (1 + condition | item))
# summary(lmRSE2)
# 
# lmLand = lmer(data = q, lineland ~ condition * Length + (1 + condition | subject) + (1 + condition | item))
# summary(lmLand)
# 
# q <- q[q$wordnum == 2,]
# q4 <- qplot(X4OVP, data = q, linetype = condition, geom = "density", xlim = c(0,1)) + xlab("Landing Position Percent")
# q4
# ggsave(file = "OZRSerror2.pdf", dpi = 1200)