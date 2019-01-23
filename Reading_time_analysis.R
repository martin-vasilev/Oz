
# Martin R. Vasilev, 2019

# Page reading time analysis:


rm(list= ls())

Rdata<- NULL

for(i in 1:32){
  da_file<- readLines(paste("processed_files/DA1/reOZ", toString(i), ".DA1", sep= ''))
  
  sub<- NULL
  seq<- NULL
  item<- NULL
  cond<- NULL
  time<- NULL
  
  for(j in 1:length(da_file)){
    line<- data.frame( do.call( rbind, strsplit(da_file[j], ' ' ) ) )
    
    sub[j]<- i
    seq[j]<- as.numeric(as.character(line$X1))
    cond[j]<- as.numeric(as.character(line$X2))
    item[j]<- as.numeric(as.character(line$X3))
    time[j]<- as.numeric(as.character(line$X4))
    
  }
  
  tmp<- data.frame(sub, item, seq, cond, time)
  Rdata<- rbind(Rdata, tmp)
  
  cat(sprintf("Subject %i ", i))
}

save(Rdata, file= "data/Rdata.Rda")

Rdata$time_s= Rdata$time/1000 # in seconds

Rdata$cond<- factor(Rdata$cond)
levels(Rdata$cond)<- c("Normal","Bold")


# Descriptives:
library(reshape)
Des<- melt(Rdata, id=c('sub', 'item', 'cond'), 
            measure=c("time_s") , na.rm=TRUE)

m<- cast(Des, cond ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))


# statistical analysis:
library(lme4)

contrasts(Rdata$cond)

summary(R_M<- lmer(log(time_s)~ cond+ (cond|sub)+ (cond|item), REML= T, data= Rdata))
round(coef(summary(R_M)),3)
