
# Martin R. Vasilev, 2018

# Calculate the length of lines in the text:

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


Line_length= NULL

for(i in 1:length(files)){ # for each text page..
  text<- readLines(files[i])
  
  for(j in 1:length(text)){ # for each line in text
    Line_length<- c(Line_length, nchar(text[j]))
  }
  
}



## Line beginings that will fall in parafoveal vision if you are at the end of the
# current line:

visdeg= 0.3
parafovea<- 2/visdeg

(length(which(Line_length<= parafovea))/ length(Line_length))*100

hist(Line_length, breaks = 30, xlim= c(0, 100), xlab= "Line length (in char.)",
     col= "steelblue", main= "", family= "serif", cex.lab= 1.4, cex.axis=1.2)
abline(v= parafovea, col= "darkred", lwd=2)

range(Line_length)
mean(Line_length)
sd(Line_length)
