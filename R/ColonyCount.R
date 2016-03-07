library(xlsx)
library(vegan)
library(reshape)
library(ggplot2)
library(Rmisc)
mydata <- read.xlsx("~/github/JanthinoDiversity/data/JanthinoProjectKJW_Colony_Counts.xlsx", 1)
sapply(mydata, class)

mydata$White <- as.numeric(as.character(mydata$White))
mydata$Purple.wr <- as.numeric(as.character(mydata$Purple.wr))
mydata$Purple.S <- as.numeric(as.character(mydata$Purple.S))

sapply(mydata, class)
# Add column of means

mydata.noNaN <- mydata[is.finite(mydata$White) & is.finite(mydata$Purple.wr) & is.finite(mydata$Purple.S), ]
# Add column with total colony count
mydata.noNaN$Total <- rowSums(mydata.noNaN[,6:8])
mydata.noNaN$H <- with(mydata.noNaN, diversity(mydata.noNaN[,6:8]))
mydata.noNaN$TreatDateSampDilDays <- do.call(paste, 
                                             c(mydata.noNaN[c("Treatment", "SamplingDate", "PlateDilution", "PictureDate.days.since.plating.")], 
                                               sep = "")) 


mydata.Day7 <- mydata.noNaN[ which(mydata.noNaN$PictureDate.days.since.plating==7),] 

# We don't trust plates with less than 30 colonies. 
# Let's remove them.
mydata.Day7.C30 <- subset(mydata.Day7, mydata.Day7$Total >= 30)

mydata.Day7.Dil6 <- mydata.Day7.C30[ which(mydata.Day7.C30$PlateDilution==-6),] 
mydata.Day7.Dil5 <- mydata.Day7.C30[ which(mydata.Day7.C30$PlateDilution==-5),] 
mydata.Day7.Dil7 <- mydata.Day7.C30[ which(mydata.Day7.C30$PlateDilution==-7),] 

hist(mydata.Day7.Dil5$Total, breaks = 15)
hist(mydata.Day7.Dil6$Total, breaks = 15)
hist(mydata.Day7.Dil7$Total, breaks = 15)

mydata.Day7.C30.R <- mydata.Day7.C30

mydata.Day7.C30$Total.T <- with(mydata.Day7.C30, (Total * (PlateDilution==-5) *100000 + Total * (PlateDilution==-6)*1000000 + Total * (PlateDilution==-7)*10000000) ) 
mydata.Day7.C30$White.T <- with(mydata.Day7.C30, (White * (PlateDilution==-5) *100000 + White * (PlateDilution==-6)*1000000 + White * (PlateDilution==-7)*10000000) ) 
mydata.Day7.C30.R$White.R <- with(mydata.Day7.C30, (  (White * (PlateDilution==-5) *100000 + White * (PlateDilution==-6)*1000000 + White * (PlateDilution==-7)*10000000)) / Total.T  ) 
mydata.Day7.C30$Purple.S.T <- with(mydata.Day7.C30, (Purple.S * (PlateDilution==-5) *100000 + Purple.S * (PlateDilution==-6)*1000000 + Purple.S * (PlateDilution==-7)*10000000) )
mydata.Day7.C30.R$Purple.S.R <- with(mydata.Day7.C30, (  (Purple.S * (PlateDilution==-5) *100000 + Purple.S * (PlateDilution==-6)*1000000 + Purple.S * (PlateDilution==-7)*10000000))  /Total.T ) 
mydata.Day7.C30$Purple.wr.T <- with(mydata.Day7.C30, (Purple.wr * (PlateDilution==-5) *100000 + Purple.wr * (PlateDilution==-6)*1000000 + Purple.wr * (PlateDilution==-7)*10000000) ) 
mydata.Day7.C30.R$Purple.wr.R <- with(mydata.Day7.C30, (  (Purple.wr * (PlateDilution==-5) *100000 + Purple.wr * (PlateDilution==-6)*1000000 + Purple.wr * (PlateDilution==-7)*10000000)) / Total.T ) 



mydata.Day7.long <- melt(subset(mydata.Day7.C30, select = -c(Total)), 
                         id.var= c("Treatment", "SamplingDate", "PlateDilution", "PictureDate.days.since.plating.", "TreatDateSampDilDays", "Replicate"))
mydata.Day7.long.H <- mydata.Day7.long[ which(mydata.Day7.long$variable== 'H'),] 

##### diversity plot

p <- ggplot(mydata.Day7.long.H, aes(factor(SamplingDate), value)) + 
  geom_boxplot(aes(fill = factor(Treatment)))  + theme_bw() +
  ylab("H") +
  xlab("Sampling date")  +
  scale_fill_brewer(palette="Blues") +
  scale_color_grey() + theme_classic() +
  guides(fill=guide_legend(title=NULL))

p


#######

# log transform counts
mydata.Day7.long$value.log <- log(mydata.Day7.long$value, 10)
mydata.Day7.long <- mydata.Day7.long[ which(mydata.Day7.long$value.log!=-Inf),] 

mydata.Day7.long.total <- mydata.Day7.long[ which(mydata.Day7.long$variable == "Total.T"),] 

tgc7 <- summarySE(mydata.Day7.long.total[ which(mydata.Day7.long.total$PlateDilution == "-7"),],
                 measurevar="value.log", groupvars=c("Treatment","SamplingDate"))

tgc6 <- summarySE(mydata.Day7.long.total[ which(mydata.Day7.long.total$PlateDilution == "-6"),],
                  measurevar="value.log", groupvars=c("Treatment","SamplingDate"))

tgc5 <- summarySE(mydata.Day7.long.total[ which(mydata.Day7.long.total$PlateDilution == "-5"),],
                  measurevar="value.log", groupvars=c("Treatment","SamplingDate"))

mydata.Day7.long.subset <- subset(mydata.Day7.long, variable == "Purple.wr.T" | variable == "Purple.S.T" | variable == "White.T")


ggplot(tgc6, aes(x=SamplingDate, y=value.log, colour=Treatment)) + 
  geom_errorbar(aes(ymin=value.log-se, ymax=value.log+se), width=.1) +
  geom_line() +
  geom_point() + 
  ylab("Log10(cell density)") +
  xlab("Sampling date") +
  scale_fill_brewer(palette="Blues") +
  scale_color_grey() + theme_classic()
  



