library(xlsx)
library(vegan)
library(reshape)
library(ggplot2)
library(Rmisc)
mydata <- read.xlsx("~/github/JanthinoDiversity/data/", 1)
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

mydata.Day7.Dil6 <- mydata.Day7[ which(mydata.Day7.subset$PlateDilution==-6),] 
mydata.Day7.Dil5 <- mydata.Day7[ which(mydata.Day7.subset$PlateDilution==-5),] 
mydata.Day7.Dil7 <- mydata.Day7[ which(mydata.Day7.subset$PlateDilution==-7),] 

hist(mydata.Day7.Dil5$Total, breaks = 15)
hist(mydata.Day7.Dil6$Total, breaks = 15)
hist(mydata.Day7.Dil7$Total, breaks = 15)

mydata.Day7.C30$Total.T <- with(mydata.Day7.C30, (Total * (PlateDilution==-5) *100000 + Total * (PlateDilution==-6)*1000000 + Total * (PlateDilution==-7)*10000000) ) 
mydata.Day7.C30$White.T <- with(mydata.Day7.C30, (White * (PlateDilution==-5) *100000 + White * (PlateDilution==-6)*1000000 + White * (PlateDilution==-7)*10000000) ) 
mydata.Day7.C30$Purple.S.T <- with(mydata.Day7.C30, (Purple.S * (PlateDilution==-5) *100000 + Purple.S * (PlateDilution==-6)*1000000 + Purple.S * (PlateDilution==-7)*10000000) ) 
mydata.Day7.C30$Purple.wr.T <- with(mydata.Day7.C30, (Purple.wr * (PlateDilution==-5) *100000 + Purple.wr * (PlateDilution==-6)*1000000 + Purple.wr * (PlateDilution==-7)*10000000) ) 

mydata.Day7.long <- melt(subset(mydata.Day7.C30, select = -c(Total)), 
                         id.var= c("Treatment", "SamplingDate", "PlateDilution", "PictureDate.days.since.plating.", "TreatDateSampDilDays", "Replicate"))

# log transform counts
mydata.Day7.long$value.log <- log(mydata.Day7.long$value, 10)
mydata.Day7.long <- mydata.Day7.long[ which(mydata.Day7.long$value.log!=-Inf),] 


mydata.Day7.long.subset <- subset(mydata.Day7.long, variable == "Purple.wr.T" | variable == "Purple.S.T" | variable == "White.T")

cols <- c(White.T="white",Purple.S.T="lightgrey", 
          Purple.wr.T="#330066") 

plot <- ggplot(subset(mydata.Day7.long.subset), aes(x=SamplingDate, y=value.log, fill=variable)) +
  geom_bar(stat="identity") + facet_grid(~Treatment) + theme_bw() +
  ylab("Estimated cell count, log") +
  xlab("Sampling data") 

plot + scale_fill_manual(values=c("lightgrey", "#990099", "#330066"), 
                  name="Colony morphology",
                  breaks=c("White.T", "Purple.S.T", "Purple.wr.T"),
                  labels=c("White", "Smooth purple", "Wrinkly purple"))


####### Junk code below this line



mydata.Day7.Dil6.H.SE <- summarySE(mydata.Day7.long.subset, measurevar="H", groupvars=c("TreatDateSampDilDays", "Treatment", "SamplingDate"),
                                   conf.interval = 0.95, .drop = TRUE)



mydata.Day7.Total.SE <- summarySE(mydata.Day7.Dil6, measurevar="Total", groupvars=c("TreatDateSampDilDays", "Treatment", "SamplingDate"),
                                    conf.interval = 0.95, .drop = TRUE)

mydata.Day7.Dil6.H.SE <- summarySE(mydata.Day7.long.subset, measurevar="H", groupvars=c("TreatDateSampDilDays", "Treatment", "SamplingDate"),
                        conf.interval = 0.95, .drop = TRUE)
mydata.Day7.Dil6.White.SE <- summarySE(mydata.Day7.Dil6, measurevar="White", groupvars=c("TreatDateSampDilDays", "Treatment", "SamplingDate"),
                                       conf.interval = 0.95, .drop = TRUE)

mydata.Day7.Dil6.Purple.S.SE <- summarySE(mydata.Day7.Dil6, measurevar="Purple.S", groupvars=c("TreatDateSampDilDays", "Treatment", "SamplingDate"),
                                       conf.interval = 0.95, .drop = TRUE)

mydata.Day7.Dil6.Purple.wr.SE <- summarySE(mydata.Day7.Dil6, measurevar="Purple.wr", groupvars=c("TreatDateSampDilDays", "Treatment", "SamplingDate"),
                                          conf.interval = 0.95, .drop = TRUE)


# Reshape data into long form for stacked bar chart
mydata.Day7.long <- melt(subset(mydata.Day7, select = -c(H,Total)), 
            id.var= c("Treatment", "SamplingDate", "PlateDilution", "PictureDate.days.since.plating.", "TreatDateSampDilDays", "Replicate"))

# plot stacked bar chart for each plate dilution for 7-day old plates
ggplot(subset(mydata.Day7.long, PlateDilution == -5), aes(x=SamplingDate, y=value, fill=variable)) +
  geom_bar(stat="identity") + facet_grid(~Treatment) + theme_bw()

ggplot(subset(mydata.Day7.long, PlateDilution == -6), aes(x=SamplingDate, y=value, fill=variable)) +
  geom_bar(stat="identity") + facet_grid(~Treatment) + theme_bw()

ggplot(subset(mydata.Day7.long, PlateDilution == -7), aes(x=SamplingDate, y=value, fill=variable)) +
  geom_bar(stat="identity") + facet_grid(~Treatment) + theme_bw()


# We see here that what dilution is appropriate depends on the sampling date
# This is a technical issue.
#### To do: For each sample, pick an appropriate dilution. Normal <30 & >300 threshold isn't appropriate, since 
#### we'll loose a lot of data that was countable


# Now plot Shannon's diversity 

ggplot(data=mydata.Day7.Dil6.H.SE, aes(x=SamplingDate, y=H, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")




