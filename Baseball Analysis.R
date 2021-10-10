#### Baseball Project

### Ian Scarff

### STAT 485

### Compare pitchers accuracy.


### Import cleaned data set

DATA.full <- read.csv("ready_data.csv", header = TRUE)
View(DATA.full)

### Seperate data set into variables we need

DATA.sub <- data.frame(DATA.full$Pitcher, DATA.full$PitcherThrows, 
                       DATA.full$PlateLocHeight,DATA.full$PlateLocSide,
                       DATA.full$AutoPitchType)
colnames(DATA.sub) <- c("Pitcher", "PitcherThrows", "PlateLocHeight", "PlateLocSide", 
                        "AutoPitchType")

### Look at pitcher names

summary(DATA.sub$Pitcher)

### Seperate data by pitchers

Kaylor <- na.omit(DATA.sub[DATA.sub$Pitcher == "Chafin, Kaylor",])
John <- na.omit(DATA.sub[DATA.sub$Pitcher == "Doxakis, John",])
Nolan <- na.omit(DATA.sub[DATA.sub$Pitcher == "Hoffman, Nolan",])
Chandler <- na.omit(DATA.sub[DATA.sub$Pitcher == "Jozwiak, Chandler",])
Mitchell <- na.omit(DATA.sub[DATA.sub$Pitcher == "Kilkenny, Mitchell",])
Stephen <- na.omit(DATA.sub[DATA.sub$Pitcher == "Kolek, Stephen",])
Asa <- na.omit(DATA.sub[DATA.sub$Pitcher == "Lacy, Asa",])
Christian <- na.omit(DATA.sub[DATA.sub$Pitcher == "Roa, Christian",])
Dustin <- na.omit(DATA.sub[DATA.sub$Pitcher == "Saenz, Dustin",])
Carson <- na.omit(DATA.sub[DATA.sub$Pitcher == "Sherrod, Cason",])

### Players don't have the same amount of data


### Create graphical visualizations of pitcher statistics



### Kaylor

### Dimensions for graph
max.height <- max(Kaylor$PlateLocHeight)
min.height <- min(Kaylor$PlateLocHeight)
max.side <- max(Kaylor$PlateLocSide)
min.side <- min(Kaylor$PlateLocSide)

### points in the strike zone
Kaylor.strikezone <- Kaylor[((Kaylor$PlateLocSide > -0.83) & (Kaylor$PlateLocSide < 0.83)) &
                              ((Kaylor$PlateLocHeight > 1.52) & (Kaylor$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Kaylor.NotstrikezoneCorners <- Kaylor[!((Kaylor$PlateLocSide > -0.83) & (Kaylor$PlateLocSide < 0.83)) &
                                   !((Kaylor$PlateLocHeight > 1.52) & (Kaylor$PlateLocHeight < 3.42)),]

Kaylor.NotstrikezoneMidleftright <-  Kaylor[!((Kaylor$PlateLocSide > -0.83) & (Kaylor$PlateLocSide < 0.83)) &
                                              ((Kaylor$PlateLocHeight > 1.52) & (Kaylor$PlateLocHeight < 3.42)),]

Kaylor.NotstrikezoneMidupdown <-  Kaylor[((Kaylor$PlateLocSide > -0.83) & (Kaylor$PlateLocSide < 0.83)) &
                                              !((Kaylor$PlateLocHeight > 1.52) & (Kaylor$PlateLocHeight < 3.42)),]


### Plot Kaylor's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Chafin, Kaylor Pitches")

points(Kaylor.NotstrikezoneCorners$PlateLocSide,Kaylor.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Kaylor.NotstrikezoneMidleftright$PlateLocSide,Kaylor.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Kaylor.NotstrikezoneMidupdown$PlateLocSide,Kaylor.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Kaylor.strikezone$PlateLocSide,Kaylor.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Kaylor.strikezone),
            (nrow(Kaylor.NotstrikezoneCorners) + nrow(Kaylor.NotstrikezoneMidleftright) + 
               nrow(Kaylor.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Chafin, Kaylor Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Kaylor$AutoPitchType)

Kaylor.ChangeUp <- Kaylor[Kaylor$AutoPitchType == "ChangeUp",]
Kaylor.Fastball <- Kaylor[Kaylor$AutoPitchType == "Fastball",]
Kaylor.Slider <- Kaylor[Kaylor$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Chafin, Kaylor Pitch Type")

points(Kaylor.ChangeUp$PlateLocSide,Kaylor.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Kaylor.Fastball$PlateLocSide,Kaylor.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Kaylor.Slider$PlateLocSide,Kaylor.Slider$PlateLocHeight,
       pch = 2, col = "purple")

segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

legend("bottomleft", legend = c("ChangeUp","Fastball","Slider","Strike Zone"), 
       pch = c(0,1,2,NA), col = c("orange","blue","purple","black"),
       lty = c(0,0,0,1), lwd = c(0,0,0,3), bty = "n", seg.len = 0.3)


### Look at summary of pitch type
summary(Kaylor$AutoPitchType)


### Make pie graph
slices <- c(sum(Kaylor$AutoPitchType == "ChangeUp"),
            sum(Kaylor$AutoPitchType == "Fastball"),
            sum(Kaylor$AutoPitchType == "Slider"))

labels <- c("ChangeUp","Fastball","Slider")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple"),
    main = "Chafin, Kaylor Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")





### John

max.height <- max(John$PlateLocHeight)
min.height <- min(John$PlateLocHeight)
max.side <- max(John$PlateLocSide)
min.side <- min(John$PlateLocSide)

### points in the strike zone
John.strikezone <- John[((John$PlateLocSide > -0.83) & (John$PlateLocSide < 0.83)) &
                              ((John$PlateLocHeight > 1.52) & (John$PlateLocHeight < 3.42)),]

### Points outside the strike zone
John.NotstrikezoneCorners <- John[!((John$PlateLocSide > -0.83) & (John$PlateLocSide < 0.83)) &
                                        !((John$PlateLocHeight > 1.52) & (John$PlateLocHeight < 3.42)),]

John.NotstrikezoneMidleftright <-  John[!((John$PlateLocSide > -0.83) & (John$PlateLocSide < 0.83)) &
                                              ((John$PlateLocHeight > 1.52) & (John$PlateLocHeight < 3.42)),]

John.NotstrikezoneMidupdown <-  John[((John$PlateLocSide > -0.83) & (John$PlateLocSide < 0.83)) &
                                           !((John$PlateLocHeight > 1.52) & (John$PlateLocHeight < 3.42)),]


### Plot John's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Doxakis, John Pitches")

points(John.NotstrikezoneCorners$PlateLocSide,John.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(John.NotstrikezoneMidleftright$PlateLocSide,John.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(John.NotstrikezoneMidupdown$PlateLocSide,John.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(John.strikezone$PlateLocSide,John.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(John.strikezone),
            (nrow(John.NotstrikezoneCorners) + nrow(John.NotstrikezoneMidleftright) + 
               nrow(John.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Doxakis, John Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(John$AutoPitchType)

John.ChangeUp <- John[John$AutoPitchType == "ChangeUp",]
John.Curveball <- John[John$AutoPitchType == "Curveball",]
John.Fastball <- John[John$AutoPitchType == "Fastball",]
John.Other <- John[John$AutoPitchType == "Other",]
John.Sinker <- John[John$AutoPitchType == "Sinker",]
John.Slider <- John[John$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Doxakis, John Pitch Type")

points(John.ChangeUp$PlateLocSide,John.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(John.Fastball$PlateLocSide,John.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(John.Slider$PlateLocSide,John.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(John.Curveball$PlateLocSide,John.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(John.Sinker$PlateLocSide,John.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(John.Other$PlateLocSide,John.Other$PlateLocHeight,
       pch = 5, col = "red")


segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)



legend("bottomleft", legend = c("ChangeUp","Fastball","Slider",
                                "Curveball", "Sinker","Other", "Strike Zone"), 
       pch = c(0,1,2,3,4,5,NA), 
       col = c("orange","blue","purple","pink","brown","red","black"),
       lty = c(0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(John$AutoPitchType)


### Make pie graph
slices <- c(sum(John$AutoPitchType == "ChangeUp"),
            sum(John$AutoPitchType == "Fastball"),
            sum(John$AutoPitchType == "Slider"),
            sum(John$AutoPitchType == "Curveball"),
            sum(John$AutoPitchType == "Sinker"),
            sum(John$AutoPitchType == "Other"))

labels <- c("ChangeUp","Fastball","Slider", "Curveball", "Sinker","Other")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown","red"),
    main = "Doxakis, John Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")



### Nolan
max.height <- max(Nolan$PlateLocHeight)
min.height <- min(Nolan$PlateLocHeight)
max.side <- max(Nolan$PlateLocSide)
min.side <- min(Nolan$PlateLocSide)

### points in the strike zone
Nolan.strikezone <- Nolan[((Nolan$PlateLocSide > -0.83) & (Nolan$PlateLocSide < 0.83)) &
                          ((Nolan$PlateLocHeight > 1.52) & (Nolan$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Nolan.NotstrikezoneCorners <- Nolan[!((Nolan$PlateLocSide > -0.83) & (Nolan$PlateLocSide < 0.83)) &
                                    !((Nolan$PlateLocHeight > 1.52) & (Nolan$PlateLocHeight < 3.42)),]

Nolan.NotstrikezoneMidleftright <-  Nolan[!((Nolan$PlateLocSide > -0.83) & (Nolan$PlateLocSide < 0.83)) &
                                          ((Nolan$PlateLocHeight > 1.52) & (Nolan$PlateLocHeight < 3.42)),]

Nolan.NotstrikezoneMidupdown <-  Nolan[((Nolan$PlateLocSide > -0.83) & (Nolan$PlateLocSide < 0.83)) &
                                       !((Nolan$PlateLocHeight > 1.52) & (Nolan$PlateLocHeight < 3.42)),]


### Plot Nolan's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Hoffman, Nolan Pitches")

points(Nolan.NotstrikezoneCorners$PlateLocSide,Nolan.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Nolan.NotstrikezoneMidleftright$PlateLocSide,Nolan.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Nolan.NotstrikezoneMidupdown$PlateLocSide,Nolan.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Nolan.strikezone$PlateLocSide,Nolan.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Nolan.strikezone),
            (nrow(Nolan.NotstrikezoneCorners) + nrow(Nolan.NotstrikezoneMidleftright) + 
               nrow(Nolan.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Hoffman, Nolan Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Nolan$AutoPitchType)

Nolan.ChangeUp <- Nolan[Nolan$AutoPitchType == "ChangeUp",]
Nolan.Curveball <- Nolan[Nolan$AutoPitchType == "Curveball",]
Nolan.Other <- Nolan[Nolan$AutoPitchType == "Other",]
Nolan.Sinker <- Nolan[Nolan$AutoPitchType == "Sinker",]
Nolan.Slider <- Nolan[Nolan$AutoPitchType == "Slider",]
Nolan.Splitter <- Nolan[Nolan$AutoPitchType == "Splitter",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Hoffman, Nolan Pitch Type")

points(Nolan.ChangeUp$PlateLocSide,Nolan.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Nolan.Slider$PlateLocSide,Nolan.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Nolan.Curveball$PlateLocSide,Nolan.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Nolan.Sinker$PlateLocSide,Nolan.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Nolan.Other$PlateLocSide,Nolan.Other$PlateLocHeight,
       pch = 5, col = "red")
points(Nolan.Splitter$PlateLocSide,Nolan.Splitter$PlateLocHeight,
       pch = 6, col = "yellow2")

segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)



legend("topleft", legend = c("ChangeUp","Slider",
                                "Curveball", "Sinker","Splitter","Other", "Strike Zone"), 
       pch = c(0,2,3,4,5,6,NA), 
       col = c("orange","purple","pink","brown","yellow2","red","black"),
       lty = c(0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Nolan$AutoPitchType)


### Make pie graph
slices <- c(sum(Nolan$AutoPitchType == "ChangeUp"),
            sum(Nolan$AutoPitchType == "Curveball"),
            sum(Nolan$AutoPitchType == "Slider"),
            sum(Nolan$AutoPitchType == "Sinker"),
            sum(Nolan$AutoPitchType == "Other"),
            sum(Nolan$AutoPitchType == "Splitter"))

labels <- c("ChangeUp","Curveball","Slider","Sinker","Other","Splitter")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","purple","pink","brown","yellow2","red"),
    main = "Hoffman, Nolan Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


### Chandler

max.height <- max(Chandler$PlateLocHeight)
min.height <- min(Chandler$PlateLocHeight)
max.side <- max(Chandler$PlateLocSide)
min.side <- min(Chandler$PlateLocSide)

### points in the strike zone
Chandler.strikezone <- Chandler[((Chandler$PlateLocSide > -0.83) & (Chandler$PlateLocSide < 0.83)) &
                            ((Chandler$PlateLocHeight > 1.52) & (Chandler$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Chandler.NotstrikezoneCorners <- Chandler[!((Chandler$PlateLocSide > -0.83) & (Chandler$PlateLocSide < 0.83)) &
                                      !((Chandler$PlateLocHeight > 1.52) & (Chandler$PlateLocHeight < 3.42)),]

Chandler.NotstrikezoneMidleftright <-  Chandler[!((Chandler$PlateLocSide > -0.83) & (Chandler$PlateLocSide < 0.83)) &
                                            ((Chandler$PlateLocHeight > 1.52) & (Chandler$PlateLocHeight < 3.42)),]

Chandler.NotstrikezoneMidupdown <-  Chandler[((Chandler$PlateLocSide > -0.83) & (Chandler$PlateLocSide < 0.83)) &
                                         !((Chandler$PlateLocHeight > 1.52) & (Chandler$PlateLocHeight < 3.42)),]


### Plot Chandler's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Jozwiak, Chandler Pitches")

points(Chandler.NotstrikezoneCorners$PlateLocSide,Chandler.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Chandler.NotstrikezoneMidleftright$PlateLocSide,Chandler.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Chandler.NotstrikezoneMidupdown$PlateLocSide,Chandler.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Chandler.strikezone$PlateLocSide,Chandler.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Chandler.strikezone),
            (nrow(Chandler.NotstrikezoneCorners) + nrow(Chandler.NotstrikezoneMidleftright) + 
               nrow(Chandler.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Jozwiak, Chandler Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Chandler$AutoPitchType)

Chandler.ChangeUp <- Chandler[Chandler$AutoPitchType == "ChangeUp",]
Chandler.Curveball <- Chandler[Chandler$AutoPitchType == "Curveball",]
Chandler.Fastball <- Chandler[Chandler$AutoPitchType == "Fastball",]
Chandler.Other <- Chandler[Chandler$AutoPitchType == "Other",]
Chandler.Sinker <- Chandler[Chandler$AutoPitchType == "Sinker",]
Chandler.Slider <- Chandler[Chandler$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Jozwiak, Chandler Pitch Type")

points(Chandler.ChangeUp$PlateLocSide,Chandler.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Chandler.Fastball$PlateLocSide,Chandler.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Chandler.Slider$PlateLocSide,Chandler.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Chandler.Curveball$PlateLocSide,Chandler.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Chandler.Sinker$PlateLocSide,Chandler.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Chandler.Other$PlateLocSide,Chandler.Other$PlateLocHeight,
       pch = 5, col = "red")


segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend("bottomleft", legend = c("ChangeUp","Fastball","Slider",
                             "Curveball", "Sinker","Other", "Strike Zone"), 
       pch = c(0,1,2,3,4,5,NA), 
       col = c("orange","blue","purple","pink","brown","red","black"),
       lty = c(0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Chandler$AutoPitchType)


### Make pie graph
slices <- c(sum(Chandler$AutoPitchType == "ChangeUp"),
            sum(Chandler$AutoPitchType == "Fastball"),
            sum(Chandler$AutoPitchType == "Curveball"),
            sum(Chandler$AutoPitchType == "Slider"),
            sum(Chandler$AutoPitchType == "Sinker"),
            sum(Chandler$AutoPitchType == "Other"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker","Other")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown","red"),
    main = "Jozwiak, Chandler Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


### Mitchell

max.height <- max(Mitchell$PlateLocHeight)
min.height <- min(Mitchell$PlateLocHeight)
max.side <- max(Mitchell$PlateLocSide)
min.side <- min(Mitchell$PlateLocSide)

### points in the strike zone
Mitchell.strikezone <- Mitchell[((Mitchell$PlateLocSide > -0.83) & (Mitchell$PlateLocSide < 0.83)) &
                                  ((Mitchell$PlateLocHeight > 1.52) & (Mitchell$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Mitchell.NotstrikezoneCorners <- Mitchell[!((Mitchell$PlateLocSide > -0.83) & (Mitchell$PlateLocSide < 0.83)) &
                                            !((Mitchell$PlateLocHeight > 1.52) & (Mitchell$PlateLocHeight < 3.42)),]

Mitchell.NotstrikezoneMidleftright <-  Mitchell[!((Mitchell$PlateLocSide > -0.83) & (Mitchell$PlateLocSide < 0.83)) &
                                                  ((Mitchell$PlateLocHeight > 1.52) & (Mitchell$PlateLocHeight < 3.42)),]

Mitchell.NotstrikezoneMidupdown <-  Mitchell[((Mitchell$PlateLocSide > -0.83) & (Mitchell$PlateLocSide < 0.83)) &
                                               !((Mitchell$PlateLocHeight > 1.52) & (Mitchell$PlateLocHeight < 3.42)),]


### Plot Mitchell's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Kilkenny, Mitchell Pitches")

points(Mitchell.NotstrikezoneCorners$PlateLocSide,Mitchell.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Mitchell.NotstrikezoneMidleftright$PlateLocSide,Mitchell.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Mitchell.NotstrikezoneMidupdown$PlateLocSide,Mitchell.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Mitchell.strikezone$PlateLocSide,Mitchell.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Mitchell.strikezone),
            (nrow(Mitchell.NotstrikezoneCorners) + nrow(Mitchell.NotstrikezoneMidleftright) + 
               nrow(Mitchell.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Kilkenny, Mitchell Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Mitchell$AutoPitchType)

Mitchell.ChangeUp <- Mitchell[Mitchell$AutoPitchType == "ChangeUp",]
Mitchell.Curveball <- Mitchell[Mitchell$AutoPitchType == "Curveball",]
Mitchell.Fastball <- Mitchell[Mitchell$AutoPitchType == "Fastball",]
Mitchell.Other <- Mitchell[Mitchell$AutoPitchType == "Other",]
Mitchell.Sinker <- Mitchell[Mitchell$AutoPitchType == "Sinker",]
Mitchell.Slider <- Mitchell[Mitchell$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Kilkenny, Mitchell Pitch Type")

points(Mitchell.ChangeUp$PlateLocSide,Mitchell.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Mitchell.Fastball$PlateLocSide,Mitchell.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Mitchell.Slider$PlateLocSide,Mitchell.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Mitchell.Curveball$PlateLocSide,Mitchell.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Mitchell.Sinker$PlateLocSide,Mitchell.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Mitchell.Other$PlateLocSide,Mitchell.Other$PlateLocHeight,
       pch = 5, col = "red")


segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend("bottomleft", legend = c("ChangeUp","Fastball","Slider",
                                "Curveball", "Sinker","Other", "Strike Zone"), 
       pch = c(0,1,2,3,4,5,NA), 
       col = c("orange","blue","purple","pink","brown","red","black"),
       lty = c(0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Mitchell$AutoPitchType)


### Make pie graph
slices <- c(sum(Mitchell$AutoPitchType == "ChangeUp"),
            sum(Mitchell$AutoPitchType == "Fastball"),
            sum(Mitchell$AutoPitchType == "Curveball"),
            sum(Mitchell$AutoPitchType == "Slider"),
            sum(Mitchell$AutoPitchType == "Sinker"),
            sum(Mitchell$AutoPitchType == "Other"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker","Other")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown","red"),
    main = "Kilkenny, Mitchell Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


### Stephen
max.height <- max(Stephen$PlateLocHeight)
min.height <- min(Stephen$PlateLocHeight)
max.side <- max(Stephen$PlateLocSide)
min.side <- min(Stephen$PlateLocSide)

### points in the strike zone
Stephen.strikezone <- Stephen[((Stephen$PlateLocSide > -0.83) & (Stephen$PlateLocSide < 0.83)) &
                                  ((Stephen$PlateLocHeight > 1.52) & (Stephen$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Stephen.NotstrikezoneCorners <- Stephen[!((Stephen$PlateLocSide > -0.83) & (Stephen$PlateLocSide < 0.83)) &
                                            !((Stephen$PlateLocHeight > 1.52) & (Stephen$PlateLocHeight < 3.42)),]

Stephen.NotstrikezoneMidleftright <-  Stephen[!((Stephen$PlateLocSide > -0.83) & (Stephen$PlateLocSide < 0.83)) &
                                                  ((Stephen$PlateLocHeight > 1.52) & (Stephen$PlateLocHeight < 3.42)),]

Stephen.NotstrikezoneMidupdown <-  Stephen[((Stephen$PlateLocSide > -0.83) & (Stephen$PlateLocSide < 0.83)) &
                                               !((Stephen$PlateLocHeight > 1.52) & (Stephen$PlateLocHeight < 3.42)),]


### Plot Stephen's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Kolek, Stephen Pitches")

points(Stephen.NotstrikezoneCorners$PlateLocSide,Stephen.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Stephen.NotstrikezoneMidleftright$PlateLocSide,Stephen.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Stephen.NotstrikezoneMidupdown$PlateLocSide,Stephen.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Stephen.strikezone$PlateLocSide,Stephen.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Stephen.strikezone),
            (nrow(Stephen.NotstrikezoneCorners) + nrow(Stephen.NotstrikezoneMidleftright) + 
               nrow(Stephen.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Kolek, Stephen Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Stephen$AutoPitchType)

Stephen.ChangeUp <- Stephen[Stephen$AutoPitchType == "ChangeUp",]
Stephen.Curveball <- Stephen[Stephen$AutoPitchType == "Curveball",]
Stephen.Cutter <- Stephen[Stephen$AutoPitchType == "Cutter",]
Stephen.Fastball <- Stephen[Stephen$AutoPitchType == "Fastball",]
Stephen.Other <- Stephen[Stephen$AutoPitchType == "Other",]
Stephen.Sinker <- Stephen[Stephen$AutoPitchType == "Sinker",]
Stephen.Slider <- Stephen[Stephen$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Kolek, Stephen Pitch Type")

points(Stephen.ChangeUp$PlateLocSide,Stephen.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Stephen.Fastball$PlateLocSide,Stephen.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Stephen.Slider$PlateLocSide,Stephen.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Stephen.Curveball$PlateLocSide,Stephen.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Stephen.Sinker$PlateLocSide,Stephen.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Stephen.Other$PlateLocSide,Stephen.Other$PlateLocHeight,
       pch = 5, col = "red")
points(Stephen.Cutter$PlateLocSide,Stephen.Cutter$PlateLocHeight,
       pch = 7, col = "lightblue")


segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend(x= 3.8, y = 1.8, legend = c("ChangeUp","Fastball","Slider",
                                "Curveball", "Sinker","Other","Cutter", "Strike Zone"), 
       pch = c(0,1,2,3,4,5,7,NA), 
       col = c("orange","blue","purple","pink","brown","red","lightblue","black"),
       lty = c(0,0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Stephen$AutoPitchType)


### Make pie graph
slices <- c(sum(Stephen$AutoPitchType == "ChangeUp"),
            sum(Stephen$AutoPitchType == "Fastball"),
            sum(Stephen$AutoPitchType == "Curveball"),
            sum(Stephen$AutoPitchType == "Slider"),
            sum(Stephen$AutoPitchType == "Sinker"),
            sum(Stephen$AutoPitchType == "Other"),
            sum(Stephen$AutoPitchType == "Cutter"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker","Other","Cutter")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown",
                                     "red","lightblue"),
    main = "Kolek, Stephen Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")



### Asa

max.height <- max(Asa$PlateLocHeight)
min.height <- min(Asa$PlateLocHeight)
max.side <- max(Asa$PlateLocSide)
min.side <- min(Asa$PlateLocSide)

### points in the strike zone
Asa.strikezone <- Asa[((Asa$PlateLocSide > -0.83) & (Asa$PlateLocSide < 0.83)) &
                                ((Asa$PlateLocHeight > 1.52) & (Asa$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Asa.NotstrikezoneCorners <- Asa[!((Asa$PlateLocSide > -0.83) & (Asa$PlateLocSide < 0.83)) &
                                          !((Asa$PlateLocHeight > 1.52) & (Asa$PlateLocHeight < 3.42)),]

Asa.NotstrikezoneMidleftright <-  Asa[!((Asa$PlateLocSide > -0.83) & (Asa$PlateLocSide < 0.83)) &
                                                ((Asa$PlateLocHeight > 1.52) & (Asa$PlateLocHeight < 3.42)),]

Asa.NotstrikezoneMidupdown <-  Asa[((Asa$PlateLocSide > -0.83) & (Asa$PlateLocSide < 0.83)) &
                                             !((Asa$PlateLocHeight > 1.52) & (Asa$PlateLocHeight < 3.42)),]


### Plot Asa's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Lacy, Asa Pitches")

points(Asa.NotstrikezoneCorners$PlateLocSide,Asa.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Asa.NotstrikezoneMidleftright$PlateLocSide,Asa.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Asa.NotstrikezoneMidupdown$PlateLocSide,Asa.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Asa.strikezone$PlateLocSide,Asa.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Asa.strikezone),
            (nrow(Asa.NotstrikezoneCorners) + nrow(Asa.NotstrikezoneMidleftright) + 
               nrow(Asa.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Lacy, Asa Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Asa$AutoPitchType)

Asa.ChangeUp <- Asa[Asa$AutoPitchType == "ChangeUp",]
Asa.Curveball <- Asa[Asa$AutoPitchType == "Curveball",]
Asa.Fastball <- Asa[Asa$AutoPitchType == "Fastball",]
Asa.Sinker <- Asa[Asa$AutoPitchType == "Sinker",]
Asa.Slider <- Asa[Asa$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Lacy, Asa Pitch Type")

points(Asa.ChangeUp$PlateLocSide,Asa.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Asa.Fastball$PlateLocSide,Asa.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Asa.Slider$PlateLocSide,Asa.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Asa.Curveball$PlateLocSide,Asa.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Asa.Sinker$PlateLocSide,Asa.Sinker$PlateLocHeight,
       pch = 4, col = "brown")


segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend(x = 1.5, y = 6, legend = c("ChangeUp","Fastball","Slider",
                                   "Curveball", "Sinker","Strike Zone"), 
       pch = c(0,1,2,3,4,NA), 
       col = c("orange","blue","purple","pink","brown","black"),
       lty = c(0,0,0,0,0,1), lwd = c(0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Asa$AutoPitchType)


### Make pie graph
slices <- c(sum(Asa$AutoPitchType == "ChangeUp"),
            sum(Asa$AutoPitchType == "Fastball"),
            sum(Asa$AutoPitchType == "Curveball"),
            sum(Asa$AutoPitchType == "Slider"),
            sum(Asa$AutoPitchType == "Sinker"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown"),
    main = "Lacy, Asa Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")



### Christian

max.height <- max(Christian$PlateLocHeight)
min.height <- min(Christian$PlateLocHeight)
max.side <- max(Christian$PlateLocSide)
min.side <- min(Christian$PlateLocSide)

### points in the strike zone
Christian.strikezone <- Christian[((Christian$PlateLocSide > -0.83) & (Christian$PlateLocSide < 0.83)) &
                                ((Christian$PlateLocHeight > 1.52) & (Christian$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Christian.NotstrikezoneCorners <- Christian[!((Christian$PlateLocSide > -0.83) & (Christian$PlateLocSide < 0.83)) &
                                          !((Christian$PlateLocHeight > 1.52) & (Christian$PlateLocHeight < 3.42)),]

Christian.NotstrikezoneMidleftright <-  Christian[!((Christian$PlateLocSide > -0.83) & (Christian$PlateLocSide < 0.83)) &
                                                ((Christian$PlateLocHeight > 1.52) & (Christian$PlateLocHeight < 3.42)),]

Christian.NotstrikezoneMidupdown <-  Christian[((Christian$PlateLocSide > -0.83) & (Christian$PlateLocSide < 0.83)) &
                                             !((Christian$PlateLocHeight > 1.52) & (Christian$PlateLocHeight < 3.42)),]


### Plot Christian's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Roa, Christian Pitches")

points(Christian.NotstrikezoneCorners$PlateLocSide,Christian.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Christian.NotstrikezoneMidleftright$PlateLocSide,Christian.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Christian.NotstrikezoneMidupdown$PlateLocSide,Christian.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Christian.strikezone$PlateLocSide,Christian.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Christian.strikezone),
            (nrow(Christian.NotstrikezoneCorners) + nrow(Christian.NotstrikezoneMidleftright) + 
               nrow(Christian.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Roa, Christian Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Christian$AutoPitchType)

Christian.ChangeUp <- Christian[Christian$AutoPitchType == "ChangeUp",]
Christian.Curveball <- Christian[Christian$AutoPitchType == "Curveball",]
Christian.Fastball <- Christian[Christian$AutoPitchType == "Fastball",]
Christian.Slider <- Christian[Christian$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Roa, Christian Pitch Type")

points(Christian.ChangeUp$PlateLocSide,Christian.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Christian.Fastball$PlateLocSide,Christian.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Christian.Slider$PlateLocSide,Christian.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Christian.Curveball$PlateLocSide,Christian.Curveball$PlateLocHeight,
       pch = 3, col = "pink")

segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend("left", legend = c("ChangeUp","Fastball","Slider",
                                   "Curveball","Strike Zone"), 
       pch = c(0,1,2,3,NA), 
       col = c("orange","blue","purple","pink","black"),
       lty = c(0,0,0,0,1), lwd = c(0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Christian$AutoPitchType)


### Make pie graph
slices <- c(sum(Christian$AutoPitchType == "ChangeUp"),
            sum(Christian$AutoPitchType == "Fastball"),
            sum(Christian$AutoPitchType == "Curveball"),
            sum(Christian$AutoPitchType == "Slider"))

labels <- c("ChangeUp","Fastball","Curveball","Slider")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink"),
    main = "Roa, Christian Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")




### Dustin

max.height <- max(Dustin$PlateLocHeight)
min.height <- min(Dustin$PlateLocHeight)
max.side <- max(Dustin$PlateLocSide)
min.side <- min(Dustin$PlateLocSide)

### points in the strike zone
Dustin.strikezone <- Dustin[((Dustin$PlateLocSide > -0.83) & (Dustin$PlateLocSide < 0.83)) &
                                ((Dustin$PlateLocHeight > 1.52) & (Dustin$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Dustin.NotstrikezoneCorners <- Dustin[!((Dustin$PlateLocSide > -0.83) & (Dustin$PlateLocSide < 0.83)) &
                                          !((Dustin$PlateLocHeight > 1.52) & (Dustin$PlateLocHeight < 3.42)),]

Dustin.NotstrikezoneMidleftright <-  Dustin[!((Dustin$PlateLocSide > -0.83) & (Dustin$PlateLocSide < 0.83)) &
                                                ((Dustin$PlateLocHeight > 1.52) & (Dustin$PlateLocHeight < 3.42)),]

Dustin.NotstrikezoneMidupdown <-  Dustin[((Dustin$PlateLocSide > -0.83) & (Dustin$PlateLocSide < 0.83)) &
                                             !((Dustin$PlateLocHeight > 1.52) & (Dustin$PlateLocHeight < 3.42)),]


### Plot Dustin's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Saenz, Dustin Pitches")

points(Dustin.NotstrikezoneCorners$PlateLocSide,Dustin.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Dustin.NotstrikezoneMidleftright$PlateLocSide,Dustin.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Dustin.NotstrikezoneMidupdown$PlateLocSide,Dustin.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Dustin.strikezone$PlateLocSide,Dustin.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Dustin.strikezone),
            (nrow(Dustin.NotstrikezoneCorners) + nrow(Dustin.NotstrikezoneMidleftright) + 
               nrow(Dustin.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Saenz, Dustin Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Dustin$AutoPitchType)

Dustin.ChangeUp <- Dustin[Dustin$AutoPitchType == "ChangeUp",]
Dustin.Curveball <- Dustin[Dustin$AutoPitchType == "Curveball",]
Dustin.Fastball <- Dustin[Dustin$AutoPitchType == "Fastball",]
Dustin.Slider <- Dustin[Dustin$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Saenz, Dustin Pitch Type")

points(Dustin.ChangeUp$PlateLocSide,Dustin.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Dustin.Fastball$PlateLocSide,Dustin.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Dustin.Slider$PlateLocSide,Dustin.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Dustin.Curveball$PlateLocSide,Dustin.Curveball$PlateLocHeight,
       pch = 3, col = "pink")

segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend("bottomleft", legend = c("ChangeUp","Fastball","Slider",
                                   "Curveball", "Strike Zone"), 
       pch = c(0,1,2,3,NA), 
       col = c("orange","blue","purple","pink","black"),
       lty = c(0,0,0,0,1), lwd = c(0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Dustin$AutoPitchType)


### Make pie graph
slices <- c(sum(Dustin$AutoPitchType == "ChangeUp"),
            sum(Dustin$AutoPitchType == "Fastball"),
            sum(Dustin$AutoPitchType == "Curveball"),
            sum(Dustin$AutoPitchType == "Slider"))

labels <- c("ChangeUp","Fastball","Curveball","Slider")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink"),
    main = "Saenz, Dustin Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")



### Carson

max.height <- max(Carson$PlateLocHeight)
min.height <- min(Carson$PlateLocHeight)
max.side <- max(Carson$PlateLocSide)
min.side <- min(Carson$PlateLocSide)

### points in the strike zone
Carson.strikezone <- Carson[((Carson$PlateLocSide > -0.83) & (Carson$PlateLocSide < 0.83)) &
                                ((Carson$PlateLocHeight > 1.52) & (Carson$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Carson.NotstrikezoneCorners <- Carson[!((Carson$PlateLocSide > -0.83) & (Carson$PlateLocSide < 0.83)) &
                                          !((Carson$PlateLocHeight > 1.52) & (Carson$PlateLocHeight < 3.42)),]

Carson.NotstrikezoneMidleftright <-  Carson[!((Carson$PlateLocSide > -0.83) & (Carson$PlateLocSide < 0.83)) &
                                                ((Carson$PlateLocHeight > 1.52) & (Carson$PlateLocHeight < 3.42)),]

Carson.NotstrikezoneMidupdown <-  Carson[((Carson$PlateLocSide > -0.83) & (Carson$PlateLocSide < 0.83)) &
                                             !((Carson$PlateLocHeight > 1.52) & (Carson$PlateLocHeight < 3.42)),]


### Plot Carson's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Sherrod, Cason Pitches")

points(Carson.NotstrikezoneCorners$PlateLocSide,Carson.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Carson.NotstrikezoneMidleftright$PlateLocSide,Carson.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Carson.NotstrikezoneMidupdown$PlateLocSide,Carson.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Carson.strikezone$PlateLocSide,Carson.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Carson.strikezone),
            (nrow(Carson.NotstrikezoneCorners) + nrow(Carson.NotstrikezoneMidleftright) + 
               nrow(Carson.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Sherrod, Cason Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Carson$AutoPitchType)

Carson.ChangeUp <- Carson[Carson$AutoPitchType == "ChangeUp",]
Carson.Curveball <- Carson[Carson$AutoPitchType == "Curveball",]
Carson.Fastball <- Carson[Carson$AutoPitchType == "Fastball",]
Carson.Other <- Carson[Carson$AutoPitchType == "Other",]
Carson.Sinker <- Carson[Carson$AutoPitchType == "Sinker",]
Carson.Slider <- Carson[Carson$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Sherrod, Cason Pitch Type")

points(Carson.ChangeUp$PlateLocSide,Carson.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Carson.Fastball$PlateLocSide,Carson.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Carson.Slider$PlateLocSide,Carson.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Carson.Curveball$PlateLocSide,Carson.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Carson.Sinker$PlateLocSide,Carson.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Carson.Other$PlateLocSide,Carson.Other$PlateLocHeight,
       pch = 5, col = "red")

segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend(x= 2.5, y = 0.8, legend = c("ChangeUp","Fastball","Slider",
                                   "Curveball", "Sinker","Other", "Strike Zone"), 
       pch = c(0,1,2,3,4,5,NA), 
       col = c("orange","blue","purple","pink","brown","red","black"),
       lty = c(0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Carson$AutoPitchType)


### Make pie graph
slices <- c(sum(Carson$AutoPitchType == "ChangeUp"),
            sum(Carson$AutoPitchType == "Fastball"),
            sum(Carson$AutoPitchType == "Curveball"),
            sum(Carson$AutoPitchType == "Slider"),
            sum(Carson$AutoPitchType == "Sinker"),
            sum(Carson$AutoPitchType == "Other"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker","Other")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown",
                                     "red"),
    main = "Sherrod, Cason Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")




### Seperate the data set by left and right handed

Right.hand <- na.omit(DATA.sub[DATA.sub$PitcherThrows == "Right",])
Left.hand <- na.omit(DATA.sub[DATA.sub$PitcherThrows == "Left",])


### Right handed

max.height <- max(Right.hand$PlateLocHeight)
min.height <- min(Right.hand$PlateLocHeight)
max.side <- max(Right.hand$PlateLocSide)
min.side <- min(Right.hand$PlateLocSide)

### points in the strike zone
Right.hand.strikezone <- Right.hand[((Right.hand$PlateLocSide > -0.83) & (Right.hand$PlateLocSide < 0.83)) &
                                ((Right.hand$PlateLocHeight > 1.52) & (Right.hand$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Right.hand.NotstrikezoneCorners <- Right.hand[!((Right.hand$PlateLocSide > -0.83) & (Right.hand$PlateLocSide < 0.83)) &
                                          !((Right.hand$PlateLocHeight > 1.52) & (Right.hand$PlateLocHeight < 3.42)),]

Right.hand.NotstrikezoneMidleftright <-  Right.hand[!((Right.hand$PlateLocSide > -0.83) & (Right.hand$PlateLocSide < 0.83)) &
                                                ((Right.hand$PlateLocHeight > 1.52) & (Right.hand$PlateLocHeight < 3.42)),]

Right.hand.NotstrikezoneMidupdown <-  Right.hand[((Right.hand$PlateLocSide > -0.83) & (Right.hand$PlateLocSide < 0.83)) &
                                             !((Right.hand$PlateLocHeight > 1.52) & (Right.hand$PlateLocHeight < 3.42)),]


### Plot Right.hand's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Right Handed Pitches")

points(Right.hand.NotstrikezoneCorners$PlateLocSide,Right.hand.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Right.hand.NotstrikezoneMidleftright$PlateLocSide,Right.hand.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Right.hand.NotstrikezoneMidupdown$PlateLocSide,Right.hand.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Right.hand.strikezone$PlateLocSide,Right.hand.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Right.hand.strikezone),
            (nrow(Right.hand.NotstrikezoneCorners) + nrow(Right.hand.NotstrikezoneMidleftright) + 
               nrow(Right.hand.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Right Hand Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Right.hand$AutoPitchType)

Right.hand.ChangeUp <- Right.hand[Right.hand$AutoPitchType == "ChangeUp",]
Right.hand.Curveball <- Right.hand[Right.hand$AutoPitchType == "Curveball",]
Right.hand.Cutter <- Right.hand[Right.hand$AutoPitchType == "Cutter",]
Right.hand.Fastball <- Right.hand[Right.hand$AutoPitchType == "Fastball",]
Right.hand.Other <- Right.hand[Right.hand$AutoPitchType == "Other",]
Right.hand.Sinker <- Right.hand[Right.hand$AutoPitchType == "Sinker",]
Right.hand.Slider <- Right.hand[Right.hand$AutoPitchType == "Slider",]
Right.hand.Splitter <- Right.hand[Right.hand$AutoPitchType == "Splitter",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Right Hand Pitch Type")

points(Right.hand.ChangeUp$PlateLocSide,Right.hand.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Right.hand.Fastball$PlateLocSide,Right.hand.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Right.hand.Slider$PlateLocSide,Right.hand.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Right.hand.Curveball$PlateLocSide,Right.hand.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Right.hand.Sinker$PlateLocSide,Right.hand.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Right.hand.Other$PlateLocSide,Right.hand.Other$PlateLocHeight,
       pch = 5, col = "red")
points(Right.hand.Splitter$PlateLocSide,Nolan.Splitter$PlateLocHeight,
       pch = 6, col = "yellow2")
points(Right.hand.Cutter$PlateLocSide,Right.hand.Cutter$PlateLocHeight,
       pch = 7, col = "lightblue")


segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend(x= 4, y = 2.2, legend = c("ChangeUp","Fastball","Slider",
                                   "Curveball", "Sinker","Other","Splitter","Cutter", "Strike Zone"), 
       pch = c(0,1,2,3,4,5,6,7,NA), 
       col = c("orange","blue","purple","pink","brown","red","yellow2","lightblue","black"),
       lty = c(0,0,0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Right.hand$AutoPitchType)


### Make pie graph
slices <- c(sum(Right.hand$AutoPitchType == "ChangeUp"),
            sum(Right.hand$AutoPitchType == "Fastball"),
            sum(Right.hand$AutoPitchType == "Curveball"),
            sum(Right.hand$AutoPitchType == "Slider"),
            sum(Right.hand$AutoPitchType == "Sinker"),
            sum(Right.hand$AutoPitchType == "Other"),
            sum(Right.hand$AutoPitchType == "Cutter"),
            sum(Right.hand$AutoPitchType == "Splitter"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker","Other","Cutter", "Splitter")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown",
                                     "red","lightblue", "yellow2"),
    main = "Right Hand Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")



### Left handed

max.height <- max(Left.hand$PlateLocHeight)
min.height <- min(Left.hand$PlateLocHeight)
max.side <- max(Left.hand$PlateLocSide)
min.side <- min(Left.hand$PlateLocSide)

### points in the strike zone
Left.hand.strikezone <- Left.hand[((Left.hand$PlateLocSide > -0.83) & (Left.hand$PlateLocSide < 0.83)) &
                                      ((Left.hand$PlateLocHeight > 1.52) & (Left.hand$PlateLocHeight < 3.42)),]

### Points outside the strike zone
Left.hand.NotstrikezoneCorners <- Left.hand[!((Left.hand$PlateLocSide > -0.83) & (Left.hand$PlateLocSide < 0.83)) &
                                                !((Left.hand$PlateLocHeight > 1.52) & (Left.hand$PlateLocHeight < 3.42)),]

Left.hand.NotstrikezoneMidleftright <-  Left.hand[!((Left.hand$PlateLocSide > -0.83) & (Left.hand$PlateLocSide < 0.83)) &
                                                      ((Left.hand$PlateLocHeight > 1.52) & (Left.hand$PlateLocHeight < 3.42)),]

Left.hand.NotstrikezoneMidupdown <-  Left.hand[((Left.hand$PlateLocSide > -0.83) & (Left.hand$PlateLocSide < 0.83)) &
                                                   !((Left.hand$PlateLocHeight > 1.52) & (Left.hand$PlateLocHeight < 3.42)),]


### Plot Left.hand's pitch gaph
par(mfrow=c(1,2))
plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Left Handed Pitches")

points(Left.hand.NotstrikezoneCorners$PlateLocSide,Left.hand.NotstrikezoneCorners$PlateLocHeight,
       pch = 16, col = "red")
points(Left.hand.NotstrikezoneMidleftright$PlateLocSide,Left.hand.NotstrikezoneMidleftright$PlateLocHeight,
       pch = 16, col = "red")
points(Left.hand.NotstrikezoneMidupdown$PlateLocSide,Left.hand.NotstrikezoneMidupdown$PlateLocHeight,
       pch = 16, col = "red")
points(Left.hand.strikezone$PlateLocSide,Left.hand.strikezone$PlateLocHeight,
       pch = 16, col = "green")

### add strike zone
segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)

### Add legend
legend("bottomleft", legend = c("Strike Zone"), col = "black", bty = "n", 
       lty = 1, lwd = 3)

### Pie graph for strike zone percentage

slices <- c(nrow(Left.hand.strikezone),
            (nrow(Left.hand.NotstrikezoneCorners) + nrow(Left.hand.NotstrikezoneMidleftright) + 
               nrow(Left.hand.NotstrikezoneMidupdown)))
labels <- c("In Strike Zone", "Outside Strike Zone")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("green","red"),
    main = "Left Hand Strike Zone Accuracy")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")


#### Make plot for pitch type
summary(Left.hand$AutoPitchType)

Left.hand.ChangeUp <- Left.hand[Left.hand$AutoPitchType == "ChangeUp",]
Left.hand.Curveball <- Left.hand[Left.hand$AutoPitchType == "Curveball",]
Left.hand.Fastball <- Left.hand[Left.hand$AutoPitchType == "Fastball",]
Left.hand.Other <- Left.hand[Left.hand$AutoPitchType == "Other",]
Left.hand.Sinker <- Left.hand[Left.hand$AutoPitchType == "Sinker",]
Left.hand.Slider <- Left.hand[Left.hand$AutoPitchType == "Slider",]

plot(x=NULL,y= NULL, xlim = range((min.side - 0.5):(max.side + 0.5)),
     ylim = range((min.height - 0.5):(max.height + 0.5)), xlab = "PlateLocSide", 
     ylab = "PlateLocHeight", main = "Left Hand Pitch Type")

points(Left.hand.ChangeUp$PlateLocSide,Left.hand.ChangeUp$PlateLocHeight,
       pch = 0, col = "orange")
points(Left.hand.Fastball$PlateLocSide,Left.hand.Fastball$PlateLocHeight,
       pch = 1, col = "blue")
points(Left.hand.Slider$PlateLocSide,Left.hand.Slider$PlateLocHeight,
       pch = 2, col = "purple")
points(Left.hand.Curveball$PlateLocSide,Left.hand.Curveball$PlateLocHeight,
       pch = 3, col = "pink")
points(Left.hand.Sinker$PlateLocSide,Left.hand.Sinker$PlateLocHeight,
       pch = 4, col = "brown")
points(Left.hand.Other$PlateLocSide,Left.hand.Other$PlateLocHeight,
       pch = 5, col = "red")

segments(-0.83,1.52,-0.83,3.42, lwd = 3)
segments(-0.83,3.42,0.83,3.42, lwd = 3)
segments(0.83,3.42,0.83,1.52, lwd = 3)
segments(0.83,1.52,-0.83,1.52, lwd = 3)


legend(x= -4.6, y = 1.8, legend = c("ChangeUp","Fastball","Slider",
                                 "Curveball", "Sinker","Other","Strike Zone"), 
       pch = c(0,1,2,3,4,5,NA), 
       col = c("orange","blue","purple","pink","brown","red","black"),
       lty = c(0,0,0,0,0,0,1), lwd = c(0,0,0,0,0,0,3), bty = "n",
       seg.len = 0.3)


### Look at summary of pitch type
summary(Left.hand$AutoPitchType)


### Make pie graph
slices <- c(sum(Left.hand$AutoPitchType == "ChangeUp"),
            sum(Left.hand$AutoPitchType == "Fastball"),
            sum(Left.hand$AutoPitchType == "Curveball"),
            sum(Left.hand$AutoPitchType == "Slider"),
            sum(Left.hand$AutoPitchType == "Sinker"),
            sum(Left.hand$AutoPitchType == "Other"))

labels <- c("ChangeUp","Fastball","Curveball","Slider","Sinker","Other")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels,pct, sep = "  ")
labels <- paste(labels,"%",sep="")
labels <- paste(labels,slices, sep = " = ")
pie(slices, labels = labels, col = c("orange","blue","purple","pink","brown",
                                     "red"),
    main = "Left Hand Pitch Type")
leg.lab <- paste("Total Pitches = ", sum(slices), sep = "")
legend("bottom", legend = leg.lab, bty = "n")
















