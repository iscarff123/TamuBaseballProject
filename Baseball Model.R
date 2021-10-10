### STAT 485, Fall 2018

### Ian Scarff

### GOAL: To create a model to predict what an opposing pitcher might throw next.



### Import original data set

DATA.FULL <- read.csv("original_game_data.csv", header = T)
View(DATA.FULL)

### Preliminary predictor variables:
###                                   PitchNo
###                                   PAofInning
###                                   PitchofPA
###                                   PitcherThrows
###                                   BatterSide
###                                   Inning
###                                   Outs
###                                   Balls
###                                   Strikes
###                                   PitchCall

### Response Variable:
###                                   AutoPitchType


### Since we are not using team specific variables, this allows us to have more data
### and keep track of the whole games

### QUESTION: How to keep track of what type of pitches have already been thrown

### Create a new data frame containing only those values

cols <- c("PitchNo","PAofInning","PitchofPA","PitcherThrows","BatterSide","Inning",
          "Outs","Balls","Strikes","PitchCall","AutoPitchType")

Game <- DATA.FULL[,cols]
View(Game)

### Look at structure of data

str(Game)

### Look at summary

summary(Game)

levels(Game$BatterSide)
levels(Game$PitchCall)
levels(Game$PitcherThrows)
levels(Game$AutoPitchType)

### All factor variables have an Undefined Level. Need to remove them
### Convert factors into character classes, remove undefined, then back change to factors

Game$BatterSide <- as.character(Game$BatterSide)
Game <- Game[Game$BatterSide != "Undefined",]
Game <- Game[Game$BatterSide != "Other",]
Game$BatterSide <- as.factor(Game$BatterSide)

Game$PitchCall <- as.character(Game$PitchCall)
Game <- Game[Game$PitchCall != "Undefined",]
Game$PitchCall <- as.factor(Game$PitchCall)

Game$PitcherThrows <- as.character(Game$PitcherThrows)
Game <- Game[Game$PitcherThrows != "Undefined",]
Game$PitcherThrows <- as.factor(Game$PitcherThrows)

Game$AutoPitchType <- as.character(Game$AutoPitchType)
Game <- Game[Game$AutoPitchType != "Undefined",]
Game$AutoPitchType <- as.factor(Game$AutoPitchType)

### Only removed 38

levels(Game$BatterSide)
levels(Game$PitchCall)
levels(Game$PitcherThrows)
levels(Game$AutoPitchType)

### Look at structure and summary again

str(Game)
summary(Game)

### Levels were removed. Data is now complete.

### Multinomial Probit and Logit Models
attach(Game)

library(nnet)

model1 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + PitcherThrows + BatterSide + Inning + 
                     Outs + Balls + Strikes + PitchCall, data = Game) ### Need to have a lag variable in the model
summary(model1)

probs1 <- fitted(model1)*100
head(probs1, n = 20)

### make plots.

plot.probs <- function(probs, pitcherArm, batterArm, size){
  pitches <- colnames(probs)
  
  ### Create a blank plot
  
  ### No player parameters
  if ((is.null(pitcherArm)) && (is.null(batterArm))){
    plot(NULL, xlim = c(1,size) , ylim = c(0,100), xlab = "Pitch #",
         ylab = "Probability (in %)", main = "Probability of Pitch Type")
  }
  
  ### Pitcher Parameter
  
  if ((!(is.null(pitcherArm))) && (is.null(batterArm))){
    plot(NULL, xlim = c(1,size) , ylim = c(0,100), xlab = "Pitch #",
         ylab = "Probability (in %)", main = "Probability of Pitch Type",
         sub = paste(pitcherArm, "Handed Pitcher", sep = " "))
    
  }
  
  ### Batter Parameter
  
  if ((is.null(pitcherArm)) && (!(is.null(batterArm)))){
    plot(NULL, xlim = c(1,size) , ylim = c(0,100), xlab = "Pitch #",
         ylab = "Probability (in %)", main = "Probability of Pitch Type",
         sub = paste(batterArm, "Handed Batter", sep = " "))
  }
  
  ### Pitcher and Batter Parameter
  if ((!(is.null(pitcherArm))) && (!(is.null(batterArm)))){
    plot(NULL, xlim = c(1,size) , ylim = c(0,100), xlab = "Pitch #",
         ylab = "Probability (in %)", main = "Probability of Pitch Type",
         sub = paste(pitcherArm, " Handed Pitcher, ", batterArm, " Handed Batter", sep = ""))
  }
  
  for (i in 1:length(pitches)){
    points(probs[1:size,pitches[i]], type = 'l', col = i)
    
  }
  legend(x = -5, y = 105, legend = pitches[1:4], col = c(1:4), lty = 1,
         bty = 'n', horiz = T, text.width = 15)
  legend(x = -5, y = 100, legend = pitches[5:8], col = c(5:8), lty = 1,
         bty = 'n', horiz = T, text.width = 15)
}

plot.probs(probs1,NULL,NULL, 300)

### make different symbols for what happened. Different colors for what type

### Find which pitch is most likely
compare.pre.plot <- function(real.values, pre.values, pitcherArm, batterArm){
  
  ### Find the max probability
  pre <- apply(pre.values,1,which.is.max)
  
  ### Find the second largest probability
  pre2 <- rep(NA,length = nrow(pre.values))
  for (f in 1:nrow(pre.values)){
    n <- length(pre.values[f,])
    pre2[f] <- match((sort(pre.values[f,],partial=n-1)[n-1]),pre.values[f,])
    
  }
  
  ### Create a blank plot
  
  ### No player parameters
  if ((is.null(pitcherArm)) && (is.null(batterArm))){
    plot(NULL, xlim = c(0,50) , ylim = c(0,10), xlab = "Pitch #",
         ylab = "Pitch Type", main = "Probable Pitch Type vs. Actual Pitch Types")
  }
  
  ### Pitcher Parameter
  
  if ((!(is.null(pitcherArm))) && (is.null(batterArm))){
    plot(NULL, xlim = c(0,50) , ylim = c(0,10), xlab = "Pitch #",
         ylab = "Pitch Type", main = "Probable Pitch Type vs. Actual Pitch Types",
         sub = paste(pitcherArm, "Handed Pitcher", sep = " "))
    
  }
  
  ### Batter Parameter
  
  if ((is.null(pitcherArm)) && (!(is.null(batterArm)))){
    plot(NULL, xlim = c(0,50) , ylim = c(0,10), xlab = "Pitch #",
         ylab = "Pitch Type", main = "Probable Pitch Type vs. Actual Pitch Types",
         sub = paste(batterArm, "Handed Batter", sep = " "))
  }
  
  ### Pitcher and Batter Parameter
  if ((!(is.null(pitcherArm))) && (!(is.null(batterArm)))){
    plot(NULL, xlim = c(0,50) , ylim = c(0,10), xlab = "Pitch #",
         ylab = "Pitch Type", main = "Probable Pitch Type vs. Actual Pitch Types",
         sub = paste(pitcherArm, " Handed Pitcher, ", batterArm, " Handed Batter", sep = ""))
  }
  
  
  ### plot the raw and predicted points
  for (l in 1:50){
    ### real pitches
    points(PitchNo[l], real.values[l], cex = 3, pch = 15, col = "red")
    
    ### most probable pitches
    points(PitchNo[l], pre[l], cex = 2.5, pch = 18, col = "green")
    
    ### second most probable pitches
    points(PitchNo[l], pre2[l], cex = 2, pch = 16, col = "blue")
    
  }
  
  ### Add corresponding percentages
    for (j in 1:50){
      
      ### For 1st probable
      text(PitchNo[j] + 0.07, pre[j] + 0.35, labels = paste(round(pre.values[1,pre[j]]), "%", sep = ""),
         cex = 0.7)
      
      ### For 2nd probable
      text(PitchNo[j] + 0.07, pre2[j] + 0.35, labels = paste(round(pre.values[1,pre2[j]]), "%", sep = ""),
           cex = 0.7)
    
  }

  ### Plot legend
  legend(x = -4.3, y = 10.8, legend = c("1 = ChangeUp","2 = Curveball", "3 = Cutter",
                                   "4 = Fastball"), 
         horiz = T, bty = "n", text.width = 1.5)
  legend(x = -4.3, y = 10.5, legend = c("5 = Other", "6 = Sinker",
                                      "7 = Slider", "8 = Splitter"), 
         horiz = T, bty = "n", text.width = 1.5)
  legend(x = -2.5, y = 10, legend = c("1st Probable","2nd Probable", "True"), horiz = T, bty = "n",
         col = c("green","blue","red"), pch = c(18,16,15), text.width = 3, pt.cex = c(2.5,2,3))
  segments(-5,9,18,9)
  segments(18,9,18,12)

}

compare.pre.plot(AutoPitchType, probs1, NULL, NULL)


### Now go through different combinations of pitcher and batter hands

model2 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + (PitcherThrows == "Left")
                   + BatterSide + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model3 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + (PitcherThrows == "Right")
                   + BatterSide + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model4 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + PitcherThrows 
                   + (BatterSide == "Left") + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model5 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + PitcherThrows 
                   + (BatterSide == "Right") + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model6 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + (PitcherThrows == "Left")
                   + (BatterSide == "Left") + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model7 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + (PitcherThrows == "Left")
                   + (BatterSide == "Right") + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model8 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + (PitcherThrows == "Right")
                   + (BatterSide == "Left") + Inning + 
                     Outs + Balls + Strikes + PitchCall)
model9 <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + (PitcherThrows == "Right")
                   + (BatterSide == "Right") + Inning + 
                     Outs + Balls + Strikes + PitchCall)



probs2 <- fitted(model2)*100
head(probs2, n = 20)

probs3 <- fitted(model3)*100
head(probs3, n = 20)

probs4 <- fitted(model4)*100
head(probs4, n = 20)

probs5 <- fitted(model5)*100
head(probs5, n = 20)

probs6 <- fitted(model6)*100
head(probs6, n = 20)

probs7 <- fitted(model7)*100
head(probs7, n = 20)

probs8 <- fitted(model8)*100
head(probs8, n = 20)

probs9 <- fitted(model9)*100
head(probs9, n = 20)

plot.probs(probs2, "Left", NULL,300)
plot.probs(probs3, "Right", NULL,300)
plot.probs(probs4, NULL, "Left",300)
plot.probs(probs5, NULL, "Right",300)
plot.probs(probs6, "Left", "Left",300)
plot.probs(probs7, "Left", "Right",300)
plot.probs(probs8, "Right", "Left",300)
plot.probs(probs9, "Right", "Right",300)

compare.pre.plot(AutoPitchType, probs2, "Left", NULL)
compare.pre.plot(AutoPitchType, probs3, "Right", NULL)
compare.pre.plot(AutoPitchType, probs4, NULL, "Left")
compare.pre.plot(AutoPitchType, probs5, NULL, "Right")
compare.pre.plot(AutoPitchType, probs6, "Left", "Left")
compare.pre.plot(AutoPitchType, probs7, "Left", "Right")
compare.pre.plot(AutoPitchType, probs8, "Right", "Left")
compare.pre.plot(AutoPitchType, probs9, "Right", "Right")


### Now bring in the Scrimmage data to test model


### Scrimmage on September 26, 2018
Scrim1.full <- read.csv("Scrimmage_9-26-18.csv", header = T)
Scrim2.full <- read.csv("Scrimmage_9-27-18.csv", header = T)
Scrim3.full <- read.csv("Scrimmage_9-28-18.csv", header = T)

### Clean up data

Scrim1 <- Scrim1.full[,cols]
Scrim2 <- Scrim2.full[,cols]
Scrim3 <- Scrim3.full[,cols]

summary(Scrim1)
summary(Scrim2)
summary(Scrim3)

### Remove any undefined variables that could exists in the factor variables

Scrim1$BatterSide <- as.character(Scrim1$BatterSide)
Scrim1 <- Scrim1[Scrim1$BatterSide != "Undefined",]
Scrim1$BatterSide <- as.factor(Scrim1$BatterSide)

Scrim1$PitchCall <- as.character(Scrim1$PitchCall)
Scrim1 <- Scrim1[Scrim1$PitchCall != "Undefined",]
Scrim1$PitchCall <- as.factor(Scrim1$PitchCall)

Scrim1$PitcherThrows <- as.character(Scrim1$PitcherThrows)
Scrim1 <- Scrim1[Scrim1$PitcherThrows != "Undefined",]
Scrim1$PitcherThrows <- as.factor(Scrim1$PitcherThrows)

Scrim1$AutoPitchType <- as.character(Scrim1$AutoPitchType)
Scrim1 <- Scrim1[Scrim1$AutoPitchType != "Undefined",]
Scrim1$AutoPitchType <- as.factor(Scrim1$AutoPitchType)




Scrim2$BatterSide <- as.character(Scrim2$BatterSide)
Scrim2 <- Scrim2[Scrim2$BatterSide != "Undefined",]
Scrim2$BatterSide <- as.factor(Scrim2$BatterSide)

Scrim2$PitchCall <- as.character(Scrim2$PitchCall)
Scrim2 <- Scrim2[Scrim2$PitchCall != "Undefined",]
Scrim2$PitchCall <- as.factor(Scrim2$PitchCall)

Scrim2$PitcherThrows <- as.character(Scrim2$PitcherThrows)
Scrim2 <- Scrim2[Scrim2$PitcherThrows != "Undefined",]
Scrim2$PitcherThrows <- as.factor(Scrim2$PitcherThrows)

Scrim2$AutoPitchType <- as.character(Scrim2$AutoPitchType)
Scrim2 <- Scrim2[Scrim2$AutoPitchType != "Undefined",]
Scrim2$AutoPitchType <- as.factor(Scrim2$AutoPitchType)



Scrim3$BatterSide <- as.character(Scrim3$BatterSide)
Scrim3 <- Scrim3[Scrim3$BatterSide != "Undefined",]
Scrim3$BatterSide <- as.factor(Scrim3$BatterSide)

Scrim3$PitchCall <- as.character(Scrim3$PitchCall)
Scrim3 <- Scrim3[Scrim3$PitchCall != "Undefined",]
Scrim3$PitchCall <- as.factor(Scrim3$PitchCall)

Scrim3$PitcherThrows <- as.character(Scrim3$PitcherThrows)
Scrim3 <- Scrim3[Scrim3$PitcherThrows != "Undefined",]
Scrim3$PitcherThrows <- as.factor(Scrim3$PitcherThrows)

Scrim3$AutoPitchType <- as.character(Scrim3$AutoPitchType)
Scrim3 <- Scrim3[Scrim3$AutoPitchType != "Undefined",]
Scrim3$AutoPitchType <- as.factor(Scrim3$AutoPitchType)



summary(Scrim1)
summary(Scrim2)
summary(Scrim3)

attach(Scrim1)
attach(Scrim2)
attach(Scrim3)

### Make models for each scrimmage game

Scrim1.model <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + PitcherThrows + BatterSide + Inning + 
                     Outs + Balls + Strikes + PitchCall, data = Scrim1) ### Need to have a lag variable in the model

Scrim2.model <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + PitcherThrows + BatterSide + Inning + 
                           Outs + Balls + Strikes + PitchCall, data = Scrim2) ### Need to have a lag variable in the model

Scrim3.model <- multinom(AutoPitchType ~ PitchNo + PAofInning + PitchofPA + PitcherThrows + BatterSide + Inning + 
                           Outs + Balls + Strikes + PitchCall, data = Scrim3) ### Need to have a lag variable in the model

### Look at probabilities

Scrim1.probs <- fitted(Scrim1.model)*100
head(Scrim1.probs, n = 20)

Scrim2.probs <- fitted(Scrim2.model)*100
head(Scrim2.probs, n = 20)

Scrim3.probs <- fitted(Scrim3.model)*100
head(Scrim3.probs, n = 20)

### Make probability plots


### THESE DON'T WORK
plot.probs(Scrim1.probs,NULL,NULL,100)
plot.probs(Scrim2.probs,NULL,NULL,100)
plot.probs(Scrim1.probs,NULL,NULL,100)

compare.pre.plot(Scrim1$AutoPitchType, Scrim1.probs, NULL, NULL)





#### Split data by game.
#### make variables for each pitch type.
#### number of pitch type in the last # (or fewer if necessary) of pitches
### Make counts of the first, then the second, ...



















