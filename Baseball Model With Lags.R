### STAT 485, Fall 2018

### Ian Scarff

### GOAL: To create a model to predict what an opposing pitcher might throw next.


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
###                                   Lags of pitch type

### Response Variable:
###                                   AutoPitchType


### Since we are not using team specific variables, this allows us to have more data
### and keep track of the whole games

### Set working directory to GAMES folder
setwd("D:/Ian/Desktop/Fall  2018/STAT 485/GAMES")



### This function will load in all the seperate game files in the working directory
import.data <- function(){
  
  ### Get the name of the files in the working directory
  files <- list.files()
  
  ### Load them into the global environment
  for (i in 1:length(files)){
    assign(paste("Game",i, sep = ""), read.csv(files[i], header = T), envir = .GlobalEnv)
  }
}


### This function will build a model based off all the game files that were loaded in,
### given what the current pitch is, and how many pitches the user wants to keep track of.
make.Model <- function(currentPitch,n.lookback){
  
  ### Set the number of games
  Num.games <- length(list.files())
  
  ### Maker a chr vector of the predictors
  cols <- c("PitchNo","PAofInning","PitchofPA","PitcherThrows","BatterSide","Inning",
            "Outs","Balls","Strikes","AutoPitchType")
  
  ### For each game, grab the predictor variables. These will be copies of the games
  for (i in 1:Num.games){
    assign(paste("GAME",i, sep = ""), .GlobalEnv[[paste("Game",i,sep="")]][,cols], envir = .GlobalEnv)
  }
  
  ### Make a chr vector to include the column names of the lag variables
  newcols <- cols <- c("PitchNo","PAofInning","PitchofPA","PitcherThrows","BatterSide","Inning",
                       "Outs","Balls","Strikes","AutoPitchType","FastballLag","CurveballLag",
                       "ChangeupLag","CutterLag","SinkerLag","SliderLag","SplitterLag")
  
  
  ### For each game, add 7 new columns for each lag variable
  for (i in 1:Num.games){
    
    .GlobalEnv[[paste("GAME",i,sep="")]][,c(11:17)] <- NA
    
    ### Set the new column names
    colnames(.GlobalEnv[[paste("GAME",i,sep="")]]) <- newcols
  }
  
  
  ### If the number of pitches to lookback on is > 1
  if (n.lookback > 1){
    
    ### For the number of games
    for (i in 1:Num.games){
      
      ### For the current game, starting from current pitch
      for (j in currentPitch:nrow(.GlobalEnv[[paste("GAME",i,sep="")]])){
        
        ### Create a data frame to hold the last n pitches from the current pitch
        lastN <- as.data.frame(table(.GlobalEnv[[paste("GAME",i,sep="")]][(j - n.lookback)
                                                                          :(j - 1),10]))
        
        ### Set the frequency for each lag variable. Keep track if the pitch level isn't in the game
        ### If it the pitch level is not in the data, set its lag entry = 0
        
        ### Fastball   
        .GlobalEnv[[paste("GAME",i,sep="")]][j,11] <- ifelse(
          length(lastN[lastN$Var1 == "Fastball",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Fastball",]$Freq)
        
        ### Curveball
        .GlobalEnv[[paste("GAME",i,sep="")]][j,12] <- ifelse(
          length(lastN[lastN$Var1 == "Curveball",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Curveball",]$Freq)
        
        ### ChangeUp
        .GlobalEnv[[paste("GAME",i,sep="")]][j,13] <- ifelse(
          length(lastN[lastN$Var1 == "ChangeUp",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "ChangeUp",]$Freq)
        
        ### Cutter
        .GlobalEnv[[paste("GAME",i,sep="")]][j,14] <- ifelse(
          length(lastN[lastN$Var1 == "Cutter",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Cutter",]$Freq)
        
        ### Sinker
        .GlobalEnv[[paste("GAME",i,sep="")]][j,15] <- ifelse(
          length(lastN[lastN$Var1 == "Sinker",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Sinker",]$Freq)
        
        ### Slider
        .GlobalEnv[[paste("GAME",i,sep="")]][j,16] <- ifelse(
          length(lastN[lastN$Var1 == "Slider",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Slider",]$Freq)
        
        ### Splitter
        .GlobalEnv[[paste("GAME",i,sep="")]][j,17] <- ifelse(
          length(lastN[lastN$Var1 == "Splitter",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Splitter",]$Freq)
      }
    }
  }
  
  ### If the number of pitches to lookback on is = 1
  if (n.lookback == 1){
    
    ### For the number of games
    for (i in 1:Num.games){
      
      ### For each game, starting from current pitch
      for (j in currentPitch:nrow(.GlobalEnv[[paste("GAME",i,sep="")]])){
        lastN <- as.data.frame(table(.GlobalEnv[[paste("GAME",i,sep="")]][(j - n.lookback),10]))
        
        ### Set the frequency for each lag variable. Keep track if the pitch level isn't in the game
        ### If it the pitch level is not in the data, set its lag entry = 0
        
        ### Fastball   
        .GlobalEnv[[paste("GAME",i,sep="")]][j,11] <- ifelse(
          length(lastN[lastN$Var1 == "Fastball",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Fastball",]$Freq)
        
        ### Curveball
        .GlobalEnv[[paste("GAME",i,sep="")]][j,12] <- ifelse(
          length(lastN[lastN$Var1 == "Curveball",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Curveball",]$Freq)
        
        ### ChangeUp
        .GlobalEnv[[paste("GAME",i,sep="")]][j,13] <- ifelse(
          length(lastN[lastN$Var1 == "ChangeUp",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "ChangeUp",]$Freq)
        
        ### Cutter
        .GlobalEnv[[paste("GAME",i,sep="")]][j,14] <- ifelse(
          length(lastN[lastN$Var1 == "Cutter",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Cutter",]$Freq)
        
        ### Sinker
        .GlobalEnv[[paste("GAME",i,sep="")]][j,15] <- ifelse(
          length(lastN[lastN$Var1 == "Sinker",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Sinker",]$Freq)
        
        ### Slider
        .GlobalEnv[[paste("GAME",i,sep="")]][j,16] <- ifelse(
          length(lastN[lastN$Var1 == "Slider",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Slider",]$Freq)
        
        ### Splitter
        .GlobalEnv[[paste("GAME",i,sep="")]][j,17] <- ifelse(
          length(lastN[lastN$Var1 == "Splitter",]$Freq) == 0,
          0,
          lastN[lastN$Var1 == "Splitter",]$Freq)
        
      }
    }
  }
  
  ### Now combine all the games into one data frame
  
  ### Set the first game equal to a variable
  Games <- .GlobalEnv[[paste("GAME",1,sep="")]]
  
  ### Now combine the rest of the games
  for (i in 2:Num.games){
    Games <- rbind(Games,.GlobalEnv[[paste("GAME",1,sep="")]])
  }

  ### Now clean up the data to remove undifened or "other" levels
  
  ### For BatterSide
  Games$BatterSide <- as.character(Games$BatterSide)
  Games <- Games[Games$BatterSide != "Undefined",]
  Games <- Games[Games$BatterSide != "Other",]
  Games$BatterSide <- as.factor(Games$BatterSide)
  
  ### For PitcherThrows
  Games$PitcherThrows <- as.character(Games$PitcherThrows)
  Games <- Games[Games$PitcherThrows != "Undefined",]
  Games <- Games[Games$PitcherThrows != "Other",]
  Games$PitcherThrows <- as.factor(Games$PitcherThrows)
  
  ### For Pitch Types
  Games$AutoPitchType <- as.character(Games$AutoPitchType)
  Games <- Games[Games$AutoPitchType != "Undefined",]
  Games <- Games[Games$AutoPitchType != "Other",]
  Games$AutoPitchType <- as.factor(Games$AutoPitchType)

  ### Now build model based on given parameters
  
  attach(Games)
  library(nnet)

  model <- multinom(AutoPitchType ~., data = Games) 
  
  ### Return model
  
  return (model)
}


### Import scrimage games, then make a test data set mathcing the lag number.
### Put that into predict function
load.testData <- function(direct){
  
  
  setwd(direct)
  files <- list.files()
  
  ### Load them into the global environment
  for (i in 1:length(files)){
    assign(paste("Test",i, sep = ""), read.csv(files[i], header = T), envir = .GlobalEnv)
  }
  
  
  
}

### This function will predict pitch type, given new data
predict.Pitch <- function(model, new.data){
  
  ### Separate the test data into the variables needed
  cols <- c("PitchNo","PAofInning","PitchofPA","PitcherThrows","BatterSide","Inning",
            "Outs","Balls","Strikes","AutoPitchType")
  
  new.data <- new.data[,cols]

  ### Add new columns for lag variables
  
  newcols <- cols <- c("PitchNo","PAofInning","PitchofPA","PitcherThrows","BatterSide","Inning",
                       "Outs","Balls","Strikes","AutoPitchType","FastballLag","CurveballLag",
                       "ChangeupLag","CutterLag","SinkerLag","SliderLag","SplitterLag")
  
    
  new.data[,c(11:17)] <- NA
    
  ### Set the new column names
  colnames(new.data) <- newcols
  
  ### Calculate lags. Same lag as the model created
  
  if (n.lookback > 1){
    ### For the current game, starting from current pitch
    for (j in currentPitch:nrow(new.data)){
      
      ### Create a data frame to hold the last n pitches from the current pitch
      lastN <- as.data.frame(table(new.data[(j - n.lookback):(j - 1),10]))
      
      ### Set the frequency for each lag variable. Keep track if the pitch level isn't in the game
      ### If it the pitch level is not in the data, set its lag entry = 0
      
      ### Fastball   
      new.data[j,11] <- ifelse(
        length(lastN[lastN$Var1 == "Fastball",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Fastball",]$Freq)
      
      ### Curveball
      new.data[j,12] <- ifelse(
        length(lastN[lastN$Var1 == "Curveball",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Curveball",]$Freq)
      
      ### ChangeUp
      new.data[j,13] <- ifelse(
        length(lastN[lastN$Var1 == "ChangeUp",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "ChangeUp",]$Freq)
      
      ### Cutter
      new.data[j,14] <- ifelse(
        length(lastN[lastN$Var1 == "Cutter",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Cutter",]$Freq)
      
      ### Sinker
      new.data[j,15] <- ifelse(
        length(lastN[lastN$Var1 == "Sinker",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Sinker",]$Freq)
      
      ### Slider
      new.data[j,16] <- ifelse(
        length(lastN[lastN$Var1 == "Slider",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Slider",]$Freq)
      
      ### Splitter
      new.data[j,17] <- ifelse(
        length(lastN[lastN$Var1 == "Splitter",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Splitter",]$Freq)
    }
  }

  ### If the number of pitches to lookback on is = 1
  if (n.lookback == 1){
    ### For each game, starting from current pitch
    for (j in currentPitch:nrow(new.data)){
      lastN <- as.data.frame(table(new.data[(j - n.lookback),10]))
      
      ### Set the frequency for each lag variable. Keep track if the pitch level isn't in the game
      ### If it the pitch level is not in the data, set its lag entry = 0
      
      ### Fastball   
      new.data[j,11] <- ifelse(
        length(lastN[lastN$Var1 == "Fastball",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Fastball",]$Freq)
      
      ### Curveball
      new.data[j,12] <- ifelse(
        length(lastN[lastN$Var1 == "Curveball",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Curveball",]$Freq)
      
      ### ChangeUp
      new.data[j,13] <- ifelse(
        length(lastN[lastN$Var1 == "ChangeUp",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "ChangeUp",]$Freq)
      
      ### Cutter
      new.data[j,14] <- ifelse(
        length(lastN[lastN$Var1 == "Cutter",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Cutter",]$Freq)
      
      ### Sinker
      new.data[j,15] <- ifelse(
        length(lastN[lastN$Var1 == "Sinker",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Sinker",]$Freq)
      
      ### Slider
      new.data[j,16] <- ifelse(
        length(lastN[lastN$Var1 == "Slider",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Slider",]$Freq)
      
      ### Splitter
      new.data[j,17] <- ifelse(
        length(lastN[lastN$Var1 == "Splitter",]$Freq) == 0,
        0,
        lastN[lastN$Var1 == "Splitter",]$Freq)
      
    }
  }
  
  ### create new data frame to hold input values
  
  pred.input <- new.data[,-10]
  
  #### Make prediction
  return(predict(Model, newdata = pred.input))
  
  
  
}

### This function will calculate the accuracy of a prediction against test data
Accuracy <- function(Pre,Test){
  
  ### Get number of comparisons
  len <- length(Pre[currentPitch:length(Pre)])
  
  ### Create an array to hold 1 and 0 values
  T.F <- rep(NA,len)
  
  ### Set iterating variable for array
  j = 1
  
  ### Compare predicted values and test values for correct matches. True = 1, False = 0
  for (i in currentPitch:length(Pre)){
    
    ### Input value into array
    T.F[j] <- ifelse(Results[i] == Test[i], 1, 0)
    
    ### Iterate assigning variable
    j <- j + 1
  }
  
  ### Reset iterative variable
  j = 1
  
  ### Calculate average
  avg <- round(mean(T.F) * 100, digits = 2)
  
  
  paste("The model has an accuracy of ", avg, "%", " across all predictions when compared to the test data", sep = "")
}


############################################################################################################
############################################################################################################
############################################################################################################


### Import the training data
import.data()

### Set model building paramets
currentPitch <- 10
n.lookback <- 5



### Only build a model if the current pitch is greater than the number of pitches to look back
if (currentPitch > n.lookback){
  Model <- make.Model(currentPitch = currentPitch, n.lookback = n.lookback)

}

### Only make prediction if a model has been built
if (exists("Model")){
  
  ### Load the training  data
  load.testData("D:/Ian/Desktop/Fall  2018/STAT 485/Test Data")
  
  ### Make prediction
  Results <- predict.Pitch(Model,Test1)
  Results
}

### Only calculate accuracy if a prediction exists
if (exists("Results")){
  
  ### Compare prediction to test data
  Accuracy(Results,Test1$AutoPitchType)

}











