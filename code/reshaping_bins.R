library(tidyr)
library(tidyverse)
library(dplyr)
library(data.table)
library(R.utils)
library(ggplot2)
library(ggrepel)
library(ggvoronoi)
library(ggpubr)
library(viridis)
library(pracma)
library(sf)
library(DT)
library(shiny)
library(gridExtra)
library(sfsmisc)
library(useful)
library(MASS)
library(geometry)
library(survival)
library(spdep)
library(randomForest)
library(varhandle)
library(datasets)
library(caret)
library(tidyverse)
library(reshape2)
library(ModelMetrics)
library(stringr)

area_general_info_df <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/game_general_area.csv")

bin_general_info_df <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/game_bin.csv")

pff_data <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/pffScoutingData.csv")

plays_data <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/plays.csv")

#Omitting intentional rollouts and penalties
plays_to_omit_desc_df <- plays_data[plays_data$dropBackType=="DESIGNED_ROLLOUT_RIGHT" | plays_data$dropBackType=="DESIGNED_ROLLOUT_LEFT" | plays_data$dropBackType=="DESIGNED_RUN" | plays_data$foulName1!="<NA>",]
plays_to_omit_df <-na.omit(data.frame(gameId=plays_to_omit_desc_df$gameId, playId=plays_to_omit_desc_df$playId))
row.names(plays_to_omit_df) <- NULL

unique(plays_data$dropBackType)
unique(plays_data$pff_passCoverageType)
unique(plays_data$offenseFormation)
# View(plays_data)
#include from plays in baseline model: offense formation, defendersInBox, pffPlayAction, pff_passCoverageType
#Need to get rid of certain plays, such as intentional roll outs, plays with in-play penalties

area_general_info_df <- (anti_join(area_general_info_df, plays_to_omit_df, by=c("gameId", "playId")))
bin_general_info_df <- (anti_join(bin_general_info_df, plays_to_omit_df, by=c("gameId", "playId")))

#First, generate model based only on observing a hurry on the play (without placing fault)
general_hit_allowed_info <- aggregate(pff_hitAllowed ~ playId+gameId, data = pff_data, max)
sample_hit_allowed_info <- general_hit_allowed_info[general_hit_allowed_info$gameId %in% area_general_info_df$gameId & general_hit_allowed_info$playId %in% area_general_info_df$playId,]
sample_hit_allowed_info <- sample_hit_allowed_info[order(sample_hit_allowed_info$gameId, sample_hit_allowed_info$playId),]

general_hurry_allowed_info <- aggregate(pff_hurryAllowed ~ playId+gameId, data = pff_data, max)
sample_hurry_allowed_info <- general_hurry_allowed_info[general_hurry_allowed_info$gameId %in% area_general_info_df$gameId & general_hurry_allowed_info$playId %in% area_general_info_df$playId,]
sample_hurry_allowed_info <- sample_hurry_allowed_info[order(sample_hurry_allowed_info$gameId, sample_hurry_allowed_info$playId),]

general_sack_allowed_info <- aggregate(pff_sackAllowed ~ playId+gameId, data = pff_data, max)
sample_sack_allowed_info <- general_sack_allowed_info[general_sack_allowed_info$gameId %in% area_general_info_df$gameId & general_sack_allowed_info$playId %in% area_general_info_df$playId,]
sample_sack_allowed_info <- sample_sack_allowed_info[order(sample_sack_allowed_info$gameId, sample_sack_allowed_info$playId),]

#Agnostic approach, treating all forms of pressure as equal
sample_pressure_allowed_info <- data.frame(gameId=sample_hit_allowed_info$gameId, playId=sample_hit_allowed_info$playId, 
                                           pressureAllowed=sample_hit_allowed_info$pff_hitAllowed+sample_hurry_allowed_info$pff_hurryAllowed+sample_sack_allowed_info$pff_sackAllowed)
sample_pressure_allowed_info$pressureAllowed[sample_pressure_allowed_info$pressureAllowed>1] <- 1
sample_pressure_allowed_info <- sample_pressure_allowed_info[order(sample_pressure_allowed_info$gameId, sample_pressure_allowed_info$playId),]
row.names(sample_pressure_allowed_info) <- NULL

#Assuming Pressures Occur on the last frame (causes rightward bias, ie we exclusively detect pressure late (conservative about our definition of pressure))
last_frame_df <- aggregate(frameId ~ playId+gameId,data = area_general_info_df, max)
colnames(last_frame_df)[3] <- "last_frame"
area_general_frame_info_df <- merge(last_frame_df, area_general_info_df, by=c("gameId", "playId"))
area_general_frame_info_df <- area_general_frame_info_df[area_general_frame_info_df$frameId==area_general_frame_info_df$last_frame,]
area_general_frame_info_df <- area_general_frame_info_df[order(area_general_frame_info_df$gameId, area_general_frame_info_df$playId, area_general_frame_info_df$frameId),]
area_general_frame_info_df <- area_general_frame_info_df[c(Inf,diff(area_general_frame_info_df$playId)+diff(area_general_frame_info_df$gameId))!=0,]
row.names(area_general_frame_info_df) <- NULL
pressure_occurrence_df <- data.frame(gameId=area_general_frame_info_df$gameId, playId=area_general_frame_info_df$playId, pressure_allowed_frame=area_general_frame_info_df$frameId,
                                     pressureAllowed=sample_pressure_allowed_info$pressureAllowed)
allowed_pressure_occurence_df <- pressure_occurrence_df[pressure_occurrence_df$pressureAllowed==1,]

## END OF CODE FROM YOUR FILE ##
## ######################### ##

## this is the game csv you sent me
bin_df <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/game_bin.csv")

plays <- sample_pressure_allowed_info
pressureplays <- allowed_pressure_occurence_df$playId
pressure_allowed <- allowed_pressure_occurence_df$pressure_allowed_frame

## this will be the final dataframe
all_bins_df <- data.frame()

for (i in seq_along(plays$playId)) {
  
  play <- plays$playId[i]
  game <- plays$gameId[i]
  if (play %in% pressureplays) {
    print(play)
    j <- which(pressureplays==play)
    print(j)
    pressure_at <- pressure_allowed[j]
    print(pressure_at)
    play_df <- bin_df[bin_df$playId==play,]
    realframe <- play_df$frameId[1] 
    to_reshape <- subset(play_df, select=-c(gameId, playId, frameId))
    
    ## moves rows to columns by label, we can access column / bin number by df$colname.bin_number in final df
    expanded_df <- as.data.frame(reshape(to_reshape,timevar="bin_id",idvar="relevant_frame",direction="wide"))
    expanded_df$gameId <- game
    expanded_df$playId <- play
    expanded_df$frameId <- expanded_df$relevant_frame + realframe - 1
    
    expanded_df$pressure_on_next_frame <- ifelse(expanded_df$frameId == pressure_at - 1, 1, 0)
    ## removing pressure frame -- tbd if this is needed
    expanded_df <- slice(expanded_df, 1:(n() - 1))
    
  } else {
    play_df <- bin_df[bin_df$playId==play,]
    realframe <- play_df$frameId[1] 
    to_reshape <- subset(play_df, select=-c(gameId, playId, frameId))
    
    ## moves rows to columns by label, we can access column / bin number by df$colname.bin_number in final df
    expanded_df <- as.data.frame(reshape(to_reshape,timevar="bin_id",idvar="relevant_frame",direction="wide"))
    expanded_df$gameId <- game
    expanded_df$playId <- play
    expanded_df$frameId <- expanded_df$relevant_frame + realframe - 1
    
    expanded_df$pressure_on_next_frame <- 0
    ## removing pressure frame -- tbd if this is needed
    expanded_df <- slice(expanded_df, 1:(n() - 1))
  }
  
  all_bins_df <- rbind(all_bins_df, expanded_df)
}

## columns in new dataframe called all_bins_df :: relevant_frame, 600 other columns as discussed, playId (at very end, sorry)
## plays stacked on top of one another as in original csv

## input formatting for model
train_control <- trainControl(method = "cv",number = 5, summaryFunction=mnLogLoss, classProbs = TRUE) #specifying conditions for model training, folds for cross-validation

train_df <- subset(all_bins_df, select=-c(relevant_frame, frameId, playId, gameId))
bin_input <- train_df
bin_input$pressure_on_next_frame <- ifelse(bin_input$pressure_on_next_frame == 1, "Pressure", "No.Pressure")

## training on model; logloss = 0.0066
bin_area_rf_model <- train(pressure_on_next_frame ~., data = bin_input, method = "ranger", metric="logLoss", trControl = train_control, importance="permutation")
bin_area_rf_pred_df <- caret::predict.train(bin_area_rf_model, newdata=bin_input,type = "prob") #getting predicted values

View(bin_area_rf_pred_df)

bin_area_rf_pred_df$gameId <- all_bins_df$gameId
bin_area_rf_pred_df$playId <- all_bins_df$playId
bin_area_rf_pred_df$frameId <- all_bins_df$frameId

sample_plot_df <- bin_area_rf_pred_df[bin_area_rf_pred_df$playId==c(unique(bin_area_rf_pred_df$playId))[1],]
sample_plot_df$cumulative_survival_probability <- (cumprod(sample_plot_df$No.Pressure))
ggplot(sample_plot_df, aes(x=frameId, y=cumulative_survival_probability))+geom_point()+geom_smooth(span=0.3)
theme_minimal() + ggtitle("Permutation Feature Importance (Mean Decrease in Gini)")

gini_importance_bin_area_df <- as.data.frame(bin_area_rf_model$finalModel$variable.importance)
gini_importance_bin_area_df$var <- rownames(gini_importance_bin_area_df)
ggplot(gini_importance_bin_area_df)+
  geom_bar(
    stat = "identity",
    mapping = aes(x = var, y=(bin_area_rf_model$finalModel$variable.importance), fill = var), 
    show.legend = FALSE,
    width = 1
  ) + coord_flip() + theme_minimal() + ggtitle("Permutation Feature Importance (Mean Decrease in Gini), Bin Area Model")


ModelMetrics::logLoss(actual = train_df$pressure_on_next_frame , pred = bin_area_rf_pred_df$Pressure, distribution = "binomial") #calculating log-loss
saveRDS(bin_area_rf_model, file = "C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/bin_rf.rds") #saving model file locally
