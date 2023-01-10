# area_general_info_df <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/larger_area_sample.csv")

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

bin_general_info_df <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/larger_bin_sample.csv")
bin_df <- bin_general_info_df

pff_data <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/pffScoutingData.csv")

plays_data <- read.csv("C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/plays.csv")
plays_data$offenseFormation[is.na(plays_data$offenseFormation)] <- "UNKNOWN"

#Omitting intentional rollouts and penalties
plays_to_keep <- plays_data[plays_data$dropBackType=="TRADITIONAL" & is.na(plays_data$foulNFLId1)==TRUE,]
#include from plays in baseline model: offense formation, defendersInBox, pffPlayAction, pff_passCoverageType
#Need to get rid of certain plays, such as intentional roll outs, plays with in-play penalties

area_general_info_df <- (merge(area_general_info_df, plays_to_keep, by=c("gameId", "playId")))
bin_general_info_df <- (merge(bin_general_info_df, plays_to_keep, by=c("gameId", "playId")))

game_play_index <- unique(data.frame(playId=bin_general_info_df$playId, gameId=bin_general_info_df$gameId))
row.names(game_play_index) <- NULL

#First, generate model based only on observing a hurry on the play (without placing fault)
general_hit_allowed_info <- aggregate(pff_hitAllowed ~ playId+gameId, data = pff_data, max)
sample_hit_allowed_info <- merge(general_hit_allowed_info, game_play_index, by=c("gameId", "playId"))
sample_hit_allowed_info <- sample_hit_allowed_info[order(sample_hit_allowed_info$gameId, sample_hit_allowed_info$playId),]
row.names(sample_hit_allowed_info) <- NULL


general_hurry_allowed_info <- aggregate(pff_hurryAllowed ~ playId+gameId, data = pff_data, max)
sample_hurry_allowed_info <- merge(general_hurry_allowed_info, game_play_index, by=c("gameId", "playId"))
sample_hurry_allowed_info <- sample_hurry_allowed_info[order(sample_hurry_allowed_info$gameId, sample_hurry_allowed_info$playId),]

general_sack_allowed_info <- aggregate(pff_sackAllowed ~ playId+gameId, data = pff_data, max)
sample_sack_allowed_info <- merge(general_sack_allowed_info, game_play_index, by=c("gameId", "playId"))
sample_sack_allowed_info <- sample_sack_allowed_info[order(sample_sack_allowed_info$gameId, sample_sack_allowed_info$playId),]

#Agnostic approach, treating all forms of pressure as equal
sample_pressure_allowed_info <- data.frame(gameId=sample_hit_allowed_info$gameId, playId=sample_hit_allowed_info$playId, 
                                           pressureAllowed=sample_hit_allowed_info$pff_hitAllowed+sample_hurry_allowed_info$pff_hurryAllowed+sample_sack_allowed_info$pff_sackAllowed)
sample_pressure_allowed_info$pressureAllowed[sample_pressure_allowed_info$pressureAllowed>1] <- 1
sample_pressure_allowed_info <- sample_pressure_allowed_info[order(sample_pressure_allowed_info$gameId, sample_pressure_allowed_info$playId),]
row.names(sample_pressure_allowed_info) <- NULL

#Assuming Pressures Occur on the last frame (causes rightward bias, ie we exclusively detect pressure late (conservative about our definition of pressure))
last_frame_df <- aggregate(frameId ~ playId+gameId,data = bin_general_info_df, max)
colnames(last_frame_df)[3] <- "last_frame"
bin_general_info_df <- merge(last_frame_df, bin_general_info_df, by=c("gameId", "playId"))
bin_general_info_df <- bin_general_info_df[bin_general_info_df$frameId==bin_general_info_df$last_frame,]
bin_general_info_df <- bin_general_info_df[order(bin_general_info_df$gameId, bin_general_info_df$playId, bin_general_info_df$frameId),]
bin_general_info_df <- bin_general_info_df[c(Inf,diff(bin_general_info_df$playId)+diff(bin_general_info_df$gameId))!=0,]
row.names(bin_general_info_df) <- NULL
pressure_occurrence_df <- data.frame(gameId=bin_general_info_df$gameId, playId=bin_general_info_df$playId, pressure_allowed_frame=bin_general_info_df$frameId,
                                     pressureAllowed=sample_pressure_allowed_info$pressureAllowed)
allowed_pressure_occurence_df <- pressure_occurrence_df[pressure_occurrence_df$pressureAllowed==1,]

area_pressure_info_df <- merge(bin_general_info_df, allowed_pressure_occurence_df, by=c("gameId", "playId"), all=T)
area_pressure_info_df <- area_pressure_info_df[order(area_pressure_info_df$gameId, area_pressure_info_df$playId, area_pressure_info_df$frameId),]
row.names(area_pressure_info_df) <- NULL
area_pressure_info_df$pressure_allowed_frame[is.na(area_pressure_info_df$pressure_allowed_frame)] <- Inf
area_pressure_info_df$pressureAllowed <- 0
area_pressure_info_df$pressureAllowed[area_pressure_info_df$frameId==area_pressure_info_df$pressure_allowed_frame] <- 1
area_info_until_pressure_df <- area_pressure_info_df[area_pressure_info_df$frameId<=area_pressure_info_df$pressure_allowed_frame,]

area_info_until_frame_before_pressure_df <- area_info_until_pressure_df
area_info_until_frame_before_pressure_df$pressureAllowed_nextframe <- c(area_pressure_info_df$pressureAllowed[-c(1)],0)
area_info_until_frame_before_pressure_df <- area_info_until_frame_before_pressure_df[area_info_until_frame_before_pressure_df$frameId<area_info_until_frame_before_pressure_df$pressure_allowed_frame,]

sample_pff_data <- merge(pff_data, game_play_index, by=c("gameId", "playId"))

blockType_format_df <- data.frame("Var1"=c("BH", "CH", "CL", "NB", "PA", "PP", "PR", "PT", "PU", "SR", "SW", "UP"))
block_positionType_format_df <- data.frame("Var1"=c(unique(pff_data[pff_data$pff_role=="Pass Block",]$pff_positionLinedUp)))
rush_positionType_format_df <- data.frame("Var1"=c(unique(pff_data[pff_data$pff_role=="Pass Rush",]$pff_positionLinedUp)))

plays_data$quarter <- as.factor(plays_data$quarter)
plays_data$down <- as.factor(plays_data$down)
plays_data$offenseFormation <- as.factor(plays_data$offenseFormation)
plays_data$pff_playAction <- as.factor(plays_data$pff_playAction)
plays_data$pff_passCoverageType <- as.factor(plays_data$pff_passCoverageType)
game_input_data_list <- list()
j <- 1
for (game_id in unique(sample_pff_data$gameId)) {
  game_pff_data <- sample_pff_data[sample_pff_data$gameId==game_id,]
  
  game_play_data <- plays_data[plays_data$gameId==game_id,]
  game_play_data$seconds_left_in_game <- as.numeric((strptime(15, "%M")-strptime(game_play_data$gameClock, "%M:%S"))+(60*15*(4-as.integer(as.character(game_play_data$quarter)))))
  game_play_data$seconds_left_in_half <- (strptime(15, "%M")-strptime(game_play_data$gameClock, "%M:%S"))+(60*15*(as.integer(as.character(game_play_data$quarter)) %% 2))
  # game_play_data$quarter <- as.factor(game_play_data$quarter)
  # game_play_data$down <- as.factor(game_play_data$down)
  # game_play_data$offenseFormation <- as.factor(game_play_data$offenseFormation)
  # game_play_data$pff_playAction <- as.factor(game_play_data$pff_playAction)
  # game_play_data$pff_passCoverageType <- as.factor(game_play_data$pff_passCoverageType)
  game_area_data <- bin_general_info_df[bin_general_info_df$gameId==game_id,]
  
  #game_play_data$time_left <- 
  #Simple Imputation
  game_play_data$offenseFormation[is.na(game_play_data$offenseFormation)] <- "UNKNOWN"
  game_play_data$defendersInBox[is.na(game_play_data$defendersInBox)] <- mean(game_play_data$defendersInBox)
  play_input_data_list <- list()
  i <- 1
  for (play_id in c(unique(game_pff_data$playId))) {
    print(i)
    play_pff_data <- game_pff_data[game_pff_data$playId==play_id,]
    print(play_id)
    play_pff_pblock_data <- play_pff_data[play_pff_data$pff_role=="Pass Block",]
    play_pff_prush_data <- play_pff_data[play_pff_data$pff_role=="Pass Rush",]
    
    play_area_data <- game_area_data[game_area_data$playId==play_id,]
    
    ind_play_data <- game_play_data[game_play_data$playId==play_id,]
    contextual_input_data <- data.frame(quarter=(ind_play_data$quarter),down=(ind_play_data$down), framesLasted=length(play_area_data$frameId), yardsToGo=ind_play_data$yardsToGo, 
                                        absoluteYardlineNumber=ind_play_data$absoluteYardlineNumber, offenseFormation=(ind_play_data$offenseFormation),
                                        defendersInBox=ind_play_data$defendersInBox, pff_playAction=(ind_play_data$pff_playAction), pff_passCoverageType=(ind_play_data$pff_passCoverageType)                                        )
    down <- ind_play_data$down
    yardsToGo <- ind_play_data$yardsToGo
    
    block_type_table <- as.data.frame(table(play_pff_pblock_data$pff_blockType))
    block_type_complete_df <- merge(blockType_format_df, block_type_table, by="Var1", all=T)
    block_type_complete_df$Freq[is.na(block_type_complete_df$Freq)] <- 0
    block_type_t_df <- as.data.frame(t((block_type_complete_df[,2])))
    colnames(block_type_t_df) <- block_type_complete_df[,1]
    
    block_position_type_table <- as.data.frame(table(play_pff_pblock_data$pff_positionLinedUp))
    block_position_type_complete_df <- merge(block_positionType_format_df, block_position_type_table, by="Var1", all=T)
    block_position_type_complete_df$Freq[is.na(block_position_type_complete_df$Freq)] <- 0
    block_position_type_t_df <- as.data.frame(t((block_position_type_complete_df[,2])))
    colnames(block_position_type_t_df) <- block_position_type_complete_df[,1]
    
    rush_position_type_table <- as.data.frame(table(play_pff_prush_data$pff_positionLinedUp))
    rush_position_type_complete_df <- merge(rush_positionType_format_df, rush_position_type_table, by="Var1", all=T)
    rush_position_type_complete_df$Freq[is.na(rush_position_type_complete_df$Freq)] <- 0
    rush_position_type_t_df <- as.data.frame(t((rush_position_type_complete_df[,2])))
    colnames(rush_position_type_t_df) <- rush_position_type_complete_df[,1]
    
    pass_blockers <- nrow(play_pff_pblock_data)
    pass_rushers <- nrow(play_pff_prush_data)
    net_blockers <- pass_blockers-pass_rushers
    
    pressureAllowed <- sum(play_pff_pblock_data$pff_hitAllowed)+sum(play_pff_pblock_data$pff_hurryAllowed)+sum(play_pff_pblock_data$pff_sackAllowed)
    if (pressureAllowed>1) {
      pressureAllowed <- 1
    }
    
    pff_prepared_count_data <- data.frame(gameId=game_id, playId=play_id, pressureAllowed=pressureAllowed, pass_blocker_count=pass_blockers, pass_rusher_count=pass_rushers, net_blocker_count=net_blockers)
    pff_prepared_input_play_data <- cbind(pff_prepared_count_data, contextual_input_data, block_type_t_df, block_position_type_t_df, rush_position_type_t_df)
    play_input_data_list[[i]] <- pff_prepared_input_play_data
    i <- i+1
  }
  game_input_data_list[[j]] <- rbindlist(play_input_data_list)
  j <- j+1
}

pff_prepared_input_data <- rbindlist(game_input_data_list)

#Preparing for Model Training
input_features_dupl <- subset(pff_prepared_input_data, select=-c(gameId, playId))
input_features_dupl$pressureAllowed <- ifelse(input_features_dupl$pressureAllowed==1, "Pressure", "No.Pressure")
abser_summary <- function(data, lev=NULL, model=NULL) {
  c(AbsEr=(sum(abs(data$pred - data$obs))))
}

#### Training and analyzing PFF model ####

train_control <- trainControl(method = "cv",number = 5, summaryFunction=mnLogLoss, classProbs = TRUE) #specifying conditions for model training, folds for cross-validation

#Training Random Forest Model, logloss: 0.1303644
rf_model <- train(pressureAllowed ~., data = input_features_dupl, method = "ranger", metric="logLoss", trControl = train_control, importance="permutation")
rf_pred_df <- caret::predict.train(rf_model, newdata=input_features_dupl,type = "prob") #getting predicted values
ModelMetrics::logLoss(actual = as.numeric(as.character(pff_prepared_input_data$pressureAllowed)), pred = rf_pred_df$Pressure, distribution = "binomial") #calculating log-loss
saveRDS(rf_model, file = "C:/Users/Jay Sagrolikar/OneDrive - The University of Chicago/Documents/bdb 23/baseline_rf_position.rds") #saving model file locally

#calculating and plotting feature importance
gini_importance_baseline_df <- as.data.frame(rf_model$finalModel$variable.importance)
gini_importance_baseline_df$var <- rownames(gini_importance_baseline_df)
ggplot(gini_importance_baseline_df)+
  geom_bar(
    stat = "identity",
    mapping = aes(x = var, y=rf_model$finalModel$variable.importance, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + coord_flip() + theme_minimal() + ggtitle("Permutation Feature Importance (Mean Decrease in Gini)")

#### Bin model ####
#use bin_df from top

## this will be the final dataframe

bin_info_final <- list()
k <- 1
for (game_id in unique(bin_df$gameId)) {
  print(game_id)
  game_data <- bin_df[bin_df$gameId == game_id,]
  # all_bins_df <- data.frame()
  
  plays <- sample_pressure_allowed_info[sample_pressure_allowed_info$gameId==game_id,]
  pressure_for_game <- allowed_pressure_occurence_df[allowed_pressure_occurence_df$gameId==game_id,]
  pressureplays <- pressure_for_game$playId
  pressure_allowed <- pressure_for_game$pressure_allowed_frame
  
  play_info_final <- list()
  i <- 1
  for (play in unique(game_data$playId)) {
    
    # game <- plays$gameId[i]
    game <- game_id
    if (play %in% pressureplays) {
      #print(play)
      j <- which(pressureplays==play)
      #print(j)
      pressure_at <- pressure_allowed[j]
      #print(pressure_at)
      play_df <- game_data[game_data$playId==play,]
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
      play_df <- game_data[game_data$playId==play,]
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
    play_info_final[[i]] <- expanded_df
    # all_bins_df <- rbind(all_bins_df, expanded_df)
    i <- i + 1
  }
  all_bins_df <- rbindlist(play_info_final)
  bin_info_final[[k]] <- all_bins_df
  k <- k + 1
}

final_bins <- rbindlist(bin_info_final)

all_bins_df <- as.data.frame(final_bins)
## input formatting for model
train_control <- trainControl(method = "cv",number = 5, summaryFunction=mnLogLoss, classProbs = TRUE) #specifying conditions for model training, folds for cross-validation

train_df <- subset(all_bins_df, select=-c(relevant_frame, frameId, playId, gameId))
bin_input <- train_df
bin_input$pressure_on_next_frame <- ifelse(bin_input$pressure_on_next_frame == 1, "Pressure", "No.Pressure")

## training on model; logloss = 0.0066
bin_area_rf_model <- train(pressure_on_next_frame ~., data = bin_input, method = "ranger", metric="logLoss", trControl = train_control, importance="permutation")
bin_area_rf_pred_df <- caret::predict.train(bin_area_rf_model, newdata=bin_input,type = "prob") #getting predicted values

# View(bin_area_rf_pred_df)

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

#### Metrics for individual teams ####

comparison_df <- data.frame(Pressure=rf_pred_df$Pressure, No.Pressure=rf_pred_df$No.Pressure, gameId=pff_prepared_input_data$gameId, playId=pff_prepared_input_data$playId)

comparison_df$survival_probability <- NaN
comparison_df$poss_team <- NaN
for (i in 1:nrow(comparison_df)) {
  game <- comparison_df[i, "gameId"]
  play <- comparison_df[i, "playId"]
  relevant_play_prediction <- bin_area_rf_pred_df[bin_area_rf_pred_df$gameId==game & bin_area_rf_pred_df$playId==play, ]
  survival_probability <- tail(cumprod(relevant_play_prediction$No.Pressure), n=1)
  poss_team <- plays_data[plays_data$gameId==game & plays_data$playId==play, ]$possessionTeam
  comparison_df[i, "survival_probability"] <- survival_probability
  comparison_df[i, "poss_team"] <- poss_team
}

team_aggregate_metrics <- data.frame(poss_team=unique(comparison_df$poss_team))
predicted <- vector()
survival <- vector()
for (pt in unique(comparison_df$poss_team)) {
  poss_sample <- comparison_df[comparison_df$poss_team==pt,]
  survival <- append(survival, mean(poss_sample$survival_probability))
  predicted <- append(predicted, mean(poss_sample$No.Pressure))
}

team_aggregate_metrics$pff_predicted <- predicted
team_aggregate_metrics$survival_probability <- survival
team_aggregate_metrics$wins_over_expected <- survival - predicted