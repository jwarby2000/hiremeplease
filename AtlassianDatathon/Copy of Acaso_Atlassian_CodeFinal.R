####################################
###  Acaso Project 1 Code ###
###################################

#####################################
# Data Pre Processing and Cleaning #
####################################

# Utilising the Kaggle dataset as well as the missing games sourced from
# footystats.com

past_games = read.csv('results.csv')
past_games$date <- as.Date(past_games$date)
past_games <- past_games[,c(1:5)]

extra_games = read.csv('extra_results.csv', fileEncoding = 'UTF-8-BOM')
extra_games$date <- as.Date(extra_games$date)

past_games <- rbind(past_games, extra_games)

# Fifa Historical Rankings from Kaggle

fifa_scores <- read.csv('fifa_rankings.csv')
fifa_scores$rank_date <- as.Date(fifa_scores$rank_date)

# Fifa Historical Ultimate Team Scores from Kaggle

FUT_scores_15 <- read.csv('players_15.csv')[, c(9,13)]
FUT_scores_16 <- read.csv('players_16.csv')[, c(9,13)]
FUT_scores_17 <- read.csv('players_17.csv')[, c(9,13)]
FUT_scores_18 <- read.csv('players_18.csv')[, c(9,13)]      
FUT_scores_19 <- read.csv('players_19.csv')[, c(9,13)]
FUT_scores_20 <- read.csv('players_20.csv')[, c(9,13)]
FUT_scores_21 <- read.csv('players_21.csv')[, c(9,13)]


# Federations and Relevant Teams for 2022

AFC_Teams = c('Iran', 'South Korea', 'United Arab Emirates', 'Syria', 'Lebanon',
              'Iraq', 'Australia', 'Saudi Arabia', 'Oman', 'Japan', 'Vietnam', 'China PR')

CAF_Teams = c('Djibouti','Niger','Burkina Faso','Algeria','Tunisia','Zambia','Equatorial Guinea','Mauritania','Nigeria',
              'Liberia','Cape Verde','Central African Republic','Ivory Coast','Cameroon','Malawi','Mozambique',
              'Mali', 'Kenya', 'Uganda', 'Rwanda', 'Libya', 'Egypt', 'Gabon', 'Angola', 'South Africa','Ghana',
              'Ethiopia', 'Zimbabwe', 'Senegal', 'Namibia', 'Congo', 'Togo', 'Guinea-Bissau', 'Morocco',
              'Guinea', 'Sudan', 'Tanzania', 'Benin', 'DR Congo', 'Madagascar')

CONCACAF_Teams = c('Mexico', 'Canada', 'United States', 'Panama', 'Costa Rica', 'Honduras', 'El Salvador', 'Jamaica')

CONMEBOL_Teams = c('Brazil', 'Argentina', 'Uruguay', 'Ecuador', 'Colombia', 'Paraguay', 'Peru', 'Chile', 'Bolivia', 'Venezuela')

OFC_Teams = c('New Zealand', 'Solomon Islands', 'New Caledonia', 'Tahiti', 'Fiji', 'Vanuatu', 'Papua New Guinea', 'American Samoa',
              'Samoa', 'Tonga', 'Cook Islands')

EUFA_Teams = c('Portugal','Serbia','Luxembourg','Republic of Ireland','Azerbaijan'
               ,'Spain','Sweden','Greece','Kosovo','Georgia','Italy','Switzerland'
               ,'Northern Ireland','Bulgaria','Lithuania','France','Ukraine','Finland'
               ,'Bosnia and Herzegovina','Kazakhstan','Belgium','Czech Republic'
               ,'Wales','Belarus','Estonia','Denmark','Scotland','Israel','Austria'
               ,'Faroe Islands','Moldova','Netherlands','Norway','Turkey','Montenegro'
               ,'Latvia','Gibraltar','Croatia','Russia','Slovakia','Slovenia'
               ,'Malta','Cyprus','England','Albania','Poland','Hungary','Andorra'
               ,'San Marino','Germany','Armenia','Romania','North Macedonia','Iceland','Liechtenstein')

All_Feds = c(AFC_Teams,CAF_Teams,CONCACAF_Teams,CONMEBOL_Teams,OFC_Teams,EUFA_Teams)

# We filter the data to only consider games up to 2013, and only with the teams that are relevant to this competition.
# This is an attempt to only get the most relevant data.

past_games = past_games[past_games$date >"2012-12-31",]
past_games$outcome = factor(ifelse(past_games$home_score == past_games$away_score, 2, 
                                   ifelse(past_games$home_score>past_games$away_score, 3,1)), levels = c(1,2,3))
relevant_games = past_games[past_games$home_team %in% All_Feds & past_games$away_team %in% All_Feds,]

# Cleaning up the fifa ultimate team scores into confederation and year

Feds = c(AFC_Teams, CAF_Teams, CONCACAF_Teams, CONMEBOL_Teams, OFC_Teams, EUFA_Teams)


placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_15, nationality == name), 15)$overall)
  z=z+1 
}

players_2015 = data.frame(Feds, placeholder)

placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_16, nationality == name), 15)$overall)
  z=z+1 
}

players_2016 = data.frame(Feds, placeholder)

placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_17, nationality == name), 15)$overall)
  z=z+1 
}

players_2017 = data.frame(Feds, placeholder)

placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_18, nationality == name), 15)$overall)
  z=z+1 
}

players_2018 = data.frame(Feds, placeholder)

placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_19, nationality == name), 15)$overall)
  z=z+1 
}

players_2019 = data.frame(Feds, placeholder)

placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_20, nationality == name), 15)$overall)
  z=z+1 
}

players_2020 = data.frame(Feds, placeholder)

placeholder = rep(NA, length(Feds))
z = 1
for(name in Feds){
  placeholder[z] = mean(head(subset(FUT_scores_21, nationality == name), 15)$overall)
  z=z+1 
}

players_2021 = data.frame(Feds, placeholder)

# As there will be a lot of data processing in the future, we create functions to make this processing
# generalised and easy to do.

# Creates column of fifa scores linked to each home team observation

fifa_score_func_home <- function(data){
  fifa_home = c()
  fifa_away = c()
  for (i in c(1:length(data$date))){
    if(length(tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$home_team[i],],1)$total_points) == 0){
      fifa_home[i] = NA
    }
    else if (length(tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$away_team[i],],1)$total_points) == 0){
      fifa_away[i] = NA
    }
    else{
      fifa_home[i] = tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$home_team[i],],1)$total_points
      fifa_away[i] = tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$away_team[i],],1)$total_points 
    }
  }
  return(fifa_home)
}

# Creates column of fifa scores linked to each away team observation

fifa_score_func_away <- function(data){
  fifa_home = c()
  fifa_away = c()
  for (i in c(1:length(data$date))){
    if(length(tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$home_team[i],],1)$total_points) == 0){
      fifa_home[i] = NA
    }
    else if (length(tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$away_team[i],],1)$total_points) == 0){
      fifa_away[i] = NA
    }
    else{
      fifa_home[i] = tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$home_team[i],],1)$total_points
      fifa_away[i] = tail(fifa_scores[fifa_scores$rank_date<data$date[i]& fifa_scores$country_full == data$away_team[i],],1)$total_points 
    }
  }
  return(fifa_away)
}

# Creates column of fifa ultimate team scores linked to each home team observation

fut_scores_home <- function(data){
  home_fifa = rep(0, length(data$date))
  for (i in c(1:length(data$date))){
    if(data[i,1] < "2015-01-01"){
      home_fifa[i] = NA
    }
    else if(data[i,1] < "2016-01-01"){
      home_fifa[i] = players_2015[players_2015$Feds == data[i,2],2]
    }
    else if(data[i,1] < "2017-01-01"){
      home_fifa[i] = players_2016[players_2016$Feds == data[i,2],2]
    }
    else if(data[i,1] < "2018-01-01"){
      home_fifa[i] = players_2017[players_2017$Feds == data[i,2],2]
    }
    else if(data[i,1] < "2019-01-01"){
      home_fifa[i] = players_2018[players_2018$Feds == data[i,2],2]
    }
    else if(data[i,1] < "2020-01-01"){
      home_fifa[i] = players_2019[players_2019$Feds == data[i,2],2]
    }
    else if(data[i,1] < "2021-01-01"){
      home_fifa[i] = players_2020[players_2020$Feds == data[i,2],2]
    }
    else{
      home_fifa[i] = players_2021[players_2021$Feds == data[i,2],2]
    }
  }
  return(home_fifa)
}

# Creates column of fifa ultimate team scores linked to each away team observation

fut_scores_away <- function(data){
  away_fifa = rep(0, length(data$date))
  for (i in c(1:length(data$date))){
    if(data[i,1] < "2015-01-01"){
      away_fifa[i] = NA
    }
    else if(data[i,1] < "2016-01-01"){
      away_fifa[i] = players_2015[players_2015$Feds == data[i,3],2]
    }
    else if(data[i,1] < "2017-01-01"){
      away_fifa[i] = players_2016[players_2016$Feds == data[i,3],2]
    }
    else if(data[i,1] < "2018-01-01"){
      away_fifa[i] = players_2017[players_2017$Feds == data[i,3],2]
    }
    else if(data[i,1] < "2019-01-01"){
      away_fifa[i] = players_2018[players_2018$Feds == data[i,3],2]
    }
    else if(data[i,1] < "2020-01-01"){
      away_fifa[i] = players_2019[players_2019$Feds == data[i,3],2]
    }
    else if(data[i,1] < "2021-01-01"){
      away_fifa[i] = players_2020[players_2020$Feds == data[i,3],2]
    }
    else{
      away_fifa[i] = players_2021[players_2021$Feds == data[i,3],2]
    }
  }
  return(away_fifa)
}

# Recency Adjusted recent games feature

win_timed_future <- function(t, data, name){
  filtered = data
  filtered = filtered[filtered$home_team == name | filtered$away_team == name,]
  filtered = tail(filtered, n=t)
  if(length(filtered$home_team)<t){
    return(NA)
  }
  else{
    wins_home = filtered[filtered$home_team == name & filtered$outcome == 3,]
    if(length(wins_home$date) == 0){
      win_home_score = 0
    }
    else{
      win_home_score = sum((1-(as.numeric(data$date[i]-wins_home$date)/(365*n))))
    }
    wins_away = filtered[filtered$away_team == name & filtered$outcome == 1,]
    if(length(wins_away$date) == 0){
      win_away_score = 0
    }
    else{
      win_away_score = sum((1-(as.numeric(data$date[i]-wins_away$date)/(365*n))))
    }
    loses_home = filtered[filtered$home_team == name & filtered$outcome == 1,]
    if(length(loses_home$date) == 0){
      loses_home_score = 0
    }
    else{
      loses_home_score = sum((1-(as.numeric(data$date[i]-loses_home$date)/(365*n))))
    }
    loses_away = filtered[filtered$away_team == name & filtered$outcome == 3,]
    if(length(loses_away$date) == 0){
      loses_away_score = 0
    }
    else{
      loses_away_score = sum((1-(as.numeric(data$date[i]-loses_away$date)/(365*n))))
    }
    total_games = t
  }
  return((win_home_score+win_away_score-loses_home_score-loses_away_score))
}

# Recency Adjusted h2h games feature

h2h_timed_future <- function(t, data, name, home_name, away_name){
  filtered = data
  filtered = filtered[(filtered$home_team == home_name | filtered$away_team == home_name) & (filtered$home_team == away_name | filtered$away_team == away_name),]
  filtered = tail(filtered, n=t)
  if(length(filtered$home_team)<t){
    return(NA)
  }
  else{
    wins_home = filtered[filtered$home_team == name & filtered$outcome == 3,]
    if(length(wins_home$date) == 0){
      win_home_score = 0
    }
    else{
      win_home_score = sum((1-(as.numeric(data$date[i]-wins_home$date)/(365*n))))
    }
    wins_away = filtered[filtered$away_team == name & filtered$outcome == 1,]
    if(length(wins_away$date) == 0){
      win_away_score = 0
    }
    else{
      win_away_score = sum((1-(as.numeric(data$date[i]-wins_away$date)/(365*n))))
    }
    loses_home = filtered[filtered$home_team == name & filtered$outcome == 1,]
    if(length(loses_home$date) == 0){
      loses_home_score = 0
    }
    else{
      loses_home_score = sum((1-(as.numeric(data$date[i]-loses_home$date)/(365*n))))
    }
    loses_away = filtered[filtered$away_team == name & filtered$outcome == 3,]
    if(length(loses_away$date) == 0){
      loses_away_score = 0
    }
    else{
      loses_away_score = sum((1-(as.numeric(data$date[i]-loses_away$date)/(365*n))))
    }
    total_games = t
  }
  return((win_home_score+win_away_score-loses_home_score-loses_away_score))
}

# No recency adjustment win feature

win_perc <- function(t, data, name){
  filtered = data[c(1:(i-1)),]
  filtered = filtered[filtered$home_team == name | filtered$away_team == name,]
  filtered = tail(filtered, n=t)
  if(length(filtered$home_team)<t){
    return(NA)
  }
  else{
    wins_home = length(filtered[filtered$home_team == name & filtered$outcome == 3,]$date)
    wins_away = length(filtered[filtered$away_team == name & filtered$outcome == 1,]$date) 
    total_games = t
  }
  return((wins_home+wins_away)/total_games)
}

# No recency adjustment draw feature

draw_perc <- function(t, data, name){
  filtered = data[c(1:(i-1)),]
  filtered = filtered[filtered$home_team == name | filtered$away_team == name,]
  filtered = tail(filtered, n=t)
  if(length(filtered$home_team)<t){
    return(NA)
  }
  else{
    wins_home = length(filtered[filtered$home_team == name & filtered$outcome == 2,]$date)
    wins_away = length(filtered[filtered$away_team == name & filtered$outcome == 2,]$date) 
    total_games = t
  }
  return((wins_home+wins_away)/total_games)
}

# No recency adjustment h2h win feature

h2h_win <- function(t, data, home_name, away_name){
  filtered = data[c(1:(i-1)),]
  filtered = filtered[(filtered$home_team == home_name | filtered$away_team == home_name) & (filtered$home_team == away_name | filtered$away_team == away_name),]
  filtered = tail(filtered, n=t)
  if(length(filtered$home_team)<t){
    return(NA)
  }
  else{
    wins_home = length(filtered[filtered$home_team == home_name & filtered$outcome == 3,]$date)
    wins_away = length(filtered[filtered$away_team == home_name & filtered$outcome == 1,]$date) 
    total_games = t
  }
  return((wins_home+wins_away)/total_games)
}

# No recency adjustment h2h draw feature

h2h_tie <- function(t, data, home_name, away_name){
  filtered = data[c(1:(i-1)),]
  filtered = filtered[(filtered$home_team == home_name | filtered$away_team == home_name) & (filtered$home_team == away_name | filtered$away_team == away_name),]
  filtered = tail(filtered, n=t)
  if(length(filtered$home_team)<t){
    return(NA)
  }
  else{
    wins_home = length(filtered[filtered$home_team == home_name & filtered$outcome == 2,]$date)
    wins_away = length(filtered[filtered$away_team == home_name & filtered$outcome == 2,]$date) 
    total_games = t
  }
  return((wins_home+wins_away)/total_games)
}

# Collate and create all relevant features for a point in time.

update_frame_func <- function(olddata, data, t1, t2){
  update_frame2 = data.frame(home = rep(NA, (length(data$date))),
                             away = rep(NA, (length(data$date))),
                             win_timed_home = rep(NA, (length(data$date))),
                             win_timed_away = rep(NA, (length(data$date))),
                             h2h_timed = rep(NA, (length(data$date)))
  )
  
  for(i in c(1:length(data$date))){
    home_name = data[i,]$home_team
    away_name = data[i,]$away_team
    update_frame2[i,1] = home_name
    update_frame2[i,2] = away_name
    update_frame2[i,3] = win_timed_future(t = t1, data = olddata, name = home_name)
    update_frame2[i,4] = win_timed_future(t = t1, data = olddata, name = away_name)
    update_frame2[i,5] = h2h_timed_future(t = t2, data = olddata, name = home_name, home_name, away_name)
    
  }
  update_frame2$home_fifa <- fifa_home
  update_frame2$away_fifa <- fifa_away
  update_frame2$fut_home <- fut_home
  update_frame2$fut_away <- fut_away
  update_frame2$win_timed_diff <- update_frame2$win_timed_home -update_frame2$win_timed_away
  update_frame2$fifa_diff <- update_frame2$home_fifa - update_frame2$away_fifa
  update_frame2$fut_diff <-  update_frame2$fut_home - update_frame2$fut_away
  return(update_frame2)
}

# Manual Updated Frame to include all features even the irrelevant ones for testing. The function
# only uses the relevant features

t1 = 5
t2 = 3
n = 10


update_frame2 = data.frame(home = rep(NA, (length(relevant_games$date))),
                           away = rep(NA, (length(relevant_games$date))),
                           home_win = rep(NA, (length(relevant_games$date))),
                           home_draw = rep(NA, (length(relevant_games$date))),
                           away_win = rep(NA, (length(relevant_games$date))),
                           away_draw = rep(NA, (length(relevant_games$date))),
                           home_h2h_win = rep(NA, (length(relevant_games$date))),
                           home_h2h_draw = rep(NA, (length(relevant_games$date))),
                           h2h_games = rep(NA, (length(relevant_games$date))),
                           win_timed_home = rep(NA, (length(relevant_games$date))),
                           win_timed_away = rep(NA, (length(relevant_games$date))),
                           h2h_timed = rep(NA, (length(relevant_games$date)))
)

for(i in c(1:length(relevant_games$date))){
  home_name = relevant_games[i,]$home_team
  away_name = relevant_games[i,]$away_team
  update_frame2[i,1] = home_name
  update_frame2[i,2] = away_name
  update_frame2[i,3] = win_perc(t = t1, relevant_games, name = home_name)
  update_frame2[i,4] = draw_perc(t = t1, relevant_games, name = home_name)
  update_frame2[i,5] = win_perc(t = t1, relevant_games, name = away_name)
  update_frame2[i,6] = draw_perc(t = t1, relevant_games, name = away_name)
  update_frame2[i,7] = h2h_win(t=t2, relevant_games, home_name, away_name)
  update_frame2[i,8] = h2h_tie(t=t2, relevant_games, home_name, away_name)
  update_frame2[i,9] = t2
  update_frame2[i,10] = win_timed_future(t = t1, relevant_games, name = home_name)
  update_frame2[i,11] = win_timed_future(t = t1, relevant_games, name = away_name)
  update_frame2[i,12] = h2h_timed_future(t = t2, relevant_games, name = home_name, home_name, away_name)
  
}


update_frame2$home_fifa <- fifa_score_func_home(relevant_games)
update_frame2$away_fifa <- fifa_score_func_away(relevant_games)
update_frame2$fut_home <- fut_scores_home(relevant_games)
update_frame2$fut_away <- fut_scores_away(relevant_games)
update_frame2$outcome <- relevant_games$outcome

update_frame2$win_diff <- update_frame2$home_win -update_frame2$away_win
update_frame2$win_timed_diff <- update_frame2$win_timed_home -update_frame2$win_timed_away
update_frame2$draw_diff <- update_frame2$home_draw - update_frame2$away_draw 
update_frame2$fifa_diff <- update_frame2$home_fifa - update_frame2$away_fifa
update_frame2$fut_diff <-  update_frame2$fut_home - update_frame2$fut_away 

############################
#### Data Visualisation ####
############################

library(ggplot2)

visual <- data.frame(update_frame2)
visual <- na.omit(visual)
outcome_vec <- visual$outcome
visual <- visual[,c(7,8,12,18,19,20,21,22)]
visual$outcome <- outcome_vec

plot(visual$outcome, col = c("#fc5203", "#fcdf03", "#03fc6b"))

# Predictor v Outcome Plots

for(i in c(1:(length(visual)-1))){
  plot(visual$outcome, visual[,i])
}



for(i in c(1:(length(visual)-1))){
  print(ggplot(visual, aes(x=outcome, y=as.matrix(visual[,i]), fill = outcome)) + 
          geom_violin(trim=FALSE)+
          geom_boxplot(width=0.1)+
          labs(x = 'Outcome', y = paste(names(visual)[i]))+
          scale_fill_manual(values=c("#fc5203", "#fcdf03", "#03fc6b"))+
          theme_classic())
}

# Predictor v Predictor Plots

pairs(~h2h_timed+win_timed_diff+fifa_diff+fut_diff, visual)

######################
# Modelling/CV Setup #
######################

library(caret)
library(dplyr)
library(e1071)
library(MLmetrics)

fitControl <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 1000,
                           classProbs = TRUE,
                           summaryFunction = multiClassSummary,
                           allowParallel = TRUE)

levels(visual$outcome) <- c('Away', 'Draw', 'Home')
levels(update_frame2$outcome) <- c('Away', 'Draw', 'Home')

library(nnet)

# Multinomial

log.fit <- train(outcome~h2h_timed+win_diff+ fut_diff+fifa_diff, data = visual,
                  method ='multinom',
                  trControl = fitControl,
                  tuneGrid = data.frame(decay = 0),
                  metric = 'AUC')

# Random Forest

library(randomForest)
library(ranger)

rf_grid <- expand.grid(mtry = c(1,2,3),
                       splitrule = c('gini'),
                       min.node.size = c(2,5,10))

rf.fit <- train(outcome~h2h_timed+win_diff+ fut_diff+fifa_diff, data = visual,
                method = 'ranger',
                trControl = fitControl,
                tuneGrid = rf_grid,
                verbose = TRUE,
                metric = 'AUC')

# GBM

library(gbm)
gbmGrid <- expand.grid(interaction.depth =c(1,2),
                       n.trees = c(100,200),
                       shrinkage = c(0.01),
                       n.minobsinnode =c(5,10,20))
gbm.fit <- train(outcome~h2h_timed+win_diff+ fut_diff+fifa_diff, data = visual,
                  method = 'gbm',
                  trControl = fitControl,
                  verbose = TRUE,
                  tuneGrid = gbmGrid,
                  metric = 'logLoss')

# LDA and QDA

library(MASS)
lda.fit <- train(outcome~h2h_timed+win_diff+ fut_diff+fifa_diff, data = visual,
                 method = 'lda',
                 trControl = fitControl,
                 metric = 'logLoss')

qda.fit <- train(outcome~h2h_timed+win_diff+ fut_diff+fifa_diff, data = visual,
                 method = 'qda',
                 trControl = fitControl,
                 metric = 'logLoss')

#######################
##### Simulations ####
######################

# Simulations that lead to inter-confederation knockout. This is all done in the same loop
# So that the whole process can be played out
# Note that this includes CONCACAF, CONEMBOL, ACF and OFC

# Concacaf setup

Team_Scores_Base_Concaf = data.frame(Team = c('Mexico', 'Canada', 'United States', 'Panama', 'Costa Rica', 'Honduras', 'El Salvador', 'Jamaica'), Score = c(7,5,5,5,2,2,2,1))
Qual_Frame_Concaf = data.frame(Team = c('Mexico', 'Canada', 'United States', 'Panama', 'Costa Rica', 'Honduras', 'El Salvador', 'Jamaica'), Quals = rep(0,8))

relevant_fifa = fifa_scores[fifa_scores$rank_date == '2021-05-27',]
fifas = c()
for(i in c(1:length(Team_Scores_Base_Concaf$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Team_Scores_Base_Concaf$Team[i],5]
}

Team_Scores_Base_Concaf = cbind(Team_Scores_Base_Concaf,fifas)

WC_Matches = read.csv('international-wc-qualification-concacaf-matches-2022-to-2022-stats.csv')
WC_Matches = WC_Matches[c(79:length(WC_Matches$date)),c(2,5,6)] # Future Matches to Play

fifa_home = fifa_score_func_home(WC_Matches)
fifa_away = fifa_score_func_away(WC_Matches)

fut_home = fut_scores_home(WC_Matches)
fut_away = fut_scores_away(WC_Matches)

new_data_Concaf = update_frame_func(olddata = relevant_games, data = WC_Matches, 5,3)

# CONEMBOL Setup

Team_Scores_Base_SA = data.frame(Team = c('Brazil', 'Argentina', 'Uruguay', 'Ecuador', 'Colombia', 'Paraguay', 'Peru', 'Chile', 'Bolivia', 'Venezuela'), Score = c(24,18,15,13,13,11,8,7,6,4))
Qual_Frame_SA = data.frame(Team = c('Brazil', 'Argentina', 'Uruguay', 'Ecuador', 'Colombia', 'Paraguay', 'Peru', 'Chile', 'Bolivia', 'Venezuela'), Quals = rep(0,10))


relevant_fifa = fifa_scores[fifa_scores$rank_date == '2021-05-27',]
fifas = c()
for(i in c(1:length(Team_Scores_Base_SA$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Team_Scores_Base_SA$Team[i],5]
}

Team_Scores_Base_SA = cbind(Team_Scores_Base_SA,fifas)

WC_Matches = read.csv('international-wc-qualification-south-america-matches-2020-to-2022-stats.csv')
WC_Matches = WC_Matches[c(46:length(WC_Matches$date)),c(2,5,6)] # Future Matches to Play

fifa_home = fifa_score_func_home(WC_Matches)
fifa_away = fifa_score_func_away(WC_Matches)

fut_home = fut_scores_home(WC_Matches)
fut_away = fut_scores_away(WC_Matches)

new_data_SA = update_frame_func(olddata = relevant_games, data = WC_Matches, 5,3)

# ACF

Team_Scores_G1_Base = data.frame(Team = c('Iran', 'South Korea', 'United Arab Emirates', 'Syria', 'Lebanon', 'Iraq'), Score = c(6,4,2,1,1,1))
Team_Scores_G2_Base = data.frame(Team = c('Australia', 'Saudi Arabia', 'Oman', 'Japan', 'Vietnam', 'China PR'), Score = c(6,6,3,3,0,0))
Qual_Frame_G1 = data.frame(Team = c('Iran', 'South Korea', 'United Arab Emirates','Syria', 'Lebanon', 'Iraq'), Quals = rep(0, 6))
Qual_Frame_G2 = data.frame(Team = c('Australia', 'Saudi Arabia', 'Oman', 'Japan', 'Vietnam', 'China PR'), Quals = rep(0, 6))

relevant_fifa = fifa_scores[fifa_scores$rank_date == '2021-05-27',]

fifas = c()

for(i in c(1:length(Team_Scores_G1_Base$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Team_Scores_G1_Base$Team[i],5]
}
Team_Scores_G1_Base = cbind(Team_Scores_G1_Base,fifas)
fifas = c()
for(i in c(1:length(Team_Scores_G2_Base$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Team_Scores_G2_Base$Team[i],5]
}

Team_Scores_G2_Base = cbind(Team_Scores_G2_Base,fifas)

WC_Matches = read.csv('international-wc-qualification-asia-matches-2019-to-2022-stats.csv')
WC_Matches = WC_Matches[c(185:length(WC_Matches$date)),c(2,5,6)] # Future Matches to Play

fifa_home = fifa_score_func_home(WC_Matches)
fifa_away = fifa_score_func_away(WC_Matches)

fut_home = fut_scores_home(WC_Matches)
fut_away = fut_scores_away(WC_Matches)

new_data_Asia = update_frame_func(olddata = relevant_games, data = WC_Matches, 5,3)

# Inter-Confederation Sims

sims = 1000 # number of sims

model_1 = gbm(outcome~h2h_timed + win_timed_diff + fifa_diff+fut_diff, data = na.omit(update_frame2[,c(12,17,19,21,22)]), n.trees = 200)
model_2 = gbm(outcome~win_timed_diff + fifa_diff + fut_diff, data = na.omit(update_frame2[,c(17,19,21,22)]), n.trees = 200)
model_3 = gbm(outcome~h2h_timed+win_timed_diff+fifa_diff,data=na.omit(update_frame2[,c(12,17,19,21)]), n.trees = 200)
model_4 = gbm(outcome~win_timed_diff+fifa_diff,data=na.omit(update_frame2[,c(17,19,21)]), n.trees = 200)

IFed_Matches = c('New Zealand', NA, NA, NA)

OFC_Qual = data.frame(Team = 'New Zealand', Quals = 0)

for(i in c(1:sims)){
  ### CONCAF
  Team_Scores_Concaf = data.frame(Team_Scores_Base_Concaf)
  for(i in c(1:length(new_data_Concaf$home))){
    if(new_data_Concaf$h2h_timed[i] %in% c(NA,NaN)|new_data_Concaf$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_Concaf$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_Concaf[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_Concaf$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_Concaf[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_Concaf[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_Concaf[i,c(5,10,11,12)], type = 'response'))
    }
    
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$home[i],2] = Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$home[i],2] + 3
    }
    else if(rand > away_range){
      Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$home[i],2] = Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$home[i],2] + 1
      Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$away[i],2] = Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$away[i],2] + 1
    }
    else{
      Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$away[i],2] = Team_Scores_Concaf[Team_Scores_Concaf$Team == new_data_Concaf$away[i],2] + 3
    }
  }
  Scores_Sorted_Concaf <- Team_Scores_Concaf[order(-Team_Scores_Concaf$Score, -Team_Scores_Concaf$fifas),]
  # Top 3 qualify:
  for(i in c(1:3)){
    Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Scores_Sorted_Concaf$Team[i], 2] = Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Scores_Sorted_Concaf$Team[i], 2] + 1
  }
  
  IFed_Matches[2] = Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Scores_Sorted_Concaf$Team[4], 1]
  
  #####
  
  ### SA
  
  Team_Scores_SA = data.frame(Team_Scores_Base_SA)
  for(i in c(1:length(new_data_SA$home))){
    if(new_data_SA$h2h_timed[i] %in% c(NA,NaN)|new_data_SA$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_SA$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_SA[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_SA$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_SA[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_SA[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_SA[i,c(5,10,11,12)], type = 'response'))
    }
    
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$home[i],2] = Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$home[i],2] + 3
    }
    else if(rand > away_range){
      Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$home[i],2] = Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$home[i],2] + 1
      Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$away[i],2] = Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$away[i],2] + 1
    }
    else{
      Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$away[i],2] = Team_Scores_SA[Team_Scores_SA$Team == new_data_SA$away[i],2] + 3
    }
  }
  Scores_Sorted_SA <- Team_Scores_SA[order(-Team_Scores_SA$Score, -Team_Scores_SA$fifas),]
  # Top 3 qualify:
  for(i in c(1:4)){
    Qual_Frame_SA[Qual_Frame_SA$Team == Scores_Sorted_SA$Team[i], 2] = Qual_Frame_SA[Qual_Frame_SA$Team == Scores_Sorted_SA$Team[i], 2] + 1
  }
  
  IFed_Matches[3] = Qual_Frame_SA[Qual_Frame_SA$Team == Scores_Sorted_SA$Team[5], 1] 
  
  #####
  
  ### Asia
  
  KnockOut = c()
  Team_Scores_G1 = data.frame(Team_Scores_G1_Base)
  new_data_G1 = new_data_Asia[new_data_Asia$home %in% Team_Scores_G1_Base$Team, ]
  for(i in c(1:length(new_data_G1$home))){
    if(new_data_G1$h2h_timed[i] %in% c(NA,NaN)|new_data_G1$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G1$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G1[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G1$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G1[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G1[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G1[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$home[i],2] = Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$home[i],2] + 3
    }
    else if(rand > away_range){
      Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$home[i],2] = Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$home[i],2] + 1
      Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$away[i],2] = Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$away[i],2] + 1
    }
    else{
      Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$away[i],2] = Team_Scores_G1[Team_Scores_G1$Team == new_data_G1$away[i],2] + 3
    }
  }
  Scores_Sorted <- Team_Scores_G1[order(-Team_Scores_G1$Score, -Team_Scores_G1$fifas),]
  # Top 2 qualify:
  for(i in c(1:2)){
    Qual_Frame_G1[Qual_Frame_G1$Team == Scores_Sorted$Team[i], 2] = Qual_Frame_G1[Qual_Frame_G1$Team == Scores_Sorted$Team[i], 2] + 1
  }
  KnockOut[1] = Qual_Frame_G1[Qual_Frame_G1$Team == Scores_Sorted$Team[3], 1]
  Team_Scores_G2 = data.frame(Team_Scores_G2_Base)
  new_data_G2 = new_data_Asia[new_data_Asia$home %in% Team_Scores_G2_Base$Team, ]
  for(i in c(1:length(new_data_G2$home))){
    if(new_data_G2$h2h_timed[i] %in% c(NA,NaN)|new_data_G2$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G2$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G2[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G2$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G2[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G2[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G2[i,c(5,10,11,12)], type = 'response'))
    }
    
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$home[i],2] = Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$home[i],2] + 3
    }
    else if(rand > away_range){
      Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$home[i],2] = Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$home[i],2] + 1
      Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$away[i],2] = Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$away[i],2] + 1
    }
    else{
      Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$away[i],2] = Team_Scores_G2[Team_Scores_G2$Team == new_data_G2$away[i],2] + 3
    }
  }
  Scores_Sorted <- Team_Scores_G2[order(-Team_Scores_G2$Score, -Team_Scores_G2$fifas),]
  # Top 2 qualify:
  for(i in c(1:2)){
    Qual_Frame_G2[Qual_Frame_G2$Team == Scores_Sorted$Team[i], 2] = Qual_Frame_G2[Qual_Frame_G2$Team == Scores_Sorted$Team[i], 2] + 1
  }
  KnockOut[2] = Qual_Frame_G2[Qual_Frame_G2$Team == Scores_Sorted$Team[3], 1]
  
  KnockOut = KnockOut[sample(1:length(KnockOut))] # Shuffling who is at home
  
  KnockOut_Games = data.frame(date = tail(WC_Matches, 1)$date, home_team = KnockOut[1], away_team = KnockOut[2])
  fifa_home = fifa_score_func_home(KnockOut_Games)
  fifa_away = fifa_score_func_away(KnockOut_Games)
  fut_home = fut_scores_home(KnockOut_Games)
  fut_away = fut_scores_away(KnockOut_Games)
  KnockOut_Games = update_frame_func(olddata = relevant_games, data = KnockOut_Games, 5,3)
  if(KnockOut_Games$h2h_timed[1] %in% c(NA,NaN)|KnockOut_Games$fut_diff[1] %in% c(NA,NaN)){
    if(!(KnockOut_Games$fut_diff[1] %in% c(NA,NaN))){
      predicted = as.numeric(predict(model_2, newdata = KnockOut_Games[1,c(10,11,12)], type = 'response'))
    }
    else if(!(KnockOut_Games$h2h_timed[1] %in% c(NA,NaN))){
      predicted= as.numeric(predict(model_3, newdata = KnockOut_Games[1,c(5,10,11)], type = 'response'))
    }
    else{
      predicted = as.numeric(predict(model_4, newdata = KnockOut_Games[1,c(10,11)], type = 'response'))
    }
  }
  else{
    predicted = as.numeric(predict(model_1, newdata = KnockOut_Games[1,c(5,10,11,12)], type = 'response'))
  }
  away_range = predicted[1]+ predicted[2]*0.5 ############
  rand = runif(1)
  
  if(rand > away_range){
    IFed_Matches[4] = KnockOut_Games$home
  }
  else{
    IFed_Matches[4] = KnockOut_Games$away
  }
  
  #####
  
  # Interconfederate Matches
  Winners = c()
  IFed_Matches = IFed_Matches[sample(1:length(IFed_Matches))]
  Games = data.frame(date = rep('2022-06-01',2), home_team = c(IFed_Matches[c(1,2)]) , away_team = c(IFed_Matches[c(3,4)]))
  fifa_home = fifa_score_func_home(Games)
  fifa_away = fifa_score_func_away(Games)
  fut_home = fut_scores_home(Games)
  fut_away = fut_scores_away(Games)
  Games = update_frame_func(olddata = relevant_games, data = Games, 5,3)
  for(i in c(1:length(Games$home))){
    if(Games$h2h_timed[1] %in% c(NA,NaN)|Games$fut_diff[1] %in% c(NA,NaN)){
      if(!(Games$fut_diff[1] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = Games[1,c(10,11,12)], type = 'response'))
      }
      else if(!(Games$h2h_timed[1] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = Games[1,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = Games[1,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = Games[1,c(5,10,11,12)], type = 'response'))
    }
    
    away_range = predicted[1]+ predicted[2]*0.5 ############
    rand = runif(1)
    if(rand > away_range){
      Winners[i] = Games$home[i]
    }
    else{
      Winners[i] = Games$away[i]
    }
  }
  if(Winners[1] %in% Team_Scores_G1_Base$Team){
    Qual_Frame_G1[Qual_Frame_G1$Team == Winners[1], 2] = Qual_Frame_G1[Qual_Frame_G1$Team == Winners[1], 2] + 1
  }
  else if(Winners[1] %in% Team_Scores_G2_Base$Team){
    Qual_Frame_G2[Qual_Frame_G2$Team == Winners[1], 2] = Qual_Frame_G2[Qual_Frame_G2$Team == Winners[1], 2] + 1
  }
  else if(Winners[1] %in% Team_Scores_Concaf$Team){
    Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Winners[1], 2] = Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Winners[1], 2] + 1
  }
  else if(Winners[1] %in% Team_Scores_SA$Team){
    Qual_Frame_SA[Qual_Frame_SA$Team == Winners[1], 2] = Qual_Frame_SA[Qual_Frame_SA$Team == Winners[1], 2] + 1
  }
  else if(Winners[1] %in% OFC_Qual$Team){
    OFC_Qual[OFC_Qual == Winners[1], 2] = OFC_Qual[OFC_Qual == Winners[1], 2] + 1
  }
  if(Winners[2] %in% Team_Scores_G1_Base$Team){
    Qual_Frame_G1[Qual_Frame_G1$Team == Winners[2], 2] = Qual_Frame_G1[Qual_Frame_G1$Team == Winners[2], 2] + 1
  }
  else if(Winners[2] %in% Team_Scores_G2_Base$Team){
    Qual_Frame_G2[Qual_Frame_G2$Team == Winners[2], 2] = Qual_Frame_G2[Qual_Frame_G2$Team == Winners[2], 2] + 1
  }
  else if(Winners[2] %in% Team_Scores_Concaf$Team){
    Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Winners[2], 2] = Qual_Frame_Concaf[Qual_Frame_Concaf$Team == Winners[2], 2] + 1
  }
  else if(Winners[2] %in% Team_Scores_SA$Team){
    Qual_Frame_SA[Qual_Frame_SA$Team == Winners[2], 2] = Qual_Frame_SA[Qual_Frame_SA$Team == Winners[2], 2] + 1
  }
  else if(Winners[2] %in% OFC_Qual$Team){
    OFC_Qual[OFC_Qual$Team == Winners[2], 2] = OFC_Qual[OFC_Qual$Team == Winners[2], 2] + 1
    
  }
}

# CAF Simulations. There is 2 stages to this elimination.

# Setup

Scores_Group_A = data.frame(Team  = c('Algeria', 'Burkina Faso', 'Niger', 'Djibouti'), Score = c(4,4,3,0))
Scores_Group_B = data.frame(Team  = c('Tunisia', 'Zambia', 'Equatorial Guinea', 'Mauritania'), Score = c(6,3,3,0))
Scores_Group_C = data.frame(Team  = c('Nigeria', 'Liberia', 'Cape Verde', 'Central African Republic'), Score = c(6,3,1,1))
Scores_Group_D = data.frame(Team  = c('Ivory Coast', 'Cameroon', 'Malawi', 'Mozambique'), Score = c(4,3,3,1))
Scores_Group_E = data.frame(Team  = c('Mali', 'Kenya', 'Uganda', 'Rwanda'), Score = c(4,2,2,1))
Scores_Group_F = data.frame(Team  = c('Libya', 'Egypt', 'Gabon', 'Angola'), Score = c(6,4,1,0))
Scores_Group_G = data.frame(Team  = c('South Africa', 'Ghana', 'Ethiopia', 'Zimbabwe'), Score = c(4,3,3,1))
Scores_Group_H = data.frame(Team  = c('Senegal', 'Namibia', 'Congo', 'Togo'), Score = c(6,4,1,0))
Scores_Group_I = data.frame(Team  = c('Guinea-Bissau', 'Morocco', 'Guinea', 'Sudan'), Score = c(4,3,1,0))
Scores_Group_J = data.frame(Team  = c('Tanzania', 'Benin', 'DR Congo', 'Madagascar'), Score = c(4,4,2,0))

Qual_Frame_Africa = data.frame(Team =c('Algeria', 'Burkina Faso', 'Niger', 'Djibouti',
                                       'Tunisia', 'Zambia', 'Equatorial Guinea', 'Mauritania',
                                       'Nigeria', 'Liberia', 'Cape Verde', 'Central African Republic',
                                       'Ivory Coast', 'Cameroon', 'Malawi', 'Mozambique',
                                       'Mali', 'Kenya', 'Uganda', 'Rwanda',
                                       'Libya', 'Egypt', 'Gabon', 'Angola',
                                       'South Africa', 'Ghana', 'Ethiopia', 'Zimbabwe',
                                       'Senegal', 'Namibia', 'Congo', 'Togo',
                                       'Guinea-Bissau', 'Morocco', 'Guinea', 'Sudan',
                                       'Tanzania', 'Benin', 'DR Congo', 'Madagascar'), Score = rep(0,40))

relevant_fifa = fifa_scores[fifa_scores$rank_date == '2021-05-27',]

fifas = c()
for(i in c(1:length(Scores_Group_A$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_A$Team[i],5]
}
Scores_Group_A = cbind(Scores_Group_A,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_B$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_B$Team[i],5]
}
Scores_Group_B = cbind(Scores_Group_B,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_C$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_C$Team[i],5]
}
Scores_Group_C = cbind(Scores_Group_C,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_D$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_D$Team[i],5]
}
Scores_Group_D = cbind(Scores_Group_D,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_E$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_E$Team[i],5]
}
Scores_Group_E = cbind(Scores_Group_E,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_F$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_F$Team[i],5]
}
Scores_Group_F = cbind(Scores_Group_F,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_D$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_D$Team[i],5]
}
Scores_Group_D = cbind(Scores_Group_D,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_E$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_E$Team[i],5]
}
Scores_Group_E = cbind(Scores_Group_E,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_F$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_F$Team[i],5]
}
Scores_Group_F = cbind(Scores_Group_F,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_G$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_G$Team[i],5]
}
Scores_Group_G = cbind(Scores_Group_G,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_H$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_H$Team[i],5]
}
Scores_Group_H = cbind(Scores_Group_H,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_I$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_I$Team[i],5]
}
Scores_Group_I = cbind(Scores_Group_I,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_J$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_J$Team[i],5]
}
Scores_Group_J = cbind(Scores_Group_J,fifas)

WC_Matches = read.csv('international-wc-qualification-africa-matches-2022-to-2022-stats.csv')
WC_Matches = WC_Matches[c(69:length(WC_Matches$date)),c(2,5,6)] # Future Matches to Play

fifa_home = fifa_score_func_home(WC_Matches)
fifa_away = fifa_score_func_away(WC_Matches)

fut_home = fut_scores_home(WC_Matches)
fut_away = fut_scores_away(WC_Matches)

new_data_Africa = update_frame_func(olddata = relevant_games, data = WC_Matches, 5,3)

# Actual Sims

for (i in c(1:sims)){
  Winners = c()
  ##### Group A
  
  Scores_1 = data.frame(Scores_Group_A)
  new_data_G1 = new_data_Africa[new_data_Africa$home %in% Scores_Group_A$Team, ]
  for(i in c(1:length(new_data_G1$home))){
    if(new_data_G1$h2h_timed[i] %in% c(NA,NaN)|new_data_G1$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G1$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G1[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G1$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G1[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G1[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G1[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_1[Scores_1$Team == new_data_G1$home[i],2] = Scores_1[Scores_1$Team == new_data_G1$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_1[Scores_1$Team == new_data_G1$home[i],2] = Scores_1[Scores_1$Team == new_data_G1$home[i],2] + 1
      Scores_1[Scores_1$Team == new_data_G1$away[i],2] = Scores_1[Scores_1$Team == new_data_G1$away[i],2] + 1
    }
    else{
      Scores_1[Scores_1$Team == new_data_G1$away[i],2] = Scores_1[Scores_1$Team == new_data_G1$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_1[order(-Scores_1$Score, -Scores_1$fifas),]
  # Top 1 qualify:
  Winners[1] = Scores_Sorted$Team[1]
  
  #####
  
  # Group B ####
  
  Scores_2 = data.frame(Scores_Group_B)
  new_data_G2 = new_data_Africa[new_data_Africa$home %in% Scores_Group_B$Team, ]
  for(i in c(1:length(new_data_G2$home))){
    if(new_data_G2$h2h_timed[i] %in% c(NA,NaN)|new_data_G2$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G2$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G2[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G2$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G2[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G2[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G2[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_2[Scores_2$Team == new_data_G2$home[i],2] = Scores_2[Scores_2$Team == new_data_G2$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_2[Scores_2$Team == new_data_G2$home[i],2] = Scores_2[Scores_2$Team == new_data_G2$home[i],2] + 1
      Scores_2[Scores_2$Team == new_data_G2$away[i],2] = Scores_2[Scores_2$Team == new_data_G2$away[i],2] + 1
    }
    else{
      Scores_2[Scores_2$Team == new_data_G2$away[i],2] = Scores_2[Scores_2$Team == new_data_G2$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_2[order(-Scores_2$Score, -Scores_2$fifas),]
  # Top 1 qualify:
  Winners[2] = Scores_Sorted$Team[1]
  
  ####
  
  # Group C ####
  Scores_3 = data.frame(Scores_Group_C)
  new_data_G3 = new_data_Africa[new_data_Africa$home %in% Scores_Group_C$Team, ]
  for(i in c(1:length(new_data_G3$home))){
    if(new_data_G3$h2h_timed[i] %in% c(NA,NaN)|new_data_G3$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G3$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G3[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G3$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G3[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G3[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G3[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_3[Scores_3$Team == new_data_G3$home[i],2] = Scores_3[Scores_3$Team == new_data_G3$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_3[Scores_3$Team == new_data_G3$home[i],2] = Scores_3[Scores_3$Team == new_data_G3$home[i],2] + 1
      Scores_3[Scores_3$Team == new_data_G3$away[i],2] = Scores_3[Scores_3$Team == new_data_G3$away[i],2] + 1
    }
    else{
      Scores_3[Scores_3$Team == new_data_G3$away[i],2] = Scores_3[Scores_3$Team == new_data_G3$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_3[order(-Scores_3$Score, -Scores_3$fifas),]
  # Top 1 qualify:
  Winners[3] = Scores_Sorted$Team[1]
  
  ####
  
  # Group D ####
  Scores_4 = data.frame(Scores_Group_D)
  new_data_G4 = new_data_Africa[new_data_Africa$home %in% Scores_Group_D$Team, ]
  for(i in c(1:length(new_data_G4$home))){
    if(new_data_G4$h2h_timed[i] %in% c(NA,NaN)|new_data_G4$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G4$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G4[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G4$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G4[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G4[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G4[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_4[Scores_4$Team == new_data_G4$home[i],2] = Scores_4[Scores_4$Team == new_data_G4$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_4[Scores_4$Team == new_data_G4$home[i],2] = Scores_4[Scores_4$Team == new_data_G4$home[i],2] + 1
      Scores_4[Scores_4$Team == new_data_G4$away[i],2] = Scores_4[Scores_4$Team == new_data_G4$away[i],2] + 1
    }
    else{
      Scores_4[Scores_4$Team == new_data_G4$away[i],2] = Scores_4[Scores_4$Team == new_data_G4$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_4[order(-Scores_4$Score, -Scores_4$fifas),]
  # Top 1 qualify:
  Winners[4] = Scores_Sorted$Team[1]
  
  ####
  
  # Group E ####
  Scores_5 = data.frame(Scores_Group_E)
  new_data_G5 = new_data_Africa[new_data_Africa$home %in% Scores_Group_E$Team, ]
  for(i in c(1:length(new_data_G5$home))){
    if(new_data_G5$h2h_timed[i] %in% c(NA,NaN)|new_data_G5$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G5$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G5[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G5$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G5[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G5[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G5[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_5[Scores_5$Team == new_data_G5$home[i],2] = Scores_5[Scores_5$Team == new_data_G5$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_5[Scores_5$Team == new_data_G5$home[i],2] = Scores_5[Scores_5$Team == new_data_G5$home[i],2] + 1
      Scores_5[Scores_5$Team == new_data_G5$away[i],2] = Scores_5[Scores_5$Team == new_data_G5$away[i],2] + 1
    }
    else{
      Scores_5[Scores_5$Team == new_data_G5$away[i],2] = Scores_5[Scores_5$Team == new_data_G5$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_5[order(-Scores_5$Score, -Scores_5$fifas),]
  # Top 1 qualify:
  Winners[5] = Scores_Sorted$Team[1]
  
  ####
  
  # Group F ####
  Scores_6 = data.frame(Scores_Group_F)
  new_data_G6 = new_data_Africa[new_data_Africa$home %in% Scores_Group_F$Team, ]
  for(i in c(1:length(new_data_G6$home))){
    if(new_data_G6$h2h_timed[i] %in% c(NA,NaN)|new_data_G6$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G6$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G6[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G6$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G6[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G6[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G6[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_6[Scores_6$Team == new_data_G6$home[i],2] = Scores_6[Scores_6$Team == new_data_G6$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_6[Scores_6$Team == new_data_G6$home[i],2] = Scores_6[Scores_6$Team == new_data_G6$home[i],2] + 1
      Scores_6[Scores_6$Team == new_data_G6$away[i],2] = Scores_6[Scores_6$Team == new_data_G6$away[i],2] + 1
    }
    else{
      Scores_6[Scores_6$Team == new_data_G6$away[i],2] = Scores_6[Scores_6$Team == new_data_G6$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_6[order(-Scores_6$Score, -Scores_6$fifas),]
  # Top 1 qualify:
  Winners[6] = Scores_Sorted$Team[1]
  
  ####
  
  # Group G ####
  Scores_7 = data.frame(Scores_Group_G)
  new_data_G7 = new_data_Africa[new_data_Africa$home %in% Scores_Group_G$Team, ]
  for(i in c(1:length(new_data_G7$home))){
    if(new_data_G7$h2h_timed[i] %in% c(NA,NaN)|new_data_G7$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G7$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G7[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G7$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G7[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G7[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G7[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_7[Scores_7$Team == new_data_G7$home[i],2] = Scores_7[Scores_7$Team == new_data_G7$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_7[Scores_7$Team == new_data_G7$home[i],2] = Scores_7[Scores_7$Team == new_data_G7$home[i],2] + 1
      Scores_7[Scores_7$Team == new_data_G7$away[i],2] = Scores_7[Scores_7$Team == new_data_G7$away[i],2] + 1
    }
    else{
      Scores_7[Scores_7$Team == new_data_G7$away[i],2] = Scores_7[Scores_7$Team == new_data_G7$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_7[order(-Scores_7$Score, -Scores_7$fifas),]
  # Top 1 qualify:
  Winners[7] = Scores_Sorted$Team[1]
  
  ####
  
  # Group H ####
  Scores_8 = data.frame(Scores_Group_H)
  new_data_G8 = new_data_Africa[new_data_Africa$home %in% Scores_Group_H$Team, ]
  for(i in c(1:length(new_data_G8$home))){
    if(new_data_G8$h2h_timed[i] %in% c(NA,NaN)|new_data_G8$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G8$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G8[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G8$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G8[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G8[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G8[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_8[Scores_8$Team == new_data_G8$home[i],2] = Scores_8[Scores_8$Team == new_data_G8$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_8[Scores_8$Team == new_data_G8$home[i],2] = Scores_8[Scores_8$Team == new_data_G8$home[i],2] + 1
      Scores_8[Scores_8$Team == new_data_G8$away[i],2] = Scores_8[Scores_8$Team == new_data_G8$away[i],2] + 1
    }
    else{
      Scores_8[Scores_8$Team == new_data_G8$away[i],2] = Scores_8[Scores_8$Team == new_data_G8$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_8[order(-Scores_8$Score, -Scores_8$fifas),]
  # Top 1 qualify:
  Winners[8] = Scores_Sorted$Team[1]
  
  ####
  
  # Group I ####
  Scores_9 = data.frame(Scores_Group_I)
  new_data_G9 = new_data_Africa[new_data_Africa$home %in% Scores_Group_I$Team, ]
  for(i in c(1:length(new_data_G9$home))){
    if(new_data_G9$h2h_timed[i] %in% c(NA,NaN)|new_data_G9$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G9$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G9[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G9$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G9[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G9[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G9[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_9[Scores_9$Team == new_data_G9$home[i],2] = Scores_9[Scores_9$Team == new_data_G9$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_9[Scores_9$Team == new_data_G9$home[i],2] = Scores_9[Scores_9$Team == new_data_G9$home[i],2] + 1
      Scores_9[Scores_9$Team == new_data_G9$away[i],2] = Scores_9[Scores_9$Team == new_data_G9$away[i],2] + 1
    }
    else{
      Scores_9[Scores_9$Team == new_data_G9$away[i],2] = Scores_9[Scores_9$Team == new_data_G9$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_9[order(-Scores_9$Score, -Scores_9$fifas),]
  # Top 1 qualify:
  Winners[9] = Scores_Sorted$Team[1]
  
  ####
  
  # Group J ####
  Scores_10 = data.frame(Scores_Group_J)
  new_data_G10 = new_data_Africa[new_data_Africa$home %in% Scores_Group_J$Team, ]
  for(i in c(1:length(new_data_G10$home))){
    if(new_data_G10$h2h_timed[i] %in% c(NA,NaN)|new_data_G10$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G10$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G10[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G10$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G10[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G10[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G10[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_10[Scores_10$Team == new_data_G10$home[i],2] = Scores_10[Scores_10$Team == new_data_G10$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_10[Scores_10$Team == new_data_G10$home[i],2] = Scores_10[Scores_10$Team == new_data_G10$home[i],2] + 1
      Scores_10[Scores_10$Team == new_data_G10$away[i],2] = Scores_10[Scores_10$Team == new_data_G10$away[i],2] + 1
    }
    else{
      Scores_10[Scores_10$Team == new_data_G10$away[i],2] = Scores_10[Scores_10$Team == new_data_G10$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_10[order(-Scores_10$Score, -Scores_10$fifas),]
  # Top 1 qualify:
  Winners[10] = Scores_Sorted$Team[1]
  
  ####
  
  Winners[sample(1:length(Winners))]
  Winners2 = rep(NA, 10)
  fifas = c()
  for(i in c(1:length(Winners))){
    fifas[i] = relevant_fifa[relevant_fifa$country_full == Winners[i],5]
  }
  Winners = data.frame(cbind(Winners,fifas))
  Winners$fifas = as.numeric(Winners$fifas)
  for(i in seq(from = 1, to = 10, by = 2)){
    placeholder = Winners[c(i,i+1),]
    placeholder = placeholder[(order(-placeholder$fifas)),]
    Winners2[i] = placeholder[1,1]
  }
  Winners2 = na.omit(Winners2)
  for(i in c(1:length(Winners2))){
    Qual_Frame_Africa[Qual_Frame_Africa$Team == Winners2[i], 2] = Qual_Frame_Africa[Qual_Frame_Africa$Team == Winners2[i], 2] + 1
  }
}

# UEFA setup. THere is also 2 stages to this confederation.

Scores_Group_A = data.frame(Team  = c('Portugal', 'Serbia', 'Luxembourg', 'Republic of Ireland', 'Azerbaijan'), Score = c(13,11,6,2,1))
Scores_Group_B = data.frame(Team  = c('Spain', 'Sweden', 'Greece', 'Kosovo', 'Georgia'), Score = c(13,9,6,4,1))
Scores_Group_C = data.frame(Team  = c('Italy', 'Switzerland', 'Northern Ireland', 'Bulgaria', 'Lithuania'), Score = c(14,8,5,5,0))
Scores_Group_D = data.frame(Team  = c('France', 'Ukraine', 'Finland', 'Bosnia and Herzegovina', 'Kazakhstan'), Score = c(12,5,5,3,3))
Scores_Group_E = data.frame(Team  = c('Belgium', 'Czech Republic', 'Wales', 'Belarus', 'Estonia'), Score = c(16,7,7,3,1))
Scores_Group_F = data.frame(Team  = c('Denmark', 'Scotland', 'Israel', 'Austria', 'Faroe Islands', 'Moldova'), Score = c(18,11,10,7,4,1))
Scores_Group_G = data.frame(Team  = c('Netherlands','Norway','Turkey','Montenegro','Latvia','Gibraltar'), Score = c(13,13,11,8,5,0))
Scores_Group_H = data.frame(Team  = c('Croatia', 'Russia', 'Slovakia', 'Slovenia', 'Malta', 'Cyprus'), Score = c(13,13,9,7,4,4))
Scores_Group_I = data.frame(Team  = c('England', 'Albania', 'Poland', 'Hungary', 'Andorra', 'San Marino'), Score = c(16,12,11,10,3,0))
Scores_Group_J = data.frame(Team  = c('Germany', 'Armenia', 'Romania', 'North Macedonia', 'Iceland', 'Liechtenstein'), Score = c(15,11,10,9,4,1))

Qual_Frame_Europe = data.frame(Team =c('Portugal', 'Serbia', 'Luxembourg', 'Republic of Ireland', 'Azerbaijan',
                                       'Spain', 'Sweden', 'Greece', 'Kosovo', 'Georgia',
                                       'Italy', 'Switzerland', 'Northern Ireland', 'Bulgaria', 'Lithuania',
                                       'France', 'Ukraine', 'Finland', 'Bosnia and Herzegovina', 'Kazakhstan',
                                       'Belgium', 'Czech Republic', 'Wales', 'Belarus', 'Estonia',
                                       'Denmark', 'Scotland', 'Israel', 'Austria', 'Faroe Islands', 'Moldova',
                                       'Netherlands','Norway','Turkey','Montenegro','Latvia','Gibraltar',
                                       'Croatia', 'Russia', 'Slovakia', 'Slovenia', 'Malta', 'Cyprus',
                                       'England', 'Albania', 'Poland', 'Hungary', 'Andorra', 'San Marino',
                                       'Germany', 'Armenia', 'Romania', 'North Macedonia', 'Iceland', 'Liechtenstein'), Score = rep(0,55))

relevant_fifa = fifa_scores[fifa_scores$rank_date == '2021-05-27',]

# Adding Fifas to Qual Frame as we need to sort using all teams later

fifas = c()
for(i in c(1:length(Qual_Frame_Europe$Team))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Qual_Frame_Europe$Team[i],5]
}

Qual_Frame_Europe = cbind(Qual_Frame_Europe, fifas)

fifas = c()
for(i in c(1:length(Scores_Group_A$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_A$Team[i],5]
}
Scores_Group_A = cbind(Scores_Group_A,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_B$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_B$Team[i],5]
}
Scores_Group_B = cbind(Scores_Group_B,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_C$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_C$Team[i],5]
}
Scores_Group_C = cbind(Scores_Group_C,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_D$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_D$Team[i],5]
}
Scores_Group_D = cbind(Scores_Group_D,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_E$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_E$Team[i],5]
}
Scores_Group_E = cbind(Scores_Group_E,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_F$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_F$Team[i],5]
}
Scores_Group_F = cbind(Scores_Group_F,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_G$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_G$Team[i],5]
}
Scores_Group_G = cbind(Scores_Group_G,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_H$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_H$Team[i],5]
}
Scores_Group_H = cbind(Scores_Group_H,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_I$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_I$Team[i],5]
}
Scores_Group_I = cbind(Scores_Group_I,fifas)
fifas = c()
for(i in c(1:length(Scores_Group_J$Score))){
  fifas[i] = relevant_fifa[relevant_fifa$country_full == Scores_Group_J$Team[i],5]
}
Scores_Group_J = cbind(Scores_Group_J,fifas)

WC_Matches = read.csv('international-wc-qualification-europe-matches-2022-to-2022-stats.csv')
WC_Matches = WC_Matches[c(150:length(WC_Matches$date)),c(2,5,6)] # Future Matches to Play

fifa_home = fifa_score_func_home(WC_Matches)
fifa_away = fifa_score_func_away(WC_Matches)

fut_home = fut_scores_home(WC_Matches)
fut_away = fut_scores_away(WC_Matches)

new_data_Europe = update_frame_func(olddata = relevant_games, data = WC_Matches, 5,3)

# Actual Sims

for (i in c(1:sims)){
  Winners = c()
  Runners = c()
  ##### Group A
  
  Scores_1 = data.frame(Scores_Group_A)
  new_data_G1 = new_data_Europe[new_data_Europe$home %in% Scores_Group_A$Team, ]
  for(i in c(1:length(new_data_G1$home))){
    if(new_data_G1$h2h_timed[i] %in% c(NA,NaN)|new_data_G1$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G1$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G1[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G1$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G1[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G1[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G1[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_1[Scores_1$Team == new_data_G1$home[i],2] = Scores_1[Scores_1$Team == new_data_G1$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_1[Scores_1$Team == new_data_G1$home[i],2] = Scores_1[Scores_1$Team == new_data_G1$home[i],2] + 1
      Scores_1[Scores_1$Team == new_data_G1$away[i],2] = Scores_1[Scores_1$Team == new_data_G1$away[i],2] + 1
    }
    else{
      Scores_1[Scores_1$Team == new_data_G1$away[i],2] = Scores_1[Scores_1$Team == new_data_G1$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_1[order(-Scores_1$Score, -Scores_1$fifas),]
  # Top 1 qualify:
  Winners[1] = Scores_Sorted$Team[1]
  Runners[1] = Scores_Sorted$Team[2]
  #####
  
  # Group B ####
  
  Scores_2 = data.frame(Scores_Group_B)
  new_data_G2 = new_data_Europe[new_data_Europe$home %in% Scores_Group_B$Team, ]
  for(i in c(1:length(new_data_G2$home))){
    if(new_data_G2$h2h_timed[i] %in% c(NA,NaN)|new_data_G2$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G2$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G2[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G2$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G2[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G2[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G2[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_2[Scores_2$Team == new_data_G2$home[i],2] = Scores_2[Scores_2$Team == new_data_G2$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_2[Scores_2$Team == new_data_G2$home[i],2] = Scores_2[Scores_2$Team == new_data_G2$home[i],2] + 1
      Scores_2[Scores_2$Team == new_data_G2$away[i],2] = Scores_2[Scores_2$Team == new_data_G2$away[i],2] + 1
    }
    else{
      Scores_2[Scores_2$Team == new_data_G2$away[i],2] = Scores_2[Scores_2$Team == new_data_G2$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_2[order(-Scores_2$Score, -Scores_2$fifas),]
  # Top 1 qualify:
  Winners[2] = Scores_Sorted$Team[1]
  Runners[2] = Scores_Sorted$Team[2]
  ####
  
  # Group C ####
  Scores_3 = data.frame(Scores_Group_C)
  new_data_G3 = new_data_Europe[new_data_Europe$home %in% Scores_Group_C$Team, ]
  for(i in c(1:length(new_data_G3$home))){
    if(new_data_G3$h2h_timed[i] %in% c(NA,NaN)|new_data_G3$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G3$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G3[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G3$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G3[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G3[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G3[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_3[Scores_3$Team == new_data_G3$home[i],2] = Scores_3[Scores_3$Team == new_data_G3$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_3[Scores_3$Team == new_data_G3$home[i],2] = Scores_3[Scores_3$Team == new_data_G3$home[i],2] + 1
      Scores_3[Scores_3$Team == new_data_G3$away[i],2] = Scores_3[Scores_3$Team == new_data_G3$away[i],2] + 1
    }
    else{
      Scores_3[Scores_3$Team == new_data_G3$away[i],2] = Scores_3[Scores_3$Team == new_data_G3$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_3[order(-Scores_3$Score, -Scores_3$fifas),]
  # Top 1 qualify:
  Winners[3] = Scores_Sorted$Team[1]
  Runners[3] = Scores_Sorted$Team[2]
  ####
  
  # Group D ####
  Scores_4 = data.frame(Scores_Group_D)
  new_data_G4 = new_data_Europe[new_data_Europe$home %in% Scores_Group_D$Team, ]
  for(i in c(1:length(new_data_G4$home))){
    if(new_data_G4$h2h_timed[i] %in% c(NA,NaN)|new_data_G4$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G4$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G4[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G4$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G4[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G4[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G4[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_4[Scores_4$Team == new_data_G4$home[i],2] = Scores_4[Scores_4$Team == new_data_G4$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_4[Scores_4$Team == new_data_G4$home[i],2] = Scores_4[Scores_4$Team == new_data_G4$home[i],2] + 1
      Scores_4[Scores_4$Team == new_data_G4$away[i],2] = Scores_4[Scores_4$Team == new_data_G4$away[i],2] + 1
    }
    else{
      Scores_4[Scores_4$Team == new_data_G4$away[i],2] = Scores_4[Scores_4$Team == new_data_G4$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_4[order(-Scores_4$Score, -Scores_4$fifas),]
  # Top 1 qualify:
  Winners[4] = Scores_Sorted$Team[1]
  Runners[4] = Scores_Sorted$Team[2]
  ####
  
  # Group E ####
  Scores_5 = data.frame(Scores_Group_E)
  new_data_G5 = new_data_Europe[new_data_Europe$home %in% Scores_Group_E$Team, ]
  for(i in c(1:length(new_data_G5$home))){
    if(new_data_G5$h2h_timed[i] %in% c(NA,NaN)|new_data_G5$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G5$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G5[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G5$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G5[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G5[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G5[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_5[Scores_5$Team == new_data_G5$home[i],2] = Scores_5[Scores_5$Team == new_data_G5$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_5[Scores_5$Team == new_data_G5$home[i],2] = Scores_5[Scores_5$Team == new_data_G5$home[i],2] + 1
      Scores_5[Scores_5$Team == new_data_G5$away[i],2] = Scores_5[Scores_5$Team == new_data_G5$away[i],2] + 1
    }
    else{
      Scores_5[Scores_5$Team == new_data_G5$away[i],2] = Scores_5[Scores_5$Team == new_data_G5$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_5[order(-Scores_5$Score, -Scores_5$fifas),]
  # Top 1 qualify:
  Winners[5] = Scores_Sorted$Team[1]
  Runners[5] = Scores_Sorted$Team[2]
  ####
  
  # Group F ####
  Scores_6 = data.frame(Scores_Group_F)
  new_data_G6 = new_data_Europe[new_data_Europe$home %in% Scores_Group_F$Team, ]
  for(i in c(1:length(new_data_G6$home))){
    if(new_data_G6$h2h_timed[i] %in% c(NA,NaN)|new_data_G6$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G6$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G6[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G6$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G6[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G6[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G6[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_6[Scores_6$Team == new_data_G6$home[i],2] = Scores_6[Scores_6$Team == new_data_G6$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_6[Scores_6$Team == new_data_G6$home[i],2] = Scores_6[Scores_6$Team == new_data_G6$home[i],2] + 1
      Scores_6[Scores_6$Team == new_data_G6$away[i],2] = Scores_6[Scores_6$Team == new_data_G6$away[i],2] + 1
    }
    else{
      Scores_6[Scores_6$Team == new_data_G6$away[i],2] = Scores_6[Scores_6$Team == new_data_G6$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_6[order(-Scores_6$Score, -Scores_6$fifas),]
  # Top 1 qualify:
  Winners[6] = Scores_Sorted$Team[1]
  Runners[6] = Scores_Sorted$Team[2]
  ####
  
  # Group G ####
  Scores_7 = data.frame(Scores_Group_G)
  new_data_G7 = new_data_Europe[new_data_Europe$home %in% Scores_Group_G$Team, ]
  for(i in c(1:length(new_data_G7$home))){
    if(new_data_G7$h2h_timed[i] %in% c(NA,NaN)|new_data_G7$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G7$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G7[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G7$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G7[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G7[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G7[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_7[Scores_7$Team == new_data_G7$home[i],2] = Scores_7[Scores_7$Team == new_data_G7$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_7[Scores_7$Team == new_data_G7$home[i],2] = Scores_7[Scores_7$Team == new_data_G7$home[i],2] + 1
      Scores_7[Scores_7$Team == new_data_G7$away[i],2] = Scores_7[Scores_7$Team == new_data_G7$away[i],2] + 1
    }
    else{
      Scores_7[Scores_7$Team == new_data_G7$away[i],2] = Scores_7[Scores_7$Team == new_data_G7$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_7[order(-Scores_7$Score, -Scores_7$fifas),]
  # Top 1 qualify:
  Winners[7] = Scores_Sorted$Team[1]
  Runners[7] = Scores_Sorted$Team[2]
  ####
  
  # Group H ####
  Scores_8 = data.frame(Scores_Group_H)
  new_data_G8 = new_data_Europe[new_data_Europe$home %in% Scores_Group_H$Team, ]
  for(i in c(1:length(new_data_G8$home))){
    if(new_data_G8$h2h_timed[i] %in% c(NA,NaN)|new_data_G8$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G8$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G8[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G8$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G8[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G8[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G8[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_8[Scores_8$Team == new_data_G8$home[i],2] = Scores_8[Scores_8$Team == new_data_G8$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_8[Scores_8$Team == new_data_G8$home[i],2] = Scores_8[Scores_8$Team == new_data_G8$home[i],2] + 1
      Scores_8[Scores_8$Team == new_data_G8$away[i],2] = Scores_8[Scores_8$Team == new_data_G8$away[i],2] + 1
    }
    else{
      Scores_8[Scores_8$Team == new_data_G8$away[i],2] = Scores_8[Scores_8$Team == new_data_G8$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_8[order(-Scores_8$Score, -Scores_8$fifas),]
  # Top 1 qualify:
  Winners[8] = Scores_Sorted$Team[1]
  Runners[8] = Scores_Sorted$Team[2]
  ####
  
  # Group I ####
  Scores_9 = data.frame(Scores_Group_I)
  new_data_G9 = new_data_Europe[new_data_Europe$home %in% Scores_Group_I$Team, ]
  for(i in c(1:length(new_data_G9$home))){
    if(new_data_G9$h2h_timed[i] %in% c(NA,NaN)|new_data_G9$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G9$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G9[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G9$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G9[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G9[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G9[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_9[Scores_9$Team == new_data_G9$home[i],2] = Scores_9[Scores_9$Team == new_data_G9$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_9[Scores_9$Team == new_data_G9$home[i],2] = Scores_9[Scores_9$Team == new_data_G9$home[i],2] + 1
      Scores_9[Scores_9$Team == new_data_G9$away[i],2] = Scores_9[Scores_9$Team == new_data_G9$away[i],2] + 1
    }
    else{
      Scores_9[Scores_9$Team == new_data_G9$away[i],2] = Scores_9[Scores_9$Team == new_data_G9$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_9[order(-Scores_9$Score, -Scores_9$fifas),]
  # Top 1 qualify:
  Winners[9] = Scores_Sorted$Team[1]
  Runners[9] = Scores_Sorted$Team[2]
  ####
  
  # Group J ####
  Scores_10 = data.frame(Scores_Group_J)
  new_data_G10 = new_data_Europe[new_data_Europe$home %in% Scores_Group_J$Team, ]
  for(i in c(1:length(new_data_G10$home))){
    if(new_data_G10$h2h_timed[i] %in% c(NA,NaN)|new_data_G10$fut_diff[i] %in% c(NA,NaN)){
      if(!(new_data_G10$fut_diff[i] %in% c(NA,NaN))){
        predicted = as.numeric(predict(model_2, newdata = new_data_G10[i,c(10,11,12)], type = 'response'))
      }
      else if(!(new_data_G10$h2h_timed[i] %in% c(NA,NaN))){
        predicted= as.numeric(predict(model_3, newdata = new_data_G10[i,c(5,10,11)], type = 'response'))
      }
      else{
        predicted = as.numeric(predict(model_4, newdata = new_data_G10[i,c(10,11)], type = 'response'))
      }
    }
    else{
      predicted = as.numeric(predict(model_1, newdata = new_data_G10[i,c(5,10,11,12)], type = 'response'))
    }
    away_range = predicted[1]
    draw_range = predicted[1] + predicted[2]
    rand = runif(1)
    if(rand > draw_range){
      Scores_10[Scores_10$Team == new_data_G10$home[i],2] = Scores_10[Scores_10$Team == new_data_G10$home[i],2] + 3
    }
    else if(rand > away_range){
      Scores_10[Scores_10$Team == new_data_G10$home[i],2] = Scores_10[Scores_10$Team == new_data_G10$home[i],2] + 1
      Scores_10[Scores_10$Team == new_data_G10$away[i],2] = Scores_10[Scores_10$Team == new_data_G10$away[i],2] + 1
    }
    else{
      Scores_10[Scores_10$Team == new_data_G10$away[i],2] = Scores_10[Scores_10$Team == new_data_G10$away[i],2] + 3
    }
  }
  Scores_Sorted <- Scores_10[order(-Scores_10$Score, -Scores_10$fifas),]
  # Top 1 qualify:
  Winners[10] = Scores_Sorted$Team[1]
  Runners[10] = Scores_Sorted$Team[2]
  ####
  for(i in c(1:length(Winners))){
    Qual_Frame_Europe[Qual_Frame_Europe$Team == Winners[i], 2] = Qual_Frame_Europe[Qual_Frame_Europe$Team == Winners[i], 2] + 1
  }
  filtered = Qual_Frame_Europe[!(Qual_Frame_Europe$Team %in% Winners) & !(Qual_Frame_Europe$Team %in% Runners), c(1,3)]
  filtered = filtered[order(-filtered$fifas),]
  New_Games = c(filtered[c(1,2),1], Runners)
  New_Games = New_Games[(sample(c(1:length(New_Games))))]
  fifas = c()
  for(i in c(1:length(New_Games))){
    fifas[i] = relevant_fifa[relevant_fifa$country_full == New_Games[i],5]
  }
  New_Games = data.frame(cbind(New_Games,fifas))
  New_Games$fifas = as.numeric(New_Games$fifas)
  
  Winners2 = c()
  for(i in seq(from = 1, to = 12, by = 4)){
    placeholder = New_Games[c(i:(i+3)),]
    placeholder = placeholder[(order(-placeholder$fifas)),]
    Winners2[i] = placeholder[1,1]
  }
  Winners2 = na.omit(Winners2)
  for(i in c(1:length(Winners2))){
    Qual_Frame_Europe[Qual_Frame_Europe$Team == Winners2[i], 2] = Qual_Frame_Europe[Qual_Frame_Europe$Team == Winners2[i], 2] + 1
  }
}

# Cleaning the Qualification frames

# Creating Probabilities through monte carlo ideas

Qual_Frame_Concaf$Quals = Qual_Frame_Concaf$Quals/sims
Qual_Frame_SA$Quals = Qual_Frame_SA$Quals/sims
Qual_Frame_G1$Quals = Qual_Frame_G1$Quals/sims
Qual_Frame_G2$Quals = Qual_Frame_G2$Quals/sims
OFC_Qual$Quals = OFC_Qual$Quals/sims
Qual_Frame_Europe$Score = Qual_Frame_Europe$Score/sims
Qual_Frame_Africa$Score = Qual_Frame_Africa$Score/sims

# Ordering the Frames

Qual_Frame_Concaf = Qual_Frame_Concaf[order(-Qual_Frame_Concaf$Quals),]
Qual_Frame_SA = Qual_Frame_SA[order(-Qual_Frame_SA$Quals),]
Qual_Frame_G1 = Qual_Frame_G1[order(-Qual_Frame_G1$Quals),]
Qual_Frame_G2 = Qual_Frame_G2[order(-Qual_Frame_G2$Quals),]
Qual_Frame_Africa = Qual_Frame_Africa[order(-Qual_Frame_Africa$Score),c(1,2)]
Qual_Frame_Europe = Qual_Frame_Europe[order(-Qual_Frame_Europe$Score),c(1,2)]

names(Qual_Frame_Africa)[2] = 'Quals'
names(Qual_Frame_Europe)[2] = 'Quals'

#Total Frame

# Note that from interconfederation we are considering the highest 2 probabilities of the relevant
#placings in each confederation (the placing that sends them to the interconfed games)

# We use qual_frame_g2 as the 3rd place has a higher probability of going to the interconfed than the g1 3rd

Inter_qual = rbind(Qual_Frame_G2[3,],OFC_Qual[1,],Qual_Frame_SA[5,], Qual_Frame_Concaf[4,])
Inter_qual = Inter_qual[order(-Inter_qual$Quals),] 



Qualification = rbind(Qual_Frame_Concaf[c(1:3),], Qual_Frame_SA[c(1:4),], 
                      Qual_Frame_G1[c(1,2),], Qual_Frame_G2[c(1,2),],
                      Qual_Frame_Africa[c(1:5),], Qual_Frame_Europe[c(1:13),],
                      Inter_qual[c(1,2),])

write.csv(Qualification, file = 'Qualifications.csv')
