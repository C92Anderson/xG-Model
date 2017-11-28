############################################################################################################################################################################
#
# PROJECT:        xG Model Measuring Goaltender Performance
#
# PURPOSE:        Using Logistic Regression create an xG for each shot a Goalie faces. 
#                 Compare xG Against to Actual Goals Against to measure performance
#
# CREATED BY:     Cole Anderson (cole92anderson@gmail.com)
#
# LAST UPDATED:   3/31/2016
#
# PROCESS:        0 - SYSTEM PREP
#                 1 - UPDATE Game_Id AND LOAD NHL PBP DATA USING NHLSCRAPR
#                 2 - LOGISTIC MODEL TO DEVELOP XG MODEL
#
############################################################################################################################################################################

############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(httr); library(data.table)

source("/Users/colander1/Documents/CWA/R Code/operations.R")

conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',
                  host='mysql.crowdscoutsports.com', db='nhl_all')
on.exit(dbDisconnect(conn))

### Load Gaolie/Skater Roster with Handedness
skater_roster <- dbGetQuery(conn, "SELECT distinct upper(playerName) as Shooter, 
                            playerId as p1_ID,  
                            playerPositionCode as `Player_Position`,
                            playerShootsCatches as Shoots,
                            playerBirthDate as shooterDOB
                            FROM hockey_roster_info AS B
                            WHERE playerPositionCode != 'G'") %>%
  #filter(!p1_ID %in% c(8474744,8466208,8471747,8468436,8466155,8476979,8471221)) %>%
  unique() %>%
  mutate_all(funs(gsub("MATTHEW ","MATT ", .))) %>%
  mutate_all(funs(gsub("PIERRE-ALEXANDRE ","PA ", .)))


goalie_roster <- dbGetQuery(conn, "SELECT distinct upper(playerName) as `SA_Goalie`, playerId as SA_Goalie_Id, playerHeight as height,  
                            playerShootsCatches as Catches, playerBirthDate as goalieDOB FROM `nhl_all`.`hockey_goalies_roster` AS A") %>%
          unique()

############################################################################################################################################################################
########1.A READ CURRENT CODE AND SAVE
############################################################################################################################################################################

pbp_read <- function(file) {
  
  data <- read.csv(paste0('~/Documents/CWA/HockeyScrape/',file,'_final.csv'), sep=",", header = TRUE) 
  data$season <- substr(file, 8, 16)
  data <- data %>% dplyr::select(-c(X.1, Unnamed..0))
  return(data)
}

# Combine season data
files <- c('nhl_pbp20092010','nhl_pbp20102011','nhl_pbp20112012','nhl_pbp20122013','nhl_pbp20132014','nhl_pbp20142015','nhl_pbp20152016','nhl_pbp20162017')

shot_data_adj <- plyr::rbind.fill(lapply(FUN=pbp_read,files))

save(shot_data_adj,file="~/Documents/CWA/Hockey Data/shot_data_adj.RData")

########################################################
##Inspect Home & Away Rink Shot Distance by Team Season
########################################################
load("~/Documents/CWA/Hockey Data/shot_data_adj.RData")

team_list <- unique(shot_data_adj$Home_Team)

team_shots <- function(team) {
      team_shots <- shot_data_adj %>%
        mutate(Rink = ifelse(Home_Team == team,"Home_Rink","Away_Rink"),
               Home_Team = ifelse(Home_Team %in% c("ATL"),"WPG",as.character(Home_Team))) %>%
        dplyr::select(season, Rink, Shot_Distance, Shot_Distance_Unadj, Shot_Angle) %>%
        mutate(Team = team)
}


shots_home_away <- plyr::rbind.fill(lapply(FUN=team_shots,team_list)) %>%
        group_by(Team, season, Rink) 


shot_distance_pctls <- shots_home_away%>%
        arrange(Team, season, Rink, Shot_Distance) %>%
        mutate(Shot_Distance_Pctl = round(rank(Shot_Distance)/length(Shot_Distance),2)) %>%
        group_by(Team, season, Rink, Shot_Distance_Pctl) %>%
        summarise(Shot_Distance = median(Shot_Distance, na.rm = TRUE)) %>%
        dcast(Team + season + Shot_Distance_Pctl ~ Rink, value.var = "Shot_Distance")

unadj_shot_distance_pctls <- shots_home_away%>%
        arrange(Team, season, Rink, Shot_Distance_Unadj) %>%
        mutate(Shot_Distance_Pctl = round(rank(Shot_Distance_Unadj)/length(Shot_Distance_Unadj),2)) %>%
        group_by(Team, season, Rink, Shot_Distance_Pctl) %>%
        summarise(Shot_Distance_Unadj = median(Shot_Distance_Unadj, na.rm = TRUE)) %>%
        dcast(Team + season + Shot_Distance_Pctl ~ paste0(Rink,"Unajd"), value.var = "Shot_Distance_Unadj")


shot_distance_pctls_compare <- shot_distance_pctls %>%
            left_join(unadj_shot_distance_pctls, by = c("Team", "season", "Shot_Distance_Pctl")) %>%
            mutate(`Venue Shot Distance, Unadjusted` = Home_RinkUnajd - Away_RinkUnajd,
                   `Venue Shot Distance, CDF Adjusted` = Home_Rink - Away_Rink) %>%
            dplyr::select(Team, season, Shot_Distance_Pctl, `Venue Shot Distance, Unadjusted`, `Venue Shot Distance, CDF Adjusted`) %>%
            melt(id.vars = c("Team", "season", "Shot_Distance_Pctl"))


shot_distance_pctls_compare_plt <- shot_distance_pctls_compare %>% na.omit() %>%
      mutate(Team1 = ifelse(Team %in% c("NYR","BOS","NYI","STL","BUF","DAL","TOR"),as.character(Team),"Other"),
             Highlight = ifelse(Team %in% c("NYR","BOS","NYI","STL","BUF","DAL"),1,0)) %>%
      filter(season %in% c("20092010") & Highlight == 1) %>%
      ggplot(aes(x=Shot_Distance_Pctl, y=value, group=Team1, color=Team1, alpha=Highlight)) +
      geom_hline(yintercept = 0, color = "grey75") +
      geom_point() +
      geom_line(alpha=0.25) +
      ylim(-15, 10) + 
      scale_x_continuous(labels = scales::percent) +
      annotate("text", y=-8, x=0.5, label = "Recorded Shots in Home Games\nCloser than Away Games", size = 5, color="grey50") +
      annotate("text", y=8, x=0.5, label = "Recorded Shots in Away Games\nCloser than Home Games", size = 5, color="grey50") +
      theme_standard() + ggthemes::scale_color_gdocs() +
  facet_free(season ~ variable) +
      labs(title = "Shot Distance CDF Recorded Difference Between Home and Away Games\nSelect Teams and Seasons",
           x="Shot Distance Percentile", y = "Home - Away Shot Distance at Percentile", color="Team") +
      guides(alpha=FALSE)

ggsave(filename=paste0("/Users/colander1/Downloads/shot_distance_pctls_compare_plt.png"), plot=shot_distance_pctls_compare_plt,
       width=16, height=8)


############################################################################################################################################################################
########1.B LIMIT TO SHOTS AGAINST AND DEVELOP FEATURES
############################################################################################################################################################################

load("~/Documents/CWA/Hockey Data/shot_data_adj.RData")

shot_data_expanded <- shot_data_adj %>%
               arrange(season, Game_Id, Period, Seconds_Elapsed) %>%
  
               mutate(season2 = ifelse(substr(Game_Id,1,1) == "3", paste0(season,"p"), paste0(season)),
                      
                      SA_Goalie = ifelse(Ev_Team == Home_Team, as.character(Away_Goalie), as.character(Home_Goalie)),
                      SA_Goalie_Id = ifelse(Ev_Team == Home_Team, Away_Goalie_Id, Home_Goalie_Id),
                    
                      Away_State = ifelse(Away_Goalie_Id %in% c(awayPlayer6_id, awayPlayer5_id, awayPlayer4_id, awayPlayer3_id),
                                           Away_Players - 1 , Away_Players),
                      Home_State = ifelse(Home_Goalie_Id %in% c(homePlayer6_id, homePlayer5_id, homePlayer4_id, homePlayer3_id),
                                          Home_Players - 1 , Home_Players),
                      
                      Results_inRebound = lead(is_Rebound),
                      
                      Shooter_State = ifelse(Ev_Team == Away_Team, Away_State, Home_State),
                      Goalie_State = ifelse(Ev_Team != Away_Team, Away_State, Home_State),
                      Game_State = ifelse(Ev_Team == Away_Team, 
                                              paste0(Away_State,"v",Home_State),
                                              paste0(Home_State,"v",Away_State)),
                      Game_State = ifelse(Game_State %in% c("3v5","3v4","3v6","4v5","4v6","5v6"),"SH.SA",
                                        ifelse(Game_State %in% c("6v3","6v4","5v3"),"PP.2p.SA",
                                        ifelse(Game_State %in% c("5v5","6v6"),"5v5",
                                               Game_State))),
                      p1_ID = as.character(p1_ID)) %>%
  
            left_join(skater_roster[c("p1_ID","Shoots","Player_Position","Shooter")], by = "p1_ID") %>% 
  
            left_join(goalie_roster[c("Catches","SA_Goalie_Id")], by="SA_Goalie_Id") %>%
  
            mutate(Shooter_Handedness = ifelse(Shoots == "L","L",
                                                ifelse(Shoots == "R","R",
                                                       ifelse(is.na(Shoots),"U","U"))),
                    Player_Position = as.factor(ifelse(!is.na(Player_Position), Player_Position, "U")),
                    Shooter_Handedness = as.factor(ifelse(!is.na(Shooter_Handedness), Shooter_Handedness, "U")),
                    #Catches = ifelse(is.na(Catches) | Catches == "NULL","L",Catches),
                    Handed_Class = as.factor(ifelse(Shooter_Handedness == "L" & Catches == "L", "LL",
                                           ifelse(Shooter_Handedness == "L" & Catches == "R", "LR",
                                                  ifelse(Shooter_Handedness == "R" & Catches == "L", "RL",
                                                         ifelse(Shooter_Handedness == "R" & Catches == "R", "RR",
                                                                "U"))))),
                    Handed_Class2 = as.factor(ifelse(Handed_Class %in% c("LL","RR"),"Same",
                                            ifelse(Handed_Class %in% c("LR","RL"),"Opposite",
                                                   "U"))),
                    Player_Position2 = as.factor(ifelse(Player_Position == "D", "D", 
                                               ifelse(Player_Position %in% c("C","L","R","F"),"F",
                                                      "U"))))

print(nrow(shot_data_adj))
print(nrow(shot_data_expanded))


unknown_shooter <- shot_data_expanded %>%  filter(Player_Position %in% c("U","G")) %>% group_by(Shooter, p1_name, Player_Position) %>% count(cnt = n()) %>% arrange(-cnt)

unknown_goalie <- shot_data_expanded %>%  filter((is.na(Catches) | is.na(SA_Goalie_Id)) & EmptyNet_SA == 0) %>% 
          group_by(EmptyNet_SA, Home_Team, SA_Goalie, SA_Goalie_Id, Ev_Team, Home_Goalie, Away_Goalie) %>% count(cnt = n()) %>% arrange(-cnt) %>%
          mutate(Missing_Goalie = ifelse(Ev_Team == Home_Team, as.character(Away_Goalie), as.character(as.factor(Home_Goalie))))


# Check counts of factors
shot_data_expanded %>% 
        group_by(season) %>%
        summarise(Mean_Shot_Distance = mean(Shot_Distance),
               Shooting_Percentage = mean(Goal),
               Miss_Share = mean(Event == "MISS"))

############################################################################################################################################################################
########1.C CALCULATE SHOOTER SKILL (not including blocked/missed shots)
############################################################################################################################################################################

# Find average shooting by position
shooting_percentage_D <- shot_data_expanded %>% filter(Player_Position == "D") %>% summarise(shooting_percentage = mean(Goal,na.rm=T)) %>% as.numeric()
shooting_percentage_F <- shot_data_expanded %>% filter(Player_Position != "D") %>% summarise(shooting_percentage = mean(Goal,na.rm=T)) %>% as.numeric()

# Track shooter level cumulative shooting percentage and apply kr21_stabilizer by position
shot_data_expanded2 <- shot_data_expanded %>%
    group_by(Shooter) %>%
    mutate(Shooter_Cum_Shots = seq(n()),
           Shooter_Cum_Shooting_Percentage = cumsum(Goal) / seq(n()),
           kr21_stabilizer_F = 375, #http://www.statisticshowto.com/kuder-richardson/ 
           kr21_stabilizer_D = 275, #http://www.statisticshowto.com/kuder-richardson/ 
           #https://hockey-graphs.com/2015/10/01/expected-Goals-are-a-better-predictor-of-future-scoring-than-corsi-Goals/
           Regressed_Shooting_Percentage = ifelse(Player_Position == "D",   
                                            (cumsum(Goal) + (kr21_stabilizer_D * shooting_percentage_D)) / (seq(n()) + kr21_stabilizer_D),
                                            (cumsum(Goal) + (kr21_stabilizer_F * shooting_percentage_F)) / (seq(n()) + kr21_stabilizer_F)),
           Regressed_Shooting_Percentage_Indexed = ifelse(Player_Position == "D", 
                                                          Regressed_Shooting_Percentage / shooting_percentage_D,
                                                          Regressed_Shooting_Percentage / shooting_percentage_F)) %>%
          ungroup()
  
  #(2062/(2062-1)) * (1-(0.08870118*(2062-0.08870118)/(2062*0.005585664)))

#(2062/(2062-1)) * -1*((1-((0.08870118*(2062-0.08870118))/(2062*0.005585664))))

  #n =2062
  #M = 0.08870118
  #var = 0.005585664
  
save(shot_data_expanded2, file="~/Documents/CWA/Hockey Data/shot_data_expanded_wShootingTalent.RData")

############################################################################################################################################################################
########2.A LOGISTIC MODEL TO DEVELOP XG MODEL
############################################################################################################################################################################
load("~/Documents/CWA/Hockey Data/shot_data_expanded_wShootingTalent.RData")

model_data <- shot_data_expanded2 %>%
          #group_by(season) %>%
          #sample_n(10000) %>% ungroup() %>%
          dplyr::select(Goal, Type, EmptyNet_SA, Player_Position2,Handed_Class2,Shooter_State, Goalie_State,
                starts_with("Shot"),
                -ends_with("Unadj"),
                starts_with("is"),
                starts_with("LN_"),
                starts_with("LastEV"),
                ends_with("Indexed")) %>% na.omit()
  
############################################################
### Find highly correlated variables
############################################################
model_num_data <- model_data %>% dplyr::select(-c(Type, Player_Position2, Handed_Class2))
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
# calculate correlation matrix
correlationMatrix <- model_num_data %>% cor()
# summarize the correlation matrix
corr_matrix <- corrplot::corrplot(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

############################################################
### Modeling
############################################################
library(mlbench)
library(caret)
library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2); library(MLmetrics)
set.seed(1234)  

# Set folds
folds <- crossv_kfold(model_data, k = 10)

# Run model over folds
model_folds <- folds %>% 
  mutate(model = map(train, ~ glm(Goal ~ . #poly(Shot_Distance,3) + poly(Shot_Angle, 3) + .
                                  , data = ., family = "binomial", na.action = na.exclude)))

# Predict test data
predicted <- model_folds %>% 
  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y, , type.predict = "response"))) %>% 
  unnest(predicted)

#Calculate residual
model_metrics <- predicted %>% 
  mutate(residual = .fitted - Goal) %>%
   group_by(.id) %>% 
   summarise(sst = sum((Goal - mean(Goal)) ^ 2), # Sum of Squares Total
              sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
              r.squared = 1 - sse / sst ,        # Proportion of variance accounted for
              brierScore = mean((.fitted-Goal)^2), ## Brier Score
              mae = mean(abs(.fitted-Goal)), # Mean Absolute Error
              rmse = sqrt(mean((.fitted-Goal)^2))) %>%   # RMSE
    arrange(brierScore)

model_aucs <- c()
model_gain_aucs <- c()

for(i in unique(predicted$.id)) {
  
  pred_sample <- predicted %>% filter(.id == i)
  
  model_aucs[i] <- AUC(pred_sample$.fitted,pred_sample$Goal)
  model_gain_aucs[i] <- GainAUC(pred_sample$.fitted,pred_sample$Goal)
  
}


          
# Find best model to apply to data
best_model_no <- as.data.frame(cbind(model_aucs, model_gain_aucs)) %>%
        mutate(id = row_number()) %>% 
        filter(model_aucs == max(model_aucs)) %>% 
        dplyr::select(id) %>% as.character()

best_model <- model_folds$model[[paste0(as.numeric(best_model_no))]] 


# Output xG
xG_raw <- predict(best_model, shot_data_expanded2, type='response')

## Scored Data          
scored_data_raw <-cbind(xG_raw,shot_data_expanded2)
          
    
          
          AUC.curve <- data.frame(TPR=cumsum(predicted$Goal)/sum(predicted$Goal), 
                                  FPR=cumsum(!predicted$Goal)/sum(!predicted$Goal),
                                  Rand = cumsum(predicted$shot)/sum(predicted$shot),
                                  Target=predicted$Goal,
                                  Season = predicted$season) %>%
                        ggplot() +
                        geom_line(aes(x=FPR, y=TPR, color="TPR")) +
                        geom_line(aes(x=FPR, y=Rand, color="Random")) +
                        labs(x="FPR", y="TPR", title="AUC", color="") +
                        annotate("text", x = 0.1, y = 0.9, hjust=0, label = paste0("@CrowdScoutSprts\nxG Model built using nhlscrapr\nAUC: ", 
                                                                                   round(AUC.sample,2))) +
                        theme(panel.background = element_blank()) 
                                  
          # Gains Chart
          gains.chart <- data.frame(Gain=cumsum(predicted$Goal)/sum(predicted$Goal), 
                                    Random = cumsum(predicted$shot)/sum(predicted$shot)) %>%
                      ggplot() +
                      geom_line(aes(x=Random, y=Gain,colour="Model")) +
                      geom_line(aes(x=Random, y=Random,colour="Random")) +
                      labs(y="Cumulative Share of Goals", x="Cumulative Rank-Ordered Shots", title="Gains Chart", color="") +
                      annotate("text", x = 0.1, y = 0.9, hjust=0, label = paste0("@CrowdScoutSprts\nxG Model built using nhlscrapr\nGains AUC: ", 
                                                                                 round(GainAUC.sample,2))) +
                      theme(panel.background = element_blank(),
                            panel.grid.major.y = element_line(colour = "light grey", size = 0.1),
                            panel.grid.major.x = element_line(colour = "light grey", size = 0.1))
                    
          colnames(predicted) <- c("xG","Goal","shot")
          
          return(list(seasonal.model, measurement.metrics, class.plot, con.mat, scored_data.raw, AUC.curve, gains.chart))
  }      
  
  load("~/Documents/CWA/Hockey Data/shot_data_expanded_wShootingTalent.RData")
  
  # Check variable distributions
  shot_data_expanded2 %>% ggplot(aes(x=Shot_Distance)) + geom_density()
  shot_data_expanded2 %>% ggplot(aes(x=Shot_Angle)) + geom_density()
  shot_data_expanded2 %>% ggplot(aes(x=Rebound.Angle.Second)) + geom_density()
  shot_data_expanded2 %>% ggplot(aes(x=time.last.off.shot)) + geom_density()
  shot_data_expanded2 %>% ggplot(aes(x=time.last.def.shot)) + geom_density()
  shot_data_expanded2 %>% ggplot(aes(x=regressed.shooting.skill.index)) + geom_density()
  shot_data_expanded2 %>% ggplot(aes(x=LN.Rebound.Angle.Distance)) + geom_density()
  shot_data_expanded2 %>% group_by(gamestate) %>% count()
  shot_data_expanded2 %>% group_by(last.off.shot) %>% count()
  shot_data_expanded2 %>% group_by(last.def.shot) %>% count()
  shot_data_expanded2 %>% group_by(last.off.give) %>% count()
  shot_data_expanded2 %>% group_by(last.def.give) %>% count()
  shot_data_expanded2 %>% group_by(last.neu.give) %>% count()
  shot_data_expanded2 %>% group_by(is_Rebound) %>% count()
  shot_data_expanded2 %>% group_by(is_Rush) %>% count()
  
  
  predicted.Goal.model <- lm.cv.10(input = shot_data_expanded2,
                                
                                   model.vars = c("Goal",
                                     "Shot_Distance",
                                    "Shot_Angle",
                                  "is_Rush", 
                                  "is_Rebound",
                                  "LN_Rebound_Distance_Traveled_byAngle",
                                  #"LN_Last_Event_Time", Bad Variable
                                  "LastEV_Off_Faceoff","LastEV_Def_Faceoff","LastEV_Neu_Faceoff",                 
                                  "LastEV_Off_Shot","LastEV_Def_Shot","LastEV_Neu_Shot",                      
                                  "LastEV_Off_Give","LastEV_Def_Give","LastEV_Neu_Give",                      
                                  "Type","Regressed_Shooting_Percentage_Indexed",
                                  "Strength","Goalie_State", #"gamestate",
                                  "Player_Position2",#"Off.Hand.Shot", Bad Variable
                                  "Handed.Class2","EmptyNet_SA",
                                  "Home_Score","Away_Score"
                                  ),  
                      
                                extra.vars = c("season","season2","Game_Id","Period","Seconds_Elapsed","Ev_Team","Event","Away_Team",
                                               "Home_Team","Home_Goalie","Away_Goalie","Shooter",
                                   "SA_Goalie","Date","Type","LS.shot","Shooter_Handedness",
                                  #"a1", "a2", "a3", "a4", "a5", "a6", "h1", "h2", "h3", "h4", "h5", "h6","XC","YC","same.period.shot",
                                  "LN_Last_Event_Time"))   
  
model_coefficients <- as.data.frame(rbind(model_folds$model$`1`$coefficients,
                                    model_folds$model$`2`$coefficients,
                                    model_folds$model$`3`$coefficients,
                                    model_folds$model$`4`$coefficients,
                                    model_folds$model$`5`$coefficients,
                                    model_folds$model$`6`$coefficients,
                                    model_folds$model$`7`$coefficients,
                                    model_folds$model$`8`$coefficients,
                                    model_folds$model$`9`$coefficients,
                                    model_folds$model$`10`$coefficients)) %>% 
          mutate(model_id = row_number())



model_coefficients %>% 
    melt(id.vars = "model_id") %>%
    ggplot(aes(x=reorder(variable,-value), y=value)) +
    geom_boxplot(outlier.shape = NA) + 
    coord_flip() +
    #ylim(c(-3,3)) +
    labs(title = "xG Logistic Regression Coefficient Stability by Season", x="Variable", y="Coefficient Value") +
    annotate("text", color = "grey50", x=15, y=-2, label = "Higher Value\nDecreases Chance of Goal") +
    annotate("text", color = "grey50", x=15, y=2, label = "Higher Value\nIncreases Chance of Goal") 
  
  

# R-squared
1 - (best_model$deviance / best_model$null.deviance)
  #  0.1002869
  # 0.1123704
  
# Check xG and Goals ratio
sum(as.numeric(best_model$Goal))
sum(xG_raw)
# 62788
# 63121.07
  
## By Season compare xG to Goals
scored_data_raw %>% 
          group_by(season) %>% 
          summarise(xG=sum(xG_raw, na.rm=T), Goals=sum(Goal), 
                    Shot_Distance=mean(Shot_Distance), Shot_Angle=mean(Shot_Angle), is_Rebound=mean(as.numeric(is_Rebound)),
                    Regressed_Shooting_Percentage_Indexed = mean(Regressed_Shooting_Percentage_Indexed)) %>%
    ggplot() +
    geom_line(aes(x=season, y=xG,group=1),color="blue") +
    geom_line(aes(x=season, y=Goals,group=1),color="green") +
    theme(panel.background = element_blank()) +
    ylim(0,8000) +
    labs(title="Actual Goals (Green) v Expected Goals (Blue) by Season", x="Season",y="")
    
  
########################################################################
#########SCORE DATA AND ADJUST FOR REBOUNDS
########################################################################
# Adjust for rebounds
scored_data <- scored_data_raw %>%
  arrange(season, Game_Id, Seconds_Elapsed) %>%
  mutate(xG = xG_raw,
         xG_team = ifelse(is_Rebound == 0, xG_raw,
                   ifelse(is_Rebound == 1 & lag(is_Rebound) == 0, xG_raw * (1-lag(xG_raw)),
                   ifelse(is_Rebound == 1 & lag(is_Rebound) == 1 & lag(is_Rebound,2) == 0,
                                    xG_raw * (1-lag(xG_raw)) * (1-lag(xG_raw,2)),
                   ifelse(is_Rebound == 1 & lag(is_Rebound) == 1 & lag(is_Rebound,2) == 1 & lag(is_Rebound,3) == 0,
                                 xG_raw * (1-lag(xG_raw)) * (1-lag(xG_raw,2)) * (1-lag(xG_raw,3)),
                   ifelse(is_Rebound == 1 & lag(is_Rebound) == 1 & lag(is_Rebound,2) == 1 & lag(is_Rebound,3) == 1 & lag(is_Rebound,4) == 0,
                                 xG_raw * (1-lag(xG_raw)) * (1-lag(xG_raw,2)) * (1-lag(xG_raw,3)) * (1-lag(xG_raw,4)),
                                 xG_raw * (1-lag(xG_raw)) * (1-lag(xG_raw,2)) * (1-lag(xG_raw,3)) * (1-lag(xG_raw,4))))))))

## Bucket Sh% and xSh%
scored_data %>% 
  mutate(xG_bucket = round(xG,1)) %>%
  group_by(xG_bucket) %>% 
  summarise(xG_shot=sum(xG) / n(), Goals_shot=sum(as.numeric(Goal)) / n(), shots=n()) %>%
  ggplot() +
  #geom_boxplot(aes(x=xG_bucket, y=Goals_shot, group= xG_bucket))
  geom_point(aes(x=xG_bucket, y=Goals_shot,size=shots))  

#####################
### Error Checks
#####################

### Log Loss
library(MLmetrics)
LogLoss(scored_data$xG,as.numeric(scored_data$Goal)-1)
# 0.2036276 ## by season - all shots
# 0.2567852 by season - shots only

error <- scored_data$xG-(as.numeric(scored_data$Goal)-1)

## Brier Score - Base Line (Goal <- 0): 0.08919192
mean(error^2)
# 0.05334452 ## by season - all shots
# 0.07151661 # by season - shots only

# Mean Absolute Error
mean(abs(error))
# 0.1067507 ## by season - all shots
# 0.142824 # by season - shots only

# RMSE
sqrt(mean(error^2))
#0.2309643 ## by season - all shots
#0.2674259 # by season - shots only

# View by season
scored_data %>% 
  group_by(season2) %>% 
  summarise(xG=sum(xG), Goals=sum(as.numeric(Goal)-1), xG.shot = xG / n(),
            avg.dist=mean(shot.dist), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is_Rebound)-1))

# xG Distribution
scored_data %>%
  ggplot() +
  #geom_histogram(aes(xG), binwidth =  0.01)
  geom_density(aes(xG, color = season)) +
  theme(panel.background = element_blank()) +
  labs(title="xG Distribution by Season", y="Density", x="xG Probability") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) 

############################################################################################################################################################################
########PREDICT REBOUND
############################################################################################################################################################################
library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2); library(MLmetrics)

rebound.model.data <- scored_data %>% filter(Goal == 0)

rebound.model <- glm(data = rebound.model.data,
          shot.results.inRebound ~ xG + poly(shot.dist,3, raw = T) + poly(shot.angle,3, raw = T) +  shot.type + Player_Position2 +
          time.last.off.shot + gamestate, family="binomial")

# Set folds
folds <- crossv_kfold(rebound.model.data, k = 10)

# Run model over folds
rebound.model.folds <- folds %>% 
  mutate(rebound.model = map(train, ~ glm(shot.results.inRebound ~ poly(xG,3, raw = T) + (1-xG) + poly(shot.dist,3, raw = T) + poly(shot.angle,3, raw = T) +  shot.type + Player_Position2 +
                                    time.last.off.shot + gamestate, data = ., family = "binomial", na.action = na.exclude)))

# Predict test data
predicted.rebound <- rebound.model.folds %>% 
  mutate(predicted.rebound = map2(rebound.model, test, ~ augment(.x, newdata = .y, , type.predict = "response"))) %>% 
  unnest(predicted.rebound)

#Calculate residual
predicted.rebound <- predicted.rebound %>% 
  mutate(residual = .fitted - shot.results.inRebound)


# Calculate measurement metrics
rb.mm <- predicted.rebound %>%
  group_by(.id) %>% 
  summarise(
    sst = sum((shot.results.inRebound - mean(shot.results.inRebound)) ^ 2), # Sum of Squares Total
    sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
    r.squared = 1 - sse / sst ,        # Proportion of variance accounted for
    brierScore = mean((.fitted-shot.results.inRebound)^2), ## Brier Score
    brierScore.Base = mean((0-shot.results.inRebound)^2), ## Brier Score
    mae = mean(abs(.fitted-shot.results.inRebound)), # Mean Absolute Error
    rmse = sqrt(mean((.fitted-shot.results.inRebound)^2))   # RMSE
  )


# Find best model to apply to data
best.model.no <- rb.mm %>% filter(rmse == min(rmse)) %>% select(.id) %>% as.character()
best.rebound.model <- rebound.model.folds$rebound.model[[paste0(as.numeric(best.model.no))]] 

# Output Predicted
pred.rebound <- predict(best.rebound.model, rebound.model.data, type='response')


best.rebound.model %>% summary()
# R-squared
1 - (best.rebound.model$deviance / best.rebound.model$null.deviance)
#0.03453118

## Combine Data and Impute
scored_data <- cbind(scored_data,pred.rebound) 

### Log Loss
library(MLmetrics)
LogLoss(scored_data$pred.rebound,scored_data$shot.results.inRebound)
#0.1520091
# 0.1711709 - shots only

rb.error <- scored_data$pred.rebound-scored_data$shot.results.inRebound

## Brier Score - Base Line (Goal <- 0): 0.08919192
mean(rb.error^2)
# 0.03336117 - all shots
# 0.04005604 - shots only

# Mean Absolute Error
mean(abs(rb.error))
# 0.06954544 - all shots
# 0.08467813 - shots only

# RMSE
sqrt(mean(rb.error^2))
# 0.1826504 - all shots
# 0.2001401 - shots only

############################################################################################################################################################################
########2.B SAVE MODEL AND SCORED SHOTS
############################################################################################################################################################################
#save(scored_data, file="~/Documents/CWA/Hockey Data/xG.scored_data.RData")
save(scored_data, file="~/Documents/CWA/Hockey Data/xG.allattempts.scored_data.RData")
#save(best.model, file="~/Documents/CWA/Hockey Data/xG.best.model.RData")
