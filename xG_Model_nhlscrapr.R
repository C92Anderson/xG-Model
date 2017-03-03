############################################################################################################################################################################
#
# PROJECT:        xG Model Measuring Goaltender Performance
#
# PURPOSE:        Using Logistic Regression create an xG for each shot a goalie faces. 
#                 Compare xG Against to Actual Goals Against to measure performance
#
# CREATED BY:     Cole Anderson (cole92anderson@gmail.com)
#
# LAST UPDATED:   12/07/2016
#
# PROCESS:        0 - SYSTEM PREP
#                 1 - UPDATE GCODE AND LOAD NHL PBP DATA USING NHLSCRAPR
#                 2 - LOGISTIC MODEL TO DEVELOP XG MODEL
#                 3 - DEVELOP FUNCTIONS SELECT GOALIE-SEASON AND PLOT GOALIE XG SAVE SUCCESS
#
############################################################################################################################################################################

############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret)
getwd()
load("~/Documents/CWA/Hockey Data/roster.Rda")

############################################################################################################################################################################
########1.A UPDATE GCODE AND LOAD NHL PBP DATA USING NHLSCRAPR
############################################################################################################################################################################

# Function to pull games from nhlscrapr from current season
game.pull <- function(game) {
  game <- retrieve.game(season="20162017", gcode=game)[[1]]
  if(length(game) > 0) { 
  return(game)
  }
}

# Check data and update based on current game (replace to most current gcode)
current.data <- function(last.game) {
###load current data
  load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17.RData")
  
  last.data.game <- max(as.character(na.omit(game.events.17$gcode)))
  
  game.list.17 <- as.character(last.data.game:last.game)

  new.game.events.17 <- plyr::rbind.fill(lapply(FUN=game.pull,game.list.17))
  
  game.events.17 <- plyr::rbind.fill(game.events.17, new.game.events.17)

  game.events.17$refdate[is.na(game.events.17$refdate)]  <- 0

  return(game.events.17) 
}

# Update with last game from current season
game.events.17 <- current.data("20938")
save(game.events.17, file="/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17.RData")

#load(url("http://war-on-ice.com/data/nhlscrapr-core.RData"))
#load("~/Documents/CWA/nhlscrapr-master/game.events.08.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.09.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.10.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.11.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.12.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.13.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.14.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.15.RData")
#load("~/Documents/CWA/nhlscrapr-master/game.events.16.RData")

# Combine season data
pbp.all <- plyr::rbind.fill(game.events.08,game.events.09,game.events.10,game.events.11,
                            game.events.12,game.events.13,game.events.14,game.events.15,
                            game.events.16,game.events.17)

pbp.all <- pbp.all[order(pbp.all$season, pbp.all$gcode, pbp.all$period, pbp.all$seconds), ]
pbp.all <- slide(pbp.all, Var = "seconds", NewVar = "lag.seconds", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "etype", NewVar = "lag.event", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "homezone", NewVar = "lag.zone", slideBy = -1)

############################################################################################################################################################################
########1.B LIMIT TO SHOTS AGAINST AND DEVELOP FEATURES
############################################################################################################################################################################

# Create shots dataset
shots.all <- pbp.all %>%
             filter(etype %in% c("SHOT","GOAL") & period %in% c(1:4)) %>%
             mutate(Player = toupper(trimws(substr(ev.player.1, 3, nchar(ev.player.1)))),
                    SA.Goalie = ifelse(ev.team == hometeam, 
                                       trimws(substr(away.G, 3, nchar(away.G))),
                                       trimws(substr(home.G, 3, nchar(home.G)))),
                    SA.Goalie = ifelse(SA.Goalie == "ILJA BRYZGALOV","ILYA BRYZGALOV",
                                       ifelse(SA.Goalie == "ALEXANDER AULD","ALEX AULD",
                                       ifelse(SA.Goalie == "EMMANUEL LEGACE","MANNY LEGACE",
                                       ifelse(SA.Goalie == "EMMANUEL FERNANDEZ","MANNY FERNANDEZ",
                                       ifelse(SA.Goalie == "TIMOTHY JR. THOMAS","TIM THOMAS",
                                       ifelse(SA.Goalie == "SIMEON VARLAMOV","SEMYON VARLAMOV",
                                       ifelse(SA.Goalie == "JAMES HOWARD","JIMMY HOWARD",
                                       ifelse(SA.Goalie == "JEFF DROUIN-DESLAURIERS","JEFF DESLAURIERS",
                                                     SA.Goalie)))))))),
                    ENG = ifelse(ev.team == awayteam & nchar(home.G) == 0,1,
                          ifelse(ev.team == hometeam & nchar(away.G) == 0,1,
                                        0)),
                    goal = as.factor(ifelse(etype =="GOAL",1,0)),
                    time.index = paste0(as.character(gcode),seconds),
                    even.second = ifelse(as.numeric(seconds) %% 2 == 0,1,0),
                    awaystate = ifelse(nchar(a4) == 0, 3,
                                       ifelse(nchar(a5) == 0, 4,
                                           ifelse(nchar(a6) == 0, 5,
                                            6))),
                    homestate = ifelse(nchar(h4) == 0, 3,
                                       ifelse(nchar(h5) == 0, 4,
                                           ifelse(nchar(h6) == 0, 5,        
                                            6))),
                    gamestate = ifelse(ev.team == awayteam, 
                                            paste0(awaystate,"v",homestate),
                                            paste0(homestate,"v",awaystate)),
                    gamestate = ifelse(gamestate %in% c("3v5","3v4","3v6","4v5","4v6","5v6"),"SH.SA",
                                      ifelse(gamestate %in% c("6v3","6v4","5v3"),"PP.2p.SA",
                                      ifelse(gamestate %in% c("5v5","6v6"),"5v5",
                                              gamestate))),
                    home.shooter = ifelse(ev.team == awayteam, 0, 1),
                    LS.shot = ifelse(ycoord < 0, 1, 0),
                    zone.shot = ifelse(home.shooter == 0 & homezone == "Def", "Off",
                                       ifelse(home.shooter == 0 & homezone == "Off", "Def",
                                              ifelse(home.shooter == 1 & homezone == "Off", "Off",
                                                     ifelse(home.shooter == 1 & homezone == "Def", "Def",
                                                            "Neu")))),
                    shot.type = as.factor(ifelse(type %in% c("Deflected","Tip-In"), "Deflected",
                                       ifelse(type %in% c("Wrist","Snap","Unspecified"),"Wrist",
                                              type))),
                    is.Rebound = as.factor(ifelse(lag.event == "SHOT" & ((seconds - lag.seconds) <= 2),1,0)),
                    is.Rush = as.factor(ifelse(((seconds - lag.seconds) <= 4) & lag.zone != homezone,1,0))) %>%
            left_join(roster.master[c("numfirstlast","firstlast","pos","Shoots")], by = c("ev.player.1" = "numfirstlast")) %>%
            mutate(Player = firstlast) %>%
            filter(ENG == 0)

# Summarize most popular xcoord by game
xbypg <- aggregate(xcoord ~ home.shooter + period + gcode + hometeam + season, data=shots.all, FUN=mean)

# Summarize to period level
xbygame <- reshape(xbypg, idvar = c("gcode","season","hometeam","period"), timevar = "home.shooter", direction = "wide")

# Reshape xcoord east facing and merge to master data
shots.all1 <- xbygame %>%
          filter(period %in% c(1,2,3)) %>%
          mutate(home.align.east = ifelse(xcoord.1 > xcoord.0 & period %in% c(1,3), 1,
                                   ifelse(xcoord.0 > xcoord.1 & period %in% c(2,4), 1,
                                   ifelse(xcoord.0 > xcoord.1 & period %in% c(1,3), 0,
                                   ifelse(xcoord.0 < xcoord.1 & period %in% c(2,4), 0, NA))))) %>%
          filter(!is.na(home.align.east)) %>%
          group_by(season, gcode, hometeam) %>%
          summarise(home.align.east = mean(home.align.east)) %>%
          filter(home.align.east == 0 || home.align.east == 1) %>%
          merge(shots.all, by=c("season","gcode","hometeam")) %>%
          mutate(XC = ifelse(home.align.east == 1 & home.shooter == 1 & period %in% c(1,3), xcoord,
                      ifelse(home.align.east == 1 & home.shooter == 1 & period %in% c(2), -1 * xcoord,
                      ifelse(home.align.east == 0 & home.shooter == 1 & period %in% c(1,3), -1 * xcoord,
                      ifelse(home.align.east == 0 & home.shooter == 1 & period %in% c(2), xcoord,
                      ifelse(home.align.east == 1 & home.shooter == 0 & period %in% c(1,3), -1 * xcoord,
                      ifelse(home.align.east == 1 & home.shooter == 0 & period %in% c(2), xcoord,
                      ifelse(home.align.east == 0 & home.shooter == 0 & period %in% c(1,3), xcoord,
                      ifelse(home.align.east == 0 & home.shooter == 0 & period %in% c(2), -1 * xcoord,
                             abs(xcoord))))))))),
                YC = ycoord) %>%
          filter(!is.na(XC) & !is.na(YC) & !is.na(distance))

aggregate(xcoord ~ home.shooter + period, data=shots.all1, FUN=median)
aggregate(XC ~ home.shooter + period, data=shots.all1, FUN=median)

# Shot Distance and Shot Angle Function
coord.calculator <- function(x,y, dist, thresh = 5) {
  
   shot.dist = sqrt((89 - x)**2 + (y ** 2))
  
  # Shot distance pretty close
  if (abs(dist - shot.dist) < thresh) {
    # Assign shot angle based on proper coordinates
    shot.angle = atan(abs(89 - x) / abs(0 - y)) * (180 / pi)
    coord.check = "clean"
  } else {
    # Flip coordinates and test
    x.flip = y
    y.flip = x
    shot.dist = sqrt((89 - x.flip)**2 + (y.flip ** 2))
    
    # Test new shot distance    
    if (abs(dist - shot.dist) < thresh) {
      
      # Assign shot angle based on proper coordinates
      shot.angle = atan(abs(89 - x.flip) / abs(0 - y.flip)) * (180 / pi)
      y = y.flip
      x = x.flip
      coord.check = "x-y flip"
    } else {
      # Flip x-coord
      x.direction.change = -1 * x
      shot.dist = sqrt((89 - x.direction.change)**2 + (y.flip ** 2))
      
      # Test new shot distance
      if (abs(dist - shot.dist) < thresh) {
        shot.angle = atan(abs(89 - x.direction.change) / abs(0 - y.flip)) * (180 / pi)
        x = x.direction.change
        coord.check = "x flip"
      } else {
        shot.dist = dist
        shot.angle = atan(abs(89 - x) / abs(0 - y)) * (180 / pi)
        coord.check = "pbp dist"
      } 
    }
  }
  return(list(round(shot.dist,2), 
              round(shot.angle,2),
              coord.check
              #round(x,2), 
              #round(y,2), 
  ))
}


shot.mat <- mapply(coord.calculator,shots.all1$XC, shots.all1$YC, shots.all1$distance)
shot.df <- as.data.frame(t(shot.mat))
colnames(shot.df) <- c("shot.dist","shot.angle","coord.origin")
shot.df$shot.dist <- as.numeric(shot.df$shot.dist)
shot.df$shot.angle <- as.numeric(shot.df$shot.angle)
shot.df$coord.origin <- as.character(shot.df$coord.origin)
shots.all2 <- cbind(shots.all1, shot.df)


# Slide last shot coordinates
shots.all2 <- shots.all2[order(shots.all2$season, shots.all2$gcode, shots.all2$period, shots.all2$seconds), ]
shots.all2 <- slide(shots.all2, Var = "shot.dist", NewVar = "int.shot.dist", slideBy = -1)
shots.all2 <- slide(shots.all2, Var = "shot.angle", NewVar = "int.shot.angle", slideBy = -1)
shots.all2 <- slide(shots.all2, Var = "LS.shot", NewVar = "int.LS.shot", slideBy = -1)

shots.all2 <- shots.all2 %>%
                mutate(Rebound.Angle.Change = ifelse(is.Rebound == 0, 0, 
                                              ifelse(LS.shot == int.LS.shot, abs(shot.angle - int.shot.angle),
                                              ifelse(LS.shot != int.LS.shot, 180 - shot.angle - int.shot.angle, 0))),
                       Rebound.Second.Change = ifelse(is.Rebound == 0, 0, seconds - lag.seconds),
                       Rebound.Distance = ifelse(is.Rebound == 0, 0, shot.dist + int.shot.dist),
                       Rebound.Angle.Distance = ifelse(is.Rebound == 0, 0, Rebound.Angle.Change / Rebound.Distance),
                       Rebound.Angle.Second = ifelse(is.Rebound == 0, 0, Rebound.Angle.Change / Rebound.Second.Change))


# Check counts of factors
aggregate(goal ~ gamestate, data = shots.all2, FUN = length)
aggregate(goal ~ shot.type, data = shots.all2, FUN = length)
aggregate(distance ~ season, data = shots.all2, FUN = mean)
aggregate(goal ~ season, data = shots.all2, FUN = length)
aggregate(distance ~ SA.Goalie, data = shots.all2, FUN = length)


# Check even distribution
sum(shots.all2$even.second) / length(shots.all2$even.second) #0.498641

############################################################################################################################################################################
########1.C CALCULATE SHOOTER SKILL (not including blocked/missed shots)
############################################################################################################################################################################
load(url("http://war-on-ice.com/data/nhlscrapr-core.RData")) # Not updated

# Find player-level shooting percentage
shooter.skill <- shots.all2 %>%
            select(Player, goal) %>%
#            mutate(Player = toupper(trimws(substr(ev.player.1, 3, nchar(ev.player.1))))) %>%
            group_by(Player) %>%
            dplyr::summarise(shooting.percentage = sum(as.numeric(goal)-1) / length(goal),
                             goals = sum(as.numeric(goal)-1),
                             shots = length(goal))

#(2062/(2062-1)) * (1-(0.08870118*(2062-0.08870118)/(2062*0.005585664)))
#n =2062
#M = 0.08870118
#var = 0.005585664

# Find average shooting and regress
regressed.shooter.skill <- shooter.skill %>%
            dplyr::summarise(mean.shooting.percentage = sum(goals) / sum(shots)) %>%
            cbind(shooter.skill) %>%
            mutate(kr21.stabilizer = 375, #http://www.statisticshowto.com/kuder-richardson/ 
                                          #https://hockey-graphs.com/2015/10/01/expected-goals-are-a-better-predictor-of-future-scoring-than-corsi-goals/
            regressed.shooting.skill.index = ((goals + (kr21.stabilizer * mean.shooting.percentage)) / (shots + kr21.stabilizer)) / mean.shooting.percentage) %>%
            
          select(Player, regressed.shooting.skill.index)

shots.all.reg <- shots.all2 %>%
            left_join(regressed.shooter.skill, by = c("Player" = "Player"))

save(shots.all.reg, file="~/Documents/CWA/Hockey Data/shots.all.reg.RData")

############################################################################################################################################################################
########2.A LOGISTIC MODEL TO DEVELOP XG MODEL
############################################################################################################################################################################
lm.cv.10 <- function(input, model.vars, extra.vars) {
      
      library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2)
      set.seed(1234)  
      
      # Random Sort & Select Variables
      input.cc <- input[sample(nrow(input)), names(input) %in% c(model.vars,extra.vars)]
      
      # Remove Nan, Inf
      library(data.table)
      for (j in 1:ncol(input.cc)) set(input.cc, which(is.infinite(input.cc[[j]])), j, NA)
      for (j in 1:ncol(input.cc)) set(input.cc, which(is.nan(input.cc[[j]])), j, NA)
      
      input.cc <- input.cc[complete.cases(input.cc),]
      model.data <- input.cc[names(input.cc) %in% c(model.vars)]
      
      
      # Set folds
      folds <- crossv_kfold(model.data, k = 10)
      
      # Run model over folds
      model.folds <- folds %>% 
          mutate(model = map(train, ~ glm(goal ~ ., data = ., family = "binomial", na.action = na.exclude)))
      
      # Predict test data
      predicted <- model.folds %>% 
                  mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y, , type.predict = "response"))) %>% 
                  unnest(predicted)
      
      #Calculate residual
      predicted <- predicted %>% 
        mutate(goal = (as.numeric(goal)-1),
               residual = .fitted - goal)
      
      # Calculate r-squared
      rs <- predicted %>%
        group_by(.id) %>% 
        summarise(
          sst = sum((goal - mean(goal)) ^ 2), # Sum of Squares Total
          sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
          r.squared = 1 - sse / sst         # Proportion of variance accounted for
        )
      
      # Probability Cutoff
      cutoff.roc.scores <- function(cut, prob, y) {
        
        yhat <- ifelse(prob > cut,1,0)
        sensitivity <- length(which(yhat == 1 & y == 1)) / length(which(y == 1)) ##total true positives / total positives
        specificity <- length(which(yhat == 0 & y == 0)) / length(which(y == 0)) ##total true negatives / total negatives
        class.rate <- length(which(yhat == y)) / length(y)                      ## total true / total obs
        distance <- sqrt( (sensitivity - 1)^2 + (specificity - 1)^2 )
        out = t(as.matrix(c(cut,sensitivity, specificity, class.rate,distance)))
        colnames(out) = c("cutoff","sensitivity", "specificity", "c.rate", "distance")
        return(out)
      }
      
      s = seq(.01,.99,length=100)
      OUT = matrix(0,100,5)
      for(i in 1:100) {
        OUT[i,]=cutoff.roc.scores(s[i], predicted$.fitted, predicted$goal)
        colnames(OUT) = c("cutoff","sensitivity", "specificity", "c.rate", "distance")
      }
      
      class.rates <- as.data.frame(OUT)
      
      # Plot Classification Rate
      class.plot <- class.rates %>%
        as.data.frame() %>%
        ggplot() +
        geom_line(aes(x=cutoff, y=sensitivity,colour="Sensitivity")) +
        geom_line(aes(x=cutoff, y=specificity,colour="Specificity")) +
        geom_line(aes(x=cutoff, y=c.rate,colour="Classification Rate")) +
        geom_line(aes(x=cutoff, y=distance,colour="Distance")) +
        labs(x="Probability Cutoff", y="", title="Classification Rate", color="") +
        theme(panel.background = element_blank())
      
      # Find Prob Cutoff at minimum distance
      cutoff <- class.rates[class.rates$distance == min(class.rates$distance),"cutoff"]
      print(cutoff)
      
      # Create confusion matrix
      con.mat <- table(predicted$goal, predicted$.fitted > (median(cutoff)))
      
      # AUC
      predicted <- predicted[order(predicted$.fitted, decreasing=TRUE), c(".fitted","goal")] 
      predicted$shot <- 1
      
      AUC.curve <- data.frame(TPR=cumsum(predicted$goal)/sum(predicted$goal), 
                              FPR=cumsum(!predicted$goal)/sum(!predicted$goal),
                              Rand = cumsum(predicted$shot)/sum(predicted$shot),
                              Target=predicted$goal) %>%
                    ggplot() +
                    geom_line(aes(x=FPR, y=TPR, color="TPR")) +
                    geom_line(aes(x=FPR, y=Rand, color="Random")) +
                    labs(x="FPR", y="TPR", title="AUC", color="") +
                    theme(panel.background = element_blank())
                  
      # Gains Chart
      gains.chart <- data.frame(Gain=cumsum(predicted$goal)/sum(predicted$goal), 
                                Random = cumsum(predicted$shot)/sum(predicted$shot)) %>%
                  ggplot() +
                  geom_line(aes(x=Random, y=Gain,colour="Model")) +
                  geom_line(aes(x=Random, y=Random,colour="Random")) +
                  labs(y="Cumulative Share of Goals", x="Cumulative Rank-Ordered Shots", title="Gains Chart", color="") +
                  theme(panel.background = element_blank())
                
      colnames(predicted) <- c("xG","goal","shot")
      
      return(list(model.folds, rs, class.plot, con.mat, cbind(predicted[1],input.cc), AUC.curve, gains.chart))
}      


predicted.goal.model <- lm.cv.10(shots.all.reg,
                              
                                 c("goal",
                                   
                                "shot.angle","shot.dist","is.Rush", 
                                "Rebound.Angle.Second",
                                "is.Rebound","shot.type","gamestate","regressed.shooting.skill.index"
                                ),  
                                
                                #"Rebound.Angle.Distance", 
                                
                                
                              c("season","gcode","period","seconds","ev.team","awayteam","hometeam","away.G","home.G","Player",
                                "even.second","SA.Goalie","time.index","refdate"))   

#prob.goal = shot.angle + distance + is.Rebound + shot.type + gamestate + regressed.shooting.skill.index

# Summary best plot
best.model.no <- predicted.goal.model[[2]] %>% filter(r.squared == max(r.squared)) %>% select(.id) %>% as.character()
best.model <- predicted.goal.model[[1]]$model[[paste0(as.numeric(best.model.no))]] 
best.model %>% summary()

# Confusion Matrix
predicted.goal.model[[4]]

predicted.goal.model[[6]]

# FALSE   TRUE
# 0 434202 196205
# 1  18295  40674

# Classification Plot
predicted.goal.model[[3]]

# R-squared
1 - (best.model$deviance / best.model$null.deviance)
#  0.1002869

# Check xG and goals ratio
sum(as.numeric(predicted.goal.model[[5]]$goal)-1)
sum(predicted.goal.model[[5]]$xG)
# 59672
# 59672.47

predicted.goal.model[[5]] %>% 
        group_by(season) %>% 
        summarise(xG=sum(xG), goals=sum(as.numeric(goal)-1), 
                  avg.shot=mean(shot.dist), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is.Rebound)-1),
                  regressed.shooting.skill.index = mean(regressed.shooting.skill.index)) %>%
  ggplot() +
  geom_line(aes(x=season, y=xG,group=1)) +
  geom_line(aes(x=season, y=goals,group=1))
  

# Output Predicted
xG <- predict(best.model, predicted.goal.model[[5]], type='response')

scored.data <- cbind(xG,predicted.goal.model[[5]][2:ncol(predicted.goal.model[[5]])])

scored.data <- scored.data %>%
      arrange(season, time.index)

# View by season
scored.data %>% 
  group_by(season) %>% 
  summarise(xG=sum(xG), goals=sum(as.numeric(goal)-1), xG.shot = xG / n(),
            avg.dist=mean(shot.dist), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is.Rebound)-1))

# xG Distribution
scored.data %>%
  ggplot() +
  #geom_histogram(aes(xG), binwidth =  0.01)
  geom_density(aes(xG, color = season))

############################################################################################################################################################################
########2.B SAVE MODEL AND SCORED SHOTS
############################################################################################################################################################################
save(scored.data, file="~/Documents/CWA/Hockey Data/xG.scored.data.RData")
#save(best.model, file="~/Documents/CWA/Hockey Data/xG.best.model.RData")

############################################################################################################################################################################
########3.A DEVELOP FUNCTIONS SELECT GOALIE-SEASON AND PLOT GOALIE XG SAVE SUCCESS
############################################################################################################################################################################

# Load Scored Data
load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

# Return dataset of goalie-game level data for selected goalies and seasons
QREAM.fun <- function(goalie, seasons=c("20072008","20082009","20092010","20102011","20112012",
                                        "20122013","20132014","20142015","20152016","20162017")) {
  
  library(dplyr)
  
  # Select Goalies, Order by season, game, shot
  goalie.set <- scored.data %>%
        filter(SA.Goalie %in% goalie & season %in% seasons) 
  
  # Count cumulative numbers by goalie
  goalie.set <- goalie.set %>%
        mutate(Game.ID = as.character(gcode),
               SA = 1,
               GA = as.numeric(goalie.set$goal)-1) %>%
        select(SA.Goalie, season, Game.ID, seconds, goal, xG, SA, GA) %>%
        group_by(SA.Goalie) %>%
        arrange(season, Game.ID, seconds) %>%
        #Cumulative Counts
        mutate(cum.xG = cumsum(xG),
               cum.Goals = cumsum(GA),
               cum.Shots = cumsum(SA),
               QREAM = cum.xG - cum.Goals)
  
  # Calculate game shots
  game.shots <- goalie.set %>%
    group_by(SA.Goalie, season, Game.ID) %>%
    summarise(game.SA=sum(SA), 
              game.GA=sum(GA),
              game.xGA=sum(xG))
  
  # Keep last shot of each game and combine
  goalie.game <- goalie.set %>%
        group_by(SA.Goalie, season, Game.ID) %>%
        do(tail(., n=1)) %>%
    left_join(game.shots, by=c("SA.Goalie","season","Game.ID")) %>%
    arrange(SA.Goalie, season, Game.ID) %>%
    select(SA.Goalie, season, QREAM, Game.ID, game.SA, game.xGA, game.GA, cum.Shots, cum.Goals , cum.xG)
  
  # Return dataset of goalie-game level data for selected goalies and seasons
  return(goalie.game)
  
}

# Plot all goalies xG lift
xG.plot.fun <- function(goalies, seasons, data) {
  
  library(ggplot2); library(dplyr); library(ggrepel)
  
  # Subset goalies to highlight  
  select.goalies <- data %>%
      filter(SA.Goalie %in% goalies & season %in% seasons) %>%
      arrange(SA.Goalie, season, Game.ID)
  
  # Find last game to create label
  last.game <- select.goalies %>%
      group_by(SA.Goalie) %>%
      do(tail(., n=1))
  
  # In-take all goalie-game level data and select season
  data <- data %>%
    filter(season %in% seasons)

  # Overlay select goalies on all goalies limited to season  
  ggplot(data=data, aes(x=cum.Shots,y=QREAM, group=SA.Goalie)) + 
    geom_line(colour="grey") +
    geom_line(data=select.goalies,size=1.5,aes(x=cum.Shots,y=QREAM,color=as.factor(select.goalies$season))) +
    geom_line(data=select.goalies,size=0.3,aes(x=cum.Shots,y=QREAM)) +
    labs(color="Season") +
    #theme(text = element_text(size=20)) +
    annotate("segment",x=0,y=0,xend=max(data$cum.Shots),yend=0) +
    labs(title="Expected Goals Against - Actual Goals") +
    labs(x="Cumulative Shots Against", y="Expected Goals Against - Actual Goals") +
    geom_text_repel(data=last.game,aes(x=cum.Shots,y=QREAM,label = SA.Goalie),
                    point.padding = unit(0.5, "lines"),
                    segment.color = 'black') +
    annotate("text", x = 1, y = (max(data$QREAM) * 0.8), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
    theme(panel.background = element_blank())
}


# Call function with goalie list and season, call cumulative counts and plot function
goalie.plot <- function(goalies, 
                        seasons=c("20072008","20082009","20092010","20102011","20112012",
                                  "20122013","20132014","20142015","20152016","20162017")) {

      # List of goalies in season
      all.goalie.list <- scored.data %>% 
              filter(season %in% seasons & nchar(SA.Goalie) > 0) %>% 
              distinct(SA.Goalie) %>% as.list()
       
      # Loop through each goalie and append
      all.goalie.game <- plyr::rbind.fill(lapply(FUN=QREAM.fun,all.goalie.list, seasons))
      
      # Call function to plot games by season
      p <- xG.plot.fun(goalies, seasons, all.goalie.game)
      
      return(list(p,all.goalie.game))
      
}

############################################################################################################################################################################
########3.B CALL FUNCTION FOR SELECT GOALIE-SEASONS AND PLOT
############################################################################################################################################################################

#goalie.plot(c("CAREY PRICE","BRADEN HOLTBY","MARTIN JONES", "SERGEI BOBROVSKY", "COREY CRAWFORD","PEKKA RINNE",
#              "BRIAN ELLIOTT","TUUKKA RASK"),
#            c("20152016","20162017"))[[1]]

goalie.plot(c("CAREY PRICE","CAM TALBOT","BRADEN HOLTBY","SERGEI BOBROVSKY", "COREY CRAWFORD","DEVAN DUBNYK","TUUKKA RASK","MARTIN JONES",
              "MICHAL NEUVIRTH","BRIAN ELLIOTT","SEMYON VARLAMOV","CAM WARD","STEVE MASON","JOHN GIBSON","HENRIK LUNDQVIST","PEKKA RINNE",
              "PETR MRAZEK","FREDERIK ANDERSEN"),
            c("20162017"))[[1]]

#goalie.plot(c("STEVE MASON"))[[1]]
goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"))[[1]]

goalie.plot(c("MARC-ANDRE FLEURY","MATTHEW MURRAY"),c("20152016","20162017"))[[1]]

#goalie.plot(c("TUUKKA RASK","BRADEN HOLTBY"),c("20152016","20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","JONATHAN BERNIER","JAMES REIMER","KARRI RAMO","JHONAS ENROTH"),c("20152016","20162017"))[[1]]

#goalie.plot(c("HENRIK LUNDQVIST","STEVE MASON","CORY SCHNEIDER"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]

#goalie.plot(c("DEVAN DUBNYK","CAREY PRICE","COREY CRAWFORD","BRADEN HOLTBY","SERGEI BOBROVSKY","JAROSLAV HALAK"),c("20152016","20162017"))[[1]]

#goalie.plot(c("CAREY PRICE","COREY CRAWFORD","CORY SCHNEIDER","JAROSLAV HALAK","CAM WARD","JONATHAN QUICK","MARC-ANDRE FLEURY",
#              "PEKKA RINNE","TUUKKA RASK","HENRIK LUNDQVIST","ROBERTO LUONGO","RYAN MILLER","STEVE MASON"))[[1]]

#goalie.plot(c("CAREY PRICE","COREY CRAWFORD","CORY SCHNEIDER","JAROSLAV HALAK","CAM WARD","JONATHAN QUICK","MARC-ANDRE FLEURY",
#              "PEKKA RINNE","TUUKKA RASK","HENRIK LUNDQVIST","ROBERTO LUONGO","RYAN MILLER","STEVE MASON"),
#            c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("ANDREW HAMMOND","MIKE CONDON","CRAIG ANDERSON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("JAKE ALLEN"),c("20142015","20152016","20162017"))[[1]]
goalie.plot(c("JAKE ALLEN","BRIAN ELLIOTT"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONAS GUSTAVSSON","CAM TALBOT","LAURENT BROSSOIT","ANDERS NILSSON"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("MIIKKA KIPRUSOFF"))[[1]]

goalie.plot(c("HENRIK LUNDQVIST","ANTTI RAANTA"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("TUUKKA RASK","JONAS GUSTAVSSON","ANTON KHUDOBIN","MALCOLM SUBBAN","CHAD JOHNSON","ZANE MCINTYRE",
#              "NIKLAS SVEDBERG"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("JOHN GIBSON"),c("20162017"))[[1]]
goalie.plot(c("CAM TALBOT"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ANDREW HAMMOND"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONATHAN QUICK","PETER BUDAJ"),c("20152016","20162017"))[[1]]

#goalie.plot(c("ANTTI NIEMI","KARI LEHTONEN"),c("20152016","20162017"))[[1]]

#goalie.plot(c("CORY SCHNEIDER","KEITH KINKAID"),c("20152016","20162017"))[[1]]

#goalie.plot(c("JAKE ALLEN","CARTER HUTTON"))[[1]]

#goalie.plot(c("PETR MRAZEK","JIMMY HOWARD"),c("20112012","20122013","20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("PEKKA RINNE","JUUSE SAROS"),c("20152016","20162017"))[[1]]

goalie.plot(c("ANTON KHUDOBIN","CARTER HUTTON"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("CAM TALBOT","SERGEI BOBROVSKY"),c("20162017"))[[1]]
goalie.plot(c("CAM TALBOT"),c("20132014","20142015","20152016","20162017"))[[1]]


goalie.plot(c("SERGEI BOBROVSKY","BRADEN HOLTBY","SCOTT DARLING","COREY CRAWFORD"),c("20162017"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ZANE MCINTYRE"),c("20162017"))[[1]]

goalie.plot(c("PEKKA RINNE","JUUSE SAROS","SCOTT DARLING","COREY CRAWFORD","JAKE ALLEN","CARTER HUTTON",
              "DEVAN DUBNYK","DARCY KUEMPER","ANTTI NIEMI","KARI LEHTONEN"),c("20162017"))[[1]]

goalie.plot(c("ONDREJ PAVELEC","CONNOR HELLEBUYCK","MICHAEL HUTCHINSON"),c("20162017"))[[1]]

goalie.plot(c("CRAIG ANDERSON","MIKE CONDON","ANDREW HAMMOND"),c("20152016","20162017"))[[1]]

goalie.plot(c("ANDREW HAMMOND","MIKE CONDON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("RYAN MILLER","JACOB MARKSTROM"),c("20152016","20162017"))[[1]]
goalie.plot(c("RYAN MILLER","JACOB MARKSTROM"))[[1]]

goalie.plot(c("ROBIN LEHNER","ANDERS NILSSON"),c("20162017"))[[1]]
goalie.plot(c("ROBIN LEHNER","ANDERS NILSSON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("CAREY PRICE","HENRIK LUNDQVIST"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("CHAD JOHNSON","BRIAN ELLIOTT"),c("20162017"))[[1]]

goalie.plot(c("JONATHAN QUICK","BEN BISHOP","CHAD JOHNSON","BRIAN ELLIOTT","PETER BUDAJ"),c("20152016","20162017"))[[1]]

goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("EDDIE LACK", "CAM WARD"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("EDDIE LACK","CAM WARD"),c("20162017"))[[1]]

############################################################################################################################################################################
########4.A TEAM GOALTENDING QUALITY BY SEASON
############################################################################################################################################################################

QREAM.byteam <- function(seasons) {
  
goalie.set <- scored.data %>%
  filter(season %in% seasons) 

# Count cumulative numbers by goalie
team.goalie.season <- goalie.set %>%
  mutate(Game.ID = as.character(gcode),
         Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
         SA = 1,
         GA = as.numeric(goalie.set$goal)-1) %>%
  select(SA.Team, SA.Goalie, season, Goalie, seconds, goal, xG, SA, GA) %>%
  group_by(SA.Team, SA.Goalie, Goalie) %>%
  #Total xG and Goals
  summarise(total.xG = sum(xG),
         total.Goals = sum(GA),
         total.shots = sum(SA),
         QREAM = total.xG - total.Goals)
  
team.season <- team.goalie.season %>%
    group_by(SA.Team) %>%
    summarise(team.QREAM = sum(QREAM)) %>%
    left_join(team.goalie.season, by = "SA.Team") %>%
    arrange(QREAM) 

team.season %>%
    ggplot(aes(x=reorder(SA.Team, - team.QREAM), y=QREAM, fill=total.shots, label=Goalie)) +
    geom_bar(stat="identity", colour="white") +
    geom_point(aes(x=reorder(SA.Team, - team.QREAM), y=team.QREAM)) +
    geom_text(size = 2, position = position_stack(vjust = 0.5)) +
    scale_fill_gradient2(low="light grey",mid="grey",high="red") +
    theme(panel.background = element_blank()) +
    labs(title=paste0("Goaltending Performance by Team, ", seasons), #, " - ", Sys.Date(), " YTD"),
         x="Team", y="Expected Goals Against - Actual Goals", fill="Total Shots") +
   annotate("text", x = 1, y = (min(team.season$team.QREAM) * 0.5), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model")
  
}

QREAM.byteam("20162017")


############################################################################################################################################################################
########5.A PLAYER SHOOTING SKILL
############################################################################################################################################################################

# Return dataset of goalie-game level data for selected goalies and seasons
GSAA.fun <- function(player, seasons=c("20072008","20082009","20092010","20102011","20112012",
                                        "20122013","20132014","20142015","20152016","20162017")) {
  
  library(dplyr)
  
player.set <- scored.data %>%
  filter(season %in% seasons & Player %in% player) 

# Count cumulative numbers by player
player.set <- player.set %>%
  mutate(Game.ID = as.character(gcode),
         SF = 1,
         GF = as.numeric(player.set$goal)-1) %>%
  select(Player, season, Game.ID, seconds, goal, xG, SF, GF) %>%
  group_by(Player) %>%
  arrange(season, Game.ID, seconds) %>%
  #Cumulative Counts
  mutate(cum.xG = cumsum(xG),
         cum.Goals = cumsum(GF),
         cum.Shots = cumsum(SF),
         GSAA = cum.Goals - cum.xG)

# Calculate game shots
player.shots <- player.set %>%
  group_by(Player, season, Game.ID) %>%
  summarise(game.SF=sum(SF), 
            game.GF=sum(GF),
            game.xGF=sum(xG))

# Keep last shot of each game and combine
player.game <- player.set %>%
  group_by(Player, season, Game.ID) %>%
  do(tail(., n=1)) %>%
  left_join(player.shots, by=c("Player","season","Game.ID")) %>%
  arrange(Player, season, Game.ID) %>%
  select(Player, season, GSAA, Game.ID, game.SF, game.xGF, game.GF, cum.Shots, cum.Goals , cum.xG)

# Return dataset of goalie-game level data for selected goalies and seasons
return(player.game)

}

# Plot all goalies xG lift
shooter.plot.fun <- function(players, seasons, data) {
  
  library(ggplot2); library(dplyr); library(ggrepel)
  
  # Subset goalies to highlight  
  select.players <- data %>%
    filter(Player %in% players & season %in% seasons) %>%
    arrange(Player, season, Game.ID)
  
  # Find last game to create label
  last.game <- select.players %>%
    group_by(Player) %>%
    do(tail(., n=1))
  
  # In-take all goalie-game level data and select season
  data <- data %>%
    filter(season %in% seasons)
  
  # Overlay select goalies on all goalies limited to season  
  ggplot(data=data, aes(x=cum.Shots,y=GSAA, group=Player)) + 
    geom_line(colour="grey") +
    geom_line(data=select.players,size=1.5,aes(x=cum.Shots,y=GSAA,color=as.factor(select.players$season))) +
    geom_line(data=select.players,size=0.3,aes(x=cum.Shots,y=GSAA)) +
    labs(color="Season") +
    #theme(text = element_text(size=20)) +
    annotate("segment",x=0,y=0,xend=max(data$cum.Shots),yend=0) +
    labs(title="Actual Goals - Expected Goals For") +
    labs(x="Cumulative Individual Shots For", y="Actual Goals - Expected Goals For") +
    geom_text_repel(data=last.game,aes(x=cum.Shots,y=GSAA,label = Player),
                    point.padding = unit(0.5, "lines"),
                    segment.color = 'black') +
    annotate("text", x = 1, y = (max(data$GSAA) * 0.8), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
    theme(panel.background = element_blank())
}

# Call function with goalie list and season, call cumulative counts and plot function
shooter.plot <- function(players, 
                        seasons=c("20072008","20082009","20092010","20102011","20112012",
                                  "20122013","20132014","20142015","20152016","20162017")) {
  
  # List of goalies in season
  all.player.list <- scored.data %>% 
    filter(season %in% seasons & nchar(Player) > 0) %>% 
    distinct(Player) %>% as.list()
  
  # Loop through each goalie and append
  all.player.game <- plyr::rbind.fill(lapply(FUN=GSAA.fun,all.player.list, seasons))
  
  # Call function to plot games by season
  p <- shooter.plot.fun(players, seasons, all.player.game)
  
  return(list(p,all.player.game))
  
}

shooter.plot(c("STEVEN STAMKOS","ALEX OVECHKIN","JEFF CARTER","RICK NASH","PHIL KESSEL","HENRIK ZETTERBERG","ZACH PARISE",
                    "JOE PAVELSKI","JAROME IGINLA","EVGENI MALKIN","PATRICK KANE","SIDNEY CROSBY","BRENT BURNS","SHEA WEBER","DANIEL SEDIN"))[[1]]

shooter.plot(c("CONNOR MCDAVID","AUSTIN MATTHEWS","PATRICK LAINE","JACK EICHEL"),c("20152016","20162017"))[[1]]

############################################################################################################################################################################
########5.B SHOOTER SKILL GAP PER SHOT BY SEASON
############################################################################################################################################################################


xG_v_GF_pershot <- function(player) {
  
scored.data %>%
      filter(Player == player) %>%
      group_by(Player, season) %>%
      mutate(SF = 1,
             GF = as.numeric(goal)-1) %>%
      summarise(SF = sum(SF),
                GF = sum(GF),
                xG = sum(xG)) %>%
      mutate(`xG per Shot` = xG / SF,
             `GF per Shot` = GF / SF) %>%
      select(Player, season, `xG per Shot`, `GF per Shot`) %>%
      reshape2::melt() %>%
      ggplot() +
      geom_line(aes(x=season, y=value, group=variable,color=variable)) +
    labs(color="") +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    labs(title=paste0(player, " xG and GF per Shot by Season")) +
    labs(x="Season", y="Individual xG and GF per Shot Taken") +
    annotate("text", x = 1, y = .025, hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
    theme(panel.background = element_blank())

}

xG_v_GF_pershot("DUSTIN BYFUGLIEN")