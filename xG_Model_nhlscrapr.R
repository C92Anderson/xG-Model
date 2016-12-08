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
  return(game)
}

# Check data and update based on current game (replace to most current gcode)
current.data <- function(last.game) {
###load current data
  load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17.RData")
  
  last.data.game <- max(as.character(game.events.17$gcode))
  
  game.list.17 <- as.character(last.data.game:last.game)

  new.game.events.17 <- plyr::rbind.fill(lapply(FUN=game.pull,game.list.17))
  
  game.events.17 <- plyr::rbind.fill(game.events.17, new.game.events.17)

  return(game.events.17) 
}

# Update with last game from current season
game.events.17 <- current.data("20390")
save(game.events.17, file="/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17.RData")

load(url("http://war-on-ice.com/data/nhlscrapr-core.RData"))
load("~/Documents/CWA/nhlscrapr-master/game.events.08.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.09.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.10.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.11.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.12.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.13.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.14.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.15.RData")
load("~/Documents/CWA/nhlscrapr-master/game.events.16.RData")

# Combine season data
pbp.all <- plyr::rbind.fill(game.events.08,game.events.09,game.events.10,game.events.11,
                            game.events.12,game.events.13,game.events.14,game.events.15,
                            game.events.16,game.events.17)

pbp.all <- slide(pbp.all, Var = "seconds", NewVar = "lag.seconds", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "etype", NewVar = "lag.event", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "homezone", NewVar = "lag.zone", slideBy = -1)

############################################################################################################################################################################
########1.B LIMIT TO SHOTS AGAINST AND DEVELOP FEATURES
############################################################################################################################################################################
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
                                       ifelse(SA.Goalie == "JEFF DROUIN-DESLAURIERS","JEFF DESLAURIERS",
                                                     SA.Goalie))))))),
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
                    xcoord = ifelse(period %in% c(2), -1 * xcoord, xcoord),
                    zone.shot = ifelse(ev.team == awayteam & homezone == "Def", "Off",
                                ifelse(ev.team == awayteam & homezone == "Off", "Def",
                                ifelse(ev.team == hometeam & homezone == "Off", "Off",
                                ifelse(ev.team == hometeam & homezone == "Def", "Def",
                                                  "Neu")))),
                    shot.dist = ifelse(ev.team == awayteam,
                                              sqrt((-89 - xcoord)**2 + (ycoord**2)),
                                              sqrt((89 - xcoord)**2 + (ycoord**2))),
                    dist.check = ifelse(abs(shot.dist - distance) > 5, 0, 1),
                    distance.clean = ifelse(shot.dist == 0, distince, shot.dist),
                    xcoord.new = ifelse(abs(shot.dist - distance) > 5, -1 * xcoord, xcoord),
                    ycoord.new = ifelse(abs(shot.dist - distance) > 5, -1 * ycoord, ycoord),
                    shot.radians = ifelse(ev.team == awayteam,
                                              atan((abs(-89 - xcoord.new)) / abs(0 - ycoord.new)),
                                              atan(abs(89 - xcoord.new) / abs(0 - ycoord.new))),
                    shot.angle = shot.radians * (180 / pi),
                    shot.type = as.factor(ifelse(type %in% c("Deflected","Tip-In"), "Deflected",
                                       ifelse(type %in% c("Wrist","Snap","Unspecified"),"Wrist",
                                              type))),
                    is.Rebound = as.factor(ifelse(lag.event == "SHOT" & ((seconds - lag.seconds) < 3),1,0)),
                    is.Rush = as.factor(ifelse(((seconds - lag.seconds) < 8) & lag.zone != homezone,1,0))) %>%
            filter(ENG == 0) %>%
            filter(distance.clean > 0)

# Check counts of factors
aggregate(goal ~ gamestate, data = shots.all, FUN = length)
aggregate(goal ~ shot.type, data = shots.all, FUN = length)
aggregate(distance ~ season, data = shots.all, FUN = mean)
aggregate(goal ~ season, data = shots.all, FUN = length)
aggregate(distance ~ SA.Goalie, data = shots.all, FUN = length)

# Check even distribution
sum(shots.all$even.second) / length(shots.all$even.second) #0.498641

############################################################################################################################################################################
########1.C CALCULATE SHOOTER SKILL (not including blocked/missed shots)
############################################################################################################################################################################
load(url("http://war-on-ice.com/data/nhlscrapr-core.RData")) # Not updated

# Find player-level shooting percentage
shooter.skill <- shots.all %>%
            select(ev.player.1, goal) %>%
            #left_join(roster.master, by = c("ev.player.1" = "numfirstlast")) %>%
            mutate(Player = toupper(trimws(substr(ev.player.1, 3, nchar(ev.player.1))))) %>%
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

shots.all <- shots.all %>%
            left_join(regressed.shooter.skill, by = c("Player" = "Player"))

save(shots.all, file="~/Documents/CWA/Hockey Data/xG.shots.all.RData")

############################################################################################################################################################################
########2.A LOGISTIC MODEL TO DEVELOP XG MODEL
############################################################################################################################################################################
lm.cv.10 <- function(input, model.vars, extra.vars) {
      
      library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2)
  
      input.cc <- input[names(input) %in% c(model.vars,extra.vars)]
      input.cc <- input.cc[complete.cases(input.cc),]
      model.data <- input.cc[names(input.cc) %in% c(model.vars)]
      
      # Set folds
      set.seed(1234)  
      folds <- crossv_kfold(model.data, k = 10)
      
      # Run model over folds
      model.folds <- folds %>% 
          mutate(model = map(train, ~ glm(goal ~ ., data = ., family = "binomial")))
      
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

predicted.goal.model <- lm.cv.10(shots.all,
                              c("goal","shot.angle","distance", # "is.Rush", improve this
                                "is.Rebound","shot.type","gamestate","regressed.shooting.skill.index"),
                              c("season","gcode","period","seconds","ev.team","awayteam","hometeam","away.G","home.G",
                                "even.second","SA.Goalie","time.index"))   

# Summary best plot
best.model.no <- predicted.goal.model[[2]] %>% filter(r.squared == max(r.squared)) %>% select(.id) %>% as.character()
best.model <- predicted.goal.model[[1]]$model[[paste0(as.numeric(best.model.no))]] 
best.model %>% summary()

# Classification Plot
predicted.goal.model[[3]]

# Confusion Matrix
predicted.goal.model[[4]]
# FALSE   TRUE
# 0 428293 193843
# 1  18042  40102

# R-squared
1 - (best.model$deviance / best.model$null.deviance)
# 0.09869393

# Check xG and goals ratio
sum(as.numeric(predicted.goal.model[[5]]$goal)-1)
sum(predicted.goal.model[[5]]$xG)
# 58144
# 58143.35

predicted.goal.model[[5]] %>% 
        group_by(season) %>% 
        summarise(xG=sum(xG), goals=sum(as.numeric(goal)-1), 
                  avg.shot=mean(distance), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is.Rebound)-1))

# Output Predicted
xG <- predict(best.model, predicted.goal.model[[5]], type='response')

scored.data <- cbind(xG,predicted.goal.model[[5]][2:ncol(predicted.goal.model[[5]])])

scored.data %>% 
  group_by(season) %>% 
  summarise(xG=sum(xG), goals=sum(as.numeric(goal)-1), 
            avg.shot=mean(distance), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is.Rebound)-1))

############################################################################################################################################################################
########2.B SAVE MODEL AND SCORED SHOTS
############################################################################################################################################################################
save(scored.data, file="~/Documents/CWA/Hockey Data/xG.scored.data.RData")
save(best.model, file="~/Documents/CWA/Hockey Data/xG.best.model.RData")

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
                    segment.color = 'grey50') +
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

goalie.plot(c("CAREY PRICE","BRADEN HOLTBY","MARTIN JONES", "SERGEI BOBROVSKY", "COREY CRAWFORD","PEKKA RINNE",
              "BRIAN ELLIOTT","TUUKKA RASK"),
            c("20152016","20162017"))[[1]]


goalie.plot(c("CAREY PRICE","CAM TALBOT", "SERGEI BOBROVSKY", "COREY CRAWFORD","DEVAN DUBNYK","CHAD JOHNSON","TUUKKA RASK",
              "MICHAL NEUVIRTH","BRIAN ELLIOTT","SEMYON VARLAMOV","CAM WARD","STEVE MASON","JOHN GIBSON","HENRIK LUNDQVIST"),
            c("20142015", "20152016","20162017"))[[1]]



goalie.plot(c("STEVE MASON"))[[1]]
goalie.plot(c("STEVE MASON"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("ANDREI VASILEVSKIY","MATTHEW MURRAY","CONNOR HELLEBUYCK"),c("20152016","20162017"))[[1]]

goalie.plot(c("TUUKKA RASK","BRADEN HOLTBY"),c("20152016","20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","JONATHAN BERNIER","JAMES REIMER","KARRI RAMO","JHONAS ENROTH"),c("20152016","20162017"))[[1]]

goalie.plot(c("HENRIK LUNDQVIST","STEVE MASON","CORY SCHNEIDER"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]
dat <- goalie.plot(c("HENRIK LUNDQVIST","STEVE MASON","CORY SCHNEIDER"),c("20122013","20132014","20142015","20152016","20162017"))[[2]]
