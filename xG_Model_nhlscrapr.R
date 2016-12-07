library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret)
load("~/Documents/CWA/Hockey Data/roster.Rda")

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
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.08.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.09.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.10.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.11.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.12.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.13.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.14.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.15.RData")
load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.16.RData")


pbp.all <- plyr::rbind.fill(game.events.08,game.events.09,game.events.10,game.events.11,
                            game.events.12,game.events.13,game.events.14,game.events.15,
                            game.events.16,game.events.17)

pbp.all <- slide(pbp.all, Var = "seconds", NewVar = "lag.seconds", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "etype", NewVar = "lag.event", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "homezone", NewVar = "lag.zone", slideBy = -1)

# Limit to shots and prep
shots.all <- pbp.all %>%
             filter(etype %in% c("SHOT","GOAL") & period %in% c(1:4)) %>%
             mutate(Player = toupper(trimws(substr(ev.player.1, 3, nchar(ev.player.1)))),
                    ENG = ifelse(ev.team == awayteam & nchar(home.G) == 0,1,
                          ifelse(ev.team == hometeam & nchar(away.G) == 0,1,
                                        0)),
                    goal = as.factor(ifelse(etype =="GOAL",1,0)),
                    time.index = paste0(as.character(gcode),seconds),
                    even.second = ifelse(as.numeric(seconds) %% 2 == 0,1,0),
                    SA.Goalie = ifelse(ev.team == hometeam, 
                                                    trimws(substr(away.G, 3, nchar(away.G))),
                                                    trimws(substr(home.G, 3, nchar(home.G)))),
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
aggregate(distance ~ SA.Goalie, data = shots.all, FUN = length)

# Check even distribution
sum(shots.all$even.second) / length(shots.all$even.second) #0.498641

# Calculate Shooter Skill (not including blocked/missed shots)
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

#################################################################################
##### Modeling Function
#################################################################################
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

save(scored.data, file="~/Documents/CWA/Hockey Data/xG.scored.data.RData")
save(best.model, file="~/Documents/CWA/Hockey Data/xG.best.model.RData")

#################################################################################
##### Score and Visualize
#################################################################################

# Goalie viz function
QREAM.fun <- function(goalie) {
  
  library(dplyr)

  # Select Goalie, Order by season, game, shot
  goalie.set <- scored.data %>%
        filter(SA.Goalie %in% c(goalie)) 
  

  goalie.set <- goalie.set %>%
        mutate(Game.ID = as.character(gcode)) %>%
        select(SA.Goalie, season, Game.ID, seconds, goal, xG) %>%
        arrange(SA.Goalie, season, Game.ID, seconds) %>%
        mutate(SA = 1,
               GA = as.numeric(goalie.set$goal)-1,
               #Cumulative Counts
               cum.xG = cumsum(xG),
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
  
  print(c(goalie, dim(goalie.game)))
  return(goalie.game)
  
}

# Loop through each goalie and append
goalie.list <- as.list(unique(scored.data$SA.Goalie))
goalie.list <- goalie.list[!is.na(goalie.list)]
all.goalie.game <- plyr::rbind.fill(lapply(FUN=QREAM.fun,goalie.list))


# Plot all goalies xG lift
plot.goalie <- function(goalie) {
  
  library(ggplot2); library(dplyr); library(ggrepel)
  
  select.goalies <- all.goalie.game %>%
      filter(SA.Goalie %in% goalie) %>%
      arrange(SA.Goalie, season, Game.ID)
  
  last.game <- select.goalies %>%
      group_by(SA.Goalie) %>%
      do(tail(., n=1))
   
  ggplot(data=all.goalie.game, aes(x=cum.Shots,y=QREAM, group=SA.Goalie)) + 
    geom_line(colour="grey") +
    geom_line(data=select.goalies,size=1.5,aes(x=cum.Shots,y=QREAM,color=as.factor(select.goalies$season))) +
    labs(color="Goalie") +
    #theme(text = element_text(size=20)) +
    annotate("segment",x=0,y=0,xend=max(all.goalie.game$cum.Shots),yend=0) +
    labs(title="Expected - Actual Goals, Goalies 2008-2016") +
    labs(x="Cumulative Shots Against", y="Expected Goals Against - Actual Goals") +
    geom_text_repel(aes(x=last.game$cum.Shots,y=last.game$QREAM,label = last.game$SA.Goalie)) +
    
  #  geom_text(data=last.game, aes(x=last.game$cum.Shots,y=last.game$QREAM,hjust=0,label = last.game$SA.Goalie)) +
    annotate("text", x = 1, y = (max(all.goalie.game$QREAM) * 0.8), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\nCode: github.com/C92Anderson/xG-Model") +
    theme(panel.background = element_blank())
}

plot.goalie(c("CAREY PRICE","HENRIK LUNDQVIST"))


plot.goalie(c("MATTHEW MURRAY","CONNOR HELLEBUYCK","JOHN GIBSON"))


####################
###LINE OF BEST FIT FOR GOALIE SEASON
###################

goalie.season.scores <- function(goalie) {

  output <- NULL
  ###loop through each possible season
  for(season in c("20152016","20142015","20132014","20122013","20112012","20102011","20092010","20082009")) {
    
    goalie.season <- subset(goalie.game,SA.Goalie==goalie & Season==season)
    
    ##only calculate season if over 10 games
    if(nrow(goalie.season) > 10 ) {
      
      lm.goalie.season <- lm(goalie.season$QREAM ~ goalie.season$cum.Shots)
      Season.Lift <- as.numeric(lm.goalie.season$coefficients[2])
      Season.Consistency <- summary(lm.goalie.season)$r.squared
      out <- data.frame(cbind(Season.Lift,Season.Consistency))
      
      out$Season.ShotsAgainst <- max(goalie.season$cum.Shots) - min(goalie.season$cum.Shots - goalie.season$game.SA)
      out$Season.Goals <- max(goalie.season$cum.Goals) - min(goalie.season$cum.Goals - goalie.season$game.GA)
      out$Season.xGA <- max(goalie.season$cum.xG) - min(goalie.season$cum.xG - goalie.season$game.xGA)
    
      ####Career to Date
      out$CTD.Shots <- min(goalie.season$cum.Shots - goalie.season$game.SA)
      out$CTD.Lift <- head(goalie.season$QREAM - (goalie.season$game.xGA - goalie.season$game.GA),1)
        
      goalie.CTD <- subset(goalie.game,SA.Goalie==goalie & as.numeric(Season) < as.numeric(season))
        
      if(nrow(goalie.CTD) > 10) {
        
        lm.goalie.CTD <- lm(goalie.CTD$QREAM ~ goalie.CTD$cum.Shots)
        out$CTD.Lift <- as.numeric(lm.goalie.CTD$coefficients[2])
        out$CTD.Consistency <- summary(lm.goalie.CTD)$r.squared
        
        ####Last Season
        goalie.last.season <- subset(goalie.game,SA.Goalie==goalie & as.numeric(Season) == (as.numeric(season) - 10001))
    
        ###only post if last season had over 10 games
        if(nrow(goalie.last.season) > 10) {
        
            lm.goalie.last <- lm(goalie.last.season$QREAM ~ goalie.last.season$cum.Shots)
            out$Last.Season.Lift <- as.numeric(lm.goalie.last$coefficients[2])
            out$Last.Season.Consistency <- summary(lm.goalie.last)$r.squared
            out$Last.Season.Shots <- max(goalie.last.season$cum.Shots) - min(goalie.last.season$cum.Shots - goalie.last.season$game.SA)
            out$Last.Season.Goals <- max(goalie.last.season$cum.Goals) - min(goalie.last.season$cum.Goals - goalie.last.season$game.GA)
            
          ###birthday
          dob <-as.Date(unique(roster[which(roster$Full.Name == goalie), "bd"]),format = "%B %d, %Y")
          
          out$SA.Goalie <- goalie
          out$Season <- season
          out$Age.Season.Start <- ifelse(season == "20152016", (as.Date("2015-10-05") - dob) / 365.25,
                                  ifelse(season == "20142015", (as.Date("2014-10-05") - dob) / 365.25,
                                  ifelse(season == "20132014", (as.Date("2013-10-05") - dob) / 365.25,
                                  ifelse(season == "20122013", (as.Date("2012-10-05") - dob) / 365.25,
                                  ifelse(season == "20112012", (as.Date("2011-10-05") - dob) / 365.25,
                                  ifelse(season == "20102011", (as.Date("2010-10-05") - dob) / 365.25,
                                  ifelse(season == "20092010", (as.Date("2009-10-05") - dob) / 365.25,
                                  ifelse(season == "20082009", (as.Date("2008-10-05") - dob) / 365.25,
                                  ifelse(season == "20072008", (as.Date("2007-10-05") - dob) / 365.25,
                                         (as.Date("2006-10-05") - dob) / 365.25)))))))))
          
          complete.out <- data.frame(out)
          
          output <- plyr::rbind.fill(output,complete.out)
          
       } ##last season entered
      } ##career entered
    } ##current season entered
  } ##season stacked
  print(output)
}

scored.goalie.seasons <- do.call(rbind,lapply(FUN=goalie.season.scores,
                                              unique(goalie.game$SA.Goalie)))
scored.goalie.seasons$lastname = sapply(strsplit(as.character(scored.goalie.seasons$SA.Goalie), '[.]'), function(x) x[length(x)])
scored.goalie.seasons$labs <- paste0(scored.goalie.seasons$lastname, substr(scored.goalie.seasons$Season,7,8))

scored.goalie.seasons2 <- subset(scored.goalie.seasons, Season.ShotsAgainst > 1700)

###plot consistency
ggplot(data=scored.goalie.seasons2, aes(x=Season.Consistency,y=Season.Lift * 100,color=Season.ShotsAgainst)) + 
  geom_point() +
  labs(title="Goaltender Season xG Lift vs Consistency\nMinimum 1700 Shots") +
  labs(y="Goals Prevented / 100 Shots (xG - Actual Goals)", x="Consistency (Variation around Line of Best Season Fit, 0 - 1)") +
  labs(color="Season Shots Against") +
  scale_colour_gradient2(low="grey",mid="grey", high="blue",limits=c(1500, 2700),
                         midpoint = median(scored.goalie.seasons2$Season.ShotsAgainst)) +
  geom_text_repel(aes(label = labs)) +
  #geom_text(data=scored.goalie.seasons, aes(label = labs, angle=(Season.Consistency * 350 * Season.Lift),check_overlap = TRUE)) +
  annotate("segment",x=0,y=0,xend=1,yend=0) +
  annotate("segment",x=0.5,y=-2,xend=0.5,yend=2) +
  annotate("text", x = 0.1, y = 2.5, label = "@CrowdScoutSprts\nData courtesy\ncorsica.hockey") +
  annotate("text", x = 0.1, y = -1.5, label = "Inconsistent & Bad") +
  annotate("text", x = 0.1, y = 1.5, label = "Inconsistent & Good") +
  #annotate("text", x = 0.9, y = 1.5, label = "Consistent & Good") +
  annotate("text", x = 0.9, y = -1.5, label = "Consistent & Bad") +
  theme(panel.background = element_blank())

###################
####check basic repeatability
##################

scored.goalie.seasons$savePct.t <- (scored.goalie.seasons$Season.ShotsAgainst - scored.goalie.seasons$Season.Goals) / scored.goalie.seasons$Season.ShotsAgainst
scored.goalie.seasons$savePct.t1 <- (scored.goalie.seasons$Last.Season.Shots - scored.goalie.seasons$Last.Season.Goal) / scored.goalie.seasons$Last.Season.Shots

YoY <- lm(scored.goalie.seasons$Season.Lift ~ scored.goalie.seasons$Last.Season.Lift, 
                          weights = (scored.goalie.seasons$Season.ShotsAgainst + scored.goalie.seasons$Last.Season.Shots))
as.numeric(YoY$coefficients[2])
summary(YoY)$r.squared

YoY <- lm(scored.goalie.seasons$savePct.t ~ scored.goalie.seasons$savePct.t1,
          weights = (scored.goalie.seasons$Season.ShotsAgainst + scored.goalie.seasons$Last.Season.Shots))
as.numeric(YoY$coefficients[2])
summary(YoY)$r.squared

ggplot(data=scored.goalie.seasons, aes(x=Last.Season.Lift,y=Season.Lift)) + 
  geom_point() +
  labs(title="Expected - Actual Goals, Goalies 2008-2016") +
  labs(x="Cumulative Shots Against", y="Expected Goals Against - Actual Goals") +
  geom_text(data=scored.goalie.seasons, aes(label = labs, angle=0,check_overlap = FALSE)) +
  #  annotate("text", x = 1, y = (max(goalie.QREAM$QREAM) * 0.8), hjust=0, label = "@CrowdScoutSprts\nData courtesy\ncorsica.hockey") +
  theme(panel.background = element_blank())

library(ggrepel)
####Year by year 
ggplot(scored.goalie.seasons2, aes(factor(Season), Season.Lift * 100, color=Season.ShotsAgainst)) +
  geom_boxplot() +
  geom_point() +
#  geom_line(subset(scored.goalie.seasons2, SA.Goalie=="Bria")) +
  geom_text_repel(aes(label = labs)) +
  labs(title="Goaltender Season xG Lift Distribution by Season\nMinimum 1700 Shots") +
  labs(y="Goals Prevented / 100 Shots (xG - Actual Goals)", x="Season") +
  labs(color="Season Shots Against") +
  annotate("segment",x=0,y=0,xend=6,yend=0) +
  annotate("text", x =1, y = 2.5, label = "@CrowdScoutSprts\nData courtesy\ncorsica.hockey") +
  theme(panel.background = element_blank())

#########
##predict lift
#########
library(randomForest)
####Random Forest Model
rf.cross.v10 <- function(input, formula) {
  
  library(caret)
  # load the dataset
  input
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model <- train( formula ,
                  data=input, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance=TRUE)
  
  predictions <- predict(model, newdata=input)
  
  # var importance
  var.imp <- varImp(model, scale=FALSE)
  
  return(list(cbind(predictions,input),model,var.imp))
  
}

lift.rf <- rf.cross.v10(scored.goalie.seasons, Season.Lift ~ Last.Season.Lift + Last.Season.Consistency + Last.Season.Shots + CTD.Consistency + CTD.Lift)

####summary
lift.rf.scoring <- lift.rf[[2]]
lift.rf.scoring$results

####scored data
lift.rf.scored <- lift.rf[[1]]

plot(lift.rf.scored$predictions ~ lift.rf.scored$Season.Lift)
fit <- lm(lift.rf.scored$predictions ~ lift.rf.scored$Season.Lift)
summary(fit)

################
##ANOVA, VARIABLE IMPORTANCE
################

fm = goal.model[[2]]$finalModel
anova(fm, test="Chisq")

# var importance
var.imp <- varImp(goal.model)

##############################
##create roc curve
##############################

######cut off rates
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

###sample
cutoff.roc.scores(.1,shots.scored$Prob.Goal,shots.scored$goal)


###create accuracy matrix
s = seq(.01,.99,length=1000)
accuracy.mat = matrix(0,1000,5)
    for(i in 1:1000) {
      accuracy.mat[i,]=cutoff.roc.scores(s[i],shots.scored$Prob.Goal,shots.scored$goal)
    }
accuracy.df <- as.data.frame(accuracy.mat)
colnames(accuracy.df) = c("cutoff","sensitivity", "specificity", "class.rate", "distance")
accuracy.df$false.pos.rate <- 1 - accuracy.df$specificity

##############
###ROC Curve
##############

ggplot(data=accuracy.df, aes(x=false.pos.rate,y=sensitivity)) +
    geom_line() 


height = (accuracy.df$sensitivity[-1]+accuracy.df$sensitivity[-length(accuracy.df$sensitivity)])/2
width = -diff(accuracy.df$false.pos.rate) # = diff(rev(omspec))
sum(height*width)
##0.7219591


ggplot(data=accuracy.df, aes(x=cutoff)) +
  geom_line(aes(y=distance,color="blue")) +
  geom_line(aes(y=specificity)) +
  geom_line(aes(y=sensitivity)) +
  geom_line(aes(y=class.rate)) 

