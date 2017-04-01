############################################################################################################################################################################
#
# PROJECT:        xG Model Measuring Goaltender Performance
#
# PURPOSE:        Using Logistic Regression create an xG for each shot a goalie faces. 
#                 Compare xG Against to Actual Goals Against to measure performance
#
# CREATED BY:     Cole Anderson (cole92anderson@gmail.com)
#
# LAST UPDATED:   3/31/2016
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
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr)

### Load Gaolie/Skater Roster with Handedness
skater.roster <- read_csv("https://raw.githubusercontent.com/C92Anderson/xG-Model/master/hockey_skaters_roster.csv") %>% 
                  mutate(shooterID = playerId,
                          Player = toupper(playerName),
                         `Player.Position` = playerPositionCode,
                         Shoots = playerShootsCatches,
                         shooterDOB = playerBirthDate) %>%
                  select(shooterID, Player, Player.Position, Shoots, shooterDOB) %>%
                  filter(!shooterID %in% c(8474744,8466208,8471747,8468436,8466155,8476979,8471221)) %>%
                  unique()

goalie.roster <- read_csv("https://raw.githubusercontent.com/C92Anderson/xG-Model/master/hockey_goalies_roster.csv") %>% 
          mutate(goalieID = playerId,
                 SA.Goalie = toupper(playerName),
                 goalieHeight = playerHeight,
                 Catches = playerShootsCatches,
                 goalieDOB = playerBirthDate) %>%
          select(goalieID, SA.Goalie, goalieHeight, Catches, goalieDOB) %>%
          unique()

skater.name.xwalk <- read_csv("https://raw.githubusercontent.com/C92Anderson/xG-Model/master/skater_name_xwalk.csv")


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
  #load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17.RData")
  
  last.data.game <- 20001 # max(as.character(na.omit(game.events.17$gcode)))
  
  game.list.17 <- as.character(last.data.game:last.game)
  print(game.list.17)
  game.events.17 <- data.frame()
  new.game.events.17 <- plyr::rbind.fill(lapply(FUN=game.pull,game.list.17))
  
  game.events.17 <- plyr::rbind.fill(game.events.17, new.game.events.17)

  
  game.events.17$refdate[is.na(game.events.17$refdate)]  <- 0

  return(game.events.17) 
}

# Update with last game from current season
game.events.17 <- current.data("21149")

game.events.17 <- game.events.17 %>% dplyr::distinct() %>% filter(!is.na(gcode))
test <- game.events.17 %>% group_by(gcode) %>% summarise(cnt = n())

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
load("~/Documents/CWA/nhlscrapr-master/game.events.17.RData")

# Combine season data
pbp.all <- plyr::rbind.fill(game.events.08,game.events.09,game.events.10,game.events.11,
                            game.events.12,game.events.13,game.events.14,game.events.15,
                            game.events.16,game.events.17)

pbp.all <- pbp.all[order(pbp.all$season, pbp.all$gcode, pbp.all$period, pbp.all$seconds), ]
pbp.all <- slide(pbp.all, Var = "seconds", NewVar = "lag.seconds", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "etype", NewVar = "lag.event", slideBy = -1)
pbp.all <- slide(pbp.all, Var = "homezone", NewVar = "lag.home.zone", slideBy = -1)

save(pbp.all,"~/Documents/CWA/Hockey Data/pbp.all.RData")
############################################################################################################################################################################
########1.B LIMIT TO SHOTS AGAINST AND DEVELOP FEATURES
############################################################################################################################################################################

# Create shots dataset
shots.all <- pbp.all %>%
             filter(etype %in% c("SHOT","GOAL","MISS") & period %in% c(1:4)) %>% 
             mutate(Player = toupper(trimws(substr(ev.player.1, 3, nchar(ev.player.1))))) %>%
             left_join(skater.name.xwalk, by = c("Player" = "Player1")) %>%
             mutate(Player = ifelse(!is.na(Player_clean),Player_clean,Player)) %>%
             left_join(skater.roster, by = c("Player" = "Player")) %>% 
             mutate(SA.Goalie = ifelse(ev.team == hometeam, 
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
                    shot.type = droplevels(as.factor(ifelse(type %in% c("Deflected","Tip-In"), "Deflected",
                                                     ifelse(type %in% c("Wrist","Snap","Unspecified"),"Wrist",
                                                            as.character(type))))),
             
                    last.event.time = seconds - lag.seconds,
             
                    home.shooter = ifelse(ev.team == awayteam, 0, 1),
             
                    is.Rebound = as.factor(ifelse(lag.event == "SHOT" & ((seconds - lag.seconds) <= 2),1,0)),

                   Shooter.Handedness = ifelse(Shoots == "L","L",
                                        ifelse(Shoots == "R","R",
                                        ifelse(is.na(Shoots),"U","U"))),
                   Player.Position = ifelse(!is.na(Player.Position), Player.Position, "U"),
                   Shooter.Handedness = ifelse(!is.na(Shooter.Handedness), Shooter.Handedness, "U")) %>%
            filter(ENG == 0) 

shots.all$shot.type <- droplevels(shots.all$shot.type)

shots.all1 <- goalie.roster %>%
            unique() %>%
            right_join(shots.all, by="SA.Goalie") %>%
            mutate(Catches = ifelse(is.na(Catches),"L",Catches),
                   Handed.Class = ifelse(Shooter.Handedness == "L" & Catches == "L", "LL",
                            ifelse(Shooter.Handedness == "L" & Catches == "R", "LR",
                            ifelse(Shooter.Handedness == "R" & Catches == "L", "RL",
                            ifelse(Shooter.Handedness == "R" & Catches == "R", "RR",
                                            "U")))),
                   Handed.Class2 = ifelse(Handed.Class %in% c("LL","RR"),"Same",
                                   ifelse(Handed.Class %in% c("LR","RL"),"Opposite",
                                          "Unknown")),
                  Player.Position2 = ifelse(Player.Position == "D", "D", 
                                     ifelse(Player.Position %in% c("C","L","R","F"),"F",
                                            "Unknown")))
                                   

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
         YC = ifelse(home.align.east == 1 & home.shooter == 1 & period %in% c(1,3), ycoord,
                     ifelse(home.align.east == 1 & home.shooter == 1 & period %in% c(2), -1 * ycoord,
                            ifelse(home.align.east == 0 & home.shooter == 1 & period %in% c(1,3), -1 * ycoord,
                                   ifelse(home.align.east == 0 & home.shooter == 1 & period %in% c(2), ycoord,
                                          ifelse(home.align.east == 1 & home.shooter == 0 & period %in% c(1,3), -1 * ycoord,
                                                 ifelse(home.align.east == 1 & home.shooter == 0 & period %in% c(2), ycoord,
                                                        ifelse(home.align.east == 0 & home.shooter == 0 & period %in% c(1,3), ycoord,
                                                               ifelse(home.align.east == 0 & home.shooter == 0 & period %in% c(2), -1 * ycoord,
                                                                      abs(ycoord))))))))),
         LS.shot = ifelse(YC < 0, 1, 0)) %>%
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
  return(list(round(shot.dist,3), 
              round(shot.angle,3),
              coord.check,
              dist
              #round(x,2), 
              #round(y,2), 
  ))
}


shot.mat <- mapply(coord.calculator,shots.all1$XC, shots.all1$YC, shots.all1$distance)
shot.df <- as.data.frame(t(shot.mat))
colnames(shot.df) <- c("shot.dist","shot.angle","coord.origin","shot.dist.nhl")
shot.df$shot.dist <- as.numeric(shot.df$shot.dist)
shot.df$shot.angle <- as.numeric(shot.df$shot.angle)
shot.df$shot.dist.nhl <- as.numeric(shot.df$shot.dist.nhl)
shot.df$coord.origin <- as.character(shot.df$coord.origin)


shots.all2 <- cbind(shots.all1, shot.df)

aggregate(shot.dist ~ period, data=shots.all2, FUN=mean)


################################################
## Adjust for Rink Shot Distance Bias
################################################

year.rink.dist.bias <- shots.all2 %>%
              group_by(season, hometeam, home.shooter) %>%
              summarise(mean.shotdist1 = mean(shot.dist),
                        mean.shotdist2 = mean(shot.dist.nhl),
                        shot.diff = mean(mean.shotdist1) - mean(mean.shotdist2),
                        mean.shotdist = mean(mean(mean.shotdist1), mean(mean.shotdist2))) %>%
              dcast(season + hometeam ~ home.shooter, value.var = "mean.shotdist")
shot.dist.year <- shots.all2 %>%
            group_by(season) %>%
            summarise(mean.shotdist.year = mean(mean(shot.dist), mean(shot.dist.nhl)))

year.rink.dist.bias <- shots.all2 %>%
            group_by(season, hometeam) %>%
            summarise(mean.shotdist.team = mean(mean(shot.dist), mean(shot.dist.nhl))) %>%
            left_join(shot.dist.year, by="season") %>%
            mutate(rink.year.distance.inflator = mean.shotdist.year / mean.shotdist.team)
           
################################################
# Slide last shot coordinates
################################################

shots.all.lagged <- shots.all2 %>%
        left_join(year.rink.dist.bias, by=c("season","hometeam")) %>%
        arrange(season, gcode, period, seconds) %>%
        mutate(shot.dist2 = ((shot.dist + shot.dist.nhl) / 2) * rink.year.distance.inflator,
               int.shot.dist = lag(shot.dist),
               int.shot.angle = lag(shot.angle),
               int.LS.shot = lag(LS.shot)) %>%
        head(20)


shots.all2 <- shots.all2[order(shots.all2$season, shots.all2$gcode, shots.all2$period, shots.all2$seconds), ]
shots.all2 <- slide(shots.all2, Var = "shot.dist", NewVar = "int.shot.dist", slideBy = -1)
shots.all2 <- slide(shots.all2, Var = "shot.angle", NewVar = "int.shot.angle", slideBy = -1)
shots.all2 <- slide(shots.all2, Var = "LS.shot", NewVar = "int.LS.shot", slideBy = -1)

shots.all2 <- shots.all2 %>%
  mutate(       zone.shot = ifelse(home.shooter == 0 & homezone == "Def", "Off",
                                   ifelse(home.shooter == 0 & homezone == "Off", "Def",
                                          ifelse(home.shooter == 1 & homezone == "Off", "Off",
                                                 ifelse(home.shooter == 1 & homezone == "Def", "Def",
                                                        "Neu")))),
                last.zone = ifelse(home.shooter == 0 & lag.home.zone == "Def", "Off",
                                   ifelse(home.shooter == 0 & lag.home.zone == "Off", "Def",
                                          ifelse(home.shooter == 1 & lag.home.zone == "Off", "Off",
                                                 ifelse(home.shooter == 1 & lag.home.zone == "Def", "Def",
                                                        "Neu")))),
                is.Rush = as.factor(ifelse(((seconds - lag.seconds) <= 4) & zone.shot != last.zone,1,0)),
                
                # Last Event
                last.off.faceoff = ifelse(last.zone == "Off" & lag.event == "FAC", 1, 0),
                last.def.faceoff = ifelse(last.zone == "Def" & lag.event == "FAC", 1, 0),
                last.neu.faceoff = ifelse(last.zone == "Neu" & lag.event == "FAC", 1, 0),
                last.off.shot = ifelse(last.zone == "Off" & lag.event %in% c("SHOT","BLOCK","MISS"), 1, 0),
                last.def.shot = ifelse(last.zone == "Def" & lag.event %in% c("SHOT","BLOCK","MISS"), 1, 0),
                last.neu.shot = ifelse(last.zone == "Neu" & lag.event %in% c("SHOT","BLOCK","MISS"), 1, 0),
                last.off.give = ifelse(last.zone == "Off" & lag.event %in% c("TAKE","GIVE"), 1, 0),
                last.def.give = ifelse(last.zone == "Def" & lag.event %in% c("TAKE","GIVE"), 1, 0),
                last.neu.give = ifelse(last.zone == "Neu" & lag.event %in% c("TAKE","GIVE"), 1, 0),
                
                # Time from Last Event
                time.last.off.faceoff = last.off.faceoff * last.event.time,
                time.last.def.faceoff = last.def.faceoff * last.event.time,
                time.last.neu.faceoff = last.neu.faceoff * last.event.time,
                time.last.off.shot = last.off.shot * last.event.time,
                time.last.def.shot = last.def.shot * last.event.time,
                time.last.neu.shot = last.neu.shot * last.event.time,
                time.last.off.give = last.off.give * last.event.time,
                time.last.def.give = last.def.give * last.event.time,
                time.last.neu.give = last.neu.give * last.event.time,
                
                Rebound.Angle.Change = ifelse(is.Rebound == 0, 0, 
                                       ifelse(LS.shot == int.LS.shot, abs(shot.angle - int.shot.angle),
                                              ifelse(LS.shot != int.LS.shot, 180 - shot.angle - int.shot.angle, 0))),
         Rebound.Second.Change = ifelse(is.Rebound == 0, 0, seconds - lag.seconds),
         Rebound.Distance = ifelse(is.Rebound == 0, 0, shot.dist + int.shot.dist),
         Rebound.Angle.Distance = ifelse(is.Rebound == 0, 0, Rebound.Angle.Change / Rebound.Distance),
         Rebound.Angle.Second = ifelse(is.Rebound == 0, 0, Rebound.Angle.Change / Rebound.Second.Change),
         shot.dist.pow2 = shot.dist ** 2,
         shot.dist.pow3 = shot.dist ** 3,
         shot.angle.pow2 = shot.angle ** 2,
         shot.angle.pow3 = shot.angle ** 3,
         Rink.Year = paste0(hometeam, season))


# Check counts of factors
aggregate(goal ~ gamestate, data = shots.all2, FUN = length)
aggregate(goal ~ shot.type, data = shots.all2, FUN = length)
aggregate(distance ~ season, data = shots.all2, FUN = mean)
aggregate(goal ~ season, data = shots.all2, FUN = length)
#aggregate(distance ~ SA.Goalie, data = shots.all2, FUN = length)


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
      
      library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2); library(MLmetrics)
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
      
      # Brier Score
      brierScore <- mean((predicted$.fitted-predicted$goal)^2)
      print(brierScore)
      
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
                    annotate("text", x = 0.1, y = 0.9, hjust=0, label = paste0("@CrowdScoutSprts\nxG Model built using nhlscrapr\nAUC: ", round(AUC(predicted$.fitted, predicted$goal),2))) +
                    theme(panel.background = element_blank())
                              
      # Gains Chart
      gains.chart <- data.frame(Gain=cumsum(predicted$goal)/sum(predicted$goal), 
                                Random = cumsum(predicted$shot)/sum(predicted$shot)) %>%
                  ggplot() +
                  geom_line(aes(x=Random, y=Gain,colour="Model")) +
                  geom_line(aes(x=Random, y=Random,colour="Random")) +
                  labs(y="Cumulative Share of Goals", x="Cumulative Rank-Ordered Shots", title="Gains Chart", color="") +
                  annotate("text", x = 0.1, y = 0.9, hjust=0, label = paste0("@CrowdScoutSprts\nxG Model built using nhlscrapr\nGains AUC: ", round(GainAUC(predicted$.fitted, predicted$goal),2))) +
                  theme(panel.background = element_blank())
                
      colnames(predicted) <- c("xG","goal","shot")
      
      return(list(model.folds, rs, class.plot, con.mat, cbind(predicted[1],input.cc), AUC.curve, gains.chart))
}      

load("~/Documents/CWA/Hockey Data/shots.all.reg.RData")

predicted.goal.model <- lm.cv.10(shots.all.reg,
                              
                                 c("goal",
                                   "shot.dist",
                                   "shot.dist.pow3",
                                   
                                "shot.angle",
                                "shot.angle.pow2",
                                "shot.angle.pow3",
                                "is.Rush", 
                                "is.Rebound",
                                "Rebound.Angle.Second",
                                "time.last.off.shot", "last.off.shot",
                                "time.last.def.shot", "last.def.shot",
                                "time.last.off.give", "last.off.give",
                                "time.last.def.give", "last.def.give",
                                "time.last.neu.give", "last.neu.give",
                                "shot.type","gamestate","regressed.shooting.skill.index",
                                "Player.Position","Shooter.Handedness"
                                ),  
                                

                                
                                
                              c("season","gcode","period","seconds","ev.team","etype","awayteam","hometeam","away.G","home.G","Player",
                                "even.second","SA.Goalie","time.index","refdate","event","LS.shot"))   

#prob.goal = shot.angle + distance + is.Rebound + shot.type + gamestate + regressed.shooting.skill.index

# Summary best plot
best.model.no <- predicted.goal.model[[2]] %>% filter(r.squared == max(r.squared)) %>% select(.id) %>% as.character()
best.model <- predicted.goal.model[[1]]$model[[paste0(as.numeric(best.model.no))]] 
best.model %>% summary()

# Confusion Matrix
predicted.goal.model[[4]]
# FALSE   TRUE
# 0 434202 196205
# 1  18295  40674


predicted.goal.model[[6]]

predicted.goal.model[[7]]


# Classification Plot
predicted.goal.model[[3]]

# R-squared
1 - (best.model$deviance / best.model$null.deviance)
#  0.1002869
# 0.1017881

# Check xG and goals ratio
sum(as.numeric(predicted.goal.model[[5]]$goal)-1)
sum(predicted.goal.model[[5]]$xG)
# 59672
# 59672.47

## By Season compare xG to goals
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

## Bucket Sh% and xSh%
scored.data %>% 
  mutate(xG_bucket = round(xG,1)) %>%
  group_by(xG_bucket) %>% 
  summarise(xG_shot=sum(xG) / n(), goals_shot=sum(as.numeric(goal)-1) / n(), shots=n()) %>%
  ggplot() +
  #geom_boxplot(aes(x=xG_bucket, y=goals_shot, group= xG_bucket))
  geom_point(aes(x=xG_bucket, y=goals_shot,color=season,size=shots))  

#####################
### Error Checks
#####################

### Log Loss
library(MLmetrics)
LogLoss(scored.data$xG,as.numeric(scored.data$goal)-1)
# 0.262035
# 0.2583331

error <- scored.data$xG-(as.numeric(scored.data$goal)-1)

## Brier Score - Base Line (goal <- 0): 0.08919192
mean(error^2)
# 0.07263929
# 0.07227214

# Mean Absolute Error
mean(abs(error))
# 0.1452301
# 0.1438155

# RMSE
sqrt(mean(error^2))
#0.2695168
#0.2688348

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
  geom_density(aes(xG, color = season)) +
  theme(panel.background = element_blank()) +
  labs(title="xG Distribution by Season", y="Density", x="xG Probability") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) 


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
                                        "20122013","20132014","20142015","20152016","20162017"), shot.cut) {
  
  library(dplyr)
  
  # Select Goalies, Order by season, game, shot
  goalie.set <- scored.data %>%
        filter(SA.Goalie %in% goalie & season %in% seasons) 
  
  goalie.type <- goalie.set %>%
        group_by(SA.Goalie, season) %>%
        summarise(shots = n())
  
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
    labs(title="Goaltending Performance, Expected Goals Against - Actual Goals (QREAM*), All Situations\n*Quality Rules Everything Around Me") +
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

goalie.plot(c("CAREY PRICE","CAM TALBOT","BRADEN HOLTBY","SERGEI BOBROVSKY", "COREY CRAWFORD","DEVAN DUBNYK","TUUKKA RASK","MARTIN JONES","ROBIN LEHNER",
              "JAKE ALLEN","JAKE ALLEN","STEVE MASON","JOHN GIBSON","HENRIK LUNDQVIST","PEKKA RINNE","MIKE SMITH",
              "FREDERIK ANDERSEN","CORY SCHNEIDER","RYAN MILLER","CONNOR HELLEBUYCK"),
            c("20162017"))[[1]]

goalie.plot(c("MARTIN JONES","THOMAS GREISS","COREY CRAWFORD","DEVAN DUBNYK","PEKKA RINNE","ROBIN LEHNER",
              "JAMES REIMER","FREDERIK ANDERSEN"))[[1]]

goalie.plot(c("EDDIE LACK"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("STEVE MASON"))[[1]]
goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("MARC-ANDRE FLEURY","MATTHEW MURRAY"),c("20162017"))[[1]]

#goalie.plot(c("TUUKKA RASK","BRADEN HOLTBY"),c("20152016","20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","JONATHAN BERNIER","JAMES REIMER","KARRI RAMO","JHONAS ENROTH"),c("20152016","20162017"))[[1]]

#goalie.plot(c("HENRIK LUNDQVIST","STEVE MASON","CORY SCHNEIDER"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]

#goalie.plot(c("DEVAN DUBNYK","CAREY PRICE","COREY CRAWFORD","BRADEN HOLTBY","SERGEI BOBROVSKY","JAROSLAV HALAK"),c("20152016","20162017"))[[1]]

#goalie.plot(c("CAREY PRICE","COREY CRAWFORD","CORY SCHNEIDER","JAROSLAV HALAK","CAM WARD","JONATHAN QUICK","MARC-ANDRE FLEURY",
#              "PEKKA RINNE","TUUKKA RASK","HENRIK LUNDQVIST","ROBERTO LUONGO","RYAN MILLER","STEVE MASON"))[[1]]

#goalie.plot(c("CAREY PRICE","COREY CRAWFORD","CORY SCHNEIDER","JAROSLAV HALAK","CAM WARD","JONATHAN QUICK","MARC-ANDRE FLEURY",
#              "PEKKA RINNE","TUUKKA RASK","HENRIK LUNDQVIST","ROBERTO LUONGO","RYAN MILLER","STEVE MASON"),
#            c("2  0142015","20152016","20162017"))[[1]]

#goalie.plot(c("ANDREW HAMMOND","MIKE CONDON","CRAIG ANDERSON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("JAKE ALLEN"),c("20142015","20152016","20162017"))[[1]]
goalie.plot(c("JAKE ALLEN","BRIAN ELLIOTT"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONAS GUSTAVSSON","CAM TALBOT","LAURENT BROSSOIT","ANDERS NILSSON"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("MIIKKA KIPRUSOFF"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","COREY CRAWFORD","PEKKA RINNE","PETER BUDAJ","CAM TALBOT"),c("20162017"))[[1]]


goalie.plot(c("HENRIK LUNDQVIST","CORY SCHNEIDER","JONAS HILLER","ROBERTO LUONGO","RYAN MILLER","NIKLAS BACKSTROM","PETER BUDAJ","MIIKKA KIPRUSOFF",
              "STEVE MASON","MARC-ANDRE FLEURY","CAREY PRICE","CAM WARD","PEKKA RINNE","COREY CRAWFORD","JONATHAN QUICK"))[[1]]

#goalie.plot(c("TUUKKA RASK","JONAS GUSTAVSSON","ANTON KHUDOBIN","MALCOLM SUBBAN","CHAD JOHNSON","ZANE MCINTYRE",
#              "NIKLAS SVEDBERG"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("JOHN GIBSON","JONATHAN BERNIER"),c("20162017"))[[1]]
goalie.plot(c("CAM TALBOT"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ANDREW HAMMOND"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONATHAN QUICK","PETER BUDAJ"),c("20152016","20162017"))[[1]]
goalie.plot(c("FREDERIK ANDERSEN","GARRET SPARKS","CURTIS MCELHINNEY"),c("20162017"))[[1]]

#goalie.plot(c("ANTTI NIEMI","KARI LEHTONEN"),c("20152016","20162017"))[[1]]

#goalie.plot(c("CORY SCHNEIDER","KEITH KINKAID"),c("20152016","20162017"))[[1]]

#goalie.plot(c("JAKE ALLEN","CARTER HUTTON"))[[1]]

#goalie.plot(c("PETR MRAZEK","JIMMY HOWARD"),c("20112012","20122013","20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("PEKKA RINNE","JUUSE SAROS"),c("20152016","20162017"))[[1]]

goalie.plot(c("ANTON KHUDOBIN","CARTER HUTTON"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("CAM TALBOT","SERGEI BOBROVSKY","BRADEN HOLTBY"),c("20162017"))[[1]]
goalie.plot(c("CAM TALBOT"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("STEVE MASON"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("JAROSLAV HALAK","THOMAS GREISS","JEAN-FRANCOIS BERUBE"),c("20162017"))[[1]]

goalie.plot(c("SERGEI BOBROVSKY","BRADEN HOLTBY","SCOTT DARLING","COREY CRAWFORD"),c("20162017"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ZANE MCINTYRE"),c("20162017"))[[1]]

goalie.plot(c("PEKKA RINNE","JUUSE SAROS","SCOTT DARLING","COREY CRAWFORD","JAKE ALLEN","CARTER HUTTON",
              "DEVAN DUBNYK","DARCY KUEMPER","ANTTI NIEMI","KARI LEHTONEN"),c("20162017"))[[1]]

goalie.plot(c("ONDREJ PAVELEC","CONNOR HELLEBUYCK","MICHAEL HUTCHINSON"),c("20152016","20162017"))[[1]]

goalie.plot(c("CRAIG ANDERSON","MIKE CONDON","ANDREW HAMMOND"),c("20152016","20162017"))[[1]]

goalie.plot(c("ANDREW HAMMOND","MIKE CONDON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("RYAN MILLER","JACOB MARKSTROM"),c("20152016","20162017"))[[1]]
goalie.plot(c("RYAN MILLER","JACOB MARKSTROM"))[[1]]

goalie.plot(c("ROBIN LEHNER","ANDERS NILSSON"),c("20162017"))[[1]]
goalie.plot(c("ROBIN LEHNER","ANDERS NILSSON"),c("20142015","20152016","20162017"))[[1]]


goalie.plot(c("CORY SCHNEIDER","HENRIK LUNDQVIST"),c("20162017"))[[1]]

goalie.plot(c("CHAD JOHNSON","BRIAN ELLIOTT"),c("20162017"))[[1]]

goalie.plot(c("JONATHAN QUICK","BEN BISHOP","CHAD JOHNSON","BRIAN ELLIOTT","PETER BUDAJ"),c("20152016","20162017"))[[1]]

goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("EDDIE LACK", "CAM WARD"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("BRIAN ELLIOTT"))[[1]]


goalie.plot(c("AARON DELL","MARTIN JONES"),c("20162017"))[[1]]

############################################################################################################################################################################
########4.A TEAM GOALTENDING QUALITY BY SEASON
############################################################################################################################################################################

QREAM.byteam <- function(seasons) {
  
goalie.set <- scored.data %>%
  filter(season %in% seasons) 

goalie.replacement <- goalie.set %>%
   group_by(SA.Goalie) %>%
   summarise(total.shots = n()) %>%
   left_join(goalie.set, by = "SA.Goalie") %>%
   filter(total.shots < (max(total.shots) * 0.2)) %>%
   group_by(season) %>%
   summarise(total.xG = sum(xG),
             total.Goals = sum( as.numeric(goal)-1) ,
             total.shots = n(),
             QREAM = total.xG - total.Goals,
             QREAM.per.100.shots = QREAM / (total.shots / 100))

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
    scale_fill_gradient2(low="white",mid="light grey",high="Red") +
    theme(panel.background = element_blank()) +
    labs(title=paste0("Goaltending Performance (QREAM*) by Team, ", seasons, " - ", Sys.Date(), " YTD\n*Quality Rules Everything Around Me"),
         x="Team", y="Expected Goals Against - Actual Goals", fill="Shots Against") +
   annotate("text", x = 1, y = (min(team.season$team.QREAM) * 0.5), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model")
  
}

QREAM.byteam("20162017")


############################################################################################################################################################################
########4.B GOALIE SPLITS
############################################################################################################################################################################

#####Function to split any variables
goalie.splits <- function(seasons, shot.min, var.set, desc) {

  G.handedness <- roster.master %>%
    filter(pos == "G") %>%
    dplyr::rename(Catches=Shoots) %>%
    select(firstlast,Catches) %>%
    mutate(SA.Goalie = trimws(substr(firstlast, 1, nchar(firstlast)))) %>%
    unique()
  
  goalie.set <- scored.data %>%
    filter(season %in% seasons) %>%
    left_join(G.handedness, by="SA.Goalie") %>%
    mutate(Goalie.Rink = ifelse(ev.team == hometeam, "Away", "Home"),
           Handedness.Matrix = ifelse(Shooter.Handedness == "L" & Catches == "L", "L Shot - L Catch",
                                 ifelse(Shooter.Handedness == "L" & Catches == "R", "L Shot - R Catch",
                                        ifelse(Shooter.Handedness == "R" & Catches == "L", "R Shot - L Catch",
                                               ifelse(Shooter.Handedness == "R" & Catches == "R", "R Shot - R Catch",
                                                      "U")))),
           Handedness.Matrix2 = ifelse(Handedness.Matrix %in% c("L Shot - L Catch","R Shot - R Catch"),"Same",
                                  ifelse(Handedness.Matrix %in% c("L Shot - R Catch","R Shot - L Catch"),"Opposite",
                                         "U")),
           Side.Hand.Matrix = ifelse(LS.shot == 1 & Handedness.Matrix2 == "Same", "Left Side Shot - Same Handedness",
                              ifelse(LS.shot == 0 & Handedness.Matrix2 == "Same", "Right Side Shot - Same Handedness",
                              ifelse(LS.shot == 1 & Handedness.Matrix2 == "Opposite", "Left Side Shot - Opposite Handedness",
                              ifelse(LS.shot == 0 & Handedness.Matrix2 == "Opposite", "Right Side Shot - Opposite Handedness",
                                                   "U")))),
           gamestate = ifelse(gamestate %in% c("5v4","PP.2p.SA","6v5","4v3"), "PP",
                              ifelse(gamestate %in% c("5v5","4v4","3v3"), "Even",
                                     ifelse(gamestate %in% c("SH.SA"), "SH","U"))),
           Distance.Band = ifelse(shot.dist <= quantile(scored.data$shot.dist, c(.33, .66))[1], "Inner Band Distance",
                           ifelse(shot.dist <= quantile(scored.data$shot.dist, c(.33, .66))[2], "Middle Band Distance",
                                                            "Outer Band Distance")),
           Home.Period.Matrix = paste0(Goalie.Rink,"-P",period),
           Long.Change = ifelse(Home.Period.Matrix %in% c("Home-P2","Home-P4"), "HomeLongChange",
                         ifelse(Home.Period.Matrix %in% c("Away-P2","Away-P4"), "AwayLongChange",
                                "None")),
           Distance.Band.Strength = ifelse(Distance.Band == "Inner Band Distance" & gamestate == "Even", "Even Shot - Inner Band Distance",
                                    ifelse(Distance.Band == "Middle Band Distance" & gamestate == "Even", "Even Shot - Middle Band Distance", 
                                    ifelse(Distance.Band == "Outer Band Distance" & gamestate == "Even", "Even Shot - Outer Band Distance", 
                                    ifelse(Distance.Band == "Inner Band Distance" & gamestate == "PP", "PP Shot - Inner Band Distance", 
                                    ifelse(Distance.Band == "Middle Band Distance" & gamestate == "PP", "PP Shot - Middle Band Distance", 
                                    ifelse(Distance.Band == "Outer Band Distance" & gamestate == "PP", "PP Shot - Outer Band Distance", 
                                                         "U"))))))                     
                                    #Shooter.Handedness = ifelse(Shooter.Handedness == "U" & rbinom(1,1,0.5) == 1, "L",
           #                      ifelse(Shooter.Handedness == "U","R", Shooter.Handedness)),
           #Player.Position = ifelse(Player.Position == "D", "D", 
            #                 ifelse(Player.Position %in% c("C","L","R","F"),"F",
            #                 ifelse(rbinom(1,1,0.33) <= 0.33,"D","F"))),
)
          
  goalie.set$Split <- as.factor(goalie.set[,var.set])
  
  # Count cumulative numbers by goalie
  goalie.season.splits <- goalie.set %>%
    mutate(Game.ID = as.character(gcode),
           Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
           SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
           SA = 1,
           GA = as.numeric(goalie.set$goal)-1) %>%
    #filter(Split != "U") %>%
    select(SA.Goalie, season, SA.Team, Goalie, seconds, goal, xG, SA, GA, even.second, Split) %>%
    group_by(SA.Goalie, season, Goalie, SA.Team, Split, even.second) %>%
    #Total xG and Goals
    summarise(split.xG = sum(xG),
              split.Goals = sum(GA),
              split.shots = sum(SA),
              split.QREAM = split.xG - split.Goals) 
  
  goalie.season.evens <- goalie.season.splits %>%
    group_by(SA.Goalie) %>%
    summarise(total.QREAM = sum(split.QREAM),
              total.shots = sum(split.shots)) %>%
    left_join(goalie.season.splits, by = "SA.Goalie")  %>%
    mutate(split.shot.share = paste0(round(split.shots / total.shots,2)*100,"%"),
           split.QREAM.100 = (split.QREAM / split.shots) * 100,
           total.QREAM.100 = (total.QREAM / total.shots) * 100) %>%
    arrange(total.QREAM) 
  
  goalie.season <- goalie.season.evens %>%
    group_by(SA.Goalie, Goalie, Split) %>%
    summarise(split.QREAM = sum(split.QREAM),
              split.shots = sum(split.shots),
             total.QREAM = max(total.QREAM),
             total.shots = max(total.shots)) %>%
    mutate(split.shot.share = paste0(round(split.shots / total.shots,2)*100,"%")) %>%
    arrange(total.QREAM) 
  
team.trend <- goalie.season.splits %>%
      filter(Split != "U") %>%
      group_by(season, SA.Team, Split) %>%
      summarise(team.shots.against = sum(split.shots),
                team.xG.Lift = sum(split.QREAM),
                team.xG.Lift.per.100shots = sum(split.QREAM) / (sum(split.shots) / 100))

team.trend <- team.trend %>%
        group_by(season, Split) %>%
        summarise(league.shots.against = sum(team.shots.against),
                  league.xG.Lift.per.100shots = sum(team.xG.Lift) / (sum(team.shots.against) / 100)) %>%
        left_join(team.trend, by = c("season","Split")) %>%
        mutate(team.xG.Lift.Indexed = team.xG.Lift.per.100shots - league.xG.Lift.per.100shots)

  
  corr.df <- goalie.season.evens %>%
      select(SA.Goalie, even.second, Split, split.QREAM.100, split.shots)
  
  intra.season.corrs <- function(Split) {  
    
     cor.string.df <- as.data.frame(NULL)
     
     for (i in unique(corr.df$Split)) {
        
        split.cor <- 
          corr.df %>%
            filter(Split == i) %>%
            group_by(SA.Goalie, Split) %>%
            summarise(total.split.shots = sum(split.shots)) %>%
            left_join(corr.df, by = c("SA.Goalie","Split")) %>%
            mutate(Time = ifelse(even.second == 1, "One","Zero")) %>%
            dcast(SA.Goalie + total.split.shots ~ Time, value.var = "split.QREAM.100") %>%
            select(total.split.shots, One, Zero)
        
            wt.cor <- boot::corr(split.cor[c("One","Zero")], w=split.cor$total.split.shots)
        

        cor.string <- paste0(i, " splits have a intra-season correlation of ", round(wt.cor,2),"\n")
        
        cor.string.df[i,1] <- cor.string
     }
    print(cor.string.df)
    
  }

plot <- goalie.season %>%
    filter(total.shots >= shot.min & Split != "U") %>%
    ggplot(aes(x=reorder(SA.Goalie, total.QREAM), y=split.QREAM, fill=Split, label=Goalie)) +
    geom_bar(stat="identity", colour="white") +
    geom_text(aes(label=split.shot.share),size = 2, position = position_stack(vjust = 0.5)) +
    #geom_text(aes(x=reorder(SA.Goalie, total.QREAM), y=split.QREAM, label=split.shot.share),size = 2, position = position_dodge(1)) +
    geom_point(aes(x=reorder(SA.Goalie, total.QREAM), y=total.QREAM)) +
   # geom_text(size = 2, position = position_stack(vjust = 0.5)) +
    #scale_fill_gradient2(low="white",mid="light grey",high="red") +
    theme(panel.background = element_blank()) +
    coord_flip() +
    labs(title=paste0("Goaltending Performance (QREAM*) Splits by ",desc, " - ", seasons, "\n*Quality Rules Everything Around Me: Expected Goals Against - Actual Goals\nMinimum ", shot.min, " Shots", " - ", Sys.Date()),
         x="Goalie", y="Expected Goals Against - Actual Goals", fill=paste0(desc,"\nSplits")) +
    annotate("text", x = 7, y = (max(goalie.season$split.QREAM) * 0.5), hjust=0, 
             label = paste0("@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model\nShare of Splits Shots Labelled\n"
                            #intra.season.corrs(Split)
                            ))

  return(list(plot, team.trend))

}

goalie.splits("20162017", 800, "shot.type", "Shot Type") [[1]]

goalie.splits("20162017", 800, "Shooter.Handedness", "Shooter Handedness") [[1]]

goalie.splits("20162017", 800, "is.Rebound", "Rebound Shot") [[1]]

goalie.splits("20162017", 800, "is.Rush", "Rush Shot") [[1]]

goalie.splits("20162017", 800, "period", "Period") [[1]]

goalie.splits("20162017", 800, "Home.Period.Matrix", "Home - Period") [[1]]

goalie.splits("20162017", 800, "Long.Change", "Long Change Home/Away") [[1]]

goalie.splits("20162017", 800, "Goalie.Rink", "Home/Away") [[1]]

goalie.splits("20152016", 800, "gamestate", "Game State") [[1]]

goalie.splits("20162017", 800, "Player.Position", "Shooter Position") [[1]]

goalie.splits("20162017", 800, "Handedness.Matrix", "Shooter vs Goalie Handedness") [[1]]

goalie.splits("20162017", 800, "Handedness.Matrix2", "Shooter & Goalie Handedness") [[1]]

goalie.splits("20162017", 1000, "LS.shot", "Left Side Shot") [[1]]

goalie.splits("20162017", 800, "Side.Hand.Matrix", "Shot Side, Goalie & Shooter Handedness") [[1]]

quantile(scored.data$shot.dist, c(.33, .66))
goalie.splits("20162017", 800, "Distance.Band.Strength", "Shot Strength & Distance") [[1]]


league.trends.splits <- function(var, season) {
  
  trend <- goalie.splits(season, 800, var, var) [[2]]
  trend$class <- var
  trend$season <- season
  return(as.data.frame(trend))
  print(head(trend))
}

split.list <- c("Distance.Band.Strength", "LS.shot","Side.Hand.Matrix","Handedness.Matrix2","Handedness.Matrix","Player.Position",
                "gamestate","Goalie.Rink","Long.Change","Home.Period.Matrix","period","is.Rush","is.Rebound","Shooter.Handedness","shot.type")

seasons=c("20072008","20082009","20092010","20102011","20112012",
          "20122013","20132014","20142015",
          "20152016","20162017")
  
all.league.split.trends <- plyr::rbind.fill(lapply(FUN=league.trends.splits, split.list, "20162017"))

homeaway.split.trends <- plyr::rbind.fill(lapply(FUN=league.trends.splits, "Goalie.Rink", seasons))

all.league.split.trends %>%
            filter(class %in% c("gamestate","Handedness.Matrix2","Player.Position","Long.Change",
                                "period","is.Rush","Distance.Band.Strength","Home.Period.Matrix","Side.Hand.Matrix")) %>%
            select(season, class, Split, league.xG.Lift.per.100shots, league.shots.against) %>%
            distinct() %>%
            ggplot() +
            facet_wrap(~ class, scales = "free_y") +
            geom_bar(stat = "identity", aes(x=Split, y=league.xG.Lift.per.100shots, fill=league.shots.against)) +
            theme(panel.background = element_blank()) +
            coord_flip() +
            scale_fill_gradient2(low="white",mid="light grey",high="red") +
            labs(title=paste0("League Wide xGA - GA Splits - 20162017 - YTD:", Sys.Date()),
                 x="", y="Expected Goals Against - Actual Goals per 100 Shots", fill="Total Split Shots")
      
 ###########################################################################################################################################################################
########4.C NOTABLE SEASONS
############################################################################################################################################################################

historical.goalie.season <- scored.data %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
         SA = 1,
         GA = as.numeric(scored.data$goal)-1) %>%
  select(SA.Goalie, season, Goalie, xG, SA, GA, gcode, SA.Team) %>%
  group_by(SA.Goalie, season,SA.Team) %>%
  #Total xG and Goals
  summarise(xG = sum(xG),
            Goals = sum(GA),
            shots = sum(SA),
            games = uniqueN(gcode),
            xG.shot = xG / shots,
            shots.per.game = shots / games,
            xG.Lift = xG - Goals,
            xG.Lift.per100Shots = xG.Lift / (shots / 100),
            Surplus.Pts = games / (100 / shots.per.game) * (xG.Lift.per100Shots / (1/0.3603384))) %>%
  arrange(-xG.Lift.per100Shots) 

replacement.season <- historical.goalie.season %>%
    group_by() %>%
    filter(shots < (max(shots) * 0.1)) %>%
    summarise(replacement.xG.Lift.per100Shots = (sum(xG) - sum(Goals)) / (sum(shots) / 100),
              replacement.total.shots = sum(shots),
              sv.percentage = 1 - (sum(Goals) / sum(shots))) %>%
    cbind(season = unique(historical.goalie.season$season))

## Plot Points by Goalie Season
spar.goalie.season <- historical.goalie.season %>%
      filter(season=="20162017" & shots > 750) %>%
      ungroup()  %>%
      select(SA.Goalie) %>%
      inner_join(historical.goalie.season, by="SA.Goalie") %>%
      left_join(replacement.season, by ="season") %>%
      filter(shots > 200) %>%
      group_by(SA.Goalie) %>%
      mutate(season_cnt = uniqueN(season), 
             total.QREAM = (sum(xG) - sum(Goals)) / sum(shots),
             career.shots = sum(shots),
             Surplus.Pts.AboveReplacement = games / (100 / shots.per.game) * ((xG.Lift.per100Shots - replacement.xG.Lift.per100Shots) / (1/0.3603384)),
             Total.Surplus.Pts.AboveReplacement = sum(Surplus.Pts.AboveReplacement) / season_cnt) 

## Average by Season
spar.goalie.season %>% ungroup() %>% 
          select(SA.Goalie,Total.Surplus.Pts.AboveReplacement, career.shots) %>% 
          distinct() %>%
          ggplot(aes(reorder(factor(SA.Goalie),Total.Surplus.Pts.AboveReplacement),Total.Surplus.Pts.AboveReplacement)) +
          geom_bar(stat = "identity", aes(fill = career.shots)) +
          coord_flip() +
          ylim(0,25) +
          scale_fill_gradient2(low="white",mid="light grey",high="Red") +
          theme(panel.background = element_blank(),
                panel.grid.major.x = element_line(colour = "dark grey"),
                panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
          labs(title=paste0("Goaltender Surplus Points Above Replacement per Season\nActive Goalies, Minimum 750 Shots in 2016-17\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
               y="Surplus Points Above Replacement per Season (Goals Prevented Over xG Converted to Points, 0.36pts / GA)", x="Goalie", fill = "Shots Against")

## Plot by Season
spar.goalie.season %>%
      ggplot(aes(reorder(factor(SA.Goalie),Total.Surplus.Pts.AboveReplacement),Surplus.Pts.AboveReplacement)) +
      geom_point(aes(color=season, size=shots), alpha=0.25) +
      scale_color_manual(values=c("grey85","grey80","grey75","grey70","grey65","grey60","grey55","grey50","grey45","darkorchid1")) +
      geom_boxplot(colour="black",alpha=0.1) +
      coord_flip() +
      theme(panel.background = element_blank(),
            panel.grid.major.x = element_line(colour = "dark grey"),
            panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
      labs(title=paste0("Goaltender Surplus Points Above Replacement by Season\nActive Goalies, Minimum 750 Shots in 2016-17, 200 Shots Season Threshold\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
           y="Surplus Points Above Replacement (Goals Prevented Over xG Converted to Points, 0.36pts / GA)", x="Goalie", color = "Season", size="Shots Against")
          # size = "xG Lift per\n100 Shots Against") 

## Current Season
spar.goalie.season %>%
      filter(season == "20162017") %>%
      ggplot(aes(reorder(factor(SA.Goalie),Surplus.Pts.AboveReplacement),Surplus.Pts.AboveReplacement)) +
      geom_bar(stat = "identity", aes(fill = career.shots)) +
      coord_flip() +
      ylim(0,25) +
      scale_fill_gradient2(low="white",mid="light grey",high="Red") +
      theme(panel.background = element_blank(),
            panel.grid.major.x = element_line(colour = "dark grey"),
            panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
      labs(title=paste0("Goaltender Surplus Points Above Replacement per Season\nActive Goalies, Minimum 750 Shots in 2016-17\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
           y="Surplus Points Above Replacement per Season (Goals Prevented Over xG Converted to Points, 0.36pts / GA)", x="Goalie", fill = "Shots Against")

all.seasons <- historical.goalie.season %>%
        group_by(SA.Goalie) %>%
        summarise(total.shots = sum(shots),
                  total.QREAM = sum(xG.Lift),
                  Surplus.Pts = sum(Surplus.Pts),
                  Seasons = uniqueN(season),
                  Pts.per.Season = sum(Surplus.Pts) / uniqueN(season))

## Density Plots
historical.goalie.season %>%
    filter(shots > 750) %>%
    ggplot(aes(x=xG.Lift.per100Shots)) +
   # geom_histogram(binwidth = 1, fill = "blue") + 
    geom_density(color = "dark grey") +
    facet_wrap(~ season) +
    theme(panel.background = element_blank()) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    labs(title=paste0("Goaltender xG Lift per 100 Shots by Season - 2008-17 (Min 750 Shots)\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
         x="Goaltender xG - Actual Goals per 100 Shots Faced", y="Density") 

# Historical - All
historical.goalie.season %>%
      filter(shots > 750) %>%
      group_by(SA.Goalie) %>%
      mutate(season_cnt = uniqueN(season), total.QREAM = (sum(xG) - sum(Goals)) / sum(shots)) %>%
      filter(season_cnt > 4) %>%
      ggplot(aes(reorder(factor(SA.Goalie),total.QREAM),xG.Lift.per100Shots, weight = shots)) +
      geom_point(aes(color=season, size=shots)) +
      geom_boxplot(colour="black",alpha=0.1) +
      coord_flip() +
      theme(panel.background = element_blank(),
            panel.grid.major.x = element_line(colour = "dark grey"),
            panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
      labs(title=paste0("Goaltender xG Lift per 100 Shots by Season (Minimum 5 Seasons)\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
       y="Goaltender xG - Actual Goals per 100 Shots Faced", x="Goalie", color = "Season", size = "Shots Against") 

 
# Historical - Current Goalies
historical.goalie.season %>%
  filter(season=="20162017" & shots > 750) %>%
  ungroup()  %>%
  select(SA.Goalie) %>%
  inner_join(historical.goalie.season, by="SA.Goalie") %>%
  filter(shots > 200) %>%
  group_by(SA.Goalie) %>%
  mutate(season_cnt = uniqueN(season), total.QREAM = (sum(xG) - sum(Goals)) / sum(shots)) %>%
  ggplot(aes(reorder(factor(SA.Goalie),total.QREAM),xG.Lift.per100Shots, weight = shots)) +
  geom_point(aes(color=season, size=shots)) +
  scale_color_manual(values=c("grey85","grey80","grey75","grey70","grey65","grey60","grey55","grey50","grey45","darkorchid1")) +
  geom_boxplot(colour="black",alpha=0.1) +
  coord_flip() +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "dark grey"),
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
  labs(title=paste0("Goaltender xG Lift (*QREAM) per 100 Shots by Season - *Quality Rules Everything Around Me\nActive Goalies, Minimum 750 Shots in 2016-17, 200 Shots Season Threshold\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
       y="Goaltender xG - Actual Goals per 100 Shots Faced", x="Goalie", color = "Season", size = "Shots Against") 

############################################################################################################################################################################
########4.D STANDINGS ADJUSTED FOR AVERAGE GOALTENDING
############################################################################################################################################################################

standings.adjustment <- function(year, goals.to.pts) {
  
  library(rvest); library(XML)
  
  url <- c(paste0("http://www.hockey-reference.com/leagues/NHL_",year,".html"))
  nhl.df <- readHTMLTable(url, header = FALSE)
  
  standings <- rbind(nhl.df$standings_EAS, nhl.df$standings_WES) %>% 
            na.omit() %>% 
            as.data.frame() 
  print(head(standings))
  SA.Team <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                "ARI")
  
  Team <- c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Columbus Blue Jackets",
                 "Calgary Flames", "Chicago Blackhawks", "Colorado Avalanche", "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers",
                 "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens", "New Jersey Devils", "Nashville Predators",
                 "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks",
                 "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks", "Winnipeg Jets", "Washington Capitals",
                 "Phoenix Coyotes")
  
  team.match <- cbind(SA.Team, Team) %>% data.frame()
  
  points <- standings[c(1,2,6)]
  colnames(points) <- c("Team","GP","Points")
  
  points <- points %>%
          mutate(Points = as.numeric(as.character(Points)),
                 GP = as.numeric(as.character(GP)),
                 Points.82 = (82 / GP) * Points,
                 Team = gsub("[*]","",Team)) %>%
                 left_join(team.match, by=c("Team")) %>%
            arrange(-Points.82)
  points.team <- scored.data %>%
    mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
           SA = 1,
           GA = as.numeric(scored.data$goal)-1) %>%
    filter(season == c(paste0(year-1,year))) %>%
    select(season, xG, SA, GA, gcode, SA.Team) %>%
    group_by(season,SA.Team) %>%
    #Total xG and Goals
    summarise(xG = sum(xG),
              Goals = sum(GA),
              shots = sum(SA),
              games = uniqueN(gcode),
              xG.Lift = xG - Goals) %>%
    left_join(points, by = c("SA.Team")) %>%
    arrange(-Points) %>%
    mutate( Points.Rank=row_number(),
            xG.Lift.82 = xG.Lift * (82 / GP),
            Pts.Surplus.82 = xG.Lift.82 * goals.to.pts,
            Avg.Goaltending.Adj = xG.Lift.82 * -1,
            adj.Points.82 = Points.82 -  Pts.Surplus.82) %>%
    arrange(-adj.Points.82) %>%
    mutate(Points.Rank2=row_number()) %>%
    arrange(-xG.Lift) 
  
  print(head(points.team))
  print(tail(points.team))
  
  points.plot <- points.team %>%
    ggplot() +
    geom_segment(aes(x = reorder(SA.Team, - xG.Lift), 
                     y = Points.82, xend = reorder(SA.Team, - xG.Lift), yend = adj.Points.82, colour = Avg.Goaltending.Adj), size=2) +
    scale_color_gradient2(low="firebrick2",mid="grey50",high="forestgreen") +
    theme(panel.background = element_blank()) +
    labs(title=paste0("Team Points Adjusted by League Average Goaltending - ",year-1,"-",year," Season\n@CrowdScoutSprts"),
         x="Team", y="Team Points Pace Over 82 Games", color="Adjusted  GA  \nxG - GA = 0") +
    annotate("text", x = 1, y = min(points.team$Points.82) + 10, hjust=0, 
             label = paste0("xG prevented is equal to ",round(goals.to.pts,2)," Points\nLight grey box indicates current league rank\nBlue box indicates league rank assuming averge goaltending")) +
    geom_label(aes(x=reorder(SA.Team, - xG.Lift), y=Points.82,label=as.factor(Points.Rank)), color="honeydew3", size=3) +
    geom_label(aes(x=reorder(SA.Team, - xG.Lift), y=adj.Points.82, label=(Points.Rank2)), color="blue", size=3)
    
  multiplot(points.plot, QREAM.byteam(paste0(year-1,year)), cols = 1)

}

standings.adjustment(2017, 0.36)

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

shooter.plot(c("MICHEAL FERLAND","JOHNNY GAUDREAU","SEAN MONAHAN"),c("20142015","20152016","20162017"))[[1]]

shooter.plot(c("MATTHEW TKACHUK"),c("20162017"))[[1]]

shooter.plot(c("MATTHEW TKACHUK"),c("20162017"))[[1]]

############################################################################################################################################################################
########5.B SHOOTER SKILL GAP PER SHOT BY SEASON
############################################################################################################################################################################

xG_v_GF_pershot <- function(player,   seasons=c("20072008","20082009","20092010","20102011","20112012",
                                                "20122013","20132014","20142015","20152016","20162017")) {
  
player.stats <- scored.data %>%
      filter(Player == player & season %in% seasons) %>%
      group_by(Player, season) %>%
      mutate(SF = 1,
             GF = as.numeric(goal)-1) %>%
      summarise(SF = sum(SF),
                GF = sum(GF),
                xG = sum(xG),
                mean.shot.dist = mean(shot.dist),
                mean.shot.angle = mean(shot.angle)) %>%
      mutate(`xG per Shot` = xG / SF,
             `GF per Shot` = GF / SF)
    
  print(player.stats)
      
    per.shot <- player.stats %>%
      select(Player, season, `xG per Shot`, `GF per Shot`) %>%
      as.data.frame() %>%
      reshape2::melt() %>%
      ggplot() +
      geom_point(aes(x=season, y=value, group=variable,color=variable)) +
      geom_line(aes(x=season, y=value, group=variable,color=variable)) +
    labs(color="") +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    labs(title=paste0(player, " Goal Production vs Expected by Season\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)")) +
    labs(y="Individual xG and GF per Shot Taken", x="", color="") +
    #annotate("text", x = 1, y = .025, hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
    theme(panel.background = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "top")

    absolute <- player.stats %>%
      select(Player, season, xG, GF) %>%
      as.data.frame() %>%
      reshape2::melt(id.vars = c("Player", "season")) %>%
      ggplot() +
      geom_bar(aes(x=season, y=value, group=variable,fill=variable), position="dodge", stat="identity") +
      labs(color="") +
      #labs(title=paste0(player, " xG and GF by Season")) +
      labs(x="Season", y="Individual xG and GF", fill="") +
      theme(panel.background = element_blank(),
            legend.position = "top")
    
    out <- multiplot(per.shot, absolute, cols=1)
    
    return(out)
    
}


xG_v_GF_pershot("TODD BERTUZZI")
xG_v_GF_pershot("MATTHEW TKACHUK")

xG_v_GF_pershot("MAGNUS PAAJARVI")

xG_v_GF_pershot("RICHARD PANIK")
xG_v_GF_pershot("ALEXANDER OVECHKIN")
xG_v_GF_pershot("STEVEN STAMKOS")
xG_v_GF_pershot("BRAD BOYES")
xG_v_GF_pershot("MITCHELL MARNER")


############################################################################################################################################################################
########5.C TEAM SHOOTER SKILL (NO CHART)
############################################################################################################################################################################

player.stats <- scored.data %>%
  filter(Shooter.Handedness != "U" & season == "20162017") %>%
  group_by(Player, season, Player.Position, Shooter.Handedness) %>%
  mutate(SF = 1,
         GF = as.numeric(goal)-1) %>%
  summarise(SF = sum(SF),
            GF = sum(GF),
            xG = sum(xG)) %>%
  filter(SF > 50) %>%
  mutate(`xG per Shot` = xG / SF,
         `GF per Shot` = GF / SF,
         xG.diff = GF - xG,
         xG.shoot.diff = `GF per Shot` - `xG per Shot`)

actual <- player.stats %>%
  ggplot(aes(x=xG.diff)) +
  geom_histogram(binwidth = 2.5, fill = "blue") + 
  #geom_density(color = "dark grey") +
  facet_grid(~ Shooter.Handedness) +
  theme(panel.background = element_blank()) +
  labs(title=paste0("Actual Goals - xG Distribution by Position - 2008-17 (Min 50 Shots)\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
       x="Actual Goals - xG", y="Frequency") 


percentage <- player.stats %>%
  ggplot(aes(x=xG.shoot.diff)) +
  geom_histogram(binwidth = 0.01, fill = "blue") + 
  geom_density(color = "dark grey") +
  facet_grid(~ Shooter.Handedness) +
  scale_x_continuous(labels = scales::percent) +
  theme(panel.background = element_blank()) +
  labs(title="Normalized by Shots",
                    x="Goals per Shot - xG per Shot", y="Frequency") 

multiplot(actual, percentage)

############################################################################################################################################################################
########6.A TEAM XG STORY
############################################################################################################################################################################

team.story <- function(team, year) {
  
  team.story <- scored.data %>%
          filter((hometeam == team | awayteam == team) & season == year) %>%
          select(season, gcode, hometeam, awayteam, xG, gamestate, goal, ev.team) %>%
          mutate(Game.ID = as.character(gcode),
                 Goal = as.numeric(goal)-1,
                 Strength = ifelse(gamestate %in% c("5v5","4v4","3v3"),"Even Strength xGF - xGA","Special Teams xGF - xGA"),
                 Opposition = ifelse(hometeam != team, hometeam, awayteam),
                 Home.Team = ifelse(team == hometeam, 1, 0),
                 xGF = ifelse(ev.team == team,xG,0),
                 xGA = ifelse(ev.team != team,xG,0),
                 GF = ifelse(ev.team == team,Goal,0),
                 GA = ifelse(ev.team != team,Goal,0)) %>%
          group_by(season, Game.ID, Opposition, Home.Team, Strength) %>%
          summarise(xGF = sum(xGF),
                    xGA = sum(xGA),
                    GF = sum(GF),
                    GA = sum(GA),
                    xGD = xGF - xGA)
  
   if(dim(team.story)[1] > 0) {
   
     team.rates <- team.story %>%
              dcast(season + Game.ID + Opposition + Home.Team ~ Strength, value.var="xGD")
    
     team.bygame <- team.story %>%
              group_by(season, Game.ID, Opposition, Home.Team) %>%
              summarise(xGF = sum(xGF),
                        xGA = sum(xGA),
                        GF = sum(GF),
                        GA = sum(GA)) %>%
              left_join(team.rates, by=c("season","Game.ID", "Opposition", "Home.Team")) %>%
              mutate(`Shooting Lift (GF - xGF)` = GF - xGF,
                     `Goaltending Lift (xGA - GA)` = xGA - GA,
                      `Special Teams xGF - xGA`= ifelse(is.na(`Special Teams xGF - xGA`), 0, `Special Teams xGF - xGA`),
                     Final = GF - GA) %>%
              as.data.frame()
     
       team.bygame$GameNum <- 1:nrow(team.bygame)     
       team.bygame$Team <- team
  
  game.chart <- team.bygame %>%
       
       melt(id.vars=c("season","GameNum","Final","Opposition"),
            measure.vars=c("Shooting Lift (GF - xGF)","Goaltending Lift (xGA - GA)",
                           "Even Strength xGF - xGA","Special Teams xGF - xGA")) %>%
       ggplot(aes(x=GameNum, y=value, fill=variable)) +
  #     annotate("text", x = 1, y = max(team.bygame$Final) + 0.5, hjust=0, size = 3, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
       annotate("text", x = 1, y = min(team.bygame$Final) - 0.5, hjust=0, size = 2.5, label = "Opposition team label indicates goal differential.") +
       geom_bar(stat="identity", colour="white") + 
       geom_text(aes(x=GameNum, y=Final, label=Opposition), angle = 90) +
       scale_y_continuous(breaks = seq( min(team.bygame$Final),max(team.bygame$Final), by=1)) +
       #scale_fill_gradient2(low="white",mid="light grey",high="Red") +
       theme(panel.background = element_blank(),
             legend.position = "right",
             axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank()) +
       labs(title=paste0(team," ",year," xG Components by Game - ", Sys.Date(), " YTD\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
            x="Game Number", y=paste0("xG Lift to ", team), fill="Team Component") 
     
  cum.chart <- team.bygame %>%
      group_by(season) %>%
      mutate(`Shooting Lift (GF - xGF)` = cumsum(`Shooting Lift (GF - xGF)`),
             `Goaltending Lift (xGA - GA)` = cumsum(`Goaltending Lift (xGA - GA)`),
             `Even Strength xGF - xGA` = cumsum(`Even Strength xGF - xGA`),
             `Special Teams xGF - xGA` = cumsum(`Special Teams xGF - xGA`),
             `Goal Differential` = cumsum(Final)) %>%
        as.data.frame() %>%
        melt(id.vars=c("season","GameNum","Goal Differential"),
             measure.vars=c("Shooting Lift (GF - xGF)","Goaltending Lift (xGA - GA)",
                            "Even Strength xGF - xGA","Special Teams xGF - xGA")) %>%
          ggplot() +
          theme(panel.background = element_blank(),
                legend.position="right") +  #c(0.15,0.15)) +
          annotate("segment",x=0,y=0,xend=max(team.bygame$GameNum),yend=0) +
          #annotate("text", x = 1, y = max(cumsum(team.bygame$Final)) + 1, hjust=0, size = 4, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
          annotate("text", x = 1, y = min(cumsum(team.bygame$Final)) - 1, hjust=0, size = 2.5, label = "Cumulative Goal Differential Black Line\nEmpty net and shootout goals removed. May contain inaccuracies.") +
          geom_line(aes(x=GameNum, y=value, group=variable, color=variable), size=1.5) +
          geom_line(aes(x=GameNum, y=`Goal Differential`, group=1), size=1.5) +
          #scale_y_continuous(breaks = seq( min(team.bygame$value),max(team.bygame$value), by=1)) +
          labs(#title=paste0(team," ",season," Cumulative xG Components by Game - ", Sys.Date(), " YTD"),
               x="Game Number", y=paste0("Cumulative xG Lift to ", team), color="Cumulative\nTeam Component") 
  
    out <- multiplot(game.chart, cum.chart, cols=1)
  
    return(list(out,team.bygame))
   }
}


############################################################################################################################################################################
########6.B ALL TEAM XG STORIEs
############################################################################################################################################################################

team.story("TOR", "20162017")[[1]]

team.story2 <- function(team, season) { 
  table <- team.story(team, season)[[2]]
  return(table) 
  }

teams <- c("ATL","WSH","BOS","VAN","WPG","PHX","ANA","L.A","CAR","MTL","DET","OTT","TOR","COL","DAL","NYR","FLA","T.B","N.J","NSH","CHI",
           "MIN","CGY","PHI","EDM","S.J","STL","PHX","PIT","CBJ","NYI","BUF","ARI")


all.game.stories.17 <- do.call(rbind,lapply(FUN=team.story2,teams,"20162017"))
all.game.stories.16 <- do.call(rbind,lapply(FUN=team.story2,teams,"20152016"))
all.game.stories.15 <- do.call(rbind,lapply(FUN=team.story2,teams,"20142015"))
all.game.stories.14 <- do.call(rbind,lapply(FUN=team.story2,teams,"20132014"))


all.game.stories <- plyr::rbind.fill(all.game.stories.17,all.game.stories.16,all.game.stories.15,all.game.stories.14)


game.stories.splits <- all.game.stories %>%
                mutate(Even.Game = ifelse(as.numeric(GameNum) %% 2 == 0, "Even","Odd")) %>%
                group_by(Team, season, Even.Game) %>%
                summarise(`Shooting Lift (GF - xGF)` = sum(`Shooting Lift (GF - xGF)`) / n(),
                          `Goaltending Lift (xGA - GA)` = sum(`Goaltending Lift (xGA - GA)`) / n(),
                          `Even Strength xGF - xGA` = sum(`Even Strength xGF - xGA`) / n(),
                          `Special Teams xGF - xGA` = sum(`Special Teams xGF - xGA`) / n(),
                          `Goal Differential` = sum(GF - GA) / n(),
                          xGF = sum(xGF) / n(),
                          xGA = sum(xGA) / n())


variable.splits.compare <- function(var) {
  
    splits <- game.stories.splits %>%
       # select(Team, season, Even.Game) %>%
       dcast( Team + season ~ Even.Game, value.var=var) 
    
    print(cor(splits$Odd, splits$Even))
    #print(lm(splits$Odd ~ splits$Even))
    
    print(head(splits))
    
    splits %>%
        ggplot(aes(x=Even,y=Odd, group=Team, color=season)) +
        geom_point(color="purple") +
        theme(panel.background = element_blank()) +
         annotate("segment",x=min(splits$Even),y=min(splits$Even),xend=(-1*min(splits$Even)),yend=(-1*min(splits$Even))) +
        annotate("text", x=min(splits$Even), y = (max(splits$Even)), hjust=0, 
                 label = paste0(#"@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model\n",
                 var, " Correlation: ", round(cor(splits$Odd, splits$Even),2))) +
      
        labs(title=paste0("Correlation Team-Level Splits Even/Odd Numbered Games\n",var#,"\n@CrowdScoutSprts - xG Model built using nhlscrapr"
                          ),
        x=paste0(var," per Game - Even Games"), y=paste0(var," per Game - Odd Games"), color="Season") 

}
    

multiplot(variable.splits.compare("xGF"),
variable.splits.compare("xGA"))

multiplot(variable.splits.compare("Even Strength xGF - xGA"),
  variable.splits.compare("Goal Differential"),
  variable.splits.compare("Special Teams xGF - xGA"),
  variable.splits.compare("Goaltending Lift (xGA - GA)"),
variable.splits.compare("Shooting Lift (GF - xGF)"),cols = 1)
