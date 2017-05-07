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
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(data.table)

source("/Users/colander1/Documents/CWA/R Code/operations.R")

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
  
  url.len <- length(try(read_html(print(paste0("https://statsapi.web.nhl.com/api/v1/game/20160",game,"/feed/live?site=en_nhl")))))
  
  if(url.len > 1) {
    game <- retrieve.game(season="20162017", gcode=game)[[1]]
    if(length(game) > 0) { 
      return(game)
    }
  }
}

# Check data and update based on current game (replace to most current gcode)
current.data <- function(last.game) {
###load current data
  #load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17.RData")
  
  last.data.game <- 30100 # max(as.character(na.omit(game.events.17$gcode)))
  
  game.list.17 <- as.character(last.data.game:last.game)
  print(game.list.17)
  game.events.17 <- data.frame()
  new.game.events.17 <- plyr::rbind.fill(lapply(FUN=game.pull,game.list.17))
  
  game.events.17 <- plyr::rbind.fill(game.events.17, new.game.events.17)

  
  game.events.17$refdate[is.na(game.events.17$refdate)]  <- 0

  return(game.events.17) 
}

# Update with last game from current season
game.events.17playoffs <- current.data("30250")

# Clean
game.events.17playoffs <- game.events.17playoffs %>% dplyr::distinct() %>% filter(!is.na(gcode))

# Check
recent.season.check <- game.events.17playoffs %>% group_by(gcode) %>% summarise(cnt = n())
print(dim(recent.season.check))
print(tail(recent.season.check))

save(game.events.17playoffs, file="/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.17playoffs.RData")

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
load("~/Documents/CWA/nhlscrapr-master/game.events.17playoffs.RData")

# Combine season data
pbp.all <- plyr::rbind.fill(game.events.08,game.events.09,game.events.10,game.events.11,
                            game.events.12,game.events.13,game.events.14,game.events.15,
                            game.events.16,game.events.17,game.events.17playoffs)


pbp.all <- pbp.all %>% 
          mutate(goal = as.factor(ifelse(etype =="GOAL",1,0)),
                 eytpe = ifelse(is.na(xcoord),"MISS",etype)) %>%
          filter(etype %in% c("FAC","SHOT","HIT","MISS","TAKE","GOAL","GIVE","BLOCK")) %>%
          select(-c(number,pos,lastfirst,last,first,numlast,numfirstlast))  

################################################
## Adjust for Rink Shot Distance Bias
## Using nhlscrapr/R/operations.R: https://github.com/war-on-ice/nhlscrapr/blob/master/R/operations.R
################################################

#Block the seasons into 4. This will make it easier to add new games as time goes along.
season.block <- function (season) {
  year <- as.numeric(substr(season,1,4))
  1 + 1*(year >= 2005) + 1*(year >= 2007) + 1*(year >= 2009) + 1*(year >= 2011) + 1*(year >= 2013) 
  
}
compare.CDF <- function (original, target, range=1:200) {
  #range=1:200
  
  original <- sort(original); target <- sort(target)
  oq <- quantile(original, seq(0,1,by=0.001), na.rm=TRUE)
  tq <- quantile(target, seq(0,1,by=0.001), na.rm=TRUE)
  
  values <- sapply(range, function(rr) mean(tq[oq==rr]))
  if (is.na(values[1])) values[1] <- range[1]
  
  picks <- 2:length(values)
  for (kk in picks[is.na(values[-1])]) values[kk] <- values[kk-1] + 1
  values[values > max(range)] <- max(range)
  names(values) <- range
  values
}

make.adjusted.distance.homeaway.table <- function (shots.in) { #,
  
  #shots.in = shot.data
  message ("Creating distance adjustment tables.")
  shots.in$seasonblock <- season.block(shots.in$season)
  seasonblocks <- 1:max(shots.in$seasonblock)
  shots.split <- lapply(seasonblocks,
                        function (bb) shots.in[shots.in$seasonblock==bb,
                                               c("type", "ev.team", "hometeam", "awayteam", "distance")])
  
  split.table <- expand.grid (seasonblock=seasonblocks, type=unique(shots.in$type),
                              teams=unique(shots.in$hometeam), stringsAsFactors = FALSE)
  
  quantile.adjust <- sapply (1:nrow(split.table), function(rr) {
    prop <- split.table[rr,]
    ## message (prop)
    original <- shots.split[[prop[[1]]]]$distance[shots.split[[prop[[1]]]]$type == prop[[2]] &
                                                    shots.split[[prop[[1]]]]$ev.team == prop[[3]] &
                                                    shots.split[[prop[[1]]]]$hometeam == prop[[3]]]
    target <- shots.split[[prop[[1]]]]$distance[shots.split[[prop[[1]]]]$type == prop[[2]] &
                                                  shots.split[[prop[[1]]]]$ev.team == prop[[3]] &
                                                  shots.split[[prop[[1]]]]$awayteam == prop[[3]]]
    compare.CDF (original, target)
  })
  
  return (list(split.table=split.table, quantile.adjust=quantile.adjust))
}

update.adjusted.distance <- function (shots.in, split.object) {
  
  #shots.in=shot.data; split.object=distance.adjust
  shots.in$seasonblock <- season.block(shots.in$season)
  seasonblocks <- 1:max(shots.in$seasonblock)
  shots.in$adjusted.distance <- shots.in$distance
  
  shots.split <- lapply(seasonblocks,
                        function (bb) shots.in[shots.in$seasonblock==bb,
                                               c("type", "ev.team", "hometeam", "awayteam",
                                                 "distance", "adjusted.distance")])
  
  for (rr in 1:nrow(split.object$split.table)) {
    prop <- split.object$split.table[rr,]
    ##  message(prop)
    rows <- which (shots.split[[prop[[1]]]]$type == prop[[2]] &
                     shots.split[[prop[[1]]]]$hometeam == prop[[3]] &
                     shots.split[[prop[[1]]]]$distance > 0)
    if (length(rows)>0) shots.split[[prop[[1]]]]$adjusted.distance[rows] <-
      split.object$quantile.adjust[shots.split[[prop[[1]]]]$distance[rows], rr]
  }
  
  for (bb in seasonblocks) 
    shots.in$adjusted.distance[shots.in$seasonblock==bb] <- shots.split[[bb]]$adjusted.distance
  shots.in$adjusted.distance[shots.in$adjusted.distance > 200] <- 200
  
  return(shots.in$adjusted.distance)
  
}

create.adjusted.distance <- function (sub.data, distance.adjust=NULL) {   #, split.table
  ## load ("../../nhlscrapr-probs-0.RData")
  ## sub.data=grand.data; distance.adjust=NULL
  
  message ("Correcting for Distance Anomalies.")
  sub.data$adjusted.distance <- NA
  
  etypes <- c("GOAL","SHOT","MISS")
  shot.rows <- which (sub.data$etype %in% etypes)
  shot.data <- sub.data[shot.rows,
                        c("season", "type", "ev.team", "hometeam", "awayteam", "distance")]
  #  shottypes <- unique(shot.data$type)
  if (is.null(distance.adjust)) distance.adjust <- make.adjusted.distance.homeaway.table (shot.data)
  
  dist.one <- update.adjusted.distance (shot.data, distance.adjust)
  sub.data$adjusted.distance[shot.rows] <- dist.one
  
  return(list(grand.data=sub.data,
              distance.adjust=distance.adjust))
  
}

pbp.all <- create.adjusted.distance(pbp.all)[[1]]

################################################
## Impute shot coords
################################################

impute.miss.xy <- function (grand.data) {
  #load ("ainty.RData")
  
  message ("Imputing missing shot locations using saved shots-on-goal.")
  sub.data <- grand.data[,c("etype","type","adjusted.distance","xcoord","ycoord")]
  
  etypes <- c("SHOT","MISS")   #shots and misses have the same distance distributions.
  shot.rows <- which (sub.data$etype %in% etypes)
  tiny <- sub.data[shot.rows,]
  tiny$adjusted.distance <- round(tiny$adjusted.distance)
  
  for (dd in 1:200) {
    if (dd %% 10 == 0) message ("Imputing distance from net: ", dd)
    shotset <- which(tiny$adjusted.distance == dd & !is.na(tiny$xcoord) & !is.na(tiny$ycoord)) #tiny$etype=="SHOT" & 
    missset <- which(tiny$adjusted.distance == dd & (is.na(tiny$xcoord) | is.na(tiny$ycoord)))  #tiny$etype=="MISS" & 
    
    if (length(shotset) > 0 & length(missset) > 0) {
      picks <- sample(shotset, length(missset), replace=TRUE)
      tiny$xcoord[missset] <- tiny$xcoord[picks]
      tiny$ycoord[missset] <- tiny$ycoord[picks]
    }
  }
  
  grand.data[shot.rows, c("xcoord","ycoord")] <- tiny[, c("xcoord","ycoord")]
  return(grand.data)
  
}

pbp.all <- impute.miss.xy(pbp.all)

# Create lagged events before deletion
pbp.all.clean <- pbp.all %>%
   mutate(home.event = ifelse(ev.team == hometeam, 1, 0),
         event.zone = ifelse(home.event == 1,homezone,
                            ifelse(homezone == "Def","Off","Neu")),
         
         ## Check Lag Time doesn't Cross Periods
         same.period = ifelse(gcode == lag(gcode) & period == lag(period), 1, 0),                    
         
         #Last Play Before Deletion
         lag.event = ifelse(same.period == 1, lag(etype), "Unknown"),
         lag.home.zone = ifelse(same.period == 1, lag(homezone), "Unknown"),
         
         last.event.time = ifelse(same.period == 1, seconds - lag(seconds), 0))



check <- pbp.all.clean %>% head(100000) %>% 
  select(xcoord, ycoord, etype, type, seconds,period, gcode, hometeam, homezone, ev.team, distance, adjusted.distance, 
         home.event, event.zone) 


## Plot ycoords
pbp.all.clean %>%
      tail(1000000) %>%
      filter(!is.na(ycoord) & etype %in% c("GOAL")) %>%
      ggplot(aes(x=xcoord, y=ycoord, group=etype, color=etype)) +
     #full.rink() +  
      geom_density_2d() +
      theme(panel.background = element_blank())


## Plot xcoords
pbp.all.clean %>%
  filter(!is.na(xcoord)) %>%
  ggplot(aes(x=xcoord, group=etype, color=etype)) +
  geom_density() + theme(panel.background = element_blank())

## Distance / Adjusted Distance
pbp.all.clean %>%  
  filter(etype %in% c("SHOT","GOAL","MISS")) %>%
  ggplot(aes(x=distance, y=adjusted.distance, color=xcoord, shape=event.zone)) + geom_point()

save(pbp.all.clean,file="~/Documents/CWA/Hockey Data/pbpall.RData")

############################################################################################################################################################################
########1.B LIMIT TO SHOTS AGAINST AND DEVELOP FEATURES
############################################################################################################################################################################
  load("~/Documents/CWA/Hockey Data/pbpall.RData")
  
  # Create shots dataset  1,034,072  -> 1,029,769  #### Drop misses, no x,y coordinates
  shots.all <- pbp.all.clean %>%
                # Shots Only
                filter(etype %in% c("SHOT","GOAL","MISS")) %>%
                
                # Remove Shootouts
                filter(!(period== "5" & substr(gcode,1,1) == "2")) %>%
    
               arrange(season, gcode, period, seconds) %>%
               mutate(Player = toupper(trimws(substr(ev.player.1, 3, nchar(ev.player.1)))),
                      time.index = paste0(as.character(gcode),seconds),
                      even.second = ifelse(as.numeric(seconds) %% 2 == 0,1,0),
                      season.type = ifelse(substr(gcode,1,1) == "3","PO","RS"),
                      season2 = ifelse(substr(gcode,1,1) == "3", paste0(season,"p"), paste0(season))) %>%
               
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
               
                      home.shooter = ifelse(ev.team == awayteam, 0, 1),
               
                      same.period.shot = ifelse(gcode == lag(gcode) & period == lag(period),1,0),  
                      is.Rebound = as.factor(ifelse(is.na(lag(etype)),0,
                                            ifelse(lag(etype) == "SHOT" & same.period.shot == 1 & ((seconds - lag(seconds)) <= 2),1,0))),
  
                     Shooter.Handedness = ifelse(Shoots == "L","L",
                                          ifelse(Shoots == "R","R",
                                          ifelse(is.na(Shoots),"U","U"))),
                     Player.Position = ifelse(!is.na(Player.Position), Player.Position, "U"),
                     Shooter.Handedness = ifelse(!is.na(Shooter.Handedness), Shooter.Handedness, "U")) %>%
    
            left_join(goalie.roster, by="SA.Goalie") %>%
            mutate(Catches = ifelse(is.na(Catches) | Catches == "NULL","L",Catches),
                      Handed.Class = ifelse(Shooter.Handedness == "L" & Catches == "L", "LL",
                                           ifelse(Shooter.Handedness == "L" & Catches == "R", "LR",
                                                  ifelse(Shooter.Handedness == "R" & Catches == "L", "RL",
                                                         ifelse(Shooter.Handedness == "R" & Catches == "R", "RR",
                                                                "U")))),
                     Handed.Class2 = ifelse(Handed.Class %in% c("LL","RR"),"Same",
                                            ifelse(Handed.Class %in% c("LR","RL"),"Opposite",
                                                   "U")),
                     Player.Position2 = ifelse(Player.Position == "D", "D", 
                                               ifelse(Player.Position %in% c("C","L","R","F"),"F",
                                                      "U"))) %>%
          filter(ENG == 0) %>%
          filter(Handed.Class2 != "U") 
  
  ## Re-organize  
  shots.all$shot.type <- droplevels(shots.all$shot.type)
  
  ## Check players with unknown handedness
  shots.all %>%  filter(Player.Position == "U") %>% group_by(Player, Player.Position) %>% count(cnt = n()) %>% arrange(-cnt)
  
  #####
  ### Deal with NHL PBP coordinates, see: http://hockeyanalytics.com/Research_files/SQ-RS0910-Krzywicki.pdf                                   
  shots.all.coords <- shots.all %>%
              filter(xcoord != "NA") %>%
              group_by(season.type, season, gcode, period, hometeam, home.shooter) %>%
              summarise(median_xcoord = median(xcoord),
                        mean_xcoord = mean(xcoord),
                        sample = n()) %>%
              #mutate(standardized.xcoord = ifelse(season.type == "RS" & period %in% c(2) , mean_xcoord * -1, mean_xcoord)) %>%
              #group_by(season.type, season, gcode, hometeam, home.shooter) %>%
              #summarise(standardized.xcoord = weighted.mean(standardized.xcoord, w = sample)) %>%
              dcast(season.type + season + gcode + hometeam + period ~ home.shooter, value.var = "median_xcoord") %>%
              mutate(#home.align.east = ifelse(`1` > `0`, 1, 0),
                     period.flip.away = ifelse(`0` < 0, 1, 0),
                     period.flip.home = ifelse(`1` < 0, 1, 0)) %>%
              #select(season, gcode, hometeam, home.align.east) %>%
              
              ### Home Alignment set, merge back on all shot data
              right_join(shots.all, by=c("season.type","season","gcode","hometeam","period")) %>%
    
              mutate(### Flip coords
                     #flip.coords = ifelse((season.type == "RS" & home.align.east == 1 & home.shooter == 1 & period %in% c(2)) |
                    #                      (season.type == "RS" & home.align.east == 1 & home.shooter == 0 & period %in% c(1,3,4)) |
                     #                     (season.type == "RS" & home.align.east == 0 & home.shooter == 0 & period %in% c(2)) |
                      #                    (season.type == "RS" & home.align.east == 0 & home.shooter == 1 & period %in% c(1,3,4)) |
                       #                   (season.type == "PO" & home.align.east == 1 & home.shooter == 1 & (period %% 2 == 0)) |
                        #                  (season.type == "PO" & home.align.east == 1 & home.shooter == 0 & (period %% 2 != 0)) |
                         #                 (season.type == "PO" & home.align.east == 0 & home.shooter == 0 & (period %% 2 == 0)) |
                          #                (season.type == "PO" & home.align.east == 0 & home.shooter == 1 & (period %% 2 != 0)), 1, 0),
                    
                     # Flag non ozone shots
                     #shot.outside.blueline = ifelse((home.shooter == 1 & homezone == "Off") || (home.shooter == 0 & homezone == "Def"), 0, 1),
                     
                  
                     # Standardize coordinates
                     XC = ifelse(period.flip.away == 1 & home.shooter == 0 | 
                                 period.flip.home == 1 & home.shooter == 1,xcoord * -1, xcoord),
  
                     YC = ifelse(period.flip.away == 1 & home.shooter == 0 | 
                                 period.flip.home == 1 & home.shooter == 1,ycoord * -1, ycoord),
                     
                     # Original calculation
                     shot.dist = sqrt((89 - XC)**2 + (YC ** 2)),
                     shot.angle = atan(abs(89 - XC) / abs(0 - YC)) * (180 / pi),
                     
                     # Alternate calculation
                     flip.x.coord = ifelse(abs(shot.dist - adjusted.distance) > 10, 1, 0), 
                     shot.dist = ifelse(flip.x.coord == 1, sqrt((89 + XC)**2 + (YC ** 2)), shot.dist),
  
                     # Check update, default to NHL calculation
                     nhl.distance.used = ifelse((abs(shot.dist - adjusted.distance) > 5) | is.na(shot.dist), 1, 0),
                     shot.dist = ifelse(nhl.distance.used == 1, adjusted.distance, shot.dist),
                     
                     adjusted.distance1 = adjusted.distance,
                     distance = shot.dist,
                     
                     distance.bucket = round(shot.dist,0),
                     
                     # Flip y coord to determine left-hand side shot
                     LS.shot = ifelse((flip.x.coord == 0 & YC < 0) | (flip.x.coord == 1 & YC > 0), 1, 0),
                     LS.shot = ifelse(is.na(LS.shot),0, LS.shot),
                     
                     Off.Hand.Shot = ifelse((LS.shot == 1 & Shooter.Handedness == "R") | (LS.shot == 0 & Shooter.Handedness == "L"),1,0))
  
  
  aggregate(xcoord ~ home.shooter + period, data=shots.all.coords, FUN=median)
  aggregate(XC ~ home.shooter + period + season.type, data=shots.all.coords, FUN=median)
  aggregate(shot.dist ~ period, data=shots.all.coords, FUN=mean)
  
  
  check.coords <- shots.all.coords %>% select(season, gcode, Player, hometeam, awayteam, ev.team, period, period.flip.home,period.flip.away, home.shooter, homezone, season, etype,  home.score, xcoord, ycoord, XC, YC, distance
                                              ,shot.angle,shot.dist,Off.Hand.Shot, LS.shot, Shooter.Handedness,homezone,
                                              homezone,flip.x.coord, nhl.distance.used, adjusted.distance) %>% tail(100000)
  
  
  shots.all.coords2 <- create.adjusted.distance(shots.all.coords)[[1]]
  
  shots.all.coords2 %>%
      head(100000) %>%
      ggplot(aes(x=adjusted.distance1, y=adjusted.distance, color=hometeam)) +
      geom_point()
  
  
  shots.all.coords2 %>% group_by(season2) %>% select(XC) %>% count()
  
  ################################################
  # Impute Shot Angle
  ################################################
  
  library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2); library(MLmetrics)
  set.seed(1234)  
  
  shot.angle.impute.data <- shots.all.coords2 %>%
              filter(!is.na(XC))
  
  YC.impute.model <- glm(data=shot.angle.impute.data, 
                         abs(YC) ~ adjusted.distance + Off.Hand.Shot + LS.shot + shot.type + Player.Position)
  
  YC.impute.model %>% summary()
  
  # R-squared
  1 - (YC.impute.model$deviance / YC.impute.model$null.deviance)
  #0.2704556
  
  # Mean Absolute Error
  mean(abs(predict(YC.impute.model, shot.angle.impute.data) - shot.angle.impute.data$YC))
  # 19.60633
  
  # RMSE
  sqrt(mean((predict(YC.impute.model, shot.angle.impute.data) - shot.angle.impute.data$YC)^2))
  #25.52792
    
  ## Predict Shot Angle
  pred.YC <- predict(YC.impute.model, shots.all.coords2) 
  
  ## Combine Data and Impute
  shots.all.impute1 <- cbind(shots.all.coords2,pred.YC) %>%
             mutate(imputed.shot.angle = ifelse(is.na(shot.angle), 1, 0),
                    shot.angle = ifelse(imputed.shot.angle == 1, acos(min(1,abs(pred.YC) / adjusted.distance)) * (180 / pi), shot.angle))
  
  shots.all.impute1 %>% select(shot.angle) %>% na.omit() %>% count()
  
  ################################################
  # Slide last shot coordinates
  ################################################
  
  shots.all.lagged <- shots.all.impute1 %>%
          arrange(season, gcode, period, seconds) %>%
          mutate(shot.dist.unadj = shot.dist,
                 shot.dist = adjusted.distance,
                 last.shot.dist = ifelse(same.period.shot == 1, lag(shot.dist), NA),
                 last.shot.angle = ifelse(same.period.shot == 1, lag(shot.angle), NA),
                 last.LS.shot = ifelse(same.period.shot == 1, lag(LS.shot), 0),
                 zone.shot = ifelse(home.shooter == 0 & homezone == "Def", "Off",
                                     ifelse(home.shooter == 0 & homezone == "Off", "Def",
                                            ifelse(home.shooter == 1 & homezone == "Off", "Off",
                                                   ifelse(home.shooter == 1 & homezone == "Def", "Def",
                                                          "Neu")))),
                 last.zone = ifelse(home.shooter == 0 & lag.home.zone == "Def", "Off",
                                     ifelse(home.shooter == 0 & lag.home.zone == "Off", "Def",
                                            ifelse(home.shooter == 1 & lag.home.zone == "Off", "Off",
                                                   ifelse(home.shooter == 1 & lag.home.zone == "Def", "Def",
                                                          "Neu")))),
                  is.Rush = as.factor(ifelse(last.event.time <= 6 & zone.shot != last.zone,1,0)),
                 
                  #is.Rebound = as.numeric(is.Rebound) - 1,
                  
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
                  LN.last.event.time = ifelse(last.event.time > 0, log(last.event.time), 0),
                 
                  time.last.off.faceoff = last.off.faceoff * LN.last.event.time,
                  time.last.def.faceoff = last.def.faceoff * LN.last.event.time,
                  time.last.neu.faceoff = last.neu.faceoff * LN.last.event.time,
                  time.last.off.shot = last.off.shot * LN.last.event.time,
                  time.last.def.shot = last.def.shot * LN.last.event.time,
                  time.last.neu.shot = last.neu.shot * LN.last.event.time,
                  time.last.off.give = last.off.give * LN.last.event.time,
                  time.last.def.give = last.def.give * LN.last.event.time,
                  time.last.neu.give = last.neu.give * LN.last.event.time,
                  
                  # Distance from last event
                  #dist.last.event = ifelse(is.na(lag.XC),0,sqrt((XC - lag.XC)**2 + (YC - lag.YC)**2)),
                  
                  # Speed last event
                  #speed.last.event = dist.last.event / last.event.time,
                  
                  Rebound.Angle.Change = ifelse(is.Rebound == 0, 0, 
                                         ifelse(LS.shot == last.LS.shot, abs(shot.angle - last.shot.angle),
                                                ifelse(LS.shot != last.LS.shot, 180 - shot.angle - last.shot.angle, 0))),
           Rebound.Second.Change = ifelse(is.Rebound == 0, 0, 
                                   ifelse(seconds - lag(seconds) > 2, 2, 
                                          seconds - lag(seconds))),
                                          
                                          
           Rebound.Distance = ifelse(is.Rebound == 0, 0, shot.dist + last.shot.dist),
           Rebound.Angle.Distance = ifelse(is.Rebound == 0, 0, Rebound.Angle.Change / Rebound.Distance),
           Rebound.Angle.Second = ifelse(is.Rebound == 0, 0, Rebound.Angle.Change / Rebound.Second.Change),
  
          # Transformations 
           LN.Rebound.Angle.Distance = ifelse(Rebound.Angle.Distance > 0, log(Rebound.Angle.Distance), 0),
           #LN.Rebound.Angle.Second = ifelse(Rebound.Angle.Second > 0,log(Rebound.Angle.Second), 0),
            shot.dist.pow2 = shot.dist ** 2,
           shot.dist.pow3 = shot.dist ** 3,
           shot.angle.pow2 = shot.angle ** 2,
           shot.angle.pow3 = shot.angle ** 3,
           Rink.Year = paste0(hometeam, season))
  
  #shots.all.lagged %>% filter(LN.last.event.time > 0) %>% ggplot(aes(x=LN.last.event.time)) + geom_density()
  #shots.all.lagged %>% select(LN.last.event.time) %>% na.omit() %>% count()
  check.vars <- shots.all.lagged %>% select(Rebound.Angle.Second,LN.last.event.time ,Rebound.Angle.Change,LN.Rebound.Angle.Distance, Rebound.Angle.Distance,Rebound.Distance, Rebound.Second.Change) %>% head(10000)
  
  # Check counts of factors
  aggregate(goal ~ gamestate, data = shots.all.lagged, FUN = length)
  aggregate(goal ~ shot.type, data = shots.all.lagged, FUN = length)
  aggregate(distance ~ season, data = shots.all.lagged, FUN = mean)
  distance.adj.check <- shots.all.lagged %>% 
    group_by(hometeam, season) %>% 
    summarise(mean(shot.dist),mean(shot.dist.unadj))
  aggregate(goal ~ season, data = shots.all.lagged, FUN = length)
  #aggregate(distance ~ SA.Goalie, data = shots.all2, FUN = length)
  
  # Check even distribution
  sum(shots.all.lagged$even.second) / length(shots.all.lagged$even.second) #0.498641
  
  ############################################################################################################################################################################
  ########1.C CALCULATE SHOOTER SKILL (not including blocked/missed shots)
  ############################################################################################################################################################################
  
  # Find player-level shooting percentage
  shooter.skill <- shots.all.lagged %>%
    select(Player, goal) %>%
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
  
  shots.all.reg <- shots.all.lagged %>%
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
        
        print(sapply(sapply(model.data, unique),length))
        
        print(sapply(model.data, class))
        
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
  
  # Check variable distributions
  shots.all.reg %>% ggplot(aes(x=shot.dist)) + geom_density()
  shots.all.reg %>% ggplot(aes(x=shot.angle)) + geom_density()
  shots.all.reg %>% ggplot(aes(x=Rebound.Angle.Second)) + geom_density()
  shots.all.reg %>% ggplot(aes(x=time.last.off.shot)) + geom_density()
  shots.all.reg %>% ggplot(aes(x=time.last.def.shot)) + geom_density()
  shots.all.reg %>% ggplot(aes(x=regressed.shooting.skill.index)) + geom_density()
  shots.all.reg %>% ggplot(aes(x=LN.Rebound.Angle.Distance)) + geom_density()
  shots.all.reg %>% group_by(gamestate) %>% count()
  shots.all.reg %>% group_by(last.off.shot) %>% count()
  shots.all.reg %>% group_by(last.def.shot) %>% count()
  shots.all.reg %>% group_by(last.off.give) %>% count()
  shots.all.reg %>% group_by(last.def.give) %>% count()
  shots.all.reg %>% group_by(last.neu.give) %>% count()
  shots.all.reg %>% group_by(is.Rebound1) %>% count()
  
  
  predicted.goal.model <- lm.cv.10(shots.all.reg,
                                
                                   c("goal",
                                     "shot.dist",
                                     "shot.dist.pow2",
                                     "shot.dist.pow3",
                                     
                                  "shot.angle",
                                  "shot.angle.pow2",
                                  "shot.angle.pow3",
                                  "is.Rush", 
                                  "is.Rebound",
                                  "LN.Rebound.Angle.Distance",
                                  #"last.event.time", Bad Variable
                                  "time.last.off.shot", "last.off.shot",
                                  "time.last.def.shot", "last.def.shot",
                                  "time.last.off.give", "last.off.give",
                                  "time.last.def.give", "last.def.give",
                                  "time.last.neu.give", "last.neu.give",
                                  "shot.type","gamestate","regressed.shooting.skill.index",
                                  "Player.Position2",#"Off.Hand.Shot", Bad Variable
                                  "Handed.Class2"
                                  
                                  #"speed.last.event", Bad Variable
                                 #"dist.last.event" Can't impute
                                  ),  
                      
                                c("season","season2","gcode","period","seconds","ev.team","etype","awayteam","hometeam","away.G","home.G","Player",
                                  "even.second","SA.Goalie","time.index","refdate","event","LS.shot","Shooter.Handedness",
                                  "a1", "a2", "a3", "a4", "a5", "a6", "h1", "h2", "h3", "h4", "h5", "h6"))   
  
  # Summary best plot
  best.model.no <- predicted.goal.model[[2]] %>% filter(r.squared == max(r.squared)) %>% select(.id) %>% as.character()
  best.model <- predicted.goal.model[[1]]$model[[paste0(as.numeric(best.model.no))]] 
  best.model %>% summary()
  
  # Confusion Matrix
  predicted.goal.model[[4]]
  #FALSE   TRUE
  #0 664513 309169
  #1  19368  46069
  
  
  predicted.goal.model[[6]]
  
  predicted.goal.model[[7]]
  
  
  # Classification Plot
  predicted.goal.model[[3]]
  
  # R-squared
  1 - (best.model$deviance / best.model$null.deviance)
  #  0.1002869
  # 0.1123704
  
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
    geom_line(aes(x=season, y=xG,group=1),color="blue") +
    geom_line(aes(x=season, y=goals,group=1),color="green") +
    theme(panel.background = element_blank()) +
    labs(title="Actual Goals (Green) v Expected Goals (Blue) by Season", x="Season",y="")
    
  
  ########################################################################
  #########SCORE DATA AND ADJUST FOR REBOUNDS
  ########################################################################
  # Output Predicted
  xG.raw <- predict(best.model, shots.all.reg, type='response')
  
  # Adjust for rebounds
  scored.data.raw <- cbind(xG.raw,shots.all.reg)
  
  scored.data <- scored.data.raw %>%
    arrange(season, gcode, seconds) %>%
    mutate(shot.results.inRebound = ifelse(is.na(lead(period)),0,
                                    ifelse(lead(period) == period & lead(gcode) == gcode & lead(is.Rebound) == 1, 1, 0)),
           xG = xG.raw,
           xG.team = ifelse(is.Rebound == 0, xG.raw,
                     ifelse(is.Rebound == 1 & lag(is.Rebound) == 0, xG.raw * (1-lag(xG.raw)),
                     ifelse(is.Rebound == 1 & lag(is.Rebound) == 1 & lag(is.Rebound,2) == 0,
                                      xG.raw * (1-lag(xG.raw)) * (1-lag(xG.raw,2)),
                     ifelse(is.Rebound == 1 & lag(is.Rebound) == 1 & lag(is.Rebound,2) == 1 & lag(is.Rebound,3) == 0,
                                   xG.raw * (1-lag(xG.raw)) * (1-lag(xG.raw,2)) * (1-lag(xG.raw,3)),
                     ifelse(is.Rebound == 1 & lag(is.Rebound) == 1 & lag(is.Rebound,2) == 1 & lag(is.Rebound,3) == 1 & lag(is.Rebound,4) == 0,
                                   xG.raw * (1-lag(xG.raw)) * (1-lag(xG.raw,2)) * (1-lag(xG.raw,3)) * (1-lag(xG.raw,4)),
                                   xG.raw * (1-lag(xG.raw)) * (1-lag(xG.raw,2)) * (1-lag(xG.raw,3)) * (1-lag(xG.raw,4))))))))
  
  ## Bucket Sh% and xSh%
  scored.data %>% 
    mutate(xG_bucket = round(xG,1)) %>%
    group_by(xG_bucket) %>% 
    summarise(xG_shot=sum(xG) / n(), goals_shot=sum(as.numeric(goal)-1) / n(), shots=n()) %>%
    ggplot() +
    #geom_boxplot(aes(x=xG_bucket, y=goals_shot, group= xG_bucket))
    geom_point(aes(x=xG_bucket, y=goals_shot,size=shots))  
  
  #####################
  ### Error Checks
  #####################
  
  ### Log Loss
  library(MLmetrics)
  LogLoss(scored.data$xG,as.numeric(scored.data$goal)-1)
  # 0.2036125
  # 0.2583331
  
  error <- scored.data$xG-(as.numeric(scored.data$goal)-1)
  
  ## Brier Score - Base Line (goal <- 0): 0.08919192
  mean(error^2)
  # 0.05335231
  # 0.07227214
  
  # Mean Absolute Error
  mean(abs(error))
  # 0.1065062
  # 0.1438155
  
  # RMSE
  sqrt(mean(error^2))
  #0.2309812
  #0.2688348
  
  
  # View by season
  scored.data %>% 
    group_by(season2) %>% 
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
  ########PREDICT REBOUND
  ############################################################################################################################################################################
  
  rebound.model.data <- scored.data %>% filter(goal == 0)
  
  rebound.model <- glm(data = rebound.model.data,
            shot.results.inRebound ~ xG + shot.dist + shot.dist.pow2 + shot.dist.pow3 + shot.angle + Off.Hand.Shot + shot.type + Player.Position2 +
            time.last.off.shot + gamestate, family="binomial")
  
  
  rebound.model %>% summary()
  # R-squared
  1 - (rebound.model$deviance / rebound.model$null.deviance)
  #0.03356342
  
  ## Predict Shot Angle
  pred.rebound <- predict(rebound.model, scored.data, type='response') 
  
  ## Combine Data and Impute
  scored.data <- cbind(scored.data,pred.rebound) 
  
  ### Log Loss
  library(MLmetrics)
  LogLoss(scored.data$pred.rebound,scored.data$shot.results.inRebound)
  #0.1520091
  
  rb.error <- scored.data$pred.rebound-scored.data$shot.results.inRebound
  
  ## Brier Score - Base Line (goal <- 0): 0.08919192
  mean(rb.error^2)
  # 0.03469594
  
  # Mean Absolute Error
  mean(abs(rb.error))
  # 0.07233442
  
  # RMSE
  sqrt(mean(rb.error^2))
  #0.1860957
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
          group_by(SA.Goalie, season, season2) %>%
          summarise(shots = n())
    
    # Count cumulative numbers by goalie
    goalie.set <- goalie.set %>%
          mutate(Game.ID = as.character(gcode),
                 SA = 1,
                 GA = as.numeric(goalie.set$goal)-1) %>%
          select(SA.Goalie, season,  season2, Game.ID, seconds, goal, xG, SA, GA) %>%
          group_by(SA.Goalie) %>%
          arrange(season, season2, Game.ID, seconds) %>%
          #Cumulative Counts
          mutate(cum.xG = cumsum(xG),
                 cum.Goals = cumsum(GA),
                 cum.Shots = cumsum(SA),
                 QREAM = cum.xG - cum.Goals)
    
    # Calculate game shots
    game.shots <- goalie.set %>%
      group_by(SA.Goalie, season, season2, Game.ID) %>%
      summarise(game.SA=sum(SA), 
                game.GA=sum(GA),
                game.xGA=sum(xG))
    
    # Keep last shot of each game and combine
    goalie.game <- goalie.set %>%
          group_by(SA.Goalie, season, season2, Game.ID) %>%
          do(tail(., n=1)) %>%
      left_join(game.shots, by=c("SA.Goalie","season","season2","Game.ID")) %>%
      arrange(SA.Goalie, season, season2, Game.ID) %>%
      select(SA.Goalie, season, season2, QREAM, Game.ID, game.SA, game.xGA, game.GA, cum.Shots, cum.Goals , cum.xG)
    
    # Return dataset of goalie-game level data for selected goalies and seasons
    return(goalie.game)
    
  }
  
  # Plot all goalies xG lift
  xG.plot.fun <- function(goalies, seasons, data) {
    
    library(ggplot2); library(dplyr); library(ggrepel)
    
    # Subset goalies to highlight  
    select.goalies <- data %>%
        filter(SA.Goalie %in% goalies & season %in% seasons) %>%
        arrange(SA.Goalie, season, season2, Game.ID)
    
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
      geom_line(data=select.goalies,size=1.5,aes(x=cum.Shots,y=QREAM,color=as.factor(select.goalies$SA.Goalie))) +
      geom_line(data=select.goalies,size=0.3,aes(x=cum.Shots,y=QREAM)) +
      geom_point(data=select.goalies,size=0.75,aes(x=cum.Shots,y=QREAM,shape=as.factor(select.goalies$season2))) + 
      labs(shape ="Season") +
      labs(color="Goalie") +
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
  
  goalie.plot(c("CAM TALBOT","PEKKA RINNE", "JAKE ALLEN","JOHN GIBSON","HENRIK LUNDQVIST","MARC-ANDRE FLEURY","BRADEN HOLTBY","CRAIG ANDERSON"),c("20162017"))[[1]]
  
goalie.plot(c("CAM TALBOT","MARTIN JONES", "COREY CRAWFORD","PEKKA RINNE", "DEVAN DUBNYK", "JAKE ALLEN",
              "BRIAN ELLIOTT","JOHN GIBSON"),
            c("20162017"))[[1]]

goalie.plot(c("CAREY PRICE","HENRIK LUNDQVIST","MARC-ANDRE FLEURY", "SERGEI BOBROVSKY", "FREDERIK ANDERSEN","BRADEN HOLTBY",
              "CRAIG ANDERSON","TUUKKA RASK"),
            c("20162017"))[[1]]

goalie.plot(c("CAM TALBOT","PEKKA RINNE", "JAKE ALLEN","JOHN GIBSON"),c("20162017"))[[1]]


goalie.plot(c("CAREY PRICE","HENRIK LUNDQVIST","MARC-ANDRE FLEURY", "SERGEI BOBROVSKY", "FREDERIK ANDERSEN","BRADEN HOLTBY",
              "CRAIG ANDERSON","TUUKKA RASK"),
            c("20162017"))[[1]]


goalie.plot(c("CAM TALBOT"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("HENRIK LUNDQVIST","CORY SCHNEIDER","TIM THOMAS","ROBERTO LUONGO","RYAN MILLER","BRADEN HOLTBY","MIKE SMITH","KARI LEHTONEN","CRAIG ANDERSON",
              "STEVE MASON","MARC-ANDRE FLEURY","TUUKKA RASK","CAREY PRICE","CAM WARD","TOMAS VOKOUN","PEKKA RINNE","COREY CRAWFORD","JONATHAN QUICK"))[[1]]

goalie.plot(c("CAREY PRICE","CAM TALBOT","BRADEN HOLTBY","SERGEI BOBROVSKY", "COREY CRAWFORD","DEVAN DUBNYK","TUUKKA RASK","MARTIN JONES","ROBIN LEHNER",
              "CAM WARD","JAKE ALLEN","STEVE MASON","JOHN GIBSON","HENRIK LUNDQVIST","PEKKA RINNE","MIKE SMITH",
              "FREDERIK ANDERSEN","CORY SCHNEIDER","RYAN MILLER","MATTHEW MURRAY"),
            c("20162017"))[[1]]

goalie.plot(c("MARTIN JONES","THOMAS GREISS","COREY CRAWFORD","DEVAN DUBNYK","PEKKA RINNE","ROBIN LEHNER",
              "JAMES REIMER","FREDERIK ANDERSEN"))[[1]]

goalie.plot(c("EDDIE LACK","CAM WARD"),c("20162017"))[[1]]


goalie.plot(c("PEKKA RINNE", "JAKE ALLEN"),
            c("20162017"))[[1]]


goalie.plot(c("ROBERTO LUONGO"))[[1]]
goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("BEN BISHOP","BRIAN ELLIOTT","STEVE MASON","SCOTT DARLING","PHILIPP GRUBAUER"),c("20162017"))[[1]]

goalie.plot(c("TUUKKA RASK"),c("20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","JONATHAN BERNIER","JAMES REIMER","KARRI RAMO","JHONAS ENROTH"),c("20152016","20162017"))[[1]]

#goalie.plot(c("HENRIK LUNDQVIST","STEVE MASON","CORY SCHNEIDER"),c("20122013","20132014","20142015","20152016","20162017"))[[1]]

#goalie.plot(c("DEVAN DUBNYK","CAREY PRICE","COREY CRAWFORD","BRADEN HOLTBY","SERGEI BOBROVSKY","JAROSLAV HALAK"),c("20152016","20162017"))[[1]]

#goalie.plot(c("CAREY PRICE","COREY CRAWFORD","CORY SCHNEIDER","JAROSLAV HALAK","CAM WARD","JONATHAN QUICK","MARC-ANDRE FLEURY",
#              "PEKKA RINNE","TUUKKA RASK","HENRIK LUNDQVIST","ROBERTO LUONGO","RYAN MILLER","STEVE MASON"))[[1]]

#goalie.plot(c("CAREY PRICE","COREY CRAWFORD","CORY SCHNEIDER","JAROSLAV HALAK","CAM WARD","JONATHAN QUICK","MARC-ANDRE FLEURY",
#              "PEKKA RINNE","TUUKKA RASK","HENRIK LUNDQVIST","ROBERTO LUONGO","RYAN MILLER","STEVE MASON"),
#            c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("ANDREW HAMMOND","MIKE CONDON","CRAIG ANDERSON"),c("20162017"))[[1]]

goalie.plot(c("TUUKKA RASK","CRAIG ANDERSON"),c("20142015","20152016","20162017"))[[1]]
goalie.plot(c("HENRIK LUNDQVIST","CAREY PRICE"),c("20112012","20122013","20142015","20152016","20162017"))[[1]]
goalie.plot(c("MATTHEW MURRAY","SERGEI BOBROVSKY","MARC-ANDRE FLEURY"),c("20152016","20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","CRAIG ANDERSON"),c("20162017"))[[1]]
goalie.plot(c("JAKE ALLEN","BRIAN ELLIOTT"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONAS GUSTAVSSON","CAM TALBOT","LAURENT BROSSOIT","ANDERS NILSSON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("COREY CRAWFORD","DEVAN DUBNYK"),c("20112012","20122013","20142015","20152016","20162017"))[[1]]

goalie.plot(c("FREDERIK ANDERSEN","ANTTI RAANTA","COREY CRAWFORD","PEKKA RINNE","HENRIK LUNDQVIST","CAM TALBOT"))[[1]]

#goalie.plot(c("TUUKKA RASK","JONAS GUSTAVSSON","ANTON KHUDOBIN","MALCOLM SUBBAN","CHAD JOHNSON","ZANE MCINTYRE",
#              "NIKLAS SVEDBERG"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("JOHN GIBSON","JONATHAN BERNIER"),c("20162017"))[[1]]
goalie.plot(c("CAM WARD"),c("20162017"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ANDREW HAMMOND"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONATHAN QUICK","PETER BUDAJ"),c("20152016","20162017"))[[1]]
goalie.plot(c("FREDERIK ANDERSEN","GARRET SPARKS","CURTIS MCELHINNEY"),c("20162017"))[[1]]

#goalie.plot(c("ANTTI NIEMI","KARI LEHTONEN"),c("20152016","20162017"))[[1]]

goalie.plot(c("COREY CRAWFORD","BRADEN HOLTBY","TUUKKA RASK","CAREY PRICE"),c("20162017"))[[1]]

goalie.plot(c("DEVAN DUBNYK","JAKE ALLEN"),c("20162017"))[[1]]

#goalie.plot(c("PETR MRAZEK","JIMMY HOWARD"),c("20112012","20122013","20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("PEKKA RINNE"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("SCOTT DARLING"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("CAM TALBOT","SERGEI BOBROVSKY","BRADEN HOLTBY"),c("20152016","20162017"))[[1]]
goalie.plot(c("MARTIN JONES"),c("20132014","20142015"))[[1]]

goalie.plot(c("EDDIE LACK"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("JAROSLAV HALAK","THOMAS GREISS","JEAN-FRANCOIS BERUBE"),c("20162017"))[[1]]

goalie.plot(c("ANTTI NIEMI","KARI LEHTONEN"),c("20162017"))[[1]]


goalie.plot(c("SERGEI BOBROVSKY","BRADEN HOLTBY","SCOTT DARLING","COREY CRAWFORD"),c("20162017"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ZANE MCINTYRE"),c("20162017"))[[1]]

goalie.plot(c("PEKKA RINNE","JUUSE SAROS","SCOTT DARLING","COREY CRAWFORD","JAKE ALLEN","CARTER HUTTON",
              "DEVAN DUBNYK","DARCY KUEMPER","ANTTI NIEMI","KARI LEHTONEN"),c("20162017"))[[1]]

goalie.plot(c("ONDREJ PAVELEC","CONNOR HELLEBUYCK","MICHAEL HUTCHINSON"),c("20152016","20162017"))[[1]]

goalie.plot(c("CRAIG ANDERSON","MIKE CONDON","ANDREW HAMMOND"),c("20152016","20162017"))[[1]]

goalie.plot(c("ANDREW HAMMOND","MIKE CONDON"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("RYAN MILLER","JACOB MARKSTROM"),c("20152016","20162017"))[[1]]
goalie.plot(c("RYAN MILLER","JACOB MARKSTROM"))[[1]]

goalie.plot(c("MIKE SMITH"),c("20112012"))[[1]]
goalie.plot(c("ROBIN LEHNER","ANDERS NILSSON"),c("20142015","20152016","20162017"))[[1]]


goalie.plot(c("CAM TALBOT","JOHN GIBSON","JONATHAN BERNIER"),c("20162017"))[[1]]

goalie.plot(c("CHAD JOHNSON","BRIAN ELLIOTT"),c("20162017"))[[1]]

goalie.plot(c("JONATHAN QUICK","BEN BISHOP","CHAD JOHNSON","BRIAN ELLIOTT","PETER BUDAJ"),c("20152016","20162017"))[[1]]

goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("ANTTI RAANTA","SCOTT DARLING"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot("BRIAN ELLIOTT")[[1]]


goalie.plot(c("AARON DELL","MARTIN JONES","JONATHAN QUICK","PETER BUDAJ","JOHN GIBSON","JONATHAN BERNIER"),c("20162017"))[[1]]

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

  goalie.set <- scored.data %>%
    filter(season2 %in% seasons) %>%
    #left_join(G.handedness, by="SA.Goalie") %>%
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
           Danger.Band = ifelse(xG <= quantile(scored.data$xG, c(.33, .66))[1], "Low Danger",
                                  ifelse(xG <= quantile(scored.data$xG, c(.33, .66))[2], "Middle Danger",
                                         "High Danger")),
           Time.Band = ifelse(floor(seconds / 60) %in% c(0,1,20,21,40,41),"First2Minutes",
                       ifelse(floor(seconds / 60) %in% c(18,19,38,39),"Last2Minutes-P1&P2",
                       ifelse(floor(seconds / 60) %in% c(59,59),"Last2Minutes-P3",
                        ifelse(period > 3,"Overtime",
                                     "Other")))),
           
           Rebound.Class = ifelse(is.Rebound == 0, 'First Shot',
                           ifelse(lag(pred.rebound) <= quantile(scored.data$pred.rebound, c(.33, .66))[1], "Poor Rebound",
                           ifelse(lag(pred.rebound) <= quantile(scored.data$pred.rebound, c(.33, .66))[2], "Possible Rebound",
                                                                          "Probable Rebound"))),
           Home.Period.Matrix = paste0(Goalie.Rink,"-P",period),
           Long.Change = ifelse(Home.Period.Matrix %in% c("Home-P2","Home-P4"), "HomeLongChange",
                         ifelse(Home.Period.Matrix %in% c("Away-P2","Away-P4"), "AwayLongChange",
                                "None")),
           Shot.Class = ifelse(etype == "MISS","AllAttempts","OnNet"),
           Shot.Class.Matrix = ifelse(Shot.Class =="AllAttempts" & Goalie.Rink == "Home", "AllAttempts-Home",
                               ifelse(Shot.Class =="AllAttempts" & Goalie.Rink == "Away", "AllAttempts-Away",
                               ifelse(Shot.Class =="OnNet" & Goalie.Rink == "Home", "OnNet-Home",
                               ifelse(Shot.Class =="OnNet" & Goalie.Rink == "Away", "OnNet-Away","")))),
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
    select(SA.Goalie, season2, SA.Team, Goalie, seconds, goal, xG, SA, GA, even.second, Split) %>%
    group_by(SA.Goalie, season2, Goalie, SA.Team, Split, even.second) %>%
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
    group_by(SA.Goalie, Goalie, season2, Split) %>%
    summarise(split.QREAM = sum(split.QREAM),
              split.shots = sum(split.shots),
             total.QREAM = max(total.QREAM),
             total.shots = max(total.shots)) %>%
    mutate(split.shot.share = paste0(round(split.shots / total.shots,2)*100,"%")) %>%
    arrange(total.QREAM) 
  
team.trend <- goalie.season.splits %>%
      filter(Split != "U") %>%
      group_by(season2, SA.Team, Split) %>%
      summarise(team.shots.against = sum(split.shots),
                team.xG.Lift = sum(split.QREAM),
                team.xG.Lift.per.100shots = sum(split.QREAM) / (sum(split.shots) / 100))

team.trend <- team.trend %>%
        group_by(season2, Split) %>%
        summarise(league.shots.against = sum(team.shots.against),
                  league.xG.Lift.per.100shots = sum(team.xG.Lift) / (sum(team.shots.against) / 100)) %>%
        left_join(team.trend, by = c("season2","Split")) %>%
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
            dcast(SA.Goalie + total.split.shots ~ Time, value.var = "split.QREAM.100", fun.aggregate=mean) %>%
            filter(total.split.shots > 100) %>%
            select(total.split.shots, One, Zero)
        
            wt.cor <- boot::corr(split.cor[c("One","Zero")], w=split.cor$total.split.shots)
        

        cor.string <- paste0(i, " splits normalized by shots have a intra-season correlation of ", round(wt.cor,2),"\n")
        
        cor.string.df[i,1] <- cor.string
     }
    print(cor.string.df)
    
  }

plot <- goalie.season %>%
    filter(total.shots >= shot.min & Split != "U") %>%
    ggplot(aes(x=reorder(SA.Goalie, total.QREAM), y=split.QREAM, fill=Split, label=Goalie)) +
    geom_bar(stat="identity", colour="white") +
    facet_wrap( ~ season2, ncol = 2) +
    geom_text(aes(label=split.shot.share),size = 2, position = position_stack(vjust = 0.5)) +
    #geom_text(aes(x=reorder(SA.Goalie, total.QREAM), y=split.QREAM, label=split.shot.share),size = 2, position = position_dodge(1)) +
    geom_point(aes(x=reorder(SA.Goalie, total.QREAM), y=total.QREAM)) +
   # geom_text(size = 2, position = position_stack(vjust = 0.5)) +
    #scale_fill_gradient2(low="white",mid="light grey",high="red") +
    theme(panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
    coord_flip() +
    labs(title=paste0("Goaltending Performance (QREAM*) Splits by ",desc, "\n*Quality Rules Everything Around Me: Expected Goals Against - Actual Goals - Minimum ", shot.min, " Shots", "\n@CrowdScoutSprts xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
         x="Goalie", y="QREAM: Expected Goals Against - Actual Goals", fill=paste0(desc,"\nSplits")) +
    annotate("text", x = 7, y = (max(goalie.season$split.QREAM) * 0.5), hjust=0, 
             label = paste0("Share of Splits Shots Labelled")) 

  return(list(plot, team.trend))

}

goalie.splits(c("20152016","20162017"), 1500, "Time.Band", "Period Time") [[1]]

goalie.splits(c("20162017"), 1500, "Shot.Class.Matrix", "Shot Class & Venue") [[1]]


goalie.splits(c("20152016","20162017"), 2500, "Side.Hand.Matrix", "Shot Side, Goalie & Shooter Handedness") [[1]]

goalie.splits(c("20162017"), 750, "Rebound.Class", "Rebound Type") [[1]]
goalie.splits(c("20152016","20162017"), 1500, "Rebound.Class", "Rebound Type") [[2]]


goalie.splits(c("20162017"), 1000, "Danger.Band", "Danger Bins") [[1]]

goalie.splits(c("20162017","20162017p"), 750, "Danger.Band", "Danger Bins") [[1]]



goalie.splits(c("20162017"), 750, "shot.type", "Shot Type") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "Shooter.Handedness", "Shooter Handedness") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "is.Rebound", "Rebound Shot") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "is.Rush", "Rush Shot") [[1]]

goalie.splits("20162017", 800, "period", "Period") [[1]]

goalie.splits("20162017", 800, "Home.Period.Matrix", "Home - Period") [[1]]

goalie.splits("20162017", 800, "Long.Change", "Long Change Home/Away") [[1]]

goalie.splits("20162017", 800, "Goalie.Rink", "Home/Away") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "gamestate", "Game State") [[1]]

goalie.splits("20162017", 800, "Player.Position", "Shooter Position") [[1]]

goalie.splits("20162017", 800, "Handedness.Matrix", "Shooter vs Goalie Handedness") [[1]]

goalie.splits("20162017", 800, "Handedness.Matrix2", "Shooter & Goalie Handedness") [[1]]

goalie.splits(c("20152016","20162017"),1500, "LS.shot", "Left Side Shot") [[1]]
goalie.splits(c("20162017"),750, "LS.shot", "Left Side Shot") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "Side.Hand.Matrix", "Shot Side, Goalie & Shooter Handedness") [[1]]

quantile(scored.data$shot.dist, c(.33, .66))
goalie.splits(c("20152016","20162017"), 1500, "Distance.Band.Strength", "Shot Strength & Distance") [[1]]


league.trends.splits <- function(var, season) {
  
  trend <- goalie.splits(season, 800, var, var) [[2]]
  trend$class <- var
  trend$season <- season
  return(as.data.frame(trend))
  print(head(trend))
}

#split.list <- c("Distance.Band.Strength", "LS.shot","Side.Hand.Matrix","Handedness.Matrix2","Handedness.Matrix","Player.Position",
#                "gamestate","Goalie.Rink","Long.Change","Home.Period.Matrix","period","is.Rush","is.Rebound","Shooter.Handedness","shot.type")

homeaway.split.trends <- plyr::rbind.fill(lapply(FUN=league.trends.splits, "Goalie.Rink", seasons))

seasons=c("20072008","20082009","20092010","20102011","20112012",
          "20122013","20132014","20142015",
          "20152016","20162017")
  
split.list <- c("gamestate","Handedness.Matrix2","Player.Position2","Long.Change",
                "period","is.Rush","Distance.Band.Strength","Home.Period.Matrix","Side.Hand.Matrix")

all.league.split.trends <- plyr::rbind.fill(lapply(FUN=league.trends.splits, split.list, "20162017"))


all.league.split.trends %>%
            filter(class %in% c("gamestate","Handedness.Matrix2","Player.Position2","Long.Change",
                                "period","is.Rush","Distance.Band.Strength","Home.Period.Matrix","Side.Hand.Matrix")) %>%
            select(season, class, Split, league.xG.Lift.per.100shots, league.shots.against) %>%
            distinct() %>%
            ggplot() +
            facet_wrap(~ class, scales = "free_y") +
            geom_bar(stat = "identity", aes(x=Split, y=league.xG.Lift.per.100shots, fill=league.shots.against)) +
            theme(panel.background = element_blank()) +
            coord_flip() +
            scale_fill_gradient2(low="white",mid="light grey",high="red") +
            labs(title=paste0("League Wide xGA - GA Splits - 20162017 YTD - ", Sys.Date()),
                 x="", y="Expected Goals Against - Actual Goals per 100 Shots", fill="Total Split Shots")
      
 ###########################################################################################################################################################################
########4.C NOTABLE SEASONS
############################################################################################################################################################################

historical.goalie.season <- scored.data %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         #SA.Team = ifelse(away.G == SA.Goalie, awayteam, hometeam),
         SA = 1,
         GA = as.numeric(scored.data$goal)-1) %>%
#  select(SA.Goalie, season, season2, Goalie, xG, SA, GA, gcode) %>%
  group_by(SA.Goalie, season) %>%
  #Total xG and Goals
  summarise(xG = sum(xG),
            Goals = sum(GA),
            shots = sum(SA),
            games = uniqueN(gcode),
            #mean.shot.distance.home = mean(shot.distance.home),
            #mean.shot.distance.away = mean(shot.distance.away),
            xG.shot = xG / shots,
            shots.per.game = shots / games,
            xG.Lift = xG - Goals,
            xG.Lift.per100Shots = xG.Lift / (shots / 100),
            Surplus.Pts = games / (100 / shots.per.game) * (xG.Lift.per100Shots / (1/0.3603384))) %>%
  arrange(-xG.Lift.per100Shots) 

### Plot Playoff Performances
historical.goalie.season %>%
    filter(season2 == "20162017p") %>%
  ggplot() +
  geom_segment(aes(x = reorder(SA.Goalie,-xG.shot), 
                   y = xG.shot, xend = reorder(SA.Goalie,-xG.shot), yend = G.shot, colour = (xG.shot - G.shot)),size=2) +
  scale_color_gradient2(low="firebrick2",mid="grey50",high="forestgreen") +
  geom_point(aes(x=reorder(SA.Goalie,-xG.shot), y=xG.shot, size = season.Shots), show.legend=TRUE, stat="identity", color="grey50") +
  geom_point(aes(x=reorder(SA.Goalie,-xG.shot), y=G.shot, size = season.Shots), stat="identity", color="slateblue4") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
  labs(title=paste0("Goaltending Performance (QREAM*) 2016-17 - *Quality Rules Everything Around Me\nSorted by Mean xG per Shot Faced (Minimum 750 shots)"),
       x="Goalie", y="(Expected) Goals per Shot", color="QREAM per Shot", size = "Season Shots") +
  annotate("text", x = 6, y = (min(goalie.season$xG.shot) - 0.01), hjust=0, 
           label = "Grey bubble: xG / Shot\nDark bubble: Actual GA / Shot\n@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model")


replacement.season <- historical.goalie.season %>%
    group_by() %>%
    filter(shots < (max(shots) * 0.2)) %>%
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
           scale_fill_gradient2(low="white",mid="light grey",high="Red") +
          theme(panel.background = element_blank(),
                panel.grid.major.x = element_line(colour = "dark grey"),
                panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
          labs(title=paste0("Goaltender Surplus Points Above Replacement per Season\nActive Goalies, Minimum 750 Shots in 2016-17\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
               y="Surplus Points Above Replacement per Season (Goals Prevented Over xG Converted to Points, 0.36pts / GA)", x="Goalie", fill = "Shots Against")

career.adjust <- historical.goalie.season %>% 
            group_by(SA.Goalie) %>% 
            summarise(total.QREAM.adjust = sum(xG) - sum(Goals))


career.all <- read.csv("/Users/colander1/Downloads/career_all_wadjustments.csv")

career.all <- career.all %>%
  left_join(career.adjust, by="SA.Goalie") %>%
  mutate(total.QREAM.adjust.100 = total.QREAM.adjust / (career.shots / 100),
         total.QREAM.unadjust.100 = total.QREAM.unadjust / (career.shots / 100))




career.all %>%
          filter(career.shots > 1000) %>%
          ggplot(aes(x=total.QREAM.unadjust.100, y=total.QREAM.adjust.100,label=SA.Goalie)) +
          geom_text(aes(label=SA.Goalie), angle=0, check_overlap = TRUE) +
          geom_point(aes(color=career.shots)) +
         annotate("segment",x=-2,y=-2,xend=2,yend=2) +
          labs(x="Goals Prevented per 100 Shots Unadjusted Rink Distance",
               y="Goals Prevented per 100 Shots Adjusting by Rink CDF Distance", color="Career Shots") +
          theme(panel.background = element_blank(),
                panel.grid.major.x = element_line(colour = "grey80"),
                panel.grid.major.y = element_line(colour = "grey80")) +
          geom_smooth(method = "lm", se = FALSE)

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
      geom_bar(stat = "identity", aes(fill = shots)) +
      coord_flip() +
      #ylim(0,25) +
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
  filter(season %in% c("20162017") & shots > 750) %>%
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


# Historical - Current Goalies
current.goalie.game.scores <- historical.goalie.season %>%
  #filter(season %in% c("20162017") & shots > 1550) %>%
  filter(SA.Goalie %in% c("CAM TALBOT","PEKKA RINNE", "JAKE ALLEN","JOHN GIBSON","HENRIK LUNDQVIST","MATTHEW MURRAY","MARC-ANDRE FLEURY","BRADEN HOLTBY","CRAIG ANDERSON")) %>%
  ungroup()  %>%
  select(SA.Goalie) %>%
  distinct() %>%
  inner_join(scored.data, by="SA.Goalie") %>%
  filter(season %in% c("20142015","20152016","20162017")) %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA = 1,
         GA = as.numeric(goal)-1) %>%
  group_by(SA.Goalie, season, gcode) %>%
  #Total xG and Goals
  summarise(xG = sum(xG),
            G = sum(GA),
            shots = sum(SA),
            xG.shot = xG / shots,
            xG.Lift = sum(xG) - sum(GA),
            xG.Lift.per100Shots = xG.Lift / (sum(SA) / 100))

current.goalie.game.scores %>%
      mutate(NetPositive = ifelse(xG.Lift.per100Shots > 0,1,0),
             Excellent = ifelse(xG.Lift.per100Shots > 3,1,0),
             Poor = ifelse(xG.Lift.per100Shots < -3,1,0)) %>%
      group_by(SA.Goalie) %>%
      summarise(NetPositive = mean(NetPositive),
                Excellent = mean(Excellent),
                Poor = mean(Poor)) %>%
    melt() %>%
    ggplot(aes(x=reorder(SA.Goalie,-value), y=value, group=variable, fill=variable)) +
    geom_bar(stat = "identity",position = "dodge") +
    coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      theme(panel.background = element_blank()) +
      labs(title="Playoff Goalies by Game Type, 2015-2017\nMeasured by Game xG - Actual Goals per 100 Shots, Net Positive > 0, Excellent > 3, Poor < -3\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)",
           x="", y="Share of Total Games", fill="") 



current.goalie.game.scores %>%
 ggplot() +
  geom_density(aes(x=xG.Lift.per100Shots,color=SA.Goalie),color="purple",alpha=.25) +
  facet_wrap(~SA.Goalie) +
  theme(panel.background = element_blank(),
        legend.position="none",
        panel.grid.major.x = element_line(colour = "dark grey"),
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
  labs(title=paste0("Game-Level Goaltender xG Lift (*QREAM) Distribution - *Quality Rules Everything Around Me\nPlayoff Goalies, 2015-2017\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"), #Active Goalies, Minimum 1500 Shots in 2016-17, 2015-2017\n
       x="Game xG - Actual Goals per 100 Shots", y="Density", color = "Season", size = "Shots Against") 


game.outcomes <- scored.data %>%
          mutate(home.score = ifelse(ev.team == hometeam & goal == 1,1,0),
                 away.score = ifelse(ev.team != hometeam & goal == 1,1,0)) %>%
          group_by(season, gcode, hometeam, awayteam) %>%
          summarise(home.score = sum(home.score), away.score = sum(away.score)) 

goalie.games <- scored.data %>%
    mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
           SA = 1,
           GA = as.numeric(goal)-1,
           SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
           SA.Venue = ifelse(ev.team == hometeam, "Away", "Home")) %>%
      group_by(SA.Goalie, SA.Venue, SA.Team, season, gcode) %>%
      #Total xG and Goals
      summarise(xGA = sum(xG),
                GA = sum(GA),
                xG.Lift = sum(xG) - sum(GA),
                xG.Lift.per100Shots = xG.Lift / (sum(SA) / 100))

goalie.games2 <- goalie.games %>%
      group_by(SA.Venue, SA.Team, season, gcode) %>%
      summarise(team.goalies = uniqueN(SA.Goalie)) %>%
      inner_join(goalie.games, by = c("SA.Venue", "SA.Team", "season", "gcode")) %>%
      filter(team.goalies < 2) %>%
      left_join(game.outcomes, by = c("season", "gcode")) %>%
      mutate(Goal.Support = ifelse(SA.Team == awayteam, away.score, home.score),
              Potential.Steal = ifelse(xGA > (1 + Goal.Support),1,0),
             Steal = ifelse(Potential.Steal == 1 & GA < Goal.Support, 1, 0))

goalie.steals <- goalie.games2 %>%
          group_by(SA.Goalie) %>%
          summarise(Games = uniqueN(gcode),
                    Potential.Steal = sum(Potential.Steal),
                    Steal = sum(Steal),
                    Steal.Share = sum(Steal) / sum(Potential.Steal))

goalie.steals  %>%
  #filter(season=="20162017") %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         GoalieYr = ifelse(Steal > 5 | Potential.Steal > 25,paste0(Goalie),"")) %>%
    ggplot() +
  geom_point(aes(x=Potential.Steal,y=Steal,size=Games, color=Steal.Share)) +
  geom_text_repel(aes(x=Potential.Steal,y=Steal,label=GoalieYr)) +
  #geom_text(aes(x=Potential.Steal,y=Steal,label=GoalieYr,size=Games), angle=45) +
  scale_color_gradient(high="red",low="blue") +
  geom_smooth(aes(x=Potential.Steal, y=Steal), size = 1, colour = "grey", se = FALSE, stat = "smooth", method = "lm") +
  labs(title="Goalie Games Stolen* Career - *Win With Expected Goals Against At Least 1 Greater Than Goal Support For, xGA >= GF + 1\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)") +
  labs(x="Potential Stolen Games (Expected Goals Against At Least 1 Greater Than Goal Support For)",
       y="Stolen Games (Win With Expected Goals Against At Least 1 Greater Than Goal Support For)",size="Complete Games",color="Games Stole as Share of Total") +
  theme(panel.background = element_blank()) 


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

standings.adjustment(2012, 0.36)

############################################################################################################################################################################
########4.E GOALTENDING YEAR PAIRS
############################################################################################################################################################################

goalie.year.pairs <- historical.goalie.season %>%
      select(SA.Goalie, season, xG, Goals, shots, xG.Lift) %>%
      #filter(shots > 250) %>%
      left_join(goalie.roster[c("SA.Goalie","goalieDOB")], by="SA.Goalie") %>%
      arrange(SA.Goalie, season) %>%
      mutate(Year.start = as.numeric(substr(season,1,4)),
             Age.II = floor((as.Date(paste0("12/31/",Year.start), format = "%m/%d/%Y") - as.Date(goalieDOB)) / 365.25),
              Pair = ifelse(SA.Goalie == lag(SA.Goalie) & ((Year.start - 1) == lag(Year.start)), 1,NA),
              xG.Lift.Change = ifelse(Pair == 1, xG.Lift - lag(xG.Lift), NA),
              Total.ShotsY1pY2 = ifelse(Pair == 1, shots + lag(shots), NA)
             ) #%>%
      #filter(!is.na(Pair))

goalie.year.pairs %>%
    group_by(Age.II) %>%
    summarise(xG.Lift.Change.100 = 100 * sum(xG.Lift.Change) / sum(Total.ShotsY1pY2),
              Sample = n()) %>%
    ggplot(aes(x=Age.II, y=xG.Lift.Change.100)) +
    geom_point(aes(size=Sample)) +
    geom_line()
    
############################################################################################################################################################################
########4.F GOALTENDING MOVING AVERAGE
############################################################################################################################################################################

QREAM.smooth <- function(seasons, shot.min, roll.num, goalies) {

QREAM.MA <- scored.data %>%
        filter(season %in% seasons) %>%
        arrange(season, SA.Goalie, time.index) %>%
        group_by(season, SA.Goalie) %>%
        summarise(total.shots = n(),
                  total.xG = sum(xG),
                  total.goal = sum(as.numeric(goal) - 1)) %>%
        filter(total.shots > shot.min) %>%
        left_join(scored.data, by = c("season","SA.Goalie")) %>%
        group_by(season, SA.Goalie) %>%
        mutate(shot = 1,
               goal = as.numeric(goal) - 1,
               cum.shots = cumsum(shot),
               cum.xG = cumsum(xG),
               cum.goal = cumsum(goal),
               roll.shot = zoo::rollsum(x = shot, roll.num, align = "right", fill = NA),
               roll.xG = zoo::rollsum(x = xG, roll.num, align = "right", fill = NA),
               roll.goal = zoo::rollsum(x = goal, roll.num, align = "right", fill = NA),
               roll.QREAM = 100 * (roll.xG - roll.goal) / (roll.shot))

select.goalies <- QREAM.MA %>%
        filter(SA.Goalie %in% goalies)

QREAM.MA %>%
    ggplot(aes(x=cum.shots, y=roll.QREAM, group=SA.Goalie)) +
    stat_smooth(method = "loess", formula = y ~ x, size = 1, se=FALSE, color="grey90", alpha=0.2, span = 0.5) +
    stat_smooth(data = select.goalies, aes(x=cum.shots, y=roll.QREAM, group=SA.Goalie, color=SA.Goalie),
                method = "loess", formula = y ~ x, size = 1.5, se=FALSE, span = 0.25) +
    #geom_line(data = select.goalies, aes(x=cum.shots, y=roll.QREAM, group=SA.Goalie, color=SA.Goalie)) +
    annotate("segment",x=0,y=0,xend=max(QREAM.MA$cum.shots),yend=0) +
    labs(title=paste0("Goaltending Performance, Expected Goals Against - Actual Goals (QREAM*)  per 100 Shots, All Situations\n*Quality Rules Everything Around Me, Rolling Average Last ",roll.num," Shots")) +
  labs(x="Cumulative Shots Against", y="Expected Goals Against - Actual Goals per 100 Shots") +
  annotate("text", x = 1, y = (max(QREAM.MA$roll.QREAM) * 0.8), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
  theme(panel.background = element_blank())

}

QREAM.smooth("20162017",1000,100,c("COREY CRAWFORD","BRADEN HOLTBY","TUUKKA RASK","CAREY PRICE"))

############################################################################################################################################################################
########4.G PLAYOFF PERFORMANCES
############################################################################################################################################################################

playoffs17 <- scored.data %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA.Team = ifelse(away.G == SA.Goalie, awayteam, hometeam),
         SA = 1,
         GA = as.numeric(scored.data$goal)-1) %>%
  filter(season2 == "20162017p") %>%
  group_by(SA.Goalie, season, season2) %>%
  #Total xG and Goals
  summarise(xG = sum(xG),
            Goals = sum(GA),
            shots = sum(SA),
            games = uniqueN(gcode),
            xG.shot = xG / shots,
            shots.per.game = shots / games,
            xG.Lift = xG - Goals,
            xG.Lift.per100Shots = xG.Lift / (shots / 100),
            Surplus.Pts = games / (100 / shots.per.game) * (xG.Lift.per100Shots / (1/0.3603384)),
            Wins.Added = Surplus.Pts / 2) %>%
  arrange(-xG.Lift.per100Shots) 

playoffs17 %>%
  ggplot() +
  geom_segment(aes(x = reorder(SA.Goalie,xG.Lift), 
                   y = xG, xend = reorder(SA.Goalie,xG.Lift), yend = Goals, colour = Wins.Added),size=2) +
  scale_color_gradient2(low="firebrick2",mid="grey50",high="forestgreen") +
  geom_point(aes(x=reorder(SA.Goalie,-xG.Lift), y=xG, size = shots), stat="identity", color="grey50") +
  geom_text(aes(x=reorder(SA.Goalie,-xG.Lift), y=xG, label=paste0(round(xG,0),"xGA")),hjust=0.5, vjust=0.5, size=2) +
  geom_point(aes(x=reorder(SA.Goalie,-xG.Lift), y=Goals, size = shots), stat="identity", color="slateblue4") +
  geom_text(aes(x=reorder(SA.Goalie,-xG.Lift), y=Goals, label=paste0(Goals,"GA")),hjust=0.5, vjust=0.5, size=2, color="white") +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
  labs(title=paste0("Goaltending Performance (QREAM*) 2016-17 Playoffs - *Quality Rules Everything Around Me\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)\nGrey bubble: Expected Goals Against \nDark bubble: Actual Goals Against"),
       x="Goalie", y="(Expected) Goals Against", color="Wins Added Over Average\n(Goals Prevented Over xG\nConverted to Points, 0.36pts / GA) ", size = "Playoff Shots")
  # + annotate("text", x = 2, y = min(playoffs17$xG, playoffs17$Goals), hjust=0, label = "Grey bubble: Expected Goals Against \nDark bubble: Actual Goals Against")


playoffs17 %>%
  ggplot() +
  geom_point(aes(x = Goals, y = xG, colour = Wins.Added, size=shots)) +
  scale_color_gradient2(low="firebrick2",mid="grey50",high="forestgreen") +
  #geom_text(aes(x = Goals, y = xG, label=SA.Goalie)) +
  geom_text_repel(aes(x=Goals,y=xG ,label = SA.Goalie), point.padding = unit(0.5, "lines"), segment.color = 'black') +
   theme(panel.background = element_blank()) +
  annotate("segment", x=0, y=0, xend=max(playoffs17$Goals),yend=max(playoffs17$Goals)) +
  annotate("text", x=max(playoffs17$Goals) * 0.2, y=max(playoffs17$Goals) * 0.8, hjust=0, label = "Good", size=3) +
  annotate("text", x=max(playoffs17$Goals) * 0.8, y=max(playoffs17$Goals) * 0.2, hjust=0, label = "Bad", size=3) +
  
  labs(title=paste0("Goaltending Performance (QREAM*) 2017 Playoffs - *Quality Rules Everything Around Me\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
       x="Actual Goals Against", y="Expected Goals Against", color="Wins Added Over Average\n(Goals Prevented Over xG\nConverted to Points, 0.36pts / GA) ", size = "Playoff Shots")

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

shooter.plot(c("MICHAEL FROLIK"),c("20162017"))[[1]]

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
      geom_text(data=player.stats,aes(x=season, y=min(player.stats$xG) / 2,
                                      label=paste0(round(mean.shot.dist,1),"ft\n",round(mean.shot.angle,1),"°"))) + 
      labs(color="") +
      #labs(title=paste0(player, " xG and GF by Season")) +
      labs(x="Season", y="Individual xG and GF", fill="") +
      theme(panel.background = element_blank(),
            legend.position = "top")
    
    out <- multiplot(per.shot, absolute, cols=1)
    
    return(out)
    
}



xG_v_GF_pershot("AUSTON MATTHEWS")

xG_v_GF_pershot("MAGNUS PAAJARVI")

xG_v_GF_pershot("TODD BERTUZZI")


xG_v_GF_pershot("RICHARD PANIK")

xG_v_GF_pershot("ALEX OVECHKIN")

xG_v_GF_pershot("ALEX TANGUAY")

xG_v_GF_pershot("STEVEN STAMKOS")

xG_v_GF_pershot("BRAD BOYES")

xG_v_GF_pershot("MITCHELL MARNER")

xG_v_GF_pershot("SHAWN THORNTON")

xG_v_GF_pershot("JORDAN EBERLE")

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
########6.B ALL TEAM XG STORIES
############################################################################################################################################################################

team.story("CAR", "20162017")[[1]]

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



scored.data %>%
    filter(season2 %in% c("20152016","20162017")) %>%
    mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
           gamestate = ifelse(gamestate %in% c("5v4","PP.2p.SA","6v5","4v3"), "PK",
                              ifelse(gamestate %in% c("5v5","4v4","3v3"), "Even",
                                     ifelse(gamestate %in% c("SH.SA"), "SH","U")))) %>%
    group_by(SA.Team, season2) %>%
    summarise(xG = sum(xG.team)) %>%
    dcast(SA.Team ~ season2) %>%
    ggplot() +
    geom_point(aes(x=`20152016` / 1, y=`20162017` / 1)) +
   geom_text_repel(aes(x=`20152016` / 1, y=`20162017` / 1, label=SA.Team)) +
   annotate("segment",x=175,y=175,xend=230,yend=230) +
   theme(panel.background = element_blank()) +
   labs(title="All Situations Expected Goals Against\n2015-16 vs 2016-17",
    x="All Situations Expected Goals Against 2015-16", y="All Situations Expected Goals Against 2016-17")



