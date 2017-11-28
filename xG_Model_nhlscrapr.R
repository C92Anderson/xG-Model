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


############################################################################################################################################################################
########1.A UPDATE GCODE AND LOAD NHL PBP DATA USING NHLSCRAPR
############################################################################################################################################################################

# Function to pull games from nhlscrapr from current season
game.pull <- function(game) {
  
  url.len <- length(try(read_html(print(paste0("https://statsapi.web.nhl.com/api/v1/game/20170",game,"/feed/live?site=en_nhl")))))
  
  if(url.len > 1) {
    game <- retrieve.game(season="20172018", gcode=game)[[1]]
    if(length(game) > 0) { 
      return(game)
    }
  }
}


# Check data and update based on current game (replace to most current gcode)
current.data <- function(game.list) {
###load current data
  #load("/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.18.RData")
  
  #last.data.game <- 20001 # max(as.character(na.omit(game.events.17$gcode)))
  
  game.list.18 <- game.list #as.character(last.data.game:last.game)
  print(game.list.18)
  game.events.18 <- data.frame()
  new.game.events.18 <- plyr::rbind.fill(lapply(FUN=game.pull,game.list.18))
  
  game.events.18 <- plyr::rbind.fill(game.events.18, new.game.events.18)

  
  game.events.18$refdate[is.na(game.events.18$refdate)]  <- 0

  return(game.events.18) 
}

# Playoff games 
games.18 <- as.character(c(20001:20039))

# Update with last game from current season
game.events.18 <- current.data(games.18)

# Clean
game.events.18 <- game.events.18 %>% dplyr::distinct() %>% filter(!is.na(gcode))

# Check
recent.season.check <- game.events.18 %>% group_by(gcode) %>% summarise(cnt = n())
print(dim(recent.season.check))
print(tail(recent.season.check))

save(game.events.18, file="/Users/colander1/Documents/CWA/nhlscrapr-master/game.events.18.RData")

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
pbp.all.raw <- plyr::rbind.fill(game.events.08,game.events.09,game.events.10,game.events.11,
                            game.events.12,game.events.13,game.events.14,game.events.15,
                            game.events.16,game.events.17,game.events.17playoffs,game.events.18)


save(pbp.all.raw,file="~/Documents/CWA/Hockey Data/pbp.all.raw.RData")

pbp.all <- pbp.all.raw %>% 
          mutate(goal = as.factor(ifelse(etype =="GOAL",1,0)),
                 eytpe = ifelse(is.na(xcoord),"MISS",etype)) %>%
          filter(etype %in% c("FAC","SHOT","HIT","MISS","TAKE","GOAL","GIVE","TURN","BLOCK")) %>%
          select(-c(number,pos,lastfirst,last,first,numlast,numfirstlast))  

sample <- pbp.all %>% tail(3000) %>% select(-c(a1,a2,a3,a4,a5,a6,h1,h2,h3,h4,h5,h6,ev.player.3,number,pos,lastfirst,last,first,numlast,numfirstlast))  
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

in.triangle <- function (xyp, tr) {
  area <- (-tr[5]*tr[3] + tr[4]*(tr[3]-tr[2]) + tr[1]*(-tr[6]+tr[5]) + tr[2]*tr[6])/2
  ss <- 1/2/area * (tr[4]*tr[3] - tr[1]*tr[6] + (tr[6]-tr[4])*xyp[,1] + (tr[1]-tr[3])*xyp[,2])
  tt <- 1/2/area * (tr[1]*tr[5] - tr[4]*tr[2] + (tr[4]-tr[5])*xyp[,1] + (tr[2]-tr[1])*xyp[,2])
  output <- (ss > 0 & tt > 0 & 1 > ss+tt)
  return(output)
}
in.tri.rev <- function (tr=matrix(c(0,0.5,1, 0,1,0), nrow=3), xy.points) in.triangle (xy.points, tr)

pick.section <- function (xy.points) {
  
  in.1 <- apply(nhlscrapr::quadsarray[1:3,,], 3, in.tri.rev, xy.points)
  in.2 <- apply(nhlscrapr::quadsarray[c(1,3,4),,], 3, in.tri.rev, xy.points)
  picks <- in.1 | in.2
  picks[is.na(picks)] <- FALSE
  
  picker <- function (row) if (sum(row)>0) min(which(row)) else 0
  sections <- apply (picks, 1, picker)
  return(sections)
}

#quadsarray
#(zone.adjust.prefab[[1]])
#buckets <- zone.adjust.prefab(pbp.all)

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
  
    # Remove Regular Season Shootouts
    filter(!(period== "5" & substr(gcode,1,1) == "2")) %>%
  
   mutate(home.event = ifelse(ev.team == hometeam, 1, 0),
         event.zone = ifelse(home.event == 1,homezone,
                            ifelse(homezone == "Def","Off","Neu")),
         
         ## Check Lag Time doesn't Cross Periods
         same.period = ifelse(gcode == lag(gcode) & period == lag(period), 1, 0),                    
        
         # Group Give/Take Together
         etype = ifelse(etype %in% c("GIVE","TAKE"),"TURN",etype),
         
         #Last Play Before Deletion
         lag.event = ifelse(same.period == 1, lag(etype), "Unknown"),
         lag.home.zone = ifelse(same.period == 1, lag(homezone), "Unknown"),
         
         last.event.time = ifelse(same.period == 1, seconds - lag(seconds), 0))

check <- pbp.all.clean %>% head(100000) %>% 
  select(xcoord, ycoord, etype, type, seconds,period, gcode, hometeam, homezone, ev.team, distance, adjusted.distance, 
         home.event, event.zone) 


## Plot ycoords
pbp.all.clean %>%
      #tail(1000000) %>%
      filter(!is.na(ycoord) & !etype %in% c("FAC")) %>%
      mutate(etype = ifelse(etype %in% c("SHOT","MISS","BLOCK","GOAL"),"SHOT ATTEMPT",etype)) %>%
      ggplot(aes(x=(xcoord), y=ycoord, group=etype, color=etype, linetype=etype)) +
      annotate("rect", xmin=89, xmax=91, ymin=-3, ymax=3, color="grey50") +
      annotate("rect", xmin=-89, xmax=-91, ymin=-3, ymax=3, color="grey50") +
      annotate("segment", x=89, xend=89, y=-40, yend=40, color="red", size=1) +
      annotate("segment", x=-89, xend=-89, y=-40, yend=40, color="red", size=1) +
      annotate("segment", x=25, xend=25, y=-40, yend=40, color="blue", size=1.25) +
      annotate("segment", x=-25, xend=-25, y=-40, yend=40, color="blue", size=1.25) +
      annotate("segment", x=0, xend=0, y=-40, yend=40, color="red", size=1.25) +
     #full.rink() +  
      geom_density_2d() +
      theme(panel.background = element_blank()) +
      labs(x='',y='',color='', linetype='')

## Plot xcoords
pbp.all.clean %>%
  filter(!is.na(xcoord)) %>%
  ggplot(aes(x=xcoord, group=etype, color=etype)) +
  geom_density() + theme(panel.background = element_blank())

## Distance / Adjusted Distance
pbp.all.clean %>%  
  filter(etype %in% c("SHOT","GOAL","MISS")) %>%
  ggplot(aes(x=distance, y=adjusted.distance, color=xcoord, shape=event.zone)) + geom_point()

save(pbp.all.clean,file="~/Documents/CWA/Hockey Data/pbp.all.clean.RData")

############################################################################################################################################################################
########1.B LIMIT TO SHOTS AGAINST AND DEVELOP FEATURES
############################################################################################################################################################################
  load("~/Documents/CWA/Hockey Data/pbp.all.clean.RData")
  
  # Create shots dataset  1,034,072  -> 1,029,769  #### Drop misses, no x,y coordinates
  shots.all <- pbp.all.clean %>%
                # Shots Only
                filter(etype %in% c("SHOT","GOAL","MISS")) %>%  ##
                
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
                      shooterstate = ifelse(ev.team == awayteam, awaystate, homestate),
                      goaliestate = ifelse(ev.team != awayteam, awaystate, homestate),
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
                  last.off.give = ifelse(last.zone == "Off" & lag.event %in% c("TURN"), 1, 0),
                  last.def.give = ifelse(last.zone == "Def" & lag.event %in% c("TURN"), 1, 0),
                  last.neu.give = ifelse(last.zone == "Neu" & lag.event %in% c("TURN"), 1, 0),
                  
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
        
      
        ## Model & Measurements by Season
        measurement.metrics <- list()
        seasonal.model <- list()
        scored.data <- list()
        
        for (i in unique(input.cc$season)) {
          
          input.season <- input.cc[which(input.cc$season == i),]
          model.data <- input.season[ ,names(input.season) %in% c(model.vars)] 
        
          #print(sapply(sapply(model.data, unique),length))
          #print(sapply(model.data, class))
  
          # Set folds
          folds <- crossv_kfold(model.data, k = 10)
          
          # Run model over folds
          model.folds <- folds %>% 
              mutate(model = map(train, ~ glm(goal ~ . - 1, data = ., family = "binomial", na.action = na.exclude)))
          
          # Predict test data
          predicted <- model.folds %>% 
                      mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y, , type.predict = "response"))) %>% 
                      unnest(predicted)
          
          #Calculate residual
          predicted <- predicted %>% 
            mutate(goal = (as.numeric(goal)-1),
                   residual = .fitted - goal)

          
          # Calculate measurement metrics
          mm <- predicted %>%
            group_by(.id) %>% 
            summarise(
              sst = sum((goal - mean(goal)) ^ 2), # Sum of Squares Total
              sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
              r.squared = 1 - sse / sst ,        # Proportion of variance accounted for
              brierScore = mean((.fitted-goal)^2), ## Brier Score
              mae = mean(abs(.fitted-goal)), # Mean Absolute Error
              rmse = sqrt(mean((.fitted-goal)^2))   # RMSE
            )
          
          measurement.metrics[[i]] <- mm
          
          # Find best model to apply to data
          best.model.no <- mm %>% filter(rmse == min(rmse)) %>% select(.id) %>% as.character()
          best.model <- model.folds$model[[paste0(as.numeric(best.model.no))]] 
          
          seasonal.model[[i]] <- best.model
          
          # Output Seasonal Predicted
          xG.raw <- predict(best.model, model.data, type='response')
          
          scored.data[[i]] <-cbind(xG.raw,input.season)
          
        }
        
        ## Stack Seasonal xG data
        scored.data.raw <- plyr::rbind.fill(scored.data[["20072008"]],
                                            scored.data[["20082009"]],
                                            scored.data[["20092010"]],
                                            scored.data[["20102011"]],
                                            scored.data[["20112012"]],
                                            scored.data[["20122013"]],
                                            scored.data[["20132014"]],
                                            scored.data[["20142015"]],
                                            scored.data[["20152016"]],
                                            scored.data[["20162017"]],
                                            scored.data[["20172018"]]
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
            OUT[i,]=cutoff.roc.scores(s[i], scored.data.raw$xG.raw, scored.data.raw$goal)
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
          con.mat <- table(scored.data.raw$goal, scored.data.raw$xG.raw > (median(cutoff)))
          
          # AUC
          predicted <- scored.data.raw[order(scored.data.raw$xG.raw, decreasing=TRUE), c("xG.raw","goal","season")] 
          predicted <- predicted %>%
                  mutate(shot = 1,
                         goal = as.numeric(as.factor(goal))-1)
        
          pred.sample <- predicted %>% sample_n(100000)
          AUC.sample <- AUC(pred.sample$xG.raw,pred.sample$goal)
          GainAUC.sample <- GainAUC(pred.sample$xG.raw,pred.sample$goal)
          
          AUC.curve <- data.frame(TPR=cumsum(predicted$goal)/sum(predicted$goal), 
                                  FPR=cumsum(!predicted$goal)/sum(!predicted$goal),
                                  Rand = cumsum(predicted$shot)/sum(predicted$shot),
                                  Target=predicted$goal,
                                  Season = predicted$season) %>%
                        ggplot() +
                        geom_line(aes(x=FPR, y=TPR, color="TPR")) +
                        geom_line(aes(x=FPR, y=Rand, color="Random")) +
                        labs(x="FPR", y="TPR", title="AUC", color="") +
                        annotate("text", x = 0.1, y = 0.9, hjust=0, label = paste0("@CrowdScoutSprts\nxG Model built using nhlscrapr\nAUC: ", 
                                                                                   round(AUC.sample,2))) +
                        theme(panel.background = element_blank()) 
                                  
          # Gains Chart
          gains.chart <- data.frame(Gain=cumsum(predicted$goal)/sum(predicted$goal), 
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
                    
          colnames(predicted) <- c("xG","goal","shot")
          
          return(list(seasonal.model, measurement.metrics, class.plot, con.mat, scored.data.raw, AUC.curve, gains.chart))
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
  shots.all.reg %>% group_by(is.Rebound) %>% count()
  shots.all.reg %>% group_by(is.Rush) %>% count()
  
  
  predicted.goal.model <- lm.cv.10(input = shots.all.reg,
                                
                                   model.vars = c("goal",
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
                                  "shot.type","regressed.shooting.skill.index",
                                  "shooterstate","goaliestate", #"gamestate",
                                  "Player.Position2",#"Off.Hand.Shot", Bad Variable
                                  "Handed.Class2"
                                  #"speed.last.event", Bad Variable
                                 #"dist.last.event" Can't impute
                                  ),  
                      
                                extra.vars = c("season","season2","gcode","period","seconds","ev.team","etype","awayteam","hometeam","away.G","home.G","Player",
                                  "even.second","SA.Goalie","time.index","refdate","event","LS.shot","Shooter.Handedness",
                                  "a1", "a2", "a3", "a4", "a5", "a6", "h1", "h2", "h3", "h4", "h5", "h6","XC","YC","same.period.shot",
                                  "last.event.time","away.score","home.score"))   
  
models <- rbind(predicted.goal.model[[1]][[1]][1]$coefficients,
                             predicted.goal.model[[1]][[2]][1]$coefficients,
                             predicted.goal.model[[1]][[3]][1]$coefficients,
                             predicted.goal.model[[1]][[4]][1]$coefficients,
                            predicted.goal.model[[1]][[5]][1]$coefficients,
                            predicted.goal.model[[1]][[6]][1]$coefficients,
                            predicted.goal.model[[1]][[7]][1]$coefficients,
                            predicted.goal.model[[1]][[8]][1]$coefficients,
                            predicted.goal.model[[1]][[9]][1]$coefficients,
                            predicted.goal.model[[1]][[10]][1]$coefficients) %>%
                            as.data.frame()

var.names <- data.frame(variable = c('goaliestate',	'Handed.Class2Same',	'is.Rebound1',	'is.Rush1',	'last.def.give',	'last.def.shot',	'last.neu.give',	'last.off.give',	'last.off.shot',	'LN.Rebound.Angle.Distance',	'Player.Position2F',	'regressed.shooting.skill.index',	'shooterstate',	'shot.angle.pow2',	'shot.angle.pow3',	'shot.angle',	'shot.dist.pow2',	'shot.dist.pow3',	'shot.dist',	'shot.typeBackhand',	'shot.typeDeflected',	'shot.typeSlap',	'shot.typeWrap',	'shot.typeWrist',	'time.last.def.give',	'time.last.def.shot',	'time.last.neu.give',	'time.last.off.give',	'time.last.off.shot'),
                        VarName = c('Goalie Strength State',	'Shooter/Goalie Same Handedness?',	'Is Rebound?',	'Is Rush?',	'Last Event Defensive Giveaway?',	'Last Event Defensive Shot Against?',	'Last Event Neutral Zone Giveaway?',	'Last Event Offensive Zone Giveaway?',	'Last Event Offensive Zone Shot For?',	'Rebound Angular Velocity (Radians / Shot Distance Traveled), Logged',	'Shooter Forward?',	'Regressed Shooting Skill Index',	'Shooter Strength State',	'Shot Angle (Power 2)',	'Shot Angle (Power 3)',	'Shot Angle (Power 1)',	'Shot Distance (Power 2)',	'Shot Distance (Power 3)',	'Shot Distance (Power 1)',	'Shot (Backhand)',	'Shot (Deflected)',	'Shot (Slap)',	'Shot (Wrap)',	'Shot (Wrist)',	'Seconds from Defensive Giveaway',	'Seconds from Defensive Shot Against',	'Seconds from Neutral Zone Giveaway',	'Seconds from Offensive Zone Giveaway',	'Seconds from Last Offensive Zone Shot For'))

models %>% 
    melt() %>%
    merge(var.names, by = "variable") %>%
    select(VarName, value) %>%
    ggplot(aes(x=reorder(VarName,-VarName), y=value)) +
    geom_boxplot(outlier.shape = NA) + 
    coord_flip() +
    ylim(c(-3,3)) +
    labs(title = "xG Logistic Regression Coefficient Stability by Season", x="Variable", y="Coefficient Value") +
    annotate("text", color = "grey50", x=15, y=-2, label = "Higher Value\nDecreases Chance of Goal") +
    annotate("text", color = "grey50", x=15, y=2, label = "Higher Value\nIncreases Chance of Goal") 
  
  


m1 <- predicted.goal.model[[1]][[4]]
  # Confusion Matrix
  predicted.goal.model[[4]]
  #FALSE   TRUE
  #0 664513 309169
  #1  19368  46069
  
  ## AUC Plot
  multiplot(predicted.goal.model[[6]],predicted.goal.model[[7]])
  
  # Classification Plot
  predicted.goal.model[[3]]
  
  predicted.goal.model[[2]]
  
  # R-squared
 # 1 - (best.model$deviance / best.model$null.deviance)
  #  0.1002869
  # 0.1123704
  
  # Check xG and goals ratio
  sum(as.numeric(predicted.goal.model[[5]]$goal)-1)
  sum(predicted.goal.model[[5]]$xG.raw)
  # 62788
  # 63121.07
  
  ## By Season compare xG to goals
  predicted.goal.model[[5]] %>% 
          group_by(season) %>% 
          summarise(xG=sum(xG.raw), goals=sum(as.numeric(goal)-1), 
                    avg.shot=mean(shot.dist), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is.Rebound)-1),
                    regressed.shooting.skill.index = mean(regressed.shooting.skill.index)) %>%
    ggplot() +
    geom_line(aes(x=season, y=xG,group=1),color="blue") +
    geom_line(aes(x=season, y=goals,group=1),color="green") +
    theme(panel.background = element_blank()) +
    ylim(0,8000) +
    labs(title="Actual Goals (Green) v Expected Goals (Blue) by Season", x="Season",y="")
    
  
  ########################################################################
  #########SCORE DATA AND ADJUST FOR REBOUNDS
  ########################################################################
  # Adjust for rebounds
  scored.data <- predicted.goal.model[[5]]  %>%
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
  # 0.2031056 ## by season - all shots
  # 0.2567852 by season - shots only
  
  error <- scored.data$xG-(as.numeric(scored.data$goal)-1)
  
  ## Brier Score - Base Line (goal <- 0): 0.08919192
  mean(error^2)
  # 0.0531999 ## by season - all shots
  # 0.07151661 # by season - shots only
  
  # Mean Absolute Error
  mean(abs(error))
  # 0.1063871 ## by season - all shots
  # 0.142824 # by season - shots only
  
  # RMSE
  sqrt(mean(error^2))
  #0.230651 ## by season - all shots
  #0.2674259 # by season - shots only

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
  library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2); library(MLmetrics)
  
  rebound.model.data <- scored.data
  
  rebound.model <- glm(data = rebound.model.data,
            shot.results.inRebound ~ xG + poly(shot.dist,3, raw = T) + poly(shot.angle,3, raw = T) +  shot.type + Player.Position2 +
            time.last.off.shot + goaliestate, family="binomial")
  
  # Set folds
  folds <- crossv_kfold(rebound.model.data, k = 10)
  
  # Run model over folds
  rebound.model.folds <- folds %>% 
    mutate(rebound.model = map(train, ~ glm(shot.results.inRebound ~ poly(xG,3, raw = T) + (1-xG) + poly(shot.dist,3, raw = T) + poly(shot.angle,3, raw = T) +  shot.type + Player.Position2 +
                                      time.last.off.shot + goaliestate, data = ., family = "binomial", na.action = na.exclude)))
  
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
  #0.02577423
  
  ## Combine Data and Impute
  scored.data <- cbind(scored.data,pred.rebound) 
  
  ### Log Loss
  library(MLmetrics)
  LogLoss(scored.data$pred.rebound,scored.data$shot.results.inRebound)
  #0.1470881
  # 0.1711709 - shots only
  
  rb.error <- scored.data$pred.rebound-scored.data$shot.results.inRebound
  
  ## Brier Score - Base Line (goal <- 0): 0.08919192
  mean(rb.error^2)
  # 0.03327012 - all shots
  # 0.04005604 - shots only
  
  # Mean Absolute Error
  mean(abs(rb.error))
  # 0.06659314 - all shots
  # 0.08467813 - shots only
  
  # RMSE
  sqrt(mean(rb.error^2))
  # 0.182401 - all shots
  # 0.2001401 - shots only
  
  ############################################################################################################################################################################
  ########2.B SAVE MODEL AND SCORED SHOTS
  ############################################################################################################################################################################
  #save(scored.data, file="~/Documents/CWA/Hockey Data/xG.scored.data.RData")
  save(scored.data, file="~/Documents/CWA/Hockey Data/xG.allattempts.scored.data.RData")
  #save(best.model, file="~/Documents/CWA/Hockey Data/xG.best.model.RData")
  