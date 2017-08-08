############################################################################################################################################################################
#
# PROJECT:        xG Model Measuring Goaltender Performance
#
# PURPOSE:        Using Logistic Regression create an xG for each shot a goalie faces. 
#                 Compare xG Against to Actual Goals Against to measure performance
#
# CREATED BY:     Cole Anderson (cole92anderson@gmail.com)
#
paste0("LAST UPDATED: ",Sys.Date())
#
# PROCESS:        PLOTS OF XG SCORED DATA
#
############################################################################################################################################################################
library(ggplot2);library(MASS);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(data.table); library(reshape2);
library(shiny)
library(ggplot2)
library(xts)
library(RMySQL)
library(DBI)
theme_set(theme_bw())

goalie.roster <- read_csv("https://raw.githubusercontent.com/C92Anderson/xG-Model/master/hockey_goalies_roster.csv") %>% 
  mutate(goalieID = playerId,
         SA.Goalie = toupper(playerName),
         goalieHeight = playerHeight,
         Catches = playerShootsCatches,
         goalieDOB = playerBirthDate) %>%
  select(goalieID, SA.Goalie, goalieHeight, Catches, goalieDOB) %>%
  unique()


# Multiple plot function
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
    geom_point(data=select.goalies,size=1,alpha=0.8,aes(x=cum.Shots,y=QREAM,shape=as.factor(select.goalies$season))) + 
    labs(shape ="Season") +
    labs(color="Goalie") +
    #theme(text = element_text(size=20)) +
    annotate("segment",x=0,y=0,xend=max(data$cum.Shots),yend=0) +
    labs(title="Goaltending Performance, Expected Goals Against - Actual Goals (QREAM*), All Situations\n*Quality Rules Everything Around Me") +
    labs(x="Cumulative Shots Against", y="Expected Goals Against - Actual Goals") +
    ggrepel::geom_text_repel(data=last.game,aes(x=cum.Shots,y=QREAM,label = SA.Goalie),
                    point.padding = unit(0.5, "lines"),
                    segment.color = 'black') +
    annotate("text", x = 1, y = (max(data$QREAM) * 0.8), hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
    theme_standard()
}


# Call function with goalie list and season, call cumulative counts and plot functionhttp://127.0.0.1:24282/graphics/plot_zoom_png?width=972&height=1178
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

goalie.plot(c("AARON DELL","ANTTI RAANTA","ROBERTO LUONGO","JAROSLAV HALAK","CARTER HUTTON","AL MONTOYA",
              "MARC-ANDRE FLEURY","LOUIS DOMINGUE","CAM WARD","JOONAS KORPISALO","PETR MRAZEK"),c("20162017"))[[1]]

 
goalie.plot(c("PEKKA RINNE","JUUSE SAROS","MATTHEW MURRAY","MARC-ANDRE FLEURY"),c("20162017"))[[1]]

goalie.plot(c("STEVE MASON","ANTTI RAANTA","BRIAN ELLIOTT","SCOTT DARLING","PHILIPP GRUBAUER",
              "PETR MRAZEK","ROBIN LEHNER","MIKE SMITH","AARON DELL"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("HENRIK LUNDQVIST"),c("20162017"))[[1]]



goalie.plot(c("CAM TALBOT","MARTIN JONES", "COREY CRAWFORD","PEKKA RINNE", "DEVAN DUBNYK", "JAKE ALLEN",
              "BRIAN ELLIOTT","JOHN GIBSON"),
            c("20162017"))[[1]]

goalie.plot(c("CAREY PRICE","HENRIK LUNDQVIST","MARC-ANDRE FLEURY", "SERGEI BOBROVSKY", "FREDERIK ANDERSEN","BRADEN HOLTBY",
              "CRAIG ANDERSON","TUUKKA RASK"),
            c("20162017"))[[1]]

goalie.plot(c("CAM TALBOT","PEKKA RINNE", "JAKE ALLEN","JOHN GIBSON"),c("20162017"))[[1]]

goalie.plot(c("PEKKA RINNE","JUUSE SAROS"))[[1]]

goalie.plot(c("MARC-ANDRE FLEURY","MATTHEW MURRAY"),c("20072008","20082009","20092010","20102011","20112012",
                                                     "20122013","20132014","20142015","20152016"))[[1]]

goalie.plot(c("CAREY PRICE","HENRIK LUNDQVIST","MARC-ANDRE FLEURY", "SERGEI BOBROVSKY", "FREDERIK ANDERSEN","BRADEN HOLTBY",
              "CRAIG ANDERSON","TUUKKA RASK"),
            c("20162017"))[[1]]


goalie.plot(c("EDDIE LACK"),c("20132014","20142015","20152016","20162017"))[[1]]

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

goalie.plot(c("ONDREJ PAVELEC"))[[1]]

goalie.plot(c("MIKE CONDON","CRAIG ANDERSON","ANDREW HAMMOND"))[[1]]
            
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
goalie.plot(c("HENRIK LUNDQVIST"))[[1]]

goalie.plot(c("TUUKKA RASK","ANTON KHUDOBIN","ANDREW HAMMOND"),c("20142015","20152016","20162017"))[[1]]

#goalie.plot(c("JONATHAN QUICK","PETER BUDAJ"),c("20152016","20162017"))[[1]]
goalie.plot(c("FREDERIK ANDERSEN","GARRET SPARKS","CURTIS MCELHINNEY"),c("20162017"))[[1]]


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

goalie.plot(c("MIKE CONDON","ANDREW HAMMOND"),c("20142015","20152016","20162017"))[[1]]
goalie.plot(c("ROBIN LEHNER","ANDERS NILSSON","CHAD JOHNSON"),c("20132014","20142015","20152016","20162017"))[[1]]


goalie.plot(c("CAM TALBOT","JOHN GIBSON","JONATHAN BERNIER"),c("20162017"))[[1]]

goalie.plot(c("CHAD JOHNSON","BRIAN ELLIOTT"),c("20162017"))[[1]]

goalie.plot(c("JONATHAN QUICK","BEN BISHOP","CHAD JOHNSON","BRIAN ELLIOTT","PETER BUDAJ"),c("20152016","20162017"))[[1]]

goalie.plot(c("STEVE MASON","MICHAL NEUVIRTH"),c("20142015","20152016","20162017"))[[1]]

goalie.plot(c("ANTTI RAANTA","SCOTT DARLING"),c("20132014","20142015","20152016","20162017"))[[1]]

goalie.plot(c("JOONAS KORPISALO","PHILIPP GRUBAUER","ANTTI RAANTA"),c("20132014","20142015","20152016","20162017"))[[1]]


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
goalie.splits <- function(seasons, shot.min, var.set, desc, goalies=unique(scored.data$SA.Goalie)) {
  
  goalie.set <- scored.data %>%
    filter(season %in% seasons) %>%
    left_join(goalie.roster[c("SA.Goalie","Catches")], by="SA.Goalie") %>%
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
    group_by(SA.Goalie, Goalie, season, Split) %>%
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
    filter((total.shots >= shot.min |  SA.Goalie %in% goalies) & Split != "U") %>%
    ggplot(aes(x=reorder(SA.Goalie, total.QREAM), y=split.QREAM, fill=Split, label=Goalie)) +
    geom_bar(stat="identity", colour="white") +
    facet_wrap( ~ season, ncol = 3) +
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

goalie.splits(c("20142015","20152016","20162017"), 5000, "gamestate", "Game State", goalies = "ONDREJ PAVELEC") [[1]]


goalie.splits(c("20142015","20152016","20162017"), 5000, "Rebound.Class", "Rebound Type",goalies = c("BEN BISHOP","ANTTI NIEMI","KARI LEHTONEN")) [[1]]

goalie.splits(c("20152016","20162017"), 3500, "Danger.Band", "Danger Bins",goalies = c("ANTTI RAANTA","MIKE SMITH")) [[1]]

goalie.splits(c("20152016","20162017"), 1500, "Time.Band", "Period Time") [[1]]

goalie.splits(c("20162017"), 1500, "Shot.Class.Matrix", "Shot Class & Venue") [[1]]


goalie.splits(c("20152016","20162017"), 2500, "Side.Hand.Matrix", "Shot Side, Goalie & Shooter Handedness") [[1]]

goalie.splits(c("20162017"), 750, "Rebound.Class", "Rebound Type") [[1]]

goalie.splits(c("20162017","20162017p"), 750, "Danger.Band", "Danger Bins") [[1]]

goalie.splits("20162017p", 400, "Goalie.Rink", "Home/Away") [[1]]


goalie.splits(c("20162017"), 750, "shot.type", "Shot Type") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "Shooter.Handedness", "Shooter Handedness") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "is.Rebound", "Rebound Shot") [[1]]

goalie.splits(c("20152016","20162017"), 1500, "is.Rush", "Rush Shot") [[1]]

goalie.splits("20162017", 800, "period", "Period") [[1]]

goalie.splits("20162017", 800, "Home.Period.Matrix", "Home - Period") [[1]]

goalie.splits("20162017", 800, "Long.Change", "Long Change Home/Away") [[1]]

goalie.splits("20162017", 800, "Goalie.Rink", "Home/Away") [[1]]


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
  filter(season %in% c("20162017") & shots > 1500) %>%
  #filter(SA.Goalie %in% c("MATTHEW MURRAY","PEKKA RINNE","MARC-ANDRE FLEURY")) %>%
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
  labs(title="Starting 2017 Goalies by Game Performance, 2015-2017\nMeasured by Game xG - Actual Goals per 100 Shots, Net Positive > 0, Excellent > 3, Poor < -3\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)",
       x="", y="Share of Total Games", fill="") 



current.goalie.game.scores %>%
  ggplot() +
  geom_density(aes(x=xG.Lift.per100Shots,color=SA.Goalie),color="purple",alpha=.25) +
  facet_wrap(~SA.Goalie) +
  theme(panel.background = element_blank(),
        legend.position="none",
        panel.grid.major.x = element_line(colour = "dark grey"),
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1)) +
  labs(title=paste0("Game-Level Goaltender xG Lift (*QREAM) Distribution - *Quality Rules Everything Around Me\nGoalies with >1500 Shots in 2017, 2015-2017\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"), #Active Goalies, Minimum 1500 Shots in 2016-17, 2015-2017\n
       x="Game xG - Actual Goals per 100 Shots", y="Density", color = "Season", size = "Shots Against") 

###########Steal Share
game.outcomes <- scored.data %>%
  mutate(home.score = ifelse(ev.team == hometeam & goal == 1,1,0),
         away.score = ifelse(ev.team != hometeam & goal == 1,1,0)) %>%
  group_by(season, season2, gcode, awayteam) %>%
  summarise(home.score = sum(home.score), away.score = sum(away.score)) 

goalie.games <- scored.data %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA = 1,
         GA = as.numeric(goal)-1,
         SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
         SA.Venue = ifelse(ev.team == hometeam, "Away", "Home")) %>%
  group_by(SA.Goalie, SA.Venue, SA.Team, season, season2, gcode) %>%
  #Total xG and Goals
  summarise(xGA = sum(xG),
            GA = sum(GA),
            xG.Lift = sum(xG) - sum(GA),
            xG.Lift.per100Shots = xG.Lift / (sum(SA) / 100))

goalie.games2 <- goalie.games %>%
  group_by(SA.Team, season, season2, gcode) %>%
  summarise(team.goalies = uniqueN(SA.Goalie)) %>%
  inner_join(goalie.games, by = c("SA.Team", "season","season2","gcode")) %>%
  filter(team.goalies < 2) %>%
  left_join(game.outcomes, by = c("season","season2", "gcode")) %>%
  mutate(Goal.Support = ifelse(SA.Team == awayteam, away.score, home.score),
         Potential.Steal = ifelse(xGA > (1 + Goal.Support),1,0),
         Steal = ifelse(Potential.Steal == 1 & GA < Goal.Support, 1, 0))

goalie.steals <- goalie.games2 %>%
  mutate(Season.Type = ifelse(nchar(as.character(season2)) >8,"PO","RS")) %>%
  group_by(SA.Goalie, season) %>%
  summarise(Games = uniqueN(gcode),
            Potential.Steal = sum(Potential.Steal),
            Steal = sum(Steal),
            Steal.Share = sum(Steal) / sum(Potential.Steal),
            Potential.Steal.Share = sum(Potential.Steal) / uniqueN(gcode))

goalie.steals  %>%
  #filter(season=="20162017") %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         GoalieYr = ifelse(Steal > 2 | Potential.Steal > 8,paste0(Goalie),"")) %>%
  ggplot() +
  geom_point(aes(x=Potential.Steal,y=Steal,size=Games, color=Steal.Share)) +
  geom_text_repel(aes(x=Potential.Steal,y=Steal,label=GoalieYr)) +
  #geom_text(aes(x=Potential.Steal,y=Steal,label=GoalieYr,size=Games), angle=45) +
  scale_color_gradient(high="red",low="blue") +
  geom_smooth(aes(x=Potential.Steal, y=Steal), size = 1, colour = "grey", se = FALSE, stat = "smooth", method = "lm") +
  labs(title="Goalie Games Stolen*  2017 - *Win With Expected Goals Against At Least 1 Greater Than Goal Support For, xGA >= GF + 1\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)") +
  labs(x="Potential Stolen Games (Expected Goals Against At Least 1 Greater Than Goal Support For)",
       y="Stolen Games (Win With Expected Goals Against At Least 1 Greater Than Goal Support For)",size="Complete Games",color="Games Stolen\nas Share of Total") +
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

aging_game_curve <- scored.data %>%
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
            xG.Lift.per100Shots = xG.Lift / (sum(SA) / 100)) %>%
  inner_join(goalie.roster[c("SA.Goalie","goalieDOB","goalieHeight")], by="SA.Goalie") %>%
  mutate(Year.start = as.numeric(substr(season,1,4)),
         Age.II = floor((as.Date(paste0("12/31/",Year.start), format = "%m/%d/%Y") - as.Date(goalieDOB)) / 365.25),
         Age.2 = 3 * floor(Age.II/3),
         Period = ifelse(season %in% c("20072008","20082009","20092010","20102011","20112012"),"2008-12","2013-17"),
         Name = ifelse(Age.II > 38,SA.Goalie,NA),
         Height = ifelse(goalieHeight > 74,"Above Median",
                         "Median & Below"))

library(ggjoy)

aging_game_curve %>%
  ggplot(aes(x=xG.Lift, y=reorder(as.factor(Age.II),-Age.II), group=reorder(as.factor(Age.II),-Age.II))) + geom_joy()
  geom_joy(stat="binline", scale = 0.9) +# theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +      # for both axes to remove unneeded padding
  theme_standard() + scale_color_gdocs() +
  labs(title="Game-Level xG Lift Distribution by Goalie Age, Height", y="Age", x="Game xG Lift (xGA - GA)") +
  theme(panel.grid.major.x = element_line( colour = "white", size = 2),
        panel.grid.minor.x = element_line( colour = "white", size = 1)) +
  xlim(c(-5,5))

library(gganimate)
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
set_config( config( ssl_verifypeer = 0L ) )
devtools::install_github("RcppCore/Rcpp")
devtools::install_github("dgrtwo/gganimate")


aging_plot_data <- aging_game_curve %>%
  filter(SA.Goalie %in% c("COREY CRAWFORD","CORY SCHNEIDER")) %>%
  group_by(Age.II,SA.Goalie) %>%
  mutate(median.xG = median(xG.Lift),
         Performance = ifelse(xG.Lift > 0,"Positive xG Lift","Negative xG Lift"))

aging_plot_data %>%
  ggplot(aes(xG.Lift,group=Performance, fill=Performance)) +
  geom_vline(aes(xintercept = 0), color = "grey50", size=2) +
  annotate("text",x=3, y=10, label="(+)", color="grey50") +
  annotate("text",x=-3, y=10, label="(-)", color="grey50") +
  geom_joy(bins = 20, alpha=0.5) +
  theme_standard() +  scale_fill_gdocs() + scale_color_gdocs() +
  geom_vline(aes(xintercept = median.xG, color = "Median Game-Level Performance"), size=2) +
  labs(title="Game-Level xG Lift Distribution by Goalie Age\n@crowdscoutsprts",
       y="Game Count", x="Game xG Lift (xGA - GA)", color="") +
  theme(panel.grid.major.x = element_line( colour = "white", size = 2),
        panel.grid.minor.x = element_line( colour = "white", size = 1),
        legend.position = "top") +
  facet_grid(Age.II ~SA.Goalie)

library(gganimate)

gganimate(p)

devtools::install_github("dgrtwo/gganimate")

library(gapminder)
library(ggplot2)
theme_set(theme_bw())

library(gganimate)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

gganimate(p)


gganimate(p, "output.gif")

animation::ani.options(interval = 1/20)
gganimate(p, 'hist_ex.gif', title_frame = FALSE)


aging_plot
ggsave(filename="/Users/colander1/Downloads/aging_plot.jpg", plot=aging_plot,  width=15, height=30)


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
########4.G GOALIE PERFORMANCE BY WORKLOAD
############################################################################################################################################################################

load("~/Documents/CWA/Hockey Data/pbp.all.raw.RData")

goalie.minutes <- pbp.all.raw %>% 
            filter(season =="20072008" & gcode == "20001") %>%
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
                   ## Check Lag Time doesn't Cross Periods
                   same.period = ifelse(gcode == lag(gcode) & period == lag(period), 1, 0),                    
                   duration = ifelse(same.period == 1, seconds - lag(seconds), 0)) %>%
          select(season, gcode, away.G, home.G, duration, etype, seconds, event, period, event.length) %>%
          group_by(season, gcode, away.G) %>%
          summarise(TOI = sum(event.length) / 60)



############################################################################################################################################################################
########4.H GOALIE LOCATION
############################################################################################################################################################################
load("~/Documents/CWA/Hockey Data/shots.all.reg.RData")

shot.location.map <- function(goalie, seasons, drop_other_team = 1, drop_other_season = 1) {

    goalie.shot.map <- shots.all.reg %>%
          filter(SA.Goalie %in% goalie & season %in% seasons & etype %in% c("SHOT","GOAL") & 
                  gamestate %in% c("5v4","5v5")) %>%
          mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
                 class=SA.Goalie) %>%
          select(XC, YC, etype, season, gamestate, SA.Team, goal, class)
    
    other.goalies <- shots.all.reg %>%
          filter(!SA.Goalie %in% goalie & season %in% seasons & etype %in% c("SHOT","GOAL") & 
                  gamestate %in% c("5v4","5v5")) %>% ##c("3v3","4v4","5v4","5v5","PP.2p.SA","SH.SA")
          mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam)) %>%
          select(XC, YC, etype, season, gamestate, SA.Team) %>%
          mutate(class= ifelse(SA.Team %in% unique(goalie.shot.map$SA.Team),
                               "OTHER GOALIES (SAME TEAM & SEASON)",
                               "OTHER GOALIES (SAME SEASON)")) %>%
          filter(class != "OTHER GOALIES (SAME TEAM & SEASON)" | drop_other_team == 0) %>%
          filter(class != "OTHER GOALIES (SAME SEASON)" | drop_other_season == 0)
    
    
    
    goalie.shot.map2 <- plyr::rbind.fill(goalie.shot.map, other.goalies)
      
      results <- goalie.shot.map %>%
          group_by(gamestate) %>%
          summarise(shots = n(),
                    #xG = sum(xG),
                    goals = sum(as.numeric(goal)-1),
                    Sv.Pct = (shots - goals)/ shots) 
    
      goalie.shot.map3 <- goalie.shot.map2 %>%
          ggplot() +
          annotate("rect", xmin=89, xmax=91, ymin=-3, ymax=3, color="grey50") +
          annotate("segment", x=89, xend=89, y=-40, yend=40, color="red", size=1) +
          annotate("segment", x=25, xend=25, y=-40, yend=40, color="blue", size=1.25) +
          geom_density_2d(aes(x=XC, y=YC, group=etype, color=etype, linetype=etype)) +
          xlim(20,100) +
          facet_wrap(~ class + season + gamestate, ncol=2) +
          #annotate("text",data=results, aes(x=0,y=30,label = paste0(results$Sv.Pct))) +
          theme(legend.position = "top") +
          labs(title=paste0(goalie, " Shot and Goal Locations - ",seasons,"\nCompared to Other League Goalies, Same Seasons"),
               x="",y="",color="",linetype="")
    
      return(goalie.shot.map3)
}


#shot.location.map(goalie = c("MATTHEW MURRAY","MARC-ANDRE FLEURY"),c("20162017"), drop_other = 1)

shot.location.map(goalie = c("FREDERIK ANDERSEN"),seasons = c("20152016","20162017"), drop_other_team = 0, drop_other_season = 1)

shot.location.map(goalie = c("ROBIN LEHNER","ANDERS NILSSON"),seasons = c("20162017"), drop_other_team = 1, drop_other_season = 1)

############################################################################################################################################################################
########4.H GOALIE HOT-HAND
############################################################################################################################################################################

goalie.game.season <- scored.data %>%
  filter(season == "20162017") %>% 
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA = 1,
         GA = as.numeric(scored.data$goal)-1) %>%
  group_by(SA.Goalie, season, gcode) %>%
  #Total xG and Goals
  summarise(xG = sum(xG),
            Goals = sum(GA),
            shots = sum(SA),
            xG.Lift = xG - Goals,
            xG.Lift.per100Shots = xG.Lift / (shots / 100))


limit.goalies <- goalie.game.season %>%
      group_by(SA.Goalie, season) %>%
      summarise(total.shots = sum(shots)) %>%
      #filter(total.shots > 2000) %>%
      filter(SA.Goalie %in% c("PEKKA RINNE","MARC-ANDRE FLEURY","MATTHEW MURRAY","JUUSE SAROS")) %>%
      mutate(include = 1)

goalie.game.season2 <- goalie.game.season %>%
    left_join(limit.goalies, by=c("SA.Goalie","season")) %>%
    arrange(SA.Goalie, gcode) %>%
    mutate(Goalie = ifelse(is.na(include),"Other", SA.Goalie),
           LastGame = lag(xG.Lift.per100Shots),
           PairShots = lag(shots) + shots) 

models <- goalie.game.season2 %>% 
    group_by(Goalie) %>% 
  do(model = lm(xG.Lift.per100Shots ~ LastGame, data = .))

unlist(models)

require(nlme)
lmList(xG.Lift.per100Shots ~ LastGame | Goalie, 
       data=goalie.game.season2[!is.na(goalie.game.season2$LastGame),c("xG.Lift.per100Shots","LastGame","Goalie")])



goalie.game.season2 %>% 
  filter(Goalie != "Other") %>%
      ggplot(aes(x=xG.Lift.per100Shots, LastGame, group=Goalie, color=Goalie, size=PairShots)) +
      annotate("text",x=10,y=-10,label="Good Game After Bad",size=3,color="grey50") +
      annotate("text",x=10,y=10,label="Good Game After Good",size=3,color="grey50") +
      annotate("text",x=-10,y=10,label="Bad Game After Good",size=3,color="grey50") +
      annotate("text",x=-10,y=-10,label="Bad Game After Bad",size=3,color="grey50") +
      #annotate("text",x=-10,y=-10,label=models,size=3,color="grey50") +
      annotate("segment",x=0,xend=0,y=-10,yend=10,size=1.25,color="grey50") +
      annotate("segment",y=0,yend=0,x=-10,xend=10,size=1.25,color="grey50") +
  geom_point(alpha=0.6) +
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE) +
      labs(title = "Game to Game Performance, Select Goalies",x="Game xGA - GA / 100 Shots",y="Last Game xGA - GA / 100 Shots",
           size="Total Shots (Both Games)")
  

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
    mutate(SF = 1,
           GF = as.numeric(goal)-1) %>%
    group_by(Player, season) %>%
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

multiplot(xG_v_GF_pershot("ILYA KOVALCHUK"),
          xG_v_GF_pershot("ALEXANDER SEMIN"), ncol=2)


xG_v_GF_pershot("AUSTON MATTHEWS")

xG_v_GF_pershot("MAGNUS PAAJARVI")

xG_v_GF_pershot("ILYA KOVALCHUK")


xG_v_GF_pershot("T.J. OSHIE")

xG_v_GF_pershot("ALEX OVECHKIN")

xG_v_GF_pershot("ALEX TANGUAY")

xG_v_GF_pershot("STEVEN STAMKOS")

xG_v_GF_pershot("BRAD BOYES")

xG_v_GF_pershot("MITCHELL MARNER")

xG_v_GF_pershot("LANCE BOUMA")

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

team.story("ANA", "20162017")[[1]]

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


############################################################################################################################################################################
########7.B ATTEMPTS VS SHOTS ONLY
############################################################################################################################################################################

#### Find share of xG that misses the net by goalie
load("~/Documents/CWA/Hockey Data/xG.scored.data.attempts.RData")

all.attempts.versus <- scored.data %>%
    filter(season == "20162017") %>%
    group_by(SA.Goalie) %>%
    summarise(shots17 = n()) %>%
    filter(shots17 > 2000) %>%
    inner_join(scored.data, by="SA.Goalie") %>%
    mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
           shot.type = ifelse(etype == "MISS","Miss","OnNet"),
           Goalie.Rink = ifelse(ev.team == hometeam, "Away", "Home"),
           SA = 1) %>%
    group_by( SA.Goalie, Goalie.Rink, shot.type, season) %>%
    summarise(total.xG = sum(xG),
              total.shots = sum(SA),
              total.goal = sum(as.numeric(goal)-1)) 

plot.data <- all.attempts.versus %>%
  group_by(SA.Goalie, season) %>%
  summarise(total.shots = sum(total.shots)) %>%
  inner_join(dcast(data=all.attempts.versus, SA.Goalie + season + Goalie.Rink ~ shot.type, value.var="total.xG"), by=c("SA.Goalie","season")) %>%
  mutate(xG.Share.Missed = Miss / (Miss + OnNet)) %>%
    filter(season %in% c("20132014","20142015","20152016","20162017"))

mean.miss <- round(sum(plot.data$Miss) / sum(plot.data$Miss + plot.data$OnNet),3)

plot.data %>%
  group_by(SA.Goalie) %>%
  summarise(seasons = uniqueN(season)) %>%
  filter(seasons == 4) %>%
  inner_join(plot.data, by="SA.Goalie") %>%
  ggplot() +
  geom_line(aes(x=season, y=xG.Share.Missed, group=Goalie.Rink, color=Goalie.Rink)) +
  geom_point(aes(x=season, y=xG.Share.Missed, group=Goalie.Rink, color=Goalie.Rink, size=total.shots)) +
  facet_wrap(~SA.Goalie) +
  annotate("segment",x=1,xend=4,y=mean(plot.data$xG.Share.Missed),yend=mean(plot.data$xG.Share.Missed)) +
  theme(panel.background = element_blank(),
           panel.grid.major.y = element_line(colour = "light grey", size = 0.1),
           panel.grid.major.x = element_line(colour = "light grey", size = 0.1)) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x="Season", y="Share of xG Missing the Net", size="Season Shots",
       title=paste0("Share of Expected Goals Missing the Net by Goalie, Season\nMean xG Missed: ",round(mean.miss,3)*100,"% - Black Line"))
  

######By Team

scored.data %>%
  mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
         SA.Team = ifelse(SA.Team == "PHX","ARI",
                          ifelse(SA.Team == "ATL","WPG",
                                 SA.Team)),
         shot.type = ifelse(etype == "MISS","Miss","OnNet"),
         Venue = ifelse(ev.team == hometeam, "Away", "Home"),
         SA = 1) %>%
  group_by( SA.Team, Venue, shot.type, season) %>%
  summarise(total.xG = sum(xG),
            total.shots = sum(SA),
            total.goal = sum(as.numeric(goal)-1)) %>%
  dcast(SA.Team + season +  Venue ~ shot.type, value.var="total.xG") %>%
  mutate(xG.Share.Missed = Miss / (Miss + OnNet)) %>%
  ggplot() +
  geom_line(aes(x=season, y=xG.Share.Missed, group=Venue, color=Venue)) +
  geom_point(aes(x=season, y=xG.Share.Missed, group=Venue, color=Venue)) +
  facet_wrap(~SA.Team) +
  annotate("segment",x=1,xend=10,y=mean(plot.data$xG.Share.Missed),yend=mean(plot.data$xG.Share.Missed)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1),
        panel.grid.major.x = element_line(colour = "light grey", size = 0.1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x="Season", y="Share of xG Missing the Net",
       title=paste0("Share of Expected Goals Missing the Net by Defending Team, Season\nMean xG Missed: ",round(mean.miss,3)*100,"% - Black Line"))



######By Team

shots.only <- scored.data %>%
  mutate(SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
         SA.Team = ifelse(SA.Team == "PHX","ARI",
                          ifelse(SA.Team == "ATL","WPG",
                                 SA.Team)),
         shot.type = ifelse(etype == "MISS","Miss","OnNet"),
         Venue = ifelse(ev.team == hometeam, "Away", "Home"),
         SA = 1) %>%
  group_by( SA.Team, Venue, shot.type, season) %>%
  summarise(total.xG = sum(xG),
            total.shots = sum(SA),
            total.goal = sum(as.numeric(goal)-1)) %>%
  dcast(SA.Team + season +  Venue ~ shot.type, value.var="total.shots") %>%
  mutate(Share.Missed = Miss / (Miss + OnNet)) 

shots.only %>%
  ggplot() +
  geom_line(aes(x=season, y=Share.Missed, group=Venue, color=Venue)) +
  geom_point(aes(x=season, y=Share.Missed, group=Venue, color=Venue)) +
  facet_wrap(~SA.Team) +
  annotate("segment",x=1,xend=10,y=mean(plot.data$xG.Share.Missed),yend=mean(plot.data$xG.Share.Missed)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "light grey", size = 0.1),
        panel.grid.major.x = element_line(colour = "light grey", size = 0.1),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x="Season", y="Share of Shots Missing the Net",
       title=paste0("Share of Shots Missing the Net by Defending Team, Season\nMedian Shots Missed: ",
      round(median(shots.only$Share.Missed),3)*100,"% - Black Line"))





