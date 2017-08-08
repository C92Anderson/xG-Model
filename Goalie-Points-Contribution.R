############################################################################################################################################################################
#
# PROJECT:        Goaltending Performance Using Win Probability
#

# CREATED BY:     Cole Anderson (cole92anderson@gmail.com)
#
paste0("LAST UPDATED: ",Sys.Date())
#
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

txt <- element_text(size = 18, colour = "grey25", face = "plain")
bold_txt <- element_text(size = 20, colour = "navy", face = "bold")

theme_standard <- function(base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      strip.background = element_blank(), 
      
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line( colour = "white", size = 2), 
      
      panel.background = element_rect(fill="grey90"),
      plot.background = element_rect(fill="grey90"),
      legend.background = element_rect(fill="grey90"),
      legend.key = element_rect(fill="grey90", size = 20),
      legend.key.size = unit(1,"cm"),
      
      panel.border = element_blank(), 
      
      line = element_line( colour = "white", size = 2),
      axis.text.x = element_text(angle = 90, hjust = 1),
      text = txt, 
      plot.title = bold_txt, 
      
      axis.title = txt, 
      axis.text = txt, 
      
      legend.title = bold_txt, 
      legend.text = txt ) 
}


### Find
game.outcomes <- scored.data %>%
  mutate(home.score = ifelse(ev.team == hometeam & goal == 1,1,0),
         away.score = ifelse(ev.team != hometeam & goal == 1,1,0),
         home.xG = ifelse(ev.team == hometeam & xG.team > 0,xG.team,0),
         away.xG = ifelse(ev.team != hometeam & xG.team > 0,xG.team,0)) %>%
  group_by(season, season2, gcode, awayteam) %>%
  summarise(home.score = sum(home.score), 
            away.score = sum(away.score),
            home.xG = round(sum(home.xG),1),
            away.xG = round(sum(away.xG),1)) 

rebound.goal.probability <- 0.2560365


goalie.games <- scored.data %>%
  mutate(Goalie = sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),
         SA = 1,
         GA = as.numeric(goal)-1,
         xGA = ifelse(is.Rebound == 0, xG + (pred.rebound * rebound.goal.probability),0),
         SA.Team = ifelse(ev.team == hometeam, awayteam, hometeam),
         SA.Venue = ifelse(ev.team == hometeam, "Away", "Home")) %>%
  group_by(SA.Goalie, SA.Venue, SA.Team, season, season2, gcode) %>%
  #Total xG and Goals
  summarise(xGA = round(sum(xGA),1),
            #xGA.team = sum(xG.team),
            GA = sum(GA))

## Function to calculate win probability based on xGF & xGA
win_probabibilty <- function(xG.support, GF, xGA, sims = 100, type="GF") {
  
  outcome <- vector(length=sims)
  
  for(i in 1:sims) {
    
    GF <- ifelse(type == "GF", GF, sum(rbinom(size=1, prob=xG.support/30, n=30)))
    GA <- sum(rbinom(size=1, prob=xGA/30, n=30))
    
    outcome[i] <- ifelse(GF > GA,2, 
                        ifelse(GF == GA,1.5,0))
    
  }  
  return(mean(outcome))
  
}


goalie.games_wresult <- goalie.games %>%
  group_by(SA.Team, season, season2, gcode) %>%
  summarise(team.goalies = uniqueN(SA.Goalie)) %>%
  inner_join(goalie.games, by = c("SA.Team", "season","season2","gcode")) %>%
  filter(team.goalies < 2) %>%
  left_join(game.outcomes, by = c("season","season2", "gcode")) %>%
  rowwise() %>% 
  mutate(xGF = ifelse(SA.Venue == "Home", home.xG, away.xG),
         Goal_Support = ifelse(SA.Venue == "Home", home.score, away.score),
         Expected_Points_GF = win_probabibilty(xGF, Goal_Support, xGA, 1000, type = "GF"),
         Expected_Points_xGF = win_probabibilty(xGF, Goal_Support, xGA, 1000, type = "xGF"),
         Actual_Points = ifelse(SA.Venue == "Home" & home.score > GA,2,
                         ifelse(SA.Venue == "Away" & away.score > GA,2,
                         ifelse(away.score == home.score, 1.5, 0)))) 


goalie.games_wresult2 <- goalie.games_wresult %>%
  select(-starts_with("home"),
         -starts_with("away")) %>%
  mutate(Game_Points_Lift_GF = Actual_Points - Expected_Points_GF,
         Game_Points_Lift_xGF = Actual_Points - Expected_Points_xGF
         )


goalie.seasons <- goalie.games_wresult2 %>%
        group_by(SA.Goalie, season) %>%
        summarise(Potential_Points_Lift_GF = sum(2 - Expected_Points_GF),
                  Potential_Points_Loss_GF = sum(Expected_Points_GF),
                  Actual_Points_Season = sum(Actual_Points),
                  Expected_Points_GF_Season = sum(Expected_Points_GF),
                  Game_Points_Lift_GF_Season = sum(Game_Points_Lift_GF),
                  Points_Gained_oPotential_GF_Season = (Potential_Points_Lift_GF - Game_Points_Lift_GF_Season) / (Potential_Points_Loss_GF + Potential_Points_Lift_GF),
                  
                  Game_Points_Lift_GF_Season_perGame = Game_Points_Lift_GF_Season / n(),
                  GP = n())


goalie.seasons %>%
      filter(season %in% c("20162017") & GP > 20) %>%
      ggplot(aes(x=reorder(SA.Goalie,Added_Points_perGame), y=Added_Points_perGame, size=GP, color=Expected_Points_perGame)) +
      geom_point() +
      coord_flip() +
      theme_standard() +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_gradient2(midpoint = median(goalie.seasons$Expected_Points_perGame), mid="grey50", high="forestgreen", low="red") +
      labs(x="",y="Percentage Lift in Points to Team Over Expected", size="Games Played", color="Baseline Expected\nPoints per Game",
           title="Goaltender Contributions to Winning Over Expected, 2016-17\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)")


goalie.seasons %>%
  filter((season %in% c("20162017") & GP > 40 & SA.Goalie != "PETER BUDAJ") | (SA.Goalie %in% c("CRAIG ANDERSON","JONATHAN QUICK")))  %>%
  ungroup() %>%
  select(SA.Goalie) %>%
  left_join(goalie.games_wresult2, by = c("SA.Goalie")) %>%
  filter(season %in% c("20162017")) %>%
  left_join(goalie.seasons[c("Game_Points_Lift_GF_Season","SA.Goalie","season2","GP")], by = c("SA.Goalie","season2")) %>%
  #filter(GP > 10) %>%
  ggplot(aes(x=Game_Points_Lift_GF,y=reorder(SA.Goalie,Game_Points_Lift_GF),fill=Game_Points_Lift_GF_Season),alpha=.25) +
  ggjoy::geom_joy() +
  facet_wrap(~season2) +
  theme_standard() +
  xlim(c(-2,2)) +
  annotate("text",x=1.9,y=20,label="Stolen Game - Game Won Against Odds", angle = 90, color="grey25") +
  annotate("text",x=-1.9,y=20,label="Blown Game - Game Lost Against Odds", angle = 90, color="grey25") +
  geom_vline(xintercept = 0, color="grey20") +
  scale_fill_gradient2(midpoint = 0, mid="grey50", high="forestgreen", low="red") +
  labs(y="",x="Game-Level Team Points - Expected Points (Calculated using Goal Support and xGA, Adjusted for Rebounds)", fill="Season Team Points\nAbove Expected",
       title="Goaltender Game-Level Contributions to Winning Over Expected, 2015-2017\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)")

### 
goalie.seasons %>%
  mutate(Name = ifelse(GP > 5, sapply(strsplit(as.character(SA.Goalie), ' '), function(x) x[length(x)]),"")) %>%
  filter(season %in% c("20162017"))  %>%
  ggplot(aes(x=Expected_Points_GF_Season,y=Actual_Points_Season,color=Game_Points_Lift_GF_Season, size=GP, label=Name),alpha=.25) +
  geom_point() +
  ggrepel::geom_label_repel() +
  theme_standard() +
  annotate("text",x=20,y=50,label="More Points than Expected\nConsidering Goal Support & xG Against", color="grey25") +
  annotate("text",x=50,y=20,label="Fewer Points than Expected\nConsidering Goal Support & xG Against", color="grey25") +
  geom_abline(intercept = 0, slope = 1, color="grey20") +
  scale_color_gradient2(midpoint = 0, mid="grey50", high="forestgreen", low="red") +
  labs(y="Actual Points Gained with Goalie",
       x="Expected Points Gained with Goalie (Calculated using Goal Support and xGA, Adjusted for Rebounds)", 
       color="Team Points Gained\nAbove Expected",
       title="Goaltender Contributions to Winning Compared to Expected, 2016-17\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)")


### By Goalie
goalie_list <- c("COREY CRAWFORD")

goalie.games_wresult2 %>%
  mutate(Season_Type = ifelse(length(season2)>8,"Playoffs","Regular Season")) %>%
  filter(SA.Goalie %in% goalie_list & season != "20072008")  %>%
  left_join(goalie.seasons, by = c("SA.Goalie","season","season2")) %>%
  filter(GP > 5) %>%
  ggplot(aes(x=Game_Points_Lift_GF,y=season2,fill=Game_Points_Lift_GF_Season_perGame),alpha=.25) +
  ggjoy::geom_joy() +
  theme_standard() +
  #facet_wrap(~ Season_Type) +
  xlim(c(-2,2)) +
  annotate("text",x=1.9,y=5,label="Stolen Game - Game Won Against Odds", angle = 90, color="grey25") +
  annotate("text",x=-1.9,y=5,label="Blown Game - Game Lost Against Odds", angle = 90, color="grey25") +
  geom_vline(xintercept = 0, color="grey20") +
  scale_fill_gradient2(midpoint = 0, mid="grey50", high="forestgreen", low="red") +
  labs(y="",x="Game-Level Team Points - Expected Points (Calculated using Goal Support and xGA, Adjusted for Rebounds)", fill="Season Team Points\nAbove Expected per Game",
       title=paste0(goalie_list," Game-Level Contributions to Winning Over Expected\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)"))

### Histograms
goalie.games_wresult2 %>%
  mutate(Season_Type = ifelse(nchar(season2)>8,"Playoffs"," Regular Season")) %>%
  filter(SA.Goalie %in% goalie_list & season != "20072008")  %>%
  left_join(goalie.seasons, by = c("SA.Goalie","season","season2")) %>%
  filter(GP > 5) %>%
  ggplot(aes(x=Game_Points_Lift_GF, fill=Game_Points_Lift_GF_Season, group=Season_Type, color=Season_Type),alpha=.25) +
  #ggjoy::geom_joy() +
  geom_histogram() +
  theme_standard() +
  facet_grid(season ~ SA.Goalie) +
  xlim(c(-2,2)) +
  annotate("text",x=1.9,y=5,label="Stolen Game - Game Won Against Odds", angle = 90, color="grey25") +
  annotate("text",x=-1.9,y=5,label="Blown Game - Game Lost Against Odds", angle = 90, color="grey25") +
  geom_vline(xintercept = 0, color="grey20") +
  scale_fill_gradient2(midpoint = 0, mid="grey50", high="forestgreen", low="red") +
  labs(y="",x="Game-Level Team Points - Expected Points (Calculated using Goal Support and xGA, Adjusted for Rebounds)", fill="Season Team Points\nAbove Expected per Game",
       title=paste0(goalie_list," Game-Level Contributions to Winning Over Expected\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)"))


