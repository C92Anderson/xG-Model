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

goalie_wins <- function(szn) {
  
  ### Find
  game_outcomes <- scored_data %>%
    filter(!(Season_Type == "RS" & Period %in% c(4,5))) %>%
    mutate(Home_Score = ifelse(Ev_Team == Home_Team & Goal == 1,1,0),
           Away_Score = ifelse(Ev_Team != Home_Team & Goal == 1,1,0),
           Home_xG = ifelse(Ev_Team == Home_Team & xG_team > 0,xG_team,0),
           Away_xG = ifelse(Ev_Team != Home_Team & xG_team > 0,xG_team,0)) %>%
    group_by(season, season2, Game_Id, Away_Team) %>%
    summarise(Home_Score = sum(Home_Score), 
              Away_Score = sum(Away_Score),
              Home_xG = round(sum(Home_xG),1),
              Away_xG = round(sum(Away_xG),1)) 
  
  rebound_goal_probability <- 0.2560365
  
  
  goalie_games <- scored_data %>%
    filter(!(Season_Type == "RS" & Period %in% c(4,5))) %>%
    filter(SA_Goalie != "") %>%
    mutate(Goalie = sapply(strsplit(as.character(SA_Goalie), ' '), function(x) x[length(x)]),
           SA = 1,
           xGA = ifelse(is_Rebound == 0, xG_raw + (xR * rebound_goal_probability),0),
           SA_Team = ifelse(Ev_Team == Home_Team, as.character(Away_Team),as.character(Home_Team)),
           SA_Venue = ifelse(Ev_Team == Home_Team, "Away", "Home")) %>%
    group_by(SA_Goalie, SA_Venue, SA_Team, season, season2, Game_Id) %>%
    #Total xG and Goals
    summarise(xGA = sum(xGA),
              GA = sum(Goal))
  
  ## Function to calculate win probability based on xGF & xGA
  expected_points <- function(GF=3, xGA=3, sims = 100) {
    
    outcome <- vector(length=sims)
    ot <- vector(length=sims)
    ga <- vector(length=sims)
    
    for(i in 1:sims) {

      GA <- sum(rbinom(size=1, prob=xGA/30, n=30))
      
      sim_game <- ifelse(GF == GA,1 + rbinom(size=1, prob=0.5, n=1),
                  ifelse(GA < GF,2,0))

      outcome[i] <- sim_game
      ot[i] <- GF == GA
      ga[i] <- GA
      
      print(paste0(GA," GA, ",sim_game," Points, OT:", GF == GA," Sim: ",i))
      
    }
    
    expected_points <- mean(outcome)
    #print(expected_points)
    #print(mean(ot))
    #data.frame(GA = ga) %>% group_by(GA) %>% summarise(cnt = n()) %>% print()
    
    #print(paste0("Sims: ",length(outcome)))
    #share_ot <- mean(outcome == 1.5))
    return((expected_points))
    
  }
  

out <-  expected_points(GF=3, xGA=3, sims=100)
  
  simulations <- c()

  for(i in c(1:1000)) {
    
    simulations[i] <- expected_points(GF=3, xGA=3, sims=1000)
  }
  
  mean(simulations)
  ggplot() + geom_density(aes(simulations))
  
  
  gf = c(1:8)
  GA = c(1:8)
  
  win_matrix <- do.call(rbind,lapply(FUN=expected_points,gf,gf))
  
  GA_vec <- vector(length=100)
  
  for(i in 1:100) {
    
    GA_vec[i] <- sum(rbinom(size=1, prob=3/40, n=40))
  
  }
  
  data.frame(GA = GA_vec) %>% group_by(GA) %>% summarise(cnt = n())

    goalie_games_wresult <- goalie_games %>%
    group_by(SA_Team, season, season2, Game_Id) %>%
    summarise(team.goalies = uniqueN(SA_Goalie)) %>%
    inner_join(goalie_games, by = c("SA_Team", "season","season2","Game_Id")) %>%
    filter(team.goalies < 2) %>%
    left_join(game_outcomes, by = c("season","season2", "Game_Id")) %>%
    rowwise() %>% 
    mutate(xGF = ifelse(SA_Venue == "Home", Home_xG, Away_xG),
           Goal_Support = ifelse(SA_Venue == "Home", Home_Score, Away_Score),
           Expected_Points = expected_points(Goal_Support, xGA, 1000),
           Actual_Points = ifelse(SA_Venue == "Home" & Home_Score > GA,2,
                           ifelse(SA_Venue == "Away" & Away_Score > GA,2,
                           ifelse(Away_Score == Home_Score, 1.5, 0)))) 
  

expected_points_data <- goalie_games_wresult %>% 
      mutate(xGA = round(xGA,1)) %>% 
      group_by(Goal_Support, xGA) %>% 
      summarise(Actual_Points = mean(Actual_Points), Sample = n())

expected_points_plot <- expected_points_data %>%
      #filter(Sample > 30) %>%
      ggplot(aes(y=Actual_Points, x=xGA, color = as.factor(ifelse(Goal_Support <6,Goal_Support,"6+")), alpha=Sample, size=Sample)) +
      geom_point() +
      geom_smooth(data = expected_points_data[expected_points_data$Sample > 20, ], method = "lm", se = FALSE, aes(size=2)) +
      theme_standard() + ggthemes::scale_color_gdocs() +
      #scale_fill_gradient2(low="#DC3912",high="#109618", mid="#990099") +
      scale_alpha_continuous(range = c(0.1, 0.8)) +
      #scale_x_discrete(breaks = c(1:8)) +
      labs(title = "Average Standings Points Gained by Team Goal Support and Expected Goals Against",
           color = "Goal Support",
           size="Sample Games",alpha="Sample Games",
           x="Expected Goals Against Goalie",
           y="Average Standings Points")
      
ggsave(filename=paste0("/Users/colander1/Downloads/expected_points_plot.png"), plot=expected_points_plot,  width=13,
       height=12)


  goalie_games_wresult2 <- goalie_games_wresult %>%
    select(-starts_with("home"),
           -starts_with("away")) %>%
    mutate(Game_Points_Lift_GF = Actual_Points - Expected_Points_GF,
           Game_Points_Lift_xGF = Actual_Points - Expected_Points_xGF
           )
  
  
  goalie_seasons <- goalie_games_wresult2 %>%
          group_by(SA_Goalie, season) %>%
          summarise(Potential_Points_Lift_GF = sum(2 - Expected_Points_GF),
                    Potential_Points_Loss_GF = sum(Expected_Points_GF),
                    Actual_Points_Season = sum(Actual_Points),
                    Expected_Points_GF_Season = sum(Expected_Points_GF),
                    Points_Lift = Actual_Points_Season - Expected_Points_GF_Season,
                    Game_Points_Lift_GF_Season = sum(Game_Points_Lift_GF),
                    Points_Gained_oPotential_GF_Season = (Potential_Points_Lift_GF - Game_Points_Lift_GF_Season) / (Potential_Points_Loss_GF + Potential_Points_Lift_GF),
                    
                    Game_Points_Lift_GF_Season_perGame = Game_Points_Lift_GF_Season / n(),
                    GP = n())
  
  
  goalie_wins_plot <- goalie_seasons %>%
        filter(season %in% szn) %>%
        ggplot(aes(x=reorder(SA_Goalie,Points_Lift), y=Points_Lift, size=GP, color=Game_Points_Lift_GF_Season_perGame)) +
        geom_point() +
        coord_flip() +
        theme_standard() +
        geom_hline(yintercept = 0) +
        scale_y_continuous(labels = scales::percent) +
        scale_color_gradient2(midpoint = median(goalie_seasons$Game_Points_Lift_GF_Season_perGame), mid="grey50", high="forestgreen", low="red") +
        labs(x="",y="Percentage Lift in Points to Team Over Expected", size="Games Played", color="Baseline Expected\nPoints per Game",
             title=paste0("Goaltender Contributions to Winning Over Expected, ",szn,
                          "\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)"))
  
  ggsave(filename=paste0("/Users/colander1/Downloads/goalie_wins_plot.png"), plot=goalie_wins_plot,  width=16, height=16)

}

goalie_wins("20172018")




goalie.seasons %>%
  filter((season %in% c("20162017") & GP > 40 & SA_Goalie != "PETER BUDAJ") | (SA_Goalie %in% c("CRAIG ANDERSON","JONATHAN QUICK")))  %>%
  ungroup() %>%
  select(SA_Goalie) %>%
  left_join(goalie_games_wresult2, by = c("SA_Goalie")) %>%
  filter(season %in% c("20162017")) %>%
  left_join(goalie.seasons[c("Game_Points_Lift_GF_Season","SA_Goalie","season2","GP")], by = c("SA_Goalie","season2")) %>%
  #filter(GP > 10) %>%
  ggplot(aes(x=Game_Points_Lift_GF,y=reorder(SA_Goalie,Game_Points_Lift_GF),fill=Game_Points_Lift_GF_Season),alpha=.25) +
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
goalie_wins_v_expected_plot <- goalie.seasons %>%
  mutate(Name = ifelse(Expected_Points_GF_Season > 50 & (Actual_Points_Season - Expected_Points_GF_Season) > 5, 
                       paste0(sapply(strsplit(as.character(SA_Goalie), ' '), function(x) x[length(x)])),"")) %>%
  filter(season %in% c("20122013","20132014","20142015","20152016","20162017")) %>%
  ggplot(aes(x=Expected_Points_GF_Season,y=Actual_Points_Season,color=as.factor(season), size=GP, label=Name),alpha=.25) +
  geom_abline(intercept = 0, slope = 1, color="grey20", size=2) +
  geom_point() +
  ggrepel::geom_label_repel() +
  theme_standard() + ggthemes::scale_color_gdocs() +
  annotate("text",x=20,y=50,label="More Points than Expected\nConsidering Goal Support & xG Against", color="grey25") +
  annotate("text",x=50,y=20,label="Fewer Points than Expected\nConsidering Goal Support & xG Against", color="grey25") +
  geom_abline(intercept = 0, slope = 1, color="grey20", size=2) +
  stat_smooth(method="lm", se=TRUE, fill=NA,# color=as.factor(season), 
              formula = y ~ poly(x, 2, raw=TRUE)) +
  labs(y="Actual Points Gained with Goalie",
       x="Expected Points Gained with Goalie (Calculated using Goal Support and xGA, Adjusted for Rebounds)", 
       color="Season",
       title="Goaltender Contributions to Winning Compared to Expected, 2012-17\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)")


  ggsave(filename="/Users/colander1/Downloads/goalie_wins_v_expected_plot2.png", plot=goalie_wins_v_expected_plot,  width=16, height=16)
  

### By Goalie
goalie_list <- c("COREY CRAWFORD")

goalie_games_wresult2 %>%
  mutate(Season_Type = ifelse(length(season2)>8,"Playoffs","Regular Season")) %>%
  filter(SA_Goalie %in% goalie_list & season != "20072008")  %>%
  left_join(goalie.seasons, by = c("SA_Goalie","season","season2")) %>%
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
goalie_games_wresult2 %>%
  mutate(Season_Type = ifelse(nchar(season2)>8,"Playoffs"," Regular Season")) %>%
  filter(SA_Goalie %in% goalie_list & season != "20072008")  %>%
  left_join(goalie.seasons, by = c("SA_Goalie","season","season2")) %>%
  filter(GP > 5) %>%
  ggplot(aes(x=Game_Points_Lift_GF, fill=Game_Points_Lift_GF_Season, group=Season_Type, color=Season_Type),alpha=.25) +
  #ggjoy::geom_joy() +
  geom_histogram() +
  theme_standard() +
  facet_grid(season ~ SA_Goalie) +
  xlim(c(-2,2)) +
  annotate("text",x=1.9,y=5,label="Stolen Game - Game Won Against Odds", angle = 90, color="grey25") +
  annotate("text",x=-1.9,y=5,label="Blown Game - Game Lost Against Odds", angle = 90, color="grey25") +
  geom_vline(xintercept = 0, color="grey20") +
  scale_fill_gradient2(midpoint = 0, mid="grey50", high="forestgreen", low="red") +
  labs(y="",x="Game-Level Team Points - Expected Points (Calculated using Goal Support and xGA, Adjusted for Rebounds)", fill="Season Team Points\nAbove Expected per Game",
       title=paste0(goalie_list," Game-Level Contributions to Winning Over Expected\nExpected Points Calculated using Goal Support and xGA (Adjusted for Rebounds) \n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)"))

#############
##Repeatability
#############



