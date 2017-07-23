############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine); library(data.table)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr)

load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

############################################################################################################################################################################
########SIMULATE GOALIE SEASONS
############################################################################################################################################################################

scored.data.wtype <- scored.data %>%
        group_by(SA.Goalie) %>%
        summarise(career.shots = n(),
                  season.cnt = uniqueN(season),
                  shots.per.season = career.shots / season.cnt) %>%
        left_join(scored.data, by="SA.Goalie") %>%
        mutate(SA.Goalie = ifelse(shots.per.season < 300,"REPLACEMENT-LT300/yr",
                           ifelse(shots.per.season < 500,"REPLACEMENT-LT500/yr",
                    SA.Goalie))) 
  

replacement.check <- scored.data.wtype %>% group_by(SA.Goalie) %>% summarise(career.shots = n())


############################################################################################################################################################################
########SIMULATE GOALIE SEASONS
############################################################################################################################################################################

season.sims <- function(goalies, seasons = c("20072008","20082009","20092010","20102011","20112012",
                                          "20122013","20132014","20142015","20152016","20162017"), sims) {
  
  
  season.shots <- scored.data.wtype %>%
    filter(SA.Goalie %in% goalies & season2 %in% seasons) %>%
    mutate(Goalie.Year = paste0(SA.Goalie,substr(season,7,8))) %>%
    select(xG, pred.rebound, goal, is.Rebound, gcode, Goalie.Year)
  
  print(dim(season.shots))
  
  ### Display QREAM
  actual.QREAM <- season.shots %>%
    group_by(Goalie.Year) %>%
    summarise((sum(xG) - sum(as.numeric(goal)-1))  / (length(xG) / 100))
    print(actual.QREAM)
  
  sim.QREAM.100 <- matrix(nrow = sims, ncol=0)

  #For each goalie in data
  for(g in unique(season.shots$Goalie.Year)) {
    
    print(g)
    goalie.shots <- season.shots %>% filter(Goalie.Year == g)
    print(dim(goalie.shots))
    
    non.rebound.shots <- goalie.shots %>% filter(is.Rebound == 0)
    
    print(nrow(non.rebound.shots))
    
    sim.goalie.vec <- vector(length = sims)
    
    ## Loop through simulations
    for(s in 1:sims) { 
      
        set.seed(s)
        sim.outcome <- non.rebound.shots %>%
          rowwise() %>%
           mutate(# For each shot with probability of rebound
                  sim.rebound = rbinom(size=n(), prob=pred.rebound, n=1),
                  
                  # Original shot xG plus if rebound multiplicative xG rebound
                  updated.xG = xG + (sim.rebound * (as.numeric(1 - xG) * 0.27)),
                  
                  # Simulated Goals
                  sim.goals = rbinom(size=1, prob=updated.xG, n=1)) %>%
            ungroup() %>%
            group_by() %>%
            summarise(sim = (sum(sim.goals) - sum(as.numeric(goal)-1)) / (n() / 100))
        
        sim.goalie.vec[s] <- sim.outcome[[1]]

    }
    
    sim.QREAM.100 <- cbind(sim.QREAM.100,sim.goalie.vec)

  }
  
  colnames(sim.QREAM.100) <- c(unique(season.shots$Goalie.Year))
  
  sim.QREAM.1002 <- sim.QREAM.100 %>% melt()
  colnames(sim.QREAM.1002) <- c("SeasonId", "GoalieSeason","value")
  
  ## Create histogram data
  totals <- as.data.frame(sim.QREAM.1002) %>% 
    mutate(outcome = sim.QREAM.100) %>%
    group_by(GoalieSeason, outcome) %>%
    summarise(share = n() / sims)

  ## Plot histogram
  p1 <- sim.QREAM.1002 %>%
    mutate(SA.Goalie = substr(GoalieSeason,1,length(GoalieSeason)-2),
           Season = substr(GoalieSeason,length(GoalieSeason)-1,length(GoalieSeason))) %>%
    ggplot() +
    annotate("segment", x=0, xend=0, y=0, yend=0.2, color="grey", size=1.5) +
    geom_density(aes(x=value, color=GoalieSeason, group=GoalieSeason, linetype = GoalieSeason, fill=GoalieSeason),
                  alpha = 0.15, size = 1.25, stat = "density", position = "identity") +
    labs(title=paste0(paste(goalies,sep="", collapse=", ")," ",paste(seasons,sep="", collapse=", ")," Performance\nSimulated Expected Rebounds & Goals - Actual Goals Against per 100 Shots Against - ",sims, " Simulations\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
         x="Simulated Expected Goals - Actual Goals per 100 Shots Against\n(Further right represents outcomes where goalie performed better)", 
         y="Density\n(Higher values represent greater likelihood of outcome)", color="Goalie Season",
         linetype = "Goalie Season", fill="Goalie Season") +
    theme(panel.background = element_blank()) #,legend.position = "none")
  

  return(list(p1))
  
}


playoffs <- season.sims(c("PEKKA RINNE","MATTHEW MURRAY")
                        ,c("20162017p"),100)
playoffs[[1]]

lehner <- season.sims(c("ROBIN LEHNER"),c("20132014","20142015","20152016","20162017"),100)
lehner[[1]]


backups <- season.sims(c("STEVE MASON","ANTTI RAANTA","BRIAN ELLIOTT","SCOTT DARLING","PHILIPP GRUBAUER",
                         "PETR MRAZEK","ROBIN LEHNER","MIKE SMITH","AARON DELL"),c("20162017"),500)
backups[[1]]

darling <- season.sims(c("HENRIK LUNDQVIST","ANTTI RAANTA"),c("20152016","20162017"),100)
darling[[1]]


comrie.gillies <- season.sims(c("JON GILLIES","ERIC COMRIE"),"20162017",500)
comrie.gillies[[1]]

trade <- season.sims(c("PHILIPP GRUBAUER","AARON DELL"),c("20152016","20162017"),1000)
trade[[1]]

top3 <- season.sims(c("SERGEI BOBROVSKY","BRADEN HOLTBY","CAREY PRICE"),"20162017",100)
top3[[1]]
next3 <- season.sims(c("HENRIK LUNDQVIST","CORY SCHNEIDER","TUUKKA RASK"),"20162017",10000)

lack <- season.sims(c("EDDIE LACK"),c("20132014","20142015","20152016","20162017"),1000)
lack[[1]]
ufa <- season.sims(c("BEN BISHOP","BRIAN ELLIOTT","STEVE MASON"),"20162017",10000)

draft12 <- season.sims(c("MATTHEW MURRAY","FREDERIK ANDERSEN","ANDREI VASILEVSKIY","CONNOR HELLEBUYCK"),"20162017",10000)

nshstl <- season.sims(c("PEKKA RINNE","JAKE ALLEN"),"20162017",5000)
nshstl[[1]]


hi <- season.sims(c("ANTTI RAANTA","COREY CRAWFORD","SCOTT DARLING"),"20162017",10000)

cali <- season.sims(c("AARON DELL","MARTIN JONES","JONATHAN QUICK","PETER BUDAJ","JOHN GIBSON","JONATHAN BERNIER"),"20162017",10000)

u25 <- season.sims(c("CALVIN PICKARD","JOONAS KORPISALO"),c("20152016","20162017"),1000)


tor <- season.sims(c("FREDERIK ANDERSEN","JHONAS ENROTH","CURTIS MCELHINNEY"),c("20162017"),1000)

car <- season.sims(c("EDDIE LACK","CAM WARD"),c("20152016","20162017"),1000)

cgy <- season.sims(c("CHAD JOHNSON","BRIAN ELLIOTT","JON GILLIES"),c("20152016","20162017"),10)

det <- season.sims(c("JIMMY HOWARD","PETR MRAZEK"),c("20152016","20162017"),1000)

miller <- season.sims(c("RYAN MILLER"),c("20142015","20152016","20162017"),1000)

nyr <- season.sims(c("ANTTI RAANTA","HENRIK LUNDQVIST"),c("20152016","20162017"),10)

replacement <- season.sims(c("REPLACEMENT-LT300/yr"),c("20142015","20152016","20162017"),100)
replacement[[1]]

replacement <- season.sims(c("REPLACEMENT-LT500/yr"),c("20082009","20092010","20102011","20112012",
                                                        "20122013","20132014","20142015","20152016","20162017"),50)
replacement[[1]]

talbot <- season.sims(c("CAM TALBOT"),c("20132014","20142015","20152016","20162017"),50)

season.sims(c("JOONAS KORPISALO","PHILIPP GRUBAUER","ANTTI RAANTA"),,50)[[1]]

timt <- season.sims(c("TIM THOMAS"),c("20072008","20082009","20092010","20102011","20112012",
                      "20122013","20132014"),50)

fleury <- season.sims(c("MARC-ANDRE FLEURY"),,50)


murray <- season.sims(c("MATTHEW MURRAY"),c("20152016","20162017"),50)

lack <- season.sims(c("EDDIE LACK"),,c("20132014","20142015","20152016","20162017"),50)

talbot15 <- season.sims(c("CAM TALBOT"),c("20132014","20142015"),50)
talbot15[[1]]

jones15 <- season.sims(c("MARTIN JONES"),c("20132014","20142015"),50)
jones15[[1]]

raanta <- season.sims(c("ANTTI RAANTA"),c("20142015","20152016","20162017"),50)


cgy <- season.sims(c("CHAD JOHNSON","BRIAN ELLIOTT","JON GILLIES"),c("20152016","20162017"),10)

bishop <- season.sims(c("BEN BISHOP"),c("20142015","20152016","20162017"),100)
