############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr)

load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

############################################################################################################################################################################
########SIMULATE GOALIE SEASONS
############################################################################################################################################################################

season.sims <- function(goalies, year, sims) {
  
  season.shots <- scored.data %>%
    filter(SA.Goalie %in% goalies & season == year) %>%
    select(xG, pred.rebound, goal, is.Rebound, gcode, SA.Goalie)
  
  print(dim(season.shots))
  
  ### Display QREAM
  actual.QREAM <- season.shots %>%
    group_by(SA.Goalie) %>%
    summarise((sum(xG) - sum(as.numeric(goal)-1))  / (length(xG) / 100))
    print(actual.QREAM)
  
  sim.QREAM.100 <- matrix(nrow = sims, ncol=0)

  #For each goalie in data
  for(g in unique(season.shots$SA.Goalie)) {
    
    print(g)
    goalie.shots <- season.shots %>% filter(SA.Goalie == g)
    print(dim(goalie.shots))
    
    non.rebound.shots <- goalie.shots %>% filter(is.Rebound == 0)

    sim.goalie.vec <- c()
    
    ## Loop through simulations
    for(s in 1:sims) { 
      
      ## Simulated season
      sim.season.vec <- c()
      
      for(i in 1:nrow(non.rebound.shots)) {
        
        # For each shot with probability of rebound
        sim.rebound <- rbinom(size=1, prob=non.rebound.shots[i,2], n=1)
        
        # Original shot xG plus if rebound multiplicative xG rebound
        updated.xG <-  non.rebound.shots[i,1] + (sim.rebound * ((1 - non.rebound.shots[i,1]) * 0.27))
        
        sim.season.vec[i] <- rbinom(size=1, prob=updated.xG, n=1)
        
      }
      
      ## For simulated season take difference between simulated goals and actual goals
      sim.goalie.vec[s] <- (sum(sim.season.vec) - sum(as.numeric(goalie.shots$goal)-1)) / (length(sim.season.vec) / 100)
      
    }
    
    sim.QREAM.100 <- cbind(sim.QREAM.100,sim.goalie.vec)

  }
  
  colnames(sim.QREAM.100) <- c(unique(season.shots$SA.Goalie))
  
  sim.QREAM.1002 <- sim.QREAM.100 %>% melt()
  
  ## Create histogram data
  totals <- as.data.frame(sim.QREAM.1002) %>% 
    mutate(outcome = sim.QREAM.100) %>%
    group_by(Var2, outcome) %>%
    summarise(share = n() / sims)

  ## Plot histogram
  p1 <- sim.QREAM.1002 %>%
    ggplot() +
    geom_density(aes(x=value, fill=Var2, group=Var2),
                 alpha=0.3, color="white",stat = "density", position = "identity") +
    labs(title=paste0(paste(goalies,sep="", collapse=", ")," ",year," Performance\nSimulated Expected Goals - Actual Goals Against per 100 Shots - ",sims, " Simulations\n@CrowdScoutSprts - xG Model built using nhlscrapr (github.com/C92Anderson/xG-Model)"),
         x="Simulated Expected Goals - Actual Goals per 100 Shots", y="Density", fill="Goalie") +
    annotate("segment", x=0, xend=0, y=0, yend=0.1, color="grey") +
    theme(panel.background = element_blank()) #,legend.position = "none")

  return(list(p1))
  
}


comrie.gillies <- season.sims(c("JON GILLIES","ERIC COMRIE"),"20162017",500)
comrie.gillies[[1]]



top3 <- season.sims(c("SERGEI BOBROVSKY","BRADEN HOLTBY","CAM TALBOT","CAREY PRICE"),"20162017",10000)

next3 <- season.sims(c("HENRIK LUNDQVIST","CORY SCHNEIDER","TUUKKA RASK"),"20162017",10000)


ufa <- season.sims(c("BEN BISHOP","BRIAN ELLIOTT","STEVE MASON"),"20162017",10000)

draft12 <- season.sims(c("MATTHEW MURRAY","FREDERIK ANDERSEN","ANDREI VASILEVSKIY","CONNOR HELLEBUYCK"),"20162017",10000)

nsh <- season.sims(c("PEKKA RINNE","JUUSE SAROS"),"20162017",10000)

chi <- season.sims(c("ANTTI RAANTA","COREY CRAWFORD","SCOTT DARLING"),"20162017",10000)

cali <- season.sims(c("AARON DELL","MARTIN JONES","JONATHAN QUICK","PETER BUDAJ","JOHN GIBSON","JONATHAN BERNIER"),"20162017",10000)

topbackups <- season.sims(c("AARON DELL","SCOTT DARLING","ANTTI RAANTA","JUUSE SAROS"),"20162017",10000)