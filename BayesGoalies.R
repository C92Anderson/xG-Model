############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(httr)

load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

############################################################################################################################################################################
########GOALIE BAYES
############################################################################################################################################################################

calcPosteriorForProportion <- function(successes, total, a, b)
{
  # Adapted from triplot() in the LearnBayes package
  # Plot the prior, likelihood and posterior:
  likelihood_a = successes + 1;  ## Saves + 1
  likelihood_b = total - successes + 1  ## Goals + 1
  
  posterior_a = a + successes;  ## Success + Beta A
  posterior_b = b + total - successes  ## Goals + Beta B
  
  
  theta = seq(0.005, 0.995, length = 500)
  prior = dbeta(theta, a, b)
  likelihood = dbeta(theta, likelihood_a, likelihood_b)
  posterior  = dbeta(theta, posterior_a, posterior_b)
  m = max(c(prior, likelihood, posterior))
  
  ggplot() +
       geom_line(aes(x=theta, y=posterior, color="Posterior"), linetype=1) +
       geom_line(aes(x=theta, y=likelihood, color="Likelihood"), linetype=2) +
       geom_line(aes(x=theta, y=prior, color="Prior"), linetype=3) +
    scale_colour_manual(
                        breaks = c("Posterior", "Likelihood", "Prior"),
                        values = c("green", "blue",  "red")) +
    labs(title=paste0(goalie, ""),color="")
  
    plot(theta, posterior, type = "l", ylab = "Density", lty = 2, lwd = 3,
       main = paste("beta(", a, ",", b, ") prior, B(", total, ",", successes, ") data,",
                    "beta(", posterior_a, ",", posterior_b, ") posterior"), ylim = c(0, m), col = "red")
  lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
  lines(theta, prior, lty = 3, lwd = 3, col = "green")

  # Print out summary statistics for the prior, likelihood and posterior:
  calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
  calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
  calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
  prior_mode      <- calcBetaMode(a, b)
  likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
  posterior_mode  <- calcBetaMode(posterior_a, posterior_b)
  prior_mean      <- calcBetaMean(a, b)
  likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
  posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
  prior_sd        <- calcBetaSd(a, b)
  likelihood_sd   <- calcBetaSd(likelihood_a, likelihood_b)
  posterior_sd    <- calcBetaSd(posterior_a, posterior_b)
  print(paste("mode for prior=",prior_mode,", for likelihood=",likelihood_mode,", for posterior=",posterior_mode))
  print(paste("mean for prior=",prior_mean,", for likelihood=",likelihood_mean,", for posterior=",posterior_mean))
  print(paste("sd for prior=",prior_sd,", for likelihood=",likelihood_sd,", for posterior=",posterior_sd))
}

## Find Prior Betas
goalie.priors <- scored.data %>%
  group_by(season, SA.Goalie) %>%
  summarise(xSV = 1 - (sum(xG) / n()),
            SV = 1 - (sum(as.numeric(goal)-1) / n()),
            xSV.Diff = SV - xSV,
            total.shots = n()) %>%
  filter(total.shots > 250)

##http://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html


########GOALIE BAYES FUNCTION
goalie.bayesian <- function(goalie, seasons=c("20072008","20082009","20092010","20102011","20112012",
                                              "20122013","20132014","20142015","20152016","20162017"),sims=100) {

  library("LearnBayes")
  findBeta <- function(quantile1,quantile2,quantile3)
  {
    # find the quantiles specified by quantile1 and quantile2 and quantile3
    quantile1_p <- quantile1[[1]]; quantile1_q <- quantile1[[2]]
    quantile2_p <- quantile2[[1]]; quantile2_q <- quantile2[[2]]
    quantile3_p <- quantile3[[1]]; quantile3_q <- quantile3[[2]]
    
    # find the beta prior using quantile1 and quantile2
    
    priorA <- LearnBayes::beta.select(quantile1,quantile2)
    priorA_a <- priorA[1]; priorA_b <- priorA[2]
    
    # find the beta prior using quantile1 and quantile3
    priorB <- beta.select(quantile1,quantile3)
    priorB_a <- priorB[1]; priorB_b <- priorB[2]
    
    # find the best possible beta prior
    diff_a <- abs(priorA_a - priorB_a); diff_b <- abs(priorB_b - priorB_b)
    step_a <- diff_a / 100; step_b <- diff_b / 100
    if (priorA_a < priorB_a) { start_a <- priorA_a; end_a <- priorB_a }
    else                     { start_a <- priorB_a; end_a <- priorA_a }
    if (priorA_b < priorB_b) { start_b <- priorA_b; end_b <- priorB_b }
    else                     { start_b <- priorB_b; end_b <- priorA_b }
    steps_a <- seq(from=start_a, to=end_a, length.out=1000)
    steps_b <- seq(from=start_b, to=end_b, length.out=1000)
    max_error <- 10000000000000000000
    best_a <- 0; best_b <- 0
    for (a in steps_a)
    {
      for (b in steps_b)
      {
        # priorC is beta(a,b)
        # find the quantile1_q, quantile2_q, quantile3_q quantiles of priorC:
        priorC_q1 <- qbeta(c(quantile1_p), a, b)
        priorC_q2 <- qbeta(c(quantile2_p), a, b)
        priorC_q3 <- qbeta(c(quantile3_p), a, b)
        priorC_error <- abs(priorC_q1-quantile1_q) +
          abs(priorC_q2-quantile2_q) +
          abs(priorC_q3-quantile3_q)
        if (priorC_error < max_error)
        {
          max_error <- priorC_error; best_a <- a; best_b <- b
        }
      }
    }
    print(paste("The best beta prior has a=",best_a,"b=",best_b))
    return(list(best_a, best_b))
  }
  
  goalie.shots <- scored.data %>% 
      filter(SA.Goalie == goalie & season %in% seasons) %>% 
      select(SA.Goalie, season, season2, gcode, season, xG, goal, pred.rebound, is.Rebound)
  
  ##Season Plots
  season.data <- data.frame(Season=character(),
                            Variable=character(), 
                            X=numeric(), 
                            Y=numeric()) 
  
  season.text.string <- c()
          
  ## Season xG
  season.xG.spread <- c()
  
  for(i in unique(goalie.shots$season)) {
   
    season.shots <- goalie.shots %>%
            select(season, xG, goal, pred.rebound, is.Rebound) %>%
            filter(season == i)
  
    ## Simulate Non-Rebound Shots
    non.rebound.shots <- season.shots %>% filter(is.Rebound == 0)
    
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
        summarise(sim = sum(sim.goals))
      
      sim.goalie.vec[s] <- sim.outcome[[1]]
      
    }
    
    xSV.Goalie <- 1-mean(season.shots$xG)

    ##Convert to sv%
    sim.goalie.vec2 <- (nrow(non.rebound.shots) - sim.goalie.vec) / nrow(non.rebound.shots)
    
    for(k in c(0.00001, 0.0001,0.001,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12)) {
     
        sig <- k
        xG.quantiles <- quantile(sim.goalie.vec2, c(0.50, 1-sig, sig)) 
        
        print(paste0("Simulated xG 50th, ",round(1-sig,3),"th, ",round(sig,3),"th Percentile: ",paste(xG.quantiles,sep="", collapse=", ")))
        
        quantile1 <- list(p=0.50,x = xG.quantiles[[1]])    # we believe the median of the prior is 0.85
        quantile2 <- list(p=1-sig,x = xG.quantiles[[2]]) # we believe the 99.999th percentile of the prior is 0.95
        quantile3 <- list(p=sig,x = xG.quantiles[[3]]) # we believe the 0.001st percentile of the prior is 0.60
    
        check <- try(findBeta(quantile1,quantile2,quantile3)) 
        
        if(isTRUE(class(check)=="try-error")) { 
            next 
        } else { 
          betas <- check
          season.xG.spread[i] <- abs(xG.quantiles[[2]] - xG.quantiles[[3]])
          distro.param <- sig
          break
        }
        
    }
    
    beta_a <- betas[[1]]
    beta_b <- betas[[2]]

    
    # Declare Saves & Goals
    total <- nrow(season.shots)
    successes <- nrow(season.shots) - sum(as.numeric(season.shots$goal)-1)
    
    x <- seq(.001, .999, .001) ##Set up for creating the distributions

    # Overall
    likelihood_a <- successes + 1;  ## Saves + 1
    likelihood_b <- total - successes + 1  ## Goals + 1
    
    posterior_a <- beta_a + successes;  ## Success + Beta A
    posterior_b <- beta_b + total - successes  ## Goals + Beta B
    
    theta <- seq(0.005, 0.995, length = 500)
    prior <- dbeta(theta, beta_a, beta_b)
    print(max(prior))
    likelihood <- dbeta(theta, likelihood_a, likelihood_b)
    posterior  <- dbeta(theta, posterior_a, posterior_b)
    m <- max(c(prior, likelihood, posterior))
    
    calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
    calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
    calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
    prior_mode      <- calcBetaMode(beta_a, beta_b)
    likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
    posterior_mode  <- calcBetaMode(posterior_a, posterior_b)
    prior_mean      <- calcBetaMean(beta_a, beta_b)
    likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
    posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
    prior_sd        <- calcBetaSd(beta_a, beta_b)
    likelihood_sd   <- calcBetaSd(likelihood_a, likelihood_b)
    posterior_sd    <- calcBetaSd(posterior_a, posterior_b)
    
    diff_mean <- posterior_mean - prior_mean
    diff_std <- sqrt(((posterior_sd ** 2))  +  ((prior_sd ** 2)))
    
    goals_prevented <- diff_mean * total
    goals_prevented_range <- diff_std * total
    goals_prevented_min <- (diff_mean - diff_std) * total
    goals_prevented_max <- (diff_mean + diff_std) * total
    goals_prevented_100 <- diff_mean * total / (total / 100)
    
    ## Create Plot Data
    data <- rbind(cbind(i,"Prior (Expected Sv%)",as.numeric(theta), as.numeric(prior)),
                              cbind(i,"Posterior (Bayesian Sv%)",as.numeric(theta), as.numeric(posterior)),
                              cbind(i,"Likelihood (Realized Sv%)",as.numeric(theta), as.numeric(likelihood)))
        
    colnames(data) <- c("Season", "Variable","X","Y")
    
    season.data <- rbind(season.data,data)
    
    ## Create Strings
    season.text.string[i] <- paste0("Expected Save Percentage: ", round(prior_mean*100,2),"%, ",round(prior_sd*100,2),"% SD",
                 "\nActual Save Percentage: ", round(likelihood_mean*100,2),"%, ",round(likelihood_sd*100,2),"% SD",
                 "\nPosterior Save Percentage: ", round(posterior_mean*100,2),"%, ",round(posterior_sd*100,2),"% SD",
                 "\nBayesian Save Percentage Lift: ",round(diff_mean*100,2),"%, ",round(diff_std*100,2),"% SD",
                 "\n",round(goals_prevented,1), " Goals Prevented on ",total," Non-Rebound Shots",
                 "\n",round(goals_prevented / (total / 100),2)," / 100 Shots")

  }
  
  season.data$X <- as.numeric(as.character(season.data$X))
  season.data$Y <- as.numeric(as.character(season.data$Y))
  
  p <- season.data %>% 
    filter(Season %in% c("20112012","20122013","20132014","20142015","20152016","20162017")) %>%
    mutate(X1 = as.numeric(as.character(X)),
           Y1 = as.numeric(as.character(Y))) %>%
    ggplot() +
    geom_line(aes(x=X1, y=Y1, group=Variable, color=Variable, linetype=Variable)) +
    facet_wrap(~Season, ncol = 2) +
    #scale_color_manual(name="",values=c("Prior (Expected Sv%)"= "red", "Likelihood (Realized Sv%)"="green", "Posterior (Bayesian Sv%)"="blue")) +
    guides(col = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +
    labs(title=paste0(goalie," Bayesian Performance ",max(2011,min(substr(season.data$Season,1,4))),"-2017\nExpected Sv% (Prior) Simulated ",sims,"x Using Expected Rebound & Goal Model\n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)"),
         color="",linetype="",
         x="(Expected) Save Percentage", y="Density") +
    theme(legend.position = "top") +
    theme_standard() + ggthemes::scale_color_gdocs() +
    scale_x_continuous(labels = scales::percent, limits = c(0.8, 1)) 
  
  season.text <- data.frame(Season=c(levels(season.data$Season)),  #panel
                   Text=season.text.string) %>%            #label text
    filter(Season %in% c("20112012","20122013","20132014","20142015","20152016","20162017"))
  
  # add labels to plot:
  p1 <- p + geom_text(aes(x=0.8, y=max(as.numeric(as.character(season.data$Y))), label=Text), data=season.text, hjust=0, vjust=1)
  
  #########
  ###Career Look
    ## Initialize functions
    calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
    calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
    calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }
    
    ## Goalie Shots
    goalie.shots.bayes <- goalie.shots %>%
          group_by(SA.Goalie, season, season2, gcode) %>%
          summarise(SA = n(),
                    goals = sum(as.numeric(goal)-1),
                    xG = sum(xG)) %>%
          ungroup() %>%
          mutate(#Game = paste0(season, "-",row_number()),
                 cum.shots = cumsum(SA),    # Overall
                 cum.saves = cum.shots - cumsum(goals),
                 cum.xG = cumsum(xG),
                 
                 expected.sv = (cum.shots - cum.xG) / cum.shots,
  
                 xG.spread = as.numeric(season.xG.spread[paste(season)]),
                 
                 distro.param = distro.param,
                 
                 low.bound = (expected.sv - (xG.spread/1))) 
      
        ## For each game update beta
        updated.beta <- data.frame(beta_a = numeric(),
                                   beta_b = numeric())
        
        ## Loop through each game
        for (i in 1:nrow(goalie.shots.bayes)) {
          
          out <- goalie.shots.bayes[i,c("expected.sv","distro.param","low.bound")]
          
          betas = beta.select(list(p=0.5, x=out$expected.sv), list(p=out$distro.param, x=out$low.bound))
          
          updated.beta <- rbind(updated.beta,betas)
          
        }
        
        colnames(updated.beta) <- c("beta_a","beta_b")
        
        ## With Prior, Calculate Likelihood & Posterior
        goalie.shots.bayes2 <- cbind(goalie.shots.bayes, updated.beta) %>%
          
                 mutate(prior_mean   = calcBetaMean(beta_a, beta_b),
                 prior_sd     = calcBetaSd(beta_a, beta_b),
                 
                  likelihood_a = cum.saves + 1,  ## Saves + 1
                  likelihood_b = cum.shots - cum.saves + 1,  ## Goals + 1
            
                  posterior_a = beta_a + cum.saves,  ## Success + Beta A
                  posterior_b = beta_b + cum.shots - cum.saves,  ## Goals + Beta B
 
                  prior_mean      = calcBetaMean(beta_a, beta_b),
                  likelihood_mean = calcBetaMean(likelihood_a, likelihood_b),
                  posterior_mean  = calcBetaMean(posterior_a, posterior_b),
                  prior_sd        = calcBetaSd(beta_a, beta_b),
                  likelihood_sd   = calcBetaSd(likelihood_a, likelihood_b),
                  posterior_sd    = calcBetaSd(posterior_a, posterior_b),
                  
                  diff_mean = posterior_mean - prior_mean,
                  diff_std = sqrt(((posterior_sd ** 2))  +  ((prior_sd ** 2))),
                  
                  goals_prevented = diff_mean * cum.shots,
                  goals_prevented_range = diff_std * cum.shots)
                
    p2 <- goalie.shots.bayes2 %>% 
          ggplot(aes(x=cum.shots, y=diff_mean * 100, color=season)) +
          geom_point(aes(x=cum.shots, y=diff_mean * 100, color=season), alpha=0.25) +
          stat_smooth(method="loess", se=TRUE, alpha=0.3) +
      labs(title=paste0(goalie," Bayesian Save Percentage Lift ",min(substr(season.data$Season,1,4)),"-2017\nExpected Sv% (Prior) Simulated ",sims,"x Using Expected Rebound & Goal Model\n@CrowdScoutSprts (github.com/C92Anderson/xG-Model)"),
           color="Season",
           x="Cumulative Career Shots", y="Goals Prevented / 100 Shots (Posterior Bayesian Sv% - Prior Expected Sv%)") +
      theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25)) +
      theme_standard() + ggthemes::scale_color_gdocs() +
      scale_y_continuous(labels = scales::percent) + 
      annotate("segment",x=0,y=0,yend=0, xend=max(goalie.shots.bayes2$cum.shots),color="dark grey", size=0.5)

return(list(p1,p2))

}  

mm <- goalie.bayesian("MATTHEW MURRAY")

el <- goalie.bayesian("EDDIE LACK")

sd <- goalie.bayesian("SCOTT DARLING")



bh <- goalie.bayesian("BRADEN HOLTBY")

av <- goalie.bayesian("ANDREI VASILEVSKIY")

ct <- goalie.bayesian("CAM TALBOT")

ar <- goalie.bayesian("ANTTI RAANTA")

cp <- goalie.bayesian("CAREY PRICE")



mm <- goalie.bayesian("MATTHEW MURRAY")

#hl <- goalie.bayesian("HENRIK LUNDQVIST")
##

ct <- goalie.bayesian("CAM TALBOT")


