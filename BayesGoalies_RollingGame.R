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

# just like the graph, we have to filter for the players we actually
# have a decent estimate of
alpha_beta <- function(vec) {
  
  #m <- MASS::fitdistr(vec, dbeta, start = list(shape1 = 1, shape2 = 10))
  fit_beta <- fitdistrplus::fitdist(vec, distr = "beta", method = "mme", lower = c(0, 0), start = list(scale = 0.9, shape = 1))
  alpha0 <- fit_beta$estimate[1]
  beta0 <- fit_beta$estimate[2]
  
  return(list(alpha0, beta0))
}

########GOALIE BAYES FUNCTION
goalie.bayesian <- function(goalie, yheight) {

  library("LearnBayes")
  findBeta <- function(quantile1,quantile2,quantile3)  {
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
  
  goalie_shots <- scored.data %>% 
      filter(SA.Goalie == goalie & season != "20072008") %>% 
      select(SA.Goalie, season, gcode, season, xG, goal, pred.rebound, is.Rebound) %>% 
    mutate(NonRebound_Shot = ifelse(is.Rebound == 0, 1, 0),
           xG_FirstShot = ifelse(is.Rebound == 0, xG, 0),
           xG_RB = pred.rebound *  0.2556621 ) %>%
    group_by(SA.Goalie, season) %>%
    mutate(season_chunk = as.numeric(paste0(as.numeric(as.character(season)),floor(seq_along((SA.Goalie)) / (n() / 9)))))
  

  for(i in unique(goalie_shots$season_chunk)) {
   
    ##Game Plots
    game_data <- data.frame(SeasonGame=character(),
                            prior=integer(),
                            Variable=character(), 
                            X=numeric(), 
                            Y=numeric()) 
    
    mean_data <- data.frame(SeasonGame=character(),
                            prior=integer(),
                            prior_mean=numeric(),
                            likelihood_mean=numeric(),
                            posterior_mean=numeric()) 
    
    
    game_shots <- goalie_shots %>%
            filter(season_chunk <= i)
    
    for(j in c(1000)) {
      
              beta_a <- j - (mean(game_shots$xG_FirstShot + game_shots$xG_RB) * j)
              beta_b <- mean(game_shots$xG_FirstShot + game_shots$xG_RB) * j
          
              print(beta_a / (beta_b + beta_a))
              
              # Declare Saves & Goals
              total <- nrow(game_shots)
              successes <- nrow(game_shots) - sum(as.numeric(game_shots$goal)-1)
              
          # Overall
          likelihood_a <- successes + 1;  ## Saves + 1
          likelihood_b <- total - successes + 1  ## Goals + 1
          
          posterior_a <- beta_a + successes;  ## Success + Beta A
          posterior_b <- beta_b + total - successes  ## Goals + Beta B
          
          theta <- seq(0,1,length=10000)
          prior <- dbeta(theta, beta_a, beta_b)
          print(max(prior))
          likelihood <- dbeta(theta, likelihood_a, likelihood_b)
          posterior  <- dbeta(theta, posterior_a, posterior_b)
          m <- max(c(prior, likelihood, posterior))
          
          calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
          prior_mean      <- calcBetaMean(beta_a, beta_b)
          likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
          posterior_mean  <- calcBetaMean(posterior_a, posterior_b)
      
          mean_data <- rbind(mean_data,cbind(i,j,prior_mean, likelihood_mean, posterior_mean))
          
          ## Create Plot Data
          data <- rbind(cbind(i,j,"Prior (Expected Sv%)",as.numeric(theta), as.numeric(prior)),
                                    cbind(i,j,"Final Posterior (Bayesian Sv%)",as.numeric(theta), as.numeric(posterior)),
                                    cbind(i,j,"Likelihood (Realized Sv%)",as.numeric(theta), as.numeric(likelihood)))
              
          colnames(data) <- c("SeasonGame","Prior","Variable","X","Y")
          
          game_data <- rbind(game_data,data)
          
        game_data$X <- as.numeric(as.character(game_data$X))
        game_data$Y <- as.numeric(as.character(game_data$Y))
        
    }
  
  colnames(mean_data) <- c("SeasonGame","Prior","prior_mean","likelihood_mean","posterior_mean")
  
  mean_data$Prior <- as.factor(mean_data$Prior)

  p <- game_data %>% 
    filter(X >= 0.85) %>%
    filter(Prior == 1000) %>%
    mutate(X1 = as.numeric(as.character(X)),
           Y1 = as.numeric(as.character(Y))) %>%
    ggplot() +
    #geom_segment(aes(x = prior_mean, xend = posterior_mean,y = floor(yheight * 0.12), yend=floor(yheight * 0.12)), color = ifelse(posterior_mean > prior_mean, "#3366CC","#DC3912"),
    #             size=2, alpha=0.5) +
    geom_segment(aes(x = prior_mean, xend = likelihood_mean,y = floor(yheight * 0.16), yend=floor(yheight * 0.16)), color = ifelse(likelihood_mean > prior_mean, "#109618","#DC3912"),
                 size=2, alpha=0.5) +
    geom_line(aes(x=X1, y=Y1, group=Variable, color=as.factor(Variable), linetype=as.factor(Variable)),size=1.5) +
    geom_segment(aes(x = prior_mean,xend = prior_mean, y = 0, yend=floor(yheight * 0.1)), color = "#DC3912", size=2, alpha=0.5) +
    geom_segment(aes(x = likelihood_mean, xend = likelihood_mean, y = 0, yend=floor(yheight * 0.1)), color = "#109618", size=2, alpha=0.5) +
    #geom_segment(aes(x = posterior_mean, xend = posterior_mean,y = 0, yend=floor(yheight * 0.1)), color = "#3366CC", size=2, alpha=0.5) +
    geom_segment(data=mean_data,aes(x = posterior_mean, xend = posterior_mean,y = 0, yend=floor(yheight * 0.1)), color = "#3366CC", size=2) +
    #geom_text(x=0.86, y=floor(yheight * 0.12),label="Bayes Lift (BayesSv% - xSv%)", hjust=0, size=2.5, color="grey50") +
    #geom_text(x=0.86, y=floor(yheight * 0.16),label="xG Lift (Sv% - xSv%)", hjust=0, size=2.5, color="grey50") +
  scale_color_manual(name="", values=c("Prior (Expected Sv%)"= "#DC3912", 
                                "Likelihood (Realized Sv%)"="#109618", 
                                "Final Posterior (Bayesian Sv%)"="#3366CC"
    #                            #"Posteriors (Bayesian Sv%)" = "grey50"
                                )) +
    #geom_text(aes(x=0.86, y=floor(yheight * 0.9), label=paste0("Cumulative Shots: ",total,"\nSeason: ",substr(i,1,8))), hjust=0, size=5, color="grey50") +
     guides(col = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +
    labs(title=paste0(goalie," Bayesian Performance\nCumulative Shots: ",total,"\nSeason: ",substr(i,1,8)),
         color="",linetype="",
         x="(Expected) Save Percentage", y="Density") +
    facet_grid(~paste0(Prior," Shot Prior")) +
    theme_standard() +
    ylim(0,yheight) +
    theme(legend.position = "top") +
  scale_x_continuous(labels = scales::percent, limits = c(0.85, 1))
  
  ggsave(filename=paste0("/Users/colander1/Downloads/BayesPlots/",goalie,"-game-",i,".png"), plot=p,  width=13, height=6)
  
}  

}  

goalie.bayesian("ANDREI VASILEVSKIY",110)

goalie.bayesian("HENRIK LUNDQVIST",225)

goalie.bayesian("TUUKKA RASK",200)
goalie.bayesian("PEKKA RINNE",200)


goalie.bayesian("MATTHEW MURRAY",100)

goalie.bayesian("JAROSLAV HALAK",120)

goalie.bayesian("MARTIN BIRON",120)



goalie.bayesian("CAREY PRICE",120)

goalie.bayesian("JAROSLAV HALAK",120)

goalie.bayesian("ANTTI RAANTA")

goalie.bayesian("CAM TALBOT")




goalie.bayesian("MALCOLM SUBBAN")





goalie.bayesian("EDDIE LACK")

goalie.bayesian("SCOTT DARLING")



bh <- goalie.bayesian("BRADEN HOLTBY")

av <- goalie.bayesian("ANDREI VASILEVSKIY")

ct <- goalie.bayesian("CAM TALBOT")

ar <- goalie.bayesian("ANTTI RAANTA")

cp <- goalie.bayesian("CAREY PRICE")




#hl <- goalie.bayesian("HENRIK LUNDQVIST")
##




