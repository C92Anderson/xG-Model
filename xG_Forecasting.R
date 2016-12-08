############################################################################################################################################################################
#
# PROJECT:        Use xG Model to Predict Goaltender Performance
#
# PURPOSE:        Develop features to possibly predict future goaltender performance. 
#
# CREATED BY:     Cole Anderson (cole92anderson@gmail.com)
#
# LAST UPDATED:   12/07/2016
#
# PROCESS:        0 - SYSTEM PREP
#                 1 - UPDATE GCODE AND LOAD NHL PBP DATA USING NHLSCRAPR
#                 2 - LOGISTIC MODEL TO DEVELOP XG MODEL
#                 3 - FACTOR ANALYSIS 
#                 4 - K-MEANS CLUSTERING AND OVERLAY SEGMENT AND TRANSACTION DATA
#
############################################################################################################################################################################

############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret)
getwd()
load("~/Documents/CWA/Hockey Data/roster.Rda")

# Load scored data
load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

#################################################################################
##### Summarize Goalie Season and Produce Metrics
#################################################################################

# List of goalies in season
all.goalie.list <- scored.data %>% 
  filter(nchar(SA.Goalie) > 0) %>% 
  distinct(SA.Goalie) %>% as.list()

# Loop through each goalie and append
all.goalie.game <- plyr::rbind.fill(lapply(FUN=QREAM.fun,all.goalie.list))


# List of goalies in season
all.goalie.list <- scored.data %>% 
  filter(nchar(SA.Goalie) > 0) %>% 
  distinct(SA.Goalie) %>% as.list()

# Loop through each goalie and append
all.goalie.game <- plyr::rbind.fill(lapply(FUN=QREAM.fun,all.goalie.list))

goalie.season.level <- all.goalie.game %>%
  group_by(SA.Goalie, season) %>%
  do(last.shot = tail(., n=1))

# Function to calculate goalie-season level scores
goalie.season.scores <- function(goalie) {
  
  output <- NULL
  ###loop through each possible season
  for(season in c("20162017","20152016","20142015","20132014","20122013","20112012","20102011","20092010","20082009","20072008")) {
    
    goalie.season <- subset(goalie.game,SA.Goalie==goalie & Season==season)
    
    ##only calculate season if over 10 games
    if(nrow(goalie.season) > 10 ) {
      
      lm.goalie.season <- lm(goalie.season$QREAM ~ goalie.season$cum.Shots)
      Season.Lift <- as.numeric(lm.goalie.season$coefficients[2])
      Season.Consistency <- summary(lm.goalie.season)$r.squared
      out <- data.frame(cbind(Season.Lift,Season.Consistency))
      
      out$Season.ShotsAgainst <- max(goalie.season$cum.Shots) - min(goalie.season$cum.Shots - goalie.season$game.SA)
      out$Season.Goals <- max(goalie.season$cum.Goals) - min(goalie.season$cum.Goals - goalie.season$game.GA)
      out$Season.xGA <- max(goalie.season$cum.xG) - min(goalie.season$cum.xG - goalie.season$game.xGA)
      
      ####Career to Date
      out$CTD.Shots <- min(goalie.season$cum.Shots - goalie.season$game.SA)
      out$CTD.Lift <- head(goalie.season$QREAM - (goalie.season$game.xGA - goalie.season$game.GA),1)
      
      goalie.CTD <- subset(goalie.game,SA.Goalie==goalie & as.numeric(Season) < as.numeric(season))
      
      if(nrow(goalie.CTD) > 10) {
        
        lm.goalie.CTD <- lm(goalie.CTD$QREAM ~ goalie.CTD$cum.Shots)
        out$CTD.Lift <- as.numeric(lm.goalie.CTD$coefficients[2])
        out$CTD.Consistency <- summary(lm.goalie.CTD)$r.squared
        
        ####Last Season
        goalie.last.season <- subset(goalie.game,SA.Goalie==goalie & as.numeric(Season) == (as.numeric(season) - 10001))
        
        ###only post if last season had over 10 games
        if(nrow(goalie.last.season) > 10) {
          
          lm.goalie.last <- lm(goalie.last.season$QREAM ~ goalie.last.season$cum.Shots)
          out$Last.Season.Lift <- as.numeric(lm.goalie.last$coefficients[2])
          out$Last.Season.Consistency <- summary(lm.goalie.last)$r.squared
          out$Last.Season.Shots <- max(goalie.last.season$cum.Shots) - min(goalie.last.season$cum.Shots - goalie.last.season$game.SA)
          out$Last.Season.Goals <- max(goalie.last.season$cum.Goals) - min(goalie.last.season$cum.Goals - goalie.last.season$game.GA)
          
          ###birthday
          dob <-as.Date(unique(roster[which(roster$Full.Name == goalie), "bd"]),format = "%B %d, %Y")
          
          out$SA.Goalie <- goalie
          out$Season <- season
          out$Age.Season.Start <- ifelse(season == "20152016", (as.Date("2015-10-05") - dob) / 365.25,
                                         ifelse(season == "20142015", (as.Date("2014-10-05") - dob) / 365.25,
                                                ifelse(season == "20132014", (as.Date("2013-10-05") - dob) / 365.25,
                                                       ifelse(season == "20122013", (as.Date("2012-10-05") - dob) / 365.25,
                                                              ifelse(season == "20112012", (as.Date("2011-10-05") - dob) / 365.25,
                                                                     ifelse(season == "20102011", (as.Date("2010-10-05") - dob) / 365.25,
                                                                            ifelse(season == "20092010", (as.Date("2009-10-05") - dob) / 365.25,
                                                                                   ifelse(season == "20082009", (as.Date("2008-10-05") - dob) / 365.25,
                                                                                          ifelse(season == "20072008", (as.Date("2007-10-05") - dob) / 365.25,
                                                                                                 (as.Date("2006-10-05") - dob) / 365.25)))))))))
          
          complete.out <- data.frame(out)
          
          output <- plyr::rbind.fill(output,complete.out)
          
        } ##last season entered
      } ##career entered
    } ##current season entered
  } ##season stacked
  print(output)
}

scored.goalie.seasons <- do.call(rbind,lapply(FUN=goalie.season.scores,
                                              unique(goalie.game$SA.Goalie)))
scored.goalie.seasons$lastname = sapply(strsplit(as.character(scored.goalie.seasons$SA.Goalie), '[.]'), function(x) x[length(x)])
scored.goalie.seasons$labs <- paste0(scored.goalie.seasons$lastname, substr(scored.goalie.seasons$Season,7,8))

scored.goalie.seasons2 <- subset(scored.goalie.seasons, Season.ShotsAgainst > 1700)

###plot consistency
ggplot(data=scored.goalie.seasons2, aes(x=Season.Consistency,y=Season.Lift * 100,color=Season.ShotsAgainst)) + 
  geom_point() +
  labs(title="Goaltender Season xG Lift vs Consistency\nMinimum 1700 Shots") +
  labs(y="Goals Prevented / 100 Shots (xG - Actual Goals)", x="Consistency (Variation around Line of Best Season Fit, 0 - 1)") +
  labs(color="Season Shots Against") +
  scale_colour_gradient2(low="grey",mid="grey", high="blue",limits=c(1500, 2700),
                         midpoint = median(scored.goalie.seasons2$Season.ShotsAgainst)) +
  geom_text_repel(aes(label = labs)) +
  #geom_text(data=scored.goalie.seasons, aes(label = labs, angle=(Season.Consistency * 350 * Season.Lift),check_overlap = TRUE)) +
  annotate("segment",x=0,y=0,xend=1,yend=0) +
  annotate("segment",x=0.5,y=-2,xend=0.5,yend=2) +
  annotate("text", x = 0.1, y = 2.5, label = "@CrowdScoutSprts\nData courtesy\ncorsica.hockey") +
  annotate("text", x = 0.1, y = -1.5, label = "Inconsistent & Bad") +
  annotate("text", x = 0.1, y = 1.5, label = "Inconsistent & Good") +
  #annotate("text", x = 0.9, y = 1.5, label = "Consistent & Good") +
  annotate("text", x = 0.9, y = -1.5, label = "Consistent & Bad") +
  theme(panel.background = element_blank())

###################
####check basic repeatability
##################

scored.goalie.seasons$savePct.t <- (scored.goalie.seasons$Season.ShotsAgainst - scored.goalie.seasons$Season.Goals) / scored.goalie.seasons$Season.ShotsAgainst
scored.goalie.seasons$savePct.t1 <- (scored.goalie.seasons$Last.Season.Shots - scored.goalie.seasons$Last.Season.Goal) / scored.goalie.seasons$Last.Season.Shots

YoY <- lm(scored.goalie.seasons$Season.Lift ~ scored.goalie.seasons$Last.Season.Lift, 
          weights = (scored.goalie.seasons$Season.ShotsAgainst + scored.goalie.seasons$Last.Season.Shots))
as.numeric(YoY$coefficients[2])
summary(YoY)$r.squared

YoY <- lm(scored.goalie.seasons$savePct.t ~ scored.goalie.seasons$savePct.t1,
          weights = (scored.goalie.seasons$Season.ShotsAgainst + scored.goalie.seasons$Last.Season.Shots))
as.numeric(YoY$coefficients[2])
summary(YoY)$r.squared

ggplot(data=scored.goalie.seasons, aes(x=Last.Season.Lift,y=Season.Lift)) + 
  geom_point() +
  labs(title="Expected - Actual Goals, Goalies 2008-2016") +
  labs(x="Cumulative Shots Against", y="Expected Goals Against - Actual Goals") +
  geom_text(data=scored.goalie.seasons, aes(label = labs, angle=0,check_overlap = FALSE)) +
  #  annotate("text", x = 1, y = (max(goalie.QREAM$QREAM) * 0.8), hjust=0, label = "@CrowdScoutSprts\nData courtesy\ncorsica.hockey") +
  theme(panel.background = element_blank())

library(ggrepel)
####Year by year 
ggplot(scored.goalie.seasons2, aes(factor(Season), Season.Lift * 100, color=Season.ShotsAgainst)) +
  geom_boxplot() +
  geom_point() +
  #  geom_line(subset(scored.goalie.seasons2, SA.Goalie=="Bria")) +
  geom_text_repel(aes(label = labs)) +
  labs(title="Goaltender Season xG Lift Distribution by Season\nMinimum 1700 Shots") +
  labs(y="Goals Prevented / 100 Shots (xG - Actual Goals)", x="Season") +
  labs(color="Season Shots Against") +
  annotate("segment",x=0,y=0,xend=6,yend=0) +
  annotate("text", x =1, y = 2.5, label = "@CrowdScoutSprts\nData courtesy\ncorsica.hockey") +
  theme(panel.background = element_blank())

#########
##predict lift
#########
library(randomForest)
####Random Forest Model
rf.cross.v10 <- function(input, formula) {
  
  library(caret)
  # load the dataset
  input
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model <- train( formula ,
                  data=input, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance=TRUE)
  
  predictions <- predict(model, newdata=input)
  
  # var importance
  var.imp <- varImp(model, scale=FALSE)
  
  return(list(cbind(predictions,input),model,var.imp))
  
}

lift.rf <- rf.cross.v10(scored.goalie.seasons, Season.Lift ~ Last.Season.Lift + Last.Season.Consistency + Last.Season.Shots + CTD.Consistency + CTD.Lift)

####summary
lift.rf.scoring <- lift.rf[[2]]
lift.rf.scoring$results

####scored data
lift.rf.scored <- lift.rf[[1]]

plot(lift.rf.scored$predictions ~ lift.rf.scored$Season.Lift)
fit <- lm(lift.rf.scored$predictions ~ lift.rf.scored$Season.Lift)
summary(fit)

################
##ANOVA, VARIABLE IMPORTANCE
################

fm = goal.model[[2]]$finalModel
anova(fm, test="Chisq")

# var importance
var.imp <- varImp(goal.model)

##############################
##create roc curve
##############################

######cut off rates
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

###sample
cutoff.roc.scores(.1,shots.scored$Prob.Goal,shots.scored$goal)


###create accuracy matrix
s = seq(.01,.99,length=1000)
accuracy.mat = matrix(0,1000,5)
for(i in 1:1000) {
  accuracy.mat[i,]=cutoff.roc.scores(s[i],shots.scored$Prob.Goal,shots.scored$goal)
}
accuracy.df <- as.data.frame(accuracy.mat)
colnames(accuracy.df) = c("cutoff","sensitivity", "specificity", "class.rate", "distance")
accuracy.df$false.pos.rate <- 1 - accuracy.df$specificity

##############
###ROC Curve
##############

ggplot(data=accuracy.df, aes(x=false.pos.rate,y=sensitivity)) +
  geom_line() 


height = (accuracy.df$sensitivity[-1]+accuracy.df$sensitivity[-length(accuracy.df$sensitivity)])/2
width = -diff(accuracy.df$false.pos.rate) # = diff(rev(omspec))
sum(height*width)
##0.7219591


ggplot(data=accuracy.df, aes(x=cutoff)) +
  geom_line(aes(y=distance,color="blue")) +
  geom_line(aes(y=specificity)) +
  geom_line(aes(y=sensitivity)) +
  geom_line(aes(y=class.rate)) 

