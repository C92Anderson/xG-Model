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
# PROCESS:        0 - SYSTEM PREP AND LOAD DATA FROM xG_Model_nhlscrapr.R
#                 1 - CREATE GOALIE SEASON, CAREER DATASET
#                 2 - ATTEMPT TO PREDICT GOALIE PERFORMANCE
#
############################################################################################################################################################################

############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret)
getwd()
load("~/Documents/CWA/Hockey Data/roster.Rda")

############################################################################################################################################################################
######## 1.A SUMMARY BEST GOALIE GAME, SEASON, AND CAREER
############################################################################################################################################################################
# Load scored data
load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

# Prepare all shots against goaltenders in shot sample
goalie.shots <- scored.data %>%
  mutate(Game.ID = as.character(gcode),
         SA = 1,
         GA = as.numeric(goal)-1) %>%
  select(SA.Goalie, season, Game.ID, seconds, goal, xG, SA, GA) 

# Calculate season-level xG Saved per 100 shots
goalie.game <- goalie.shots %>%
  group_by(SA.Goalie, season, Game.ID) %>%
  summarise(game.xG = sum(xG),
            game.Goals = sum(GA),
            game.Shots = sum(SA)) %>%
  mutate(game.xGS.100 = (game.xG - game.Goals) / (game.Shots / 100)) %>%
  group_by(SA.Goalie, season) %>%
  summarise(game.variance = sd(game.xGS.100), games=n())

# Calculate career xG Saved per 100 shots
goalie.career <- goalie.shots %>%
  group_by(SA.Goalie) %>%
  summarise(career.xG = sum(xG),
            career.Goals = sum(GA),
            career.Shots = sum(SA)) %>%
  mutate(career.xGS.100 = (career.xG - career.Goals) / (career.Shots / 100))

# Calculate season-level xG Saved per 100 shots
goalie.season <- goalie.shots %>%
  group_by(SA.Goalie, season) %>%
  summarise(season.xG = sum(xG),
            season.Goals = sum(GA),
            season.Shots = sum(SA)) %>%
  mutate(season.xGS.100 = (season.xG - season.Goals) / (season.Shots / 100)) %>%
  left_join(goalie.career, by="SA.Goalie") %>%
  left_join(goalie.game, by=c("SA.Goalie","season")) %>%
  filter(career.Shots > 1000 & season.Shots > 100) 



 
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

