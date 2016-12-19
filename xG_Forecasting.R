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
######## 1.A SUMMARY BY GOALIE GAME, SEASON, AND CAREER
############################################################################################################################################################################
# Load scored data
load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

# Prepare all shots against goaltenders in shot sample
goalie.shots <- scored.data %>%
  mutate(Game.ID = as.character(gcode),
         SA = 1,
         GA = as.numeric(goal)-1) %>%
  select(SA.Goalie, season, Game.ID, seconds, goal, xG, SA, GA, even.second) 

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
goalie.career.TD <- goalie.shots %>%
  group_by(SA.Goalie, season) %>%
  summarise(career.xG = sum(xG),
            career.Goals = sum(GA),
            career.Shots = sum(SA)) %>%
  group_by(SA.Goalie) %>%
  mutate(career.xG = cumsum(career.xG) - career.xG,
         career.Goals = cumsum(career.Goals) - career.Goals,
         career.Shots = cumsum(career.Shots) - career.Shots) %>%
  mutate(career.xGS.100 = (career.xG - career.Goals) / (career.Shots / 100))

# Calculate season-level xG Saved per 100 shots
goalie.season <- goalie.shots %>%
  group_by(SA.Goalie, season) %>%
  summarise(season.xG = sum(xG),
            season.Goals = sum(GA),
            season.Shots = sum(SA)) %>%
  mutate(season.xGS.100 = (season.xG - season.Goals) / (season.Shots / 100),
         xG.shot = season.xG / season.Shots) %>%
  left_join(goalie.career.TD, by=c("SA.Goalie","season")) %>%
  left_join(goalie.game, by=c("SA.Goalie","season")) %>%
  filter(season.Shots > 100) 

############################################################################################################################################################################
######## 1.B PLOT GOALIE SEASON
############################################################################################################################################################################

goalie.season.plot <- function(goalies){
  
  library(dplyr)
  goalie.plot <- goalie.season %>%
      filter(SA.Goalie %in% c(goalies)) %>%
      ggplot(aes(x=season,y=season.xGS.100,fill=season.Shots, group=as.factor(SA.Goalie))) +
      scale_fill_gradient2(low="light grey",high="blue") +
      geom_bar(stat="identity") +
      labs(title=paste0(goalies, "\nExpected Goals Against - Actual Goals Against per 100 Shots By Season\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model")) +
      labs(x="Season",y="Expected Goals Against - Actual Goals Against per 100 Shots",fill="Shots Against") +
      theme(panel.background = element_blank()) 
      
  return(goalie.plot)   

}

goalie.season.plot(c("BRADEN HOLTBY"))
goalie.season.plot(c("COREY CRAWFORD"))
goalie.season.plot(c("CORY SCHNEIDER"))
goalie.season.plot(c("MARTIN BRODEUR"))
goalie.season.plot(c("JONAS GUSTAVSSON"))
goalie.season.plot(c("MIIKKA KIPRUSOFF"))


############################################################################################################################################################################
######## 1.C ADD ROSTER AND AGE INFORMATION TO DATASET
############################################################################################################################################################################

# Prep and merge roster
load("~/Documents/CWA/Hockey Data/core.Rda")

load(url("http://war-on-ice.com/data/nhlscrapr-core.RData"))

goalie.season.roster <- roster.master %>%
      filter(nchar(DOB) > 0) %>%
      mutate(SA.Goalie = toupper(firstlast)) %>%
      select(SA.Goalie, Height, DOB, last) %>%
      distinct(SA.Goalie, Height, DOB, last) %>%
      right_join(goalie.season, by="SA.Goalie") %>%
      mutate(Age.Season.Start = as.numeric(round((as.Date(paste0(substr(season, 1, 4),"-10-05")) - as.Date(DOB)) / 365.25,2)))

# Plot by Season
library(ggrepel)
goalie.season.roster %>%
  filter(season.Shots > 1500 | (season %in% c("20122013","20162017") & season.Shots > 400)) %>%
  ggplot(aes(factor(season), season.xGS.100, color=season.Shots)) +
  geom_boxplot() +
  geom_point() +
  geom_text_repel(aes(label = last)) +
  labs(title="Goaltender Season xG Lift Distribution by Season\nMinimum 1500 Shots\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model") +
  labs(y="Goals Prevented / 100 Shots (xG - Actual Goals)", x="Season") +
  labs(color="Season Shots Against") +
  annotate("segment",x=0,y=0,xend=10,yend=0) +
  theme(panel.background = element_blank())


############################################################################################################################################################################
######## 1.D CHECK INTRA-SEASON REPEATABILITY
############################################################################################################################################################################
library(reshape)
set.seed(123434)

goalie.season.splits <- goalie.shots %>%
 group_by(SA.Goalie, season, even.second) %>%
  summarise(total.xG = sum(xG),
            total.Goals = sum(GA),
            total.Shots = sum(SA)) %>%
   mutate(game.xGS.100 = (total.xG - total.Goals) / (total.Shots / 100),
         save.pecentage = (total.Shots - total.Goals) / total.Shots) %>%
  group_by(SA.Goalie, season) %>%
  summarise(even.xGS.100 = max(ifelse(even.second == 1, game.xGS.100,-100)),
            odd.xGS.100 = max(ifelse(even.second == 0, game.xGS.100,-100)),
            even.save.pecentage = max(ifelse(even.second == 1, save.pecentage,-100)),
            odd.save.pecentage = max(ifelse(even.second == 0, save.pecentage,-100)),
            total.Shots = sum(total.Shots)) %>%
  filter(even.save.pecentage > -100 & odd.save.pecentage > -100 & total.Shots > 1500)


# SOX (Saves Over xG) 
library(boot)
SOX.corr <- corr(as.matrix(goalie.season.splits[c("even.xGS.100", "odd.xGS.100")]), w = (goalie.season.splits$total.Shots))
svP.corr <- corr(as.matrix(goalie.season.splits[c("even.save.pecentage", "odd.save.pecentage")]), w = (goalie.season.splits$total.Shots))

# Plot Sox
goalie.season.splits %>%
        ggplot() +
        geom_point(aes(x=even.xGS.100,y=odd.xGS.100,size=total.Shots, color=total.Shots)) +
        scale_color_gradient2(low="light grey",high="blue", guide = FALSE) +
        #geom_smooth(aes(x=even.xGS.100, y=odd.xGS.100), size = 1.5, colour = "black", se = TRUE, stat = "smooth", method = "lm")
        annotate("segment", x = -5, y = -5, xend = 5, yend = 5) +
        annotate("text", x = -4, y = 4, hjust = 0, label = paste0("Intra-season correlation: ", round(SOX.corr,2))) +
        labs(title="Intra-Season Correlation - Expected Goals Against - Actual Goals Against per 100 Shots\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model") +
        labs(x="Even Second Saves Over Expected (SOX)",y="Odd Second Saves Over Expected (SOX)",size="Shots Against") +
        theme(panel.background = element_blank()) 

# Plot Save Percentage
goalie.season.splits %>%
  ggplot() +
  geom_point(aes(x=even.save.pecentage,y=odd.save.pecentage,size=total.Shots, color=total.Shots)) +
  scale_color_gradient2(low="light grey",high="blue", guide = FALSE) +
  #geom_smooth(aes(x=even.xGS.100, y=odd.xGS.100), size = 1.5, colour = "black", se = TRUE, stat = "smooth", method = "lm")
  annotate("segment", x = 0.88, y = 0.88, xend = 0.95, yend = 0.95) +
  annotate("text", x = 0.885, y = 0.94, hjust = 0, label = paste0("Intra-season correlation: ", round(svP.corr,3))) +
  labs(title="Intra-Season Correlation - Save Percentage\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model") +
  labs(x="Even Second Save Percentage",y="Odd Second Save Percentage",size="Shots Against") +
  theme(panel.background = element_blank()) 

############################################################################################################################################################################
######## 2.A PREDICT NEW SEASON XGS / 100 
############################################################################################################################################################################

library(randomForest); library(DataCombine)

predict.goalie.season <- goalie.season.roster %>%
        #group_by(SA.Goalie, season) %>%
        mutate(future.season.xGS.100 = lead(season.xGS.100, n=1),
               career.xGS.100 = ifelse(is.na(career.xGS.100),0,career.xGS.100)) %>%
        filter(!is.na(future.season.xGS.100))

############################################################################################################################################################################
######## 2.B RANDOM FOREST MODEL
############################################################################################################################################################################
library(caret); library(broom); library(modelr); library(dplyr); library(purrr); library(tidyr);library(randomForest)
set.seed(1234)

train_control <- trainControl(method="cv", number=10)

# train the model
rf.model <- train(future.season.xGS.100 ~ season.xGS.100 + career.xGS.100 + career.Shots + 
                  season.Shots + game.variance + Age.Season.Start ,
                data=predict.goalie.season, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance=TRUE)

predictions <- predict(rf.model, newdata=predict.goalie.season)

# var importance
var.imp <- varImp(rf.model, scale=FALSE)

# Model Summary
rf.model$results
#mtry     RMSE   Rsquared    RMSESD RsquaredSD
#1    2 1.342684 0.09065695 0.1701442 0.07009296
#2    3 1.353351 0.08104594 0.1712042 0.06200446
#3    4 1.355680 0.08100703 0.1702766 0.06223268
#4    5 1.355776 0.08281910 0.1745964 0.06416547
#5    6 1.358122 0.07912810 0.1746796 0.05945308

# Scored data
lift.rf.scored <- cbind(predictions,as.data.frame(predict.goalie.season))

plot(lift.rf.scored$predictions ~ lift.rf.scored$future.season.xGS.100)
fit <- lm(lift.rf.scored$predictions ~ lift.rf.scored$future.season.xGS.100)
summary(fit)
#Multiple R-squared:  0.9285,	Adjusted R-squared:  0.9283 

# var importance
var.imp <- varImp(goal.model)

############################################################################################################################################################################
######## 2.C LINEAR MODEL
############################################################################################################################################################################

lm.season.cv.10 <- function(input, model.vars, extra.vars) {
  
  library(modelr); library(dplyr); library(purrr); library(broom); library(tidyr); library(ggplot2)
  
  input.cc <- input[names(input) %in% c(model.vars,extra.vars)]
  input.cc <- input.cc[complete.cases(input.cc),]
  model.data <- input.cc[names(input.cc) %in% c(model.vars)]
  
  # Set folds
  set.seed(1234)  
  folds <- crossv_kfold(model.data, k = 10)
  
  # Run model over folds
  model.folds <- folds %>% 
    mutate(model = map(train, ~ glm(future.season.xGS.100 ~ ., data = .)))
  
  # Predict test data
  predicted <- model.folds %>% 
    mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>% 
    unnest(predicted)
  
  #Calculate residual
  predicted <- predicted %>% 
    mutate(residual = .fitted - future.season.xGS.100)
  
  # Calculate r-squared
  rs <- predicted %>%
    group_by(.id) %>% 
    summarise(
      sst = sum((future.season.xGS.100 - mean(future.season.xGS.100)) ^ 2), # Sum of Squares Total
      sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
      r.squared = 1 - sse / sst         # Proportion of variance accounted for
    )
  
 
  return(list(model.folds, rs, cbind(predicted[1],input.cc)))
}      

predicted.season.model <- lm.season.cv.10(predict.goalie.season,
                                 c("future.season.xGS.100","season.xGS.100","career.xGS.100","career.Shots", 
                                     "seasonx.Shots","Age.Season.Start"),
                                 c("SA.Goalie","season"))   

# Summary best plot
best.model.no <- predicted.season.model[[2]] %>% filter(r.squared == max(r.squared)) %>% select(.id) %>% as.character()
best.model <- predicted.season.model[[1]]$model[[paste0(as.numeric(best.model.no))]] 
best.model %>% summary()

# R-squared
1 - (best.model$deviance / best.model$null.deviance)
# 0.267


lm.scored.data <- predicted.season.model[[3]] %>% 
  group_by(season) %>% 
  summarise(xG=sum(xG), goals=sum(as.numeric(goal)-1), 
            avg.shot=mean(distance), avg.angle=mean(shot.angle), rebound=mean(as.numeric(is.Rebound)-1))

