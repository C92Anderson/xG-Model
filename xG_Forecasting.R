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
         xG.shot = season.xG / season.Shots,
         G.shot = season.Goals / season.Shots) %>%
  left_join(goalie.career.TD, by=c("SA.Goalie","season")) %>%
  left_join(goalie.game, by=c("SA.Goalie","season")) %>%
  filter(season.Shots > 100) 

# Plot shot difficulty
goalie.season %>%
  filter(season == "20162017" & (season.Shots > 750)) %>%
  arrange(desc(xG.shot)) %>%
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
qream.split <- goalie.season.splits %>%
        ggplot() +
        geom_point(aes(x=even.xGS.100,y=odd.xGS.100,size=total.Shots, color=total.Shots)) +
        scale_color_gradient2(low="light grey",high="blue", guide = FALSE) +
        #geom_smooth(aes(x=even.xGS.100, y=odd.xGS.100), size = 1.5, colour = "black", se = TRUE, stat = "smooth", method = "lm")
        annotate("segment", x = -5, y = -5, xend = 5, yend = 5) +
        annotate("text", x = -4, y = 4, hjust = 0, label = paste0("Intra-season correlation: ", round(SOX.corr,2))) +
        labs(title="Intra-Season QREAM Correlation - Expected Goals Against - Actual Goals Against per 100 Shots\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model") +
        labs(x="Even Second QREAM / 100 Shots",y="Odd Second QREAM / 100 Shots",size="Shots Against") +
        theme(panel.background = element_blank()) 

# Plot Save Percentage
svp.split <- goalie.season.splits %>%
  ggplot() +
  geom_point(aes(x=even.save.pecentage,y=odd.save.pecentage,size=total.Shots, color=total.Shots)) +
  scale_color_gradient2(low="light grey",high="blue", guide = FALSE) +
  #geom_smooth(aes(x=even.xGS.100, y=odd.xGS.100), size = 1.5, colour = "black", se = TRUE, stat = "smooth", method = "lm")
  annotate("segment", x = 0.88, y = 0.88, xend = 0.95, yend = 0.95) +
  annotate("text", x = 0.885, y = 0.94, hjust = 0, label = paste0("Intra-season correlation: ", round(svP.corr,3))) +
  labs(title="Intra-Season Correlation - Save Percentage") +
  labs(x="Even Second Save Percentage",y="Odd Second Save Percentage",size="Shots Against") +
  theme(panel.background = element_blank()) 

multiplot(qream.split, svp.split)

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
var.imp <- varImp(rf.model)

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

############################################################################################################################################################################
######## 2.D X GAME LOOP
############# Maximize predictiveness in x game chunks 
############################################################################################################################################################################

# Load scored data
load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")
load(url("http://war-on-ice.com/data/nhlscrapr-core.RData"))

# Prepare all shots against goaltenders in shot sample
all.lookback.shots <- roster.master %>%
          filter(nchar(DOB) > 0) %>%
          mutate(SA.Goalie = toupper(firstlast)) %>%
          select(SA.Goalie, Height, DOB, last) %>%
          distinct(SA.Goalie, Height, DOB, last) %>%
          right_join(scored.data, by="SA.Goalie") %>%
          mutate(Game.ID = as.character(gcode),
                 DOB=as.Date(DOB),
                 season.date = paste0(season, refdate),
                 SA = 1,
                 GA = as.numeric(goal)-1) %>%
          select(SA.Goalie, season, Game.ID, seconds, goal, xG, SA, GA, even.second, season.date, Height, DOB, last) %>%
          arrange(desc(season), desc(Game.ID), desc(seconds)) %>%
          group_by(SA.Goalie) %>%
          mutate(cum.lookback.shots = cumsum(SA),
                 career.shots = max(cum.lookback.shots),
                 Height = (as.integer(substr(Height,1,1)) * 12) + as.integer(substr(Height,3,4)))

# Shot chunks to iterate over
shot.lookback <- c(seq(250, 5000, by=250))

# Prepare dataframe
chunk.correlations.df <- data.frame(Shot.Chunk.Iteration=as.integer(), 
                                    Correlation=as.numeric(),
                                    Sample=as.numeric())

chunk.glm.df <- data.frame(Shot.Chunk.Iteration=as.integer(), 
                           Fold=as.integer(),
                           SST=as.numeric(),
                           SSE=as.numeric(),
                           R.Squared=as.numeric(),
                           Sample = as.numeric())

chunk.rf.df <- data.frame(Shot.Chunk.Iteration=as.integer(), 
                          mtry=as.integer(),
                          RMSE=as.numeric(),
                          Rsquared=as.numeric(),
                          RMSESD=as.numeric(),
                          RsquaredSD=as.numeric())

chunk.predictions <- list()

for(i in shot.lookback) {
 
  goalie.chunks <- all.lookback.shots %>%
          mutate(goalie.chunks = floor(career.shots / i),
                 shot.chunk = floor(cum.lookback.shots / i) + 1) %>%
          filter(goalie.chunks >= 2 & shot.chunk <= goalie.chunks) %>%
          group_by(SA.Goalie,Height,shot.chunk) %>%
          summarise(career.shots = min(career.shots),
                    chunk.xG = sum(xG),
                    chunk.Goals = sum(GA),
                    chunk.Shots = sum(SA),
                    #chunk.Shots.Game = sum(SA) / count(unique(Game.ID)),
                    chunk.last.date = substr(max(season.date),9,12),
                    chunk.last.date = as.Date(as.integer(chunk.last.date), origin="2002-01-01"),
                    chunk.age.end = (as.integer(chunk.last.date) - as.integer(min(DOB))) / 365.25) %>%
          mutate(chunk.xGS.100 = (chunk.xG - chunk.Goals) / (chunk.Shots / 100)) %>%
          group_by(SA.Goalie) %>%
          mutate(future.chunk.xGS.100 = lag(chunk.xGS.100)) %>%
          filter(!is.na(future.chunk.xGS.100)) 
  
  # Check simple correlation
  chunk.corr <- cor(goalie.chunks$future.chunk.xGS.100, goalie.chunks$chunk.xGS.100)
  chunk.corr <- data.frame(Shot.Chunk.Iteration = i, Correlation = chunk.corr, Sample=nrow(goalie.chunks))
  chunk.correlations.df <- rbind(chunk.correlations.df, chunk.corr)
  
  # Random Forest 
  train_control <- trainControl(method="cv", number=3)
  
  # train the model
  rf.model <- train(future.chunk.xGS.100 ~ chunk.xGS.100 + chunk.age.end,
                    data=goalie.chunks, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance=TRUE)
  
  #rf.pred <- predict(rf.model, newdata=goalie.chunks)
  rf.results <- cbind(i, rf.model$results)

  chunk.rf.df <- rbind(chunk.rf.df, rf.results)

  # Cross-validation loop
  # Set folds
  set.seed(1234)  
  folds <- crossv_kfold(goalie.chunks, k = 5)
  
  # Run model over folds
  model.folds <- folds %>% 
    mutate(model = map(train, ~ glm(future.chunk.xGS.100 ~ chunk.xGS.100 + chunk.age.end, data = .)))
  
  # Predict test data
  predicted <- model.folds %>% 
    mutate(predicted = map2(model, test, ~ augment(.x, newdata = .y))) %>% 
    unnest(predicted)
  
  #Calculate residual
  predicted <- predicted %>% 
    mutate(residual = .fitted - future.chunk.xGS.100,
           ci.high = .fitted + (1.96 * .se.fit),
           ci.low = .fitted - (1.96 * .se.fit))
  
  chunk.predictions[[i]] <- predicted
  
  # Calculate r-squared
  rs <- predicted %>%
#    group_by(.id) %>% 
    summarise(
      sst = sum((future.chunk.xGS.100 - mean(future.chunk.xGS.100)) ^ 2), # Sum of Squares Total
      sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
      r.squared = 1 - sse / sst,         # Proportion of variance accounted for
      sample = nrow(predicted)
    )


  best.model.no <- rs %>% filter(r.squared == max(r.squared)) %>% select(.id) %>% as.character()
  best.model <- model.folds$model[[paste0(as.numeric(best.model.no))]] 

  best.model.pred <- predict(best.model, all.lookback.shots)
#  best.model.data <- cbind(best.model.pred, as.data.frame(goalie.chunks))
#  best.model.rs <- best.model.data %>%
#        mutate(residual = best.model.pred - future.chunk.xGS.100) %>%
#        summarise(sst = sum((future.chunk.xGS.100 - mean(future.chunk.xGS.100)) ^ 2), # Sum of Squares Total
#                  sse = sum(residual ^ 2),          # Sum of Squares Residual/Error
#                  r.squared = 1 - sse / sst,
#                  sample = max(rs$sample))
  
  chunk.glm <- cbind(i, rs)
  chunk.glm.df <- rbind(chunk.glm.df, chunk.glm)
  
}

# Slice Correlations
chunk.correlations.df %>%
    ggplot() +
    geom_point(aes(x=Shot.Chunk.Iteration, y=Correlation, size=Sample)) +
    ylim(0,0.75) +
  labs(size="Number of Slice-Pairs") +
  labs(title="Shot Slices Pair Correlations\nSlice,i to Slice,i-1") +
  labs(x="Shot Slice Size", y="Correlation") +
  annotate("text", x = 100, y = 0.6, hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
  theme(panel.background = element_blank())
  
# Plot Best Model R-Squared
chunk.glm.df %>%
  group_by(i) %>%
  summarise(mean.r.squared = max(r.squared),
            sample.size = max(sample)) %>%
  ggplot() +
  geom_point(aes(x=i, y=mean.r.squared, size=sample.size)) +
  ylim(0,0.75) +
  labs(size="Number of Slice-Pairs") +
  labs(title="Shot Slices Pairs Linear Model - Save Percentage Best Model Only\nSlice,i Predicted by Age, Slice,i-1") +
  labs(x="Shot Slice Size", y="Best Model R-Squared") +
  geom_text_repel(aes(x=i,y=mean.r.squared,label = round(mean.r.squared,2))) +
  annotate("text", x = 100, y = 0.6, hjust=0, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
  theme(panel.background = element_blank())


