Run programs in order:
1. xG_Model_nhlscrapr.R - Takes raw data from nhlscrapr and create goaltender-centric xG model
2. xG_Forecasting.R

xG_Model_nhlscrapr.R

############################################################################################################################################################################

# PROJECT:        xG Model Measuring Goaltender Performance

 PURPOSE:        Using Logistic Regression create an xG for each shot a goalie faces. 
                 Compare xG Against to Actual Goals Against to measure performance

 CREATED BY:     Cole Anderson (cole92anderson@gmail.com)

 LAST UPDATED:   05/07/2017

 PROCESS:   
 
 0 - SYSTEM PREP
 1 - UPDATE GCODE AND LOAD NHL PBP DATA USING NHLSCRAPR
 2 - LOGISTIC MODEL TO DEVELOP XG MODEL
 3 - DEVELOP FUNCTIONS SELECT GOALIE-SEASON AND PLOT GOALIE XG SAVE SUCCESS

############################################################################################################################################################################


xG_Forecasting.R

############################################################################################################################################################################

# PROJECT:        Use xG Model to Predict Goaltender Performance

 PURPOSE:        Develop features to possibly predict future goaltender performance. 

 CREATED BY:     Cole Anderson (cole92anderson@gmail.com)

 LAST UPDATED:   05/07/2017

 PROCESS: 
 
 0 - SYSTEM PREP AND LOAD DATA FROM xG_Model_nhlscrapr.R
 1 - CREATE GOALIE SEASON, CAREER DATASET
 2 - ATTEMPT TO PREDICT GOALIE PERFORMANCE

############################################################################################################################################################################

BayesGoalies.R

############################################################################################################################################################################

# PROJECT:        Use xG Model & Rebound Model for Bayesian Goaltender Analysis

PURPOSE:        Create Expected Goal Prior & Compare to Posterior Performance. 

CREATED BY:     Cole Anderson (cole92anderson@gmail.com)

LAST UPDATED:   05/07/2017



############################################################################################################################################################################
