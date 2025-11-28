# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load libraries
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Hybrid Choice Model MMNL & OL Experience EV Users",
  modelDescr ="HCM-MMNL-OL SPR 4706 Choice Experiment",
  indivID    ="ResponseId",
  nCores     = 20
)

# ####################################################### #
#### 2. Data loading                                   ####
# ####################################################### #

#the data should be named as database
# setwd("C:\\Users\\kenny\\OneDrive - purdue.edu\\Documents\\Kenny's File\\Transportation Literature\\Fall 2023\\SPR 4706 - EV Public Perception Expectation and WTP\\SPR 4706")
# database <- read.csv("res_dyneventh_apollo.csv", header=TRUE)

setwd("C:\\Users\\wijayak\\OneDrive - purdue.edu\\Documents\\Kenny's File\\Transportation Literature\\Fall 2023\\SPR 4706 - EV Public Perception Expectation and WTP\\SPR 4706")
database <- read.csv("res_dyneventh_apollo.csv", header=TRUE)

# Run model for Experienced EV users
database <- subset(database, database$EVExp == 'Have EV Experience')
# Columns to drop
columns_to_drop <- c('EV04', 'EV05', 'EV06',
                     'EvReli01', 'EvReli02','EvReli03', 'EvReli04', 
                     'EvWill01', 'EvWill02', 'EvWill03', 'EvWill04', 'EvWill05', 'EvWill06',
                     'PubReli01', 'PubReli02')
# Use select to drop specified columns
database <- database[, !(names(database) %in% columns_to_drop)]
# drop rows with value of -1 (don't know/not sure answer)
database <- subset(database, !(EV01 == -1 | EV02 == -1 | EV03 == -1 | EV07 == -1 | EV08 == -1 | 
                                 EV09 == -1 | EV010 == -1 | EV011 == -1 | EV012 == -1 | EV013 == -1))
# Drop NaN value
database <- na.omit(database)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

### Note this is unlabelled data, so we are doing this for an illustration of cheap vs expensive
### Define settings for analysis of choice data to be conducted prior to model estimation


# ####################################################### #
#### 3. Parameter definition                           ####
# ####################################################### #

### Vector of parameters, including any that are kept fixed during estimation
apollo_beta=c(b0_Level_2  = 0,
              b0_DCFC  = 0,
              b0_DWPT  = 0,
              
              # waiting time var
              b_wtime = 0,
              
              # charging time var
              b_ctime_dcfc = 0,
              
              # total travel time var
              b_tottime_dwpt = 0,
              
              b_soc_high_level_2 = 0,
              
              #heterogeneity variables
              #cratio
              mu_log_b_cratio_level_2 = 0,
              sigma_log_b_cratio_level_2 = 0,
              # cost
              mu_log_b_cost = 0,
              sigma_log_b_cost = 0,
              
              # Latent Variables
              # Environment Latent Variable
              gamma_colgrad_env = 0,
              gamma_u25k_env = 0,
              gamma_100149k_env = 0,
              
              # Positive Attitudes Towards EV Features
              gamma_u25k_evfeatconv = 0,
              
              # Latent variable coefficient for choice indicator
              lambda_env_dcfc = 1,
              lambda_evfeatconv_dwpt = 1,
              
              # Ordered Logit
              # Tau
              # Positive attitude toward Environment
              tau_env_1 = -2,  tau_env_2 = -1,  tau_env_3 = 1,  tau_env_4 = 2,
              
              # Positive Attitudes Towards EV Features
              tau_evfeatconv_1 = -2,  tau_evfeatconv_2 = -1,  tau_evfeatconv_3 = 1,  tau_evfeatconv_4 = 2,
              
              zeta_ev01 = 1,  zeta_ev07 = 1,  zeta_ev08 = 1,  zeta_ev011 = 1,
              
              zeta_env01 = 1,  zeta_env02 = 1,  zeta_env03 = 1,  zeta_env04 = 1
              
              # zeta_ev09 = 1,  zeta_ev010 = 1,  zeta_ev012 = 1
              
)

### Vector with names (in quotes) of parameters to be
### kept fixed at their starting value in apollo_beta.
### Use apollo_beta_fixed = c() for no fixed parameters.

apollo_fixed = c("b0_Level_2")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 2000,
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_cratio_level_2"
    , "draws_cost"#, "draws_wtime"
    , "draws_retired_dcfc"
    , "eta_env"
    , "eta_evfeatconv"
  ),
  intraDrawsType = "halton",
  intraNDraws    = 250,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_cratio_level_2"]] = -exp( mu_log_b_cratio_level_2 + sigma_log_b_cratio_level_2 * draws_cratio_level_2 )
  randcoeff[["b_cost"]] = -exp( mu_log_b_cost + sigma_log_b_cost * draws_cost )
  
  # LV
  randcoeff[["LV_env"]] = (
    + gamma_colgrad_env*(Education == 'College graduate' | Education == 'Graduate or professional school')
    + gamma_u25k_env*(Income == 'Under $25,000')
    + gamma_100149k_env*(Income == '$100,000 - $149,999')
    
    + eta_env)
  
  randcoeff[["LV_evfeatconv"]] = (
    gamma_u25k_evfeatconv*(Income_Encode == 1)
    + eta_evfeatconv)
  
  return(randcoeff)
}


# ####################################################### #
#### 4. Input validation                               ####
# ####################################################### #

apollo_inputs = apollo_validateInputs()
#> Missing setting for mixing, set to default of FALSE
#> Missing setting for nCores, set to default of 1
#> Missing setting for workInLogs, set to default of FALSE
#> Missing setting for seed, set to default of 13
#> Missing setting for HB, set to default of FALSE
#> Several observations per individual detected based on the value of ID.
#> Setting panelData set to TRUE.
#> All checks on apollo_control completed.
#> All checks on data completed.

# ####################################################### #
#### 5. Likelihood definition                          ####
# ####################################################### #

apollo_probabilities=function(apollo_beta, apollo_inputs,
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  ### Likelihood of indicators
  # Environmental Attitude Indicator Questions
  ol_settings1 = list(outcomeOrdered = Env01,
                      V              = zeta_env01*LV_env,
                      tau            = list(tau_env_1, tau_env_2, tau_env_3, tau_env_4), # the threshold for ordered logit (continuous instead of ordered)
                      rows           = (task==1),
                      componentName  = "indic_env01")
  ol_settings2 = list(outcomeOrdered = Env02,
                      V              = zeta_env02*LV_env,
                      tau            = list(tau_env_1, tau_env_2, tau_env_3, tau_env_4), # the threshold for ordered logit (continuous instead of ordered)
                      rows           = (task==1),
                      componentName  = "indic_env02")
  ol_settings3 = list(outcomeOrdered = Env03,
                      V              = zeta_env03*LV_env,
                      tau            = list(tau_env_1, tau_env_2, tau_env_3, tau_env_4), # the threshold for ordered logit (continuous instead of ordered)
                      rows           = (task==1),
                      componentName  = "indic_env03")
  ol_settings4 = list(outcomeOrdered = Env04,
                      V              = zeta_env04*LV_env,
                      tau            = list(tau_env_1, tau_env_2, tau_env_3, tau_env_4), # the threshold for ordered logit (continuous instead of ordered)
                      rows           = (task==1),
                      componentName  = "indic_env04")
  
  # Positive Attitudes Towards EV Features
  ol_settings10 = list(outcomeOrdered = EV01,
                       V              = zeta_ev01*LV_evfeatconv,
                       tau            = list(tau_evfeatconv_1, tau_evfeatconv_2, tau_evfeatconv_3, tau_evfeatconv_4), # the threshold for ordered logit (continuous instead of ordered)
                       rows           = (task==1),
                       componentName  = "indic_ev01")
  ol_settings11 = list(outcomeOrdered = EV07,
                       V              = zeta_ev07*LV_evfeatconv,
                       tau            = list(tau_evfeatconv_1, tau_evfeatconv_2, tau_evfeatconv_3, tau_evfeatconv_4), # the threshold for ordered logit (continuous instead of ordered)
                       rows           = (task==1),
                       componentName  = "indic_ev07")
  ol_settings12 = list(outcomeOrdered = EV08,
                       V              = zeta_ev08*LV_evfeatconv,
                       tau            = list(tau_evfeatconv_1, tau_evfeatconv_2, tau_evfeatconv_3, tau_evfeatconv_4), # the threshold for ordered logit (continuous instead of ordered)
                       rows           = (task==1),
                       componentName  = "indic_ev08")
  ol_settings13 = list(outcomeOrdered = EV011,
                       V              = zeta_ev011*LV_evfeatconv,
                       tau            = list(tau_evfeatconv_1, tau_evfeatconv_2, tau_evfeatconv_3, tau_evfeatconv_4), # the threshold for ordered logit (continuous instead of ordered)
                       rows           = (task==1),
                       componentName  = "indic_ev011")
  
  
  P[["indic_env01"]] = apollo_ol(ol_settings1, functionality)
  P[["indic_env02"]] = apollo_ol(ol_settings2, functionality)
  P[["indic_env03"]] = apollo_ol(ol_settings3, functionality)
  P[["indic_env04"]] = apollo_ol(ol_settings4, functionality)
  
  P[["indic_ev01"]] = apollo_ol(ol_settings10, functionality)
  P[["indic_ev07"]] = apollo_ol(ol_settings11, functionality)
  P[["indic_ev08"]] = apollo_ol(ol_settings12, functionality)
  P[["indic_ev011"]] = apollo_ol(ol_settings13, functionality)
  
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as
  ### in mnl_settings, order is irrelevant.
  V = list()
  
  #Include all variables
  
  #either one of b_soc_high_level_2*(SoC >= 80) or b_soc_med_dcfc*(SoC > 30 & SoC < 80)
  
  V[['Level_2']] = (b0_Level_2 
                    + b_cost*Cost_Level2 + b_cratio_level_2*(Charging_Time_Level2/Total_Time_Level2) 
                    + b_wtime*(Waiting_Time_Level2) + b_soc_high_level_2*(SoC >= 80)
  )  
  

  V[['DCFC']] = (b0_DCFC 
                 + b_cost*Cost_DCFC + b_ctime_dcfc*(Charging_Time_DCFC) 
                 + b_wtime*(Waiting_Time_DCFC) #+ b_retired_dcfc*(Occupation == 'Retired')
                 + lambda_env_dcfc*LV_env
                 # + lambda_pubbar_dcfc*LV_pubbar
                 # + lambda_evfeatconv_dcfc*LV_evfeatconv
                 # + lambda_evexpreli_dcfc*LV_evexpreli
  ) 
  
  V[['DWPT']] = (b0_DWPT + b_cost*Cost_DWPT 
                 + b_tottime_dwpt*Total_Time_DWPT
                 # + lambda_pubbar_dwpt*LV_pubbar
                 + lambda_evfeatconv_dwpt*LV_evfeatconv
                 # + lambda_evexpreli_dwpt*LV_evexpreli
  )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(Level_2=1, DCFC=2, DWPT=3),
    avail         = 1, #list(Level_2=av_Level2, DCFC=av_DCFC, DWPT=av_DWPT)
    choiceVar     = Decision_Encode,
    V             = V,
    componentName = "choice"
  )
  
  ### Compute probabilities using MNL model
  P[['choice']] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

# ####################################################### #
#### 6. Model estimation and reporting                 ####
# ####################################################### #

model = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

apollo_modelOutput(model)

# The outcomes of model estimation are saved in a list called model: model$estimates,
# model$varcov, model$robvarcov
# apollo_saveOutput(
#   # "C:/Users/kenny/OneDrive - purdue.edu/Documents/Kenny's File/Transportation Literature/Fall 2023/SPR 4706 - EV Public Perception Expectation and WTP/SPR 4706/NL Output",
#   model,
#   saveOutput_settings = list(printClassical=TRUE,
#                              printPVal = TRUE
#                              # printT1=TRUE,
#                              # printCovar=TRUE,
#                              # printCorr=TRUE,
#                              # printOutliers=TRUE,
#                              # saveCov=TRUE,
#                              # saveCorr=TRUE
#   )
# ) #for output to files check out settings

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
# apollo_sink()
# 
# # ----------------------------------------------------------------- #
# #---- MODEL PREDICTIONS                                          ----
# # ----------------------------------------------------------------- #
# Elasticity Estimation
# Cost Level 2
# predictions_base <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                       prediction_settings=list(runs=30))
# 
# database$Cost_Level2 <- 1.01*database$Cost_Level2
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_base = predictions_base[["choice"]][["at_estimates"]]
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Cost_Level2 <- 1/1.01*database$Cost_Level2 # return to original data
# apollo_inputs <- apollo_validateInputs()
# 
# # Cost DCFC
# database$Cost_DCFC <- 1.01*database$Cost_DCFC
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Cost_DCFC <- 1/1.01*database$Cost_DCFC # return to original data
# apollo_inputs <- apollo_validateInputs()
# 
# # Cost DWPT
# database$Cost_DWPT <- 1.01*database$Cost_DWPT
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Cost_DWPT <- 1/1.01*database$Cost_DWPT # return to original data
# apollo_inputs <- apollo_validateInputs()
# 
# # Waiting time Level 2
# database$Waiting_Time_Level2 <- 1.01*database$Waiting_Time_Level2
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Waiting_Time_Level2 <- 1/1.01*database$Waiting_Time_Level2 # return to original data
# apollo_inputs <- apollo_validateInputs()
# 
# # Waiting time DCFC
# database$Waiting_Time_DCFC <- 1.01*database$Waiting_Time_DCFC
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Waiting_Time_DCFC <- 1/1.01*database$Waiting_Time_DCFC # return to original data
# apollo_inputs <- apollo_validateInputs()
# 
# # Charging time Level 2
# database$Charging_Time_Level2 <- 1.01*database$Charging_Time_Level2
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Charging_Time_Level2 <- 1/1.01*database$Charging_Time_Level2 # return to original data
# apollo_inputs <- apollo_validateInputs()
# 
# # Charging time DCFC
# database$Charging_Time_DCFC <- 1.01*database$Charging_Time_DCFC
# apollo_inputs <- apollo_validateInputs()
# 
# predictions_new <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
#                                      prediction_settings=list(runs=30))
# 
# predictions_new = predictions_new[["choice"]][["at_estimates"]]
# 
# log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.1)
# log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.1)
# log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.1)
# 
# database$Charging_Time_DCFC <- 1/1.01*database$Charging_Time_DCFC # return to original data
# apollo_inputs <- apollo_validateInputs()

# 
# # ----------------------------------------------------------------- #
# #---- CONDITIONALS AND UNCONDITIONALS                            ----
# # ----------------------------------------------------------------- #
# 
# conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)
# 
# summary(conditionals)
# 
# unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)
# tiff("C:\\Users\\wijayak\\Box\\[L1FR] STSRG Lab Folder\\Prasanna\\Papers\\FMLM 3-05-24\\Purdue\\ttranpar_POR.tiff", width =6, height = 4, units = 'in', res = 500)
# #par(mar=c(4,6,1,0))
# plot(density(unconditional$b_tt_sc), xlim=c(-11,11), ylim=c(0,1),xlab="", 
#      main="", col="red", lwd=2)
# lines(density(unconditional$b_cratio_level_2), col= "orange",lwd=2)
# lines(density(unconditional$b_retired_dcfc), col= "blueviolet",lwd=2)
# lines(density(unconditional$b_cost), col= "blue",lwd=2)
# legend("topright", inset = .04, legend=c("charging time over total travel time", "Retired","Cost")
#                                          , col=c("orange","blueviolet", "blue"), lty = 1, lwd = 2, box.lty = 0)
# 
# dev.off()

# 
# # mean(unconditionals[[1]])
# # sd(unconditionals[[1]])
# 
# # ----------------------------------------------------------------- #
# #---- switch off writing to file                                 ----
# # ----------------------------------------------------------------- #
# 
# apollo_sink()