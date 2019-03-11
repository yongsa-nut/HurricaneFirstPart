## Statis Model of Human decision during Hurricane Base on questionnaires
## Description: This is the experiment to test the performance of the model
library(readr)
library(glmnet)
library(MASS)
library(rpart)
library(dplyr)
library(rstan)
library(e1071)

####################################  Function Section  #########################################
#################################################################################################
## Gen Evacuation expected utility ## 
## Description: 
##   Safety (much smaller than stay case), cost of evac = traveling + lodging, lost job
##   lodging <- money and experience
Gen_Evac_EU <- function(partial_evac_cost, safety_util, extra_utility_evac, log_transform = FALSE)
{
  if(log_transform) 
  { 
    return(-1*log(partial_evac_cost + extra_utility_evac + 0.0000001,base = 10)) 
  }
  else
  {
    return(-1*(partial_evac_cost + extra_utility_evac))
  }
}

## Gen Stay expected utility ##
## Description: 
##  Safety (+ food), Flood, Electricity
Gen_Stay_EU <- function(partial_stay_cost, safety_prob_num, safety_eq_util,log_transform = FALSE)
{
  
  if(log_transform) 
  { 
    return(-1*(log(partial_stay_cost + 0.0000001, base=10) + safety_prob_num*safety_eq_util))
  }
  else
  {
    return(-1*(partial_stay_cost + safety_prob_num*safety_eq_util) )
  }
}

## Softmax function ##
Log_softmax <- function(first,second)
{
  max_v <- max(first,second)
  first <- first - max_v 
  second <- second - max_v
  return(  first - log(exp(first) + exp(second) + 0.00001) )
}
Softmax_error <- function(evac_decision_num,evac_eu,stay_eu)
{
  if(evac_decision_num == 0) {  return(-1*log_softmax(stay_eu,evac_eu)) }
  if(evac_decision_num == 1) {  return(-1*log_softmax(evac_eu,stay_eu)) }
}

## Geometric series ##
## For calculating, discounted duration
Geometric_value <- function(discount, duration)
{
  if(discount == 1) { return(duration) }
  return( (1- (discount^duration))/(1 - discount)  )
}

## F-score
## true_positive = predict evac correctly, false negative = predict evac incorrectly (predict stay but actually evac)
## true_negative = predict stay correctly, false positive = predict stay incorrectly (predict evac but actually stay)
F_score <- function(true_positive, false_negative, true_negative, false_positive)
{
  ## Recall
  recall <- true_positive/(true_positive + false_negative)
  if(true_positive + false_negative == 0) { recall <- 0 }
  ## Precision
  precision <- true_positive/(true_positive + false_positive)
  if(true_positive + false_positive == 0) { precision <-0 }
  
  ## F score
  if(true_positive == 0) { f_score <- 0 }
  else { f_score <- (2*recall*precision)/(recall + precision) }
  
  
  return(list("recall" = recall, "precision" = precision, "f_score" = f_score))
}

## Error calculation function
## Input: the dataset to use, paramter to compute error, type of error (class vs softmax)
##   For the dataset, there are 3 things needed to compute EU: partial_stay_cost, partial_evac_cost, safety_prob_num
## Output: error
Model_error <- function(dat, safety_util, noise_util, type = "class", total = TRUE, log_transform = TRUE)
{
  #stay_eu <- mapply(Gen_Stay_EU, dat$partial_stay_cost, dat$safety_prob_num, safety_util)
  #evac_eu <- mapply(Gen_Evac_EU, dat$partial_evac_cost, safety_util, noise_util)
  ## All the EU calculation is done here in vector form to improve performance.
  if(!log_transform)
  {
    stay_eu <- -1*(dat$partial_stay_cost + safety_util*dat$safety_prob_num)
    evac_eu <- -1*(dat$partial_evac_cost + noise_util)
    ##
    # evac_eu = -1*log10(dat$partial_evac_cost + noise_util*dat$normal_condition_return + 0.000001);
    # stay_eu = -1*log10(dat$partial_stay_cost + dat$safety_prob*safety_util + 0.000001);
  }else
  {
    stay_eu <- -1*(dat$partial_stay_cost + log(safety_util +  epsilon, base = 10)*dat$safety_prob_num )
    evac_eu <- -1*(dat$partial_evac_cost + log(noise_util + epsilon, base = 10))
  }
  
  predicted_decision <- if_else(evac_eu > stay_eu, 1, 0)
  
  if(type == "class")
  {
    if(total) { return(sum(dat$evac_decision_num != predicted_decision))  }
    
    return( list( "error_evac" = sum(dat$evac_decision_num == 1 & predicted_decision == 0),
                  "error_stay" = sum(dat$evac_decision_num == 0 & predicted_decision == 1))) 
  }
  if(type == "softmax")
  {
    return(sum(mapply(Softmax_error,dat$evac_decision_num,evac_eu,stay_eu)))
  }
}

## Grid search function return the best safety util and noise util in a given sequence
Grid_search_model <- function(dat,safety_seq, noise_seq, log_transform)
{
  best_safety_util <- 0
  best_noise_util <- 0
  best_error <- Inf ## max possible error
  
  for( safety_util in safety_seq)
  {
    for( noise_util in noise_seq)
    {
      total_error <- Model_error(dat, safety_util, noise_util, log_transform = log_transform)
      if(total_error < best_error)
      {
        best_safety_util <- safety_util
        best_noise_util  <- noise_util
        best_error       <- total_error
      }
    }
  }
  
  return(list("best_safety_util" = best_safety_util, "best_noise_util" = best_noise_util))
}

#################################################################################################

#####################################  Mapping section  #########################################
#################################################################################################
## Maping from questionnaire's answer to numerical value ##

## probability mapping ##
## Two starting point: Uniform and Normal
## Uniform: (0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.825), range: [-2.5,0,+2.5]
## Normal: (0.05, 0.13, 0.32, 0.5, 0.68, 0.87, 0.95), range: [-2,-1,0,+1,+2] 
## Generate one list to loop over. 
prob_mappings <- data.frame()

uniform_mapping <- c(0.125,0.25,0.375,0.5,0.625,0.75,0.825)
uniform_range <- c(-0.025,0,0.025)

for(i in uniform_range)
{
  ## Lower half get move in the other direction of upper half
  temp_mapping <- c(uniform_mapping[1:3]+(-1*i),uniform_mapping[4],uniform_mapping[5:7]+i)
  prob_mappings <- rbind(prob_mappings, temp_mapping)
}

normal_mapping <- c(0.05, 0.13, 0.32, 0.5, 0.68, 0.87, 0.95)
normal_range <- c(-0.02,0,0.02)

for(i in normal_range)
{
  temp_mapping <- c(normal_mapping[1:3]+(-1*i),normal_mapping[4],normal_mapping[5:7]+i)
  prob_mappings <- rbind(prob_mappings, temp_mapping)
}

Gen_prob_from_map <- function(prob_map, value)
{
  if(value == "Extremely unlikely")  { return(as.numeric(prob_map[1])) }
  if(value == "Moderately unlikely") { return(as.numeric(prob_map[2])) }
  if(value == "Slightly unlikely")   { return(as.numeric(prob_map[3])) }
  if(value == "Neither likely nor unlikely (50/50)") { return(as.numeric(prob_map[4]))  }
  if(value == "Slightly likely")     { return(as.numeric(prob_map[5])) }
  if(value == "Moderately likely")   { return(as.numeric(prob_map[6])) }
  if(value == "Extremely likely")    { return(as.numeric(prob_map[7])) }
}

mixed_mapping <- c(0.05, 0.2125, 0.375, 0.5, 0.625, 0.7875, 0.95)

prob_mapping_list <- list("uniform" = uniform_mapping,
                          "normal"  = normal_mapping,
                          "mixed"   = mixed_mapping)

## Flodd depth mapping ##
## (Note: missing 1 - 2 and 3 - 4) (Fix in Michael questionnaire)
## 1     == [0.00 - 1.99]
## 2 - 3 == [2.00 - 3.99]
## 4 - 5 == [4.00 - 5.99]
## 5 +   == [6.00,)

Gen_flood_depth_mapping <- function(value)
{
  if(value == "Will not be flooded") {return(0)}
  if(value == "1 foot or less")      {return(0.5)}
  if(value == "1 - 3 feet")          {return(2)}
  if(value == "2 - 3 feet")          {return(2.5)}
  if(value == "3 - 5 feet")          {return(4)}
  if(value == "4 - 5 feet")          {return(4.5)}
  if(value == "More than 5 feet")    {return(6)}
}

## Duration Mapping ##
## This is different from other mapping, because of discount factor potentially.
## So, it returns a list instead. 
## Side effect: if discount becomes a param of the model, mapping has to occur inside validation.
## (Note: there is no 2-3 choice ... ) (Fix in Michael Questionnaire)
## < 1   == [0.00 - 0.99]
## 1 - 2 == [1.00 - 2.99]
## 3 - 4 == [3.00 - 4.99]
## 4+    == [5.00,)
Gen_normal_duration_mapping <- function(discount = 1)
{
  temp_list <- list()
  temp_list[["Less than one week"]]  <- Geometric_value(discount, 3) 
  temp_list[["1 - 2 weeks"]]         <- Geometric_value(discount, 10.5)
  temp_list[["2 -3 weeks"]]          <- Geometric_value(discount, 17.5) 
  temp_list[["3 -4 weeks"]]          <- Geometric_value(discount, 24.5)       
  temp_list[["More than a month"]]   <- Geometric_value(discount, 30) 
  return(temp_list)
}
Gen_elec_duration_mapping <- function(discount = 1)
{
  temp_list <- list()
  temp_list[["Will not lose electricity"]] <- 0
  temp_list[["Less than one week"]]        <- Geometric_value(discount,3) 
  temp_list[["1 - 2 weeks"]]               <- Geometric_value(discount,10.5) 
  temp_list[["2- 3 weeks"]]               <- Geometric_value(discount,17.5) 
  temp_list[["3 - 4 weeks"]]               <- Geometric_value(discount,24.5)      
  temp_list[["More than a month"]]         <- Geometric_value(discount,30)
  return(temp_list)
}

## generate a new dataset with features from mapping answer to number
Gen_new_data_from_map <- function(dat, mapping, skip_flag = FALSE)
{
  ## Gen new dataset from mapping 
  ## All of mappings should be independent from the dataset. (Just map from text to number)
  dat_temp <- dat 
  
  ################################# Latent Variable Estimation ######################################
  ## Set up other latent variables that may not in the datset (mainly post version)
  ## For post florence version, these are flood cost and electricity cost.
  ## Three types: median of pre/hypo, regression from pre/hypo, group - mean/median of pre/hypo
  if(mapping$out_feature == "mean")
  {
    dat_temp$flood_cost <- mean(dat_for_latent_variable$flood_cost)
    dat_temp$electricity_cost <- mean(dat_for_latent_variable$electricity_cost)
  }
  if(mapping$out_feature == "median")
  {
    dat_temp$flood_cost <- median(dat_for_latent_variable$flood_cost)
    dat_temp$electricity_cost <- median(dat_for_latent_variable$electricity_cost)
  }
  if(mapping$out_feature == "group")
  {
    group_to_use <- "income"
    flood_cost_group <- sapply(unique(dat_for_latent_variable[[group_to_use]]), 
                               function(x) median(dat_for_latent_variable$flood_cost[dat_for_latent_variable[[group_to_use]] == x]))
    electricity_cost_group <- sapply(unique(dat_for_latent_variable[[group_to_use]]), 
                                     function(x) median(dat_for_latent_variable$electricity_cost[dat_for_latent_variable[[group_to_use]] == x]))
    dat_temp$flood_cost <- sapply(dat_temp[[group_to_use]], function(x) flood_cost_group[[x]])
    dat_temp$electricity_cost <- sapply(dat_temp[[group_to_use]], function(x) electricity_cost_group[[x]])
  }
  if(mapping$out_feature == "regression")
  {
    ## regression only with demographic information ("observable")
    ## Two version: Full elastic net vs no regularization
    ## Elastic Net
    features <- observable_features
    x_train <- model.matrix(~ .-1, dat_for_latent_variable[, features])
    
    flood_cost_glmnet <- cv.glmnet(x_train, y = dat_for_latent_variable$flood_cost)
    electricity_cost_glmnet <- cv.glmnet(x_train, y = dat_for_latent_variable$electricity_cost)
    
    dat_temp$flood_cost <- predict(flood_cost_glmnet, newx = model.matrix(~ .-1, dat_temp[,features]), s="lambda.min")
    dat_temp$electricity_cost <- predict(electricity_cost_glmnet, newx = model.matrix(~ .-1, dat_temp[,features]), s="lambda.min")
    
    #flood_cost_glm <- glm(flood_cost ~ age + income + distance_to_coast + house_structure + 
    #                               years_in_resident + edu + family_size, data = dat_for_latent_variable) 
    #electricity_cost_glm <- glm(electricity_cost ~ age + income + distance_to_coast + house_structure + 
    #                                     years_in_resident + edu + family_size, data=dat_for_latent_variable)
    #dat_training$flood_cost <- predict(flood_cost_glm,newdata = dat_training)
    #dat_training$electricity_cost <- predict(electricity_cost_glm, newdata= dat_training)
  }
  
  ############################  End of Latent Variable Estimation  #####################################
  
  set.seed(mapping$seed)
  dat_temp <- dat_temp[sample(nrow(dat_temp)),]
  
  if(!skip_flag)
  {
    dat_temp$flood_cost <- dat_temp$flood_cost/15 ## 5 days 3 meters
    dat_temp$electricity_cost <- dat_temp$electricity_cost/5 ## 5 days
  
    # dat_temp$safety_prob_num <- unlist(lapply(dat_temp$safety_prob,
    #                                             function(x) Gen_prob_from_map( prob_mappings[mapping$prob,] ,x)))
    dat_temp$safety_prob_num <- unlist(lapply(dat_temp$safety_prob,
                                                function(x) Gen_prob_from_map( prob_mapping_list[[mapping$prob]] ,x)))
  
    normal_duration_map <- Gen_normal_duration_mapping(mapping$discount)
    dat_temp$normal_condition_return_num <- unlist(lapply(dat_temp$normal_condition_return, 
                                                          function(x) normal_duration_map[[x]] ))
  
    electricity_duration_map <- Gen_elec_duration_mapping(mapping$discount)
    dat_temp$electricity_lost_dur_num <- unlist(lapply(dat_temp$electricity_lost_dur, 
                                                       function(x) electricity_duration_map[[x]] ))
    
    dat_temp$flood_depth_num <- unlist(lapply(dat_temp$flood_depth, 
                                              function(x) Gen_flood_depth_mapping(x) ))
    #Log transform
    dat_temp$traveling_cost_log   <- log(dat_temp$traveling_cost + epsilon, base = 10)
    dat_temp$safe_place_cost_log  <- log(dat_temp$safe_place_cost + epsilon, base = 10)
    dat_temp$flood_cost_log       <- log(dat_temp$flood_cost + epsilon, base = 10)               
    dat_temp$electricity_cost_log <- log(dat_temp$electricity_cost + epsilon, base = 10)
  }
  
  ## Gen partial cost of each action
  if(mapping$log_tran == "log-whole"){
    # Ver 1: total
    dat_temp$partial_evac_cost <- log((dat_temp$traveling_cost + (dat_temp$safe_place_cost*dat_temp$normal_condition_return_num))
                                        + epsilon, base = 10)
    dat_temp$partial_stay_cost <- log(((dat_temp$flood_cost)*(dat_temp$flood_depth_num*dat_temp$normal_condition_return_num) +
                                    (dat_temp$electricity_cost)*(dat_temp$electricity_lost_dur_num)) + epsilon, base = 10)
  }
  if(mapping$log_trans == "log-part"){
    
    ## Ver 2: each part
    dat_temp$partial_evac_cost <- log(dat_temp$traveling_cost + epsilon , base = 10) +
      log(dat_temp$safe_place_cost*dat_temp$normal_condition_return_num + epsilon , base = 10)
    dat_temp$partial_stay_cost <- log( (dat_temp$flood_cost)*dat_temp$flood_depth_num*dat_temp$normal_condition_return_num + epsilon , base=10) +
      log((dat_temp$electricity_cost)*dat_temp$electricity_lost_dur_num + epsilon , base = 10)
    # ## Ver 3: specific part: This one does not make any sense because the multiplication of duration explodes beyond log scale.
    # dat_to_use$partial_evac_cost <- (dat_to_use$traveling_cost_log + (dat_to_use$safe_place_cost_log*dat_to_use$normal_condition_return_num))
    # dat_to_use$partial_stay_cost <- ((dat_to_use$flood_cost_log/15)*(dat_to_use$flood_depth_num*dat_to_use$normal_condition_return_num) +
    #                                    (dat_to_use$electricity_cost_log/5)*(dat_to_use$electricity_lost_dur_num))
  }
  if(mapping$log_trans == "none"){
    dat_temp$partial_evac_cost <- (dat_temp$traveling_cost + (dat_temp$safe_place_cost*dat_temp$normal_condition_return_num))
    dat_temp$partial_stay_cost <- ((dat_temp$flood_cost)*(dat_temp$flood_depth_num*dat_temp$normal_condition_return_num) +
                                       (dat_temp$electricity_cost)*(dat_temp$electricity_lost_dur_num))
  }
  
  return(dat_temp)
}

#################################################################################################

#######################################  Dataset  ###############################################
#################################################################################################
## Read in dataset ##
post_florence <- read_csv("Data/pre-processed post florence.csv")
pre_florence  <- read_csv("Data/pre-processed pre florence.csv")
post_michael  <- read_csv("Data/pre-processed post michael.csv")

## Default: exclude all none NC and SC for florence based on the zipcode provided by subject. 
## post_michael alraedy included only GA or FL
post_florence <- post_florence[which(post_florence$state == "NC" | post_florence$state == "SC"),]
pre_florence <- pre_florence[which(pre_florence$state == "NC" | pre_florence$state == "SC"),]
pre_florence <- pre_florence[which(pre_florence$heard_florence == "Yes"),]

Fill_info_na <- function(dat)
{
  dat$SM_causualities[which(is.na(dat$SM_causualities))] <- "None at all"     
  dat$SM_dmg_done[which(is.na(dat$SM_dmg_done))] <- "None at all"
  dat$SM_evac[which(is.na(dat$SM_evac))] <- "None at all"
  dat$SM_forecasted_dmg[which(is.na(dat$SM_forecasted_dmg))] <- "None at all"
  dat$SM_preparing[which(is.na(dat$SM_preparing))] <- "None at all"
  dat$SM_stay[which(is.na(dat$SM_stay))] <- "None at all"
  dat$SM_traffic_jams[which(is.na(dat$SM_traffic_jams))] <- "None at all" 
  dat$TVR_causualities[which(is.na(dat$TVR_causualities))] <- "None at all"
  dat$TVR_dmg_done[which(is.na(dat$TVR_dmg_done))] <- "None at all"
  dat$TVR_evac[which(is.na(dat$TVR_evac))] <- "None at all"
  dat$TVR_forecasted_dmg[which(is.na(dat$TVR_forecasted_dmg))] <- "None at all"
  dat$TVR_preparing[which(is.na(dat$TVR_preparing))] <- "None at all"
  dat$TVR_stay[which(is.na(dat$TVR_stay))] <- "None at all"
  dat$TVR_traffic_jams[which(is.na(dat$TVR_traffic_jams))] <- "None at all"  
  
  return(dat)
}

post_florence <- Fill_info_na(post_florence)
pre_florence  <- Fill_info_na(pre_florence)
post_michael  <- Fill_info_na(post_michael)

### New variables (independent of mapping) ###

## For the post florence, the answer to the question of estimation of cost depends on the decision of subjects
## In other word, those who evaced answered different question from those who stayed, but both of them are for the same estimation

## For those who evac, we have more information to get better estimation. 
## For traveling  cost, we know the most of evac and how far the destimation is which allow us to estimate for real cost 
## rather using the one provided by subjects. Similarly, for safe place cost of evac subjects, we know where they went to.
adjusted <- FALSE
## pre florence
pre_florence$prev_exp_decision  <- ifelse(pre_florence$prev_exp == "No","No experience",pre_florence$prev_exp_decision)
pre_florence$evac_prob_num <-  unlist(lapply(pre_florence$evac_prob, 
                                             function(x) Gen_prob_from_map( prob_mappings[5,] ,x)))
pre_florence$evac_decision_num <- ifelse(pre_florence$evac_prob_num > .5, 1,0)
pre_florence$safe_place_cost <- pre_florence$safe_place_cost/5  ## for pre, it's for 5 days
## post florence
post_florence$traveling_cost <- ifelse(post_florence$evac_decision == "Yes", post_florence$traveling_cost_evac, 
                                       post_florence$traveling_est_stay)
post_florence$safe_place_cost <- ifelse(post_florence$evac_decision == "Yes", post_florence$place_cost_evac, 
                                        post_florence$place_cost_est_stay/7)   ##for stay it's for a week
post_florence$evac_decision_num <- ifelse(post_florence$evac_decision=="Yes",1,0)
post_florence$prev_exp_decision <- ifelse(post_florence$prev_exp == "No","No experience",post_florence$prev_exp_decision)
## post michael
post_michael$traveling_cost <- ifelse(post_michael$evac_decision == "Yes", post_michael$traveling_cost_evac, 
                                      post_michael$traveling_est_stay)
post_michael$safe_place_cost <- ifelse(post_michael$evac_decision == "Yes", post_michael$place_cost_evac, 
                                       post_michael$place_cost_est_stay)   ## per day for michael
post_michael$evac_decision_num <- ifelse(post_michael$evac_decision=="Yes",1,0)
post_michael$prev_exp_decision <- ifelse(post_michael$prev_exp == "No","No experience",post_michael$prev_exp_decision)

if(adjusted)
{
  ## adjusted pre florence
  pre_florence$safe_place_cost[which(pre_florence$place_to_evac == "Yes - The house of a friend or relative")] <- 0
  
  ## adjusted post florence
  how_far_cost_map <- list("Less than 10 miles" = 1, "10 - 50 miles" = 10, "50 - 100 miles" = 50, "Greeter than 100 miles" = 100)
  by_your_vehicle_ind <- which(post_florence$how_evac == "By your vehicle")
  post_florence$traveling_cost[by_your_vehicle_ind] <- mapply(function(x,y) min(x, how_far_cost_map[[y]]), 
                                                              post_florence$traveling_cost_evac[by_your_vehicle_ind], 
                                                              post_florence$how_far_evac[by_your_vehicle_ind])
  post_florence$safe_place_cost[which(post_florence$where_evac == "Friend/relative house")] <- 0
  
  ## adjusted post michael
  by_your_vehicle_ind <- which(post_michael$how_evac == "By your vehicle")
  post_michael$traveling_cost[by_your_vehicle_ind] <- mapply(function(x,y) min(x, how_far_cost_map[[y]]), 
                                                             post_michael$traveling_cost_evac[by_your_vehicle_ind], 
                                                             post_michael$how_far_evac[by_your_vehicle_ind])
  post_michael$safe_place_cost[which(post_michael$where_evac == "Friend/relative house")] <- 0
}

### Exclusion criterias ###
## Minimal exclude all answer of any numerical questions (except family size) that are above 3 s.d.
## family size == 0 | family size >= 10 are exclude 
## duration spending on the questionnaire: exclude > one hour (3600 s) and under 3 mins (180 s)
## Only above because zero still makes sense in many cases.
## List of numerical questions:
##   post ver: traveling_cost, safe_place_cost, family_size, 
##   pre ver: traveling_cost, safe_place_cost, family_size, flood_cost, electricity_cost, shelter_cost
exclude <- TRUE
sd_threshold <- 3  ## total combination = 3 (not exclude, exclude with 3 sd, exclude with 2 sd)

to_exclude_post_florence <- c(which(post_florence$traveling_cost >= (sd_threshold*sd(post_florence$traveling_cost) + mean(post_florence$traveling_cost))),
                     which(post_florence$safe_place_cost >= (sd_threshold*sd(post_florence$safe_place_cost) + mean(post_florence$safe_place_cost))),
                     which(post_florence$family_size >= 10), 
                     which(post_florence$duration > 3600 | post_florence$duration <= 180))
to_exclude_post_florence <- unique(to_exclude_post_florence)
length(to_exclude_post_florence) ## should be 32

to_exclude_pre_florence <-  c(which(pre_florence$traveling_cost >= (sd_threshold*sd(pre_florence$traveling_cost) + mean(pre_florence$traveling_cost))),
                     which(pre_florence$safe_place_cost >= (sd_threshold*sd(pre_florence$safe_place_cost) + mean(pre_florence$safe_place_cost))),
                     which(pre_florence$flood_cost >= (sd_threshold*sd(pre_florence$flood_cost) + mean(pre_florence$flood_cost))),
                     which(pre_florence$electricity_cost >= (sd_threshold*sd(pre_florence$electricity_cost) + mean(pre_florence$electricity_cost))),
                     which(pre_florence$shelter_cost >= (sd_threshold*sd(pre_florence$shelter_cost) + mean(pre_florence$shelter_cost))),
                     which(pre_florence$family_size >= 10),
                     which(pre_florence$duration > 3600 | pre_florence$duration <= 180))
to_exclude_pre_florence <- unique(to_exclude_pre_florence)
length(to_exclude_pre_florence) ## should be 22

to_exclude_post_michael <- c(which(post_michael$traveling_cost >= (sd_threshold*sd(post_michael$traveling_cost) + mean(post_michael$traveling_cost))),
                             which(post_michael$safe_place_cost >= (sd_threshold*sd(post_michael$safe_place_cost) + mean(post_michael$safe_place_cost))),
                             which(post_michael$flood_cost >= (sd_threshold*sd(post_michael$flood_cost) + mean(post_michael$flood_cost))),
                             which(post_michael$electricity_cost >= (sd_threshold*sd(post_michael$electricity_cost) + mean(post_michael$electricity_cost))),
                             which(post_michael$family_size >= 10), 
                          ## which(post_michael$state == "FL" & post_michael$longitude <= -84), ## Extra exclusion for FL outside of north.
                             which(post_michael$duration > 3600 | post_michael$duration <= 180))
to_exclude_post_michael <- unique(to_exclude_post_michael)
length(to_exclude_post_michael) ## should be 27

if(exclude)
{
  pre_florence <- pre_florence[-to_exclude_pre_florence,]
  post_florence <- post_florence[-to_exclude_post_florence,]
  post_michael  <- post_michael[-to_exclude_post_michael,] 
}

dim(pre_florence)   # 356
dim(post_florence)  # 684
dim(post_michael)   # 542

#################################################################################################

####################################  Set up grouping  ##########################################
#################################################################################################
## Grouping here means different group have different set of parameters. (Estimate separetely)
## 5 types of group:
##   1) Income: Low, mid, high
##   2) Family size: 1, 2, 3, >=4
##   3) Distance to coast: Close, Mid, Far
##   4) age group: 18-30, 30 - 45, 45-60, 60+
##   5) evac notice group: manda, volun, none
## Default: No group (none)
## To group: divide the fold into group 
income_group <- function(value)
{
  if(value == "Less than $20,000")  { return("1") }
  if(value == "$20,000 to $40,000") { return("2") }
  if(value == "$40,000 to $60,000") { return("3") } 
  if(value == "$60,000 to $80,000") { return("3") }
  if(value == "$80,000 to $100,000") { return("3") }
  if(value == "$100,000 to $150,000" | value == "Over $150,000") { return("4") }
}

family_size_group <- function(value)
{
  if(value >= 4) {return("4")}
  return(as.character(value))
}

distance_group <- function(value)
{
  if(value == "Within 1 mile"| value=="1 to 4 miles") { return("1") }
  if(value == "4 to 10 miles"| value=="10 to 30 miles"|value == "30 to 50 miles") { return("2") }
  if(value=="More than 50 miles") { return("3") }
}

age_group <- function(value)
{
  if(value >= 18 & value <= 30) { return("1") }
  if(value > 30 & value <= 45)  { return("2") }
  if(value > 45 & value <= 60)  { return("3") }
  if(value > 60) { return("4") }
}

## three group: didn't receive, voluntary, mandatory. (if receive both vol and manda -> manda)
evac_notice_group <- function(received_volun, received_manda)
{
  if(received_manda == 1) { return("1") }
  if(received_volun == 1) { return("2") }
  return("3")
}

prev_exp_group <- function(value)
{
  if(value == "No experience") {return("1")}
  if(value == "Evacuated") {return("2")}
  if(value == "Stayed in your home") {return("3")}
}

Add_group_to_dat <- function(dat)
{
  dat$income_group <- sapply(dat$income, function(x) income_group(x))
  dat$family_size_group <- sapply(dat$family_size, function(x) family_size_group(x))
  dat$distance_group <- sapply(dat$distance_to_coast, function(x) distance_group(x))
  dat$age_group <- sapply(dat$age , function(x) age_group(x))
  dat$evac_notice_group <- mapply(evac_notice_group, dat$voluntary_evac_notice, dat$mandatory_evac_notice)
  dat$prev_exp_group  <- sapply(dat$prev_exp_decision, function(x) prev_exp_group(x))
  dat$none <- "1"
  return(dat)
}

pre_florence  <- Add_group_to_dat(pre_florence)
post_florence <- Add_group_to_dat(post_florence)
post_michael  <- Add_group_to_dat(post_michael)

group_safety_list <- list()
group_noise_list <- list()

#################################################################################################

###############################  Main Part of within dataset  ###################################
#################################################################################################

## Feature group for other model to fit ##
## Only features that appears in both target and source (manually adjust: post florence has the least features)
observable_features <- c("age","gender","edu","years_in_resident","distance_to_coast","num_vehicle",
                         "has_pet","family_size","child_0_5","child_6_12","child_13_18","elder_65_up",
                         "special_need_med","special_need_moving","income","house_structure",
                         "mandatory_evac_notice","voluntary_evac_notice"#,"prev_exp_decision"
                         )
latent_features <- c("flood_cost","electricity_cost","traveling_cost","safe_place_cost","safety_prob_num",
                     "flood_depth_num","electricity_lost_dur_num","normal_condition_return_num" )
infomation_features <- c("TVR_dmg_done","TVR_forecasted_dmg","TVR_causualities","TVR_traffic_jams","TVR_preparing",
                         "TVR_stay","TVR_evac","SM_dmg_done","SM_forecasted_dmg","SM_causualities","SM_traffic_jams",
                         "SM_preparing","SM_stay","SM_evac")
social_features <- c("contribute_final_decision","household_discussion",
                     "outside_family_discussion","outside_friend_discussion","neighbors_observation", "prev_exp_decision") 
other_features_list <- list(observable_features, latent_features, c(observable_features,latent_features),
                            c(observable_features,latent_features,infomation_features,social_features))
other_features_namae_list <- c("obs","latent","obs+latent","obs+latent+others")
## Set up the main (training) dataset for the experiment training
dat_for_latent_variable <- pre_florence  # Only use for filling in flood cost and electricity cost for post florence
dat_training <- post_florence            # pre_flornece, post_florence or post_michael
char_to_factor_training <- intersect(names(dat_training[,sapply(dat_training,is.character)]), other_features_list[[4]] )
## change string to factor for rpart to use
for(f in char_to_factor_training)
{
  dat_training[[f]] <- as.factor(dat_training[[f]])
}

## Set up the Fold of CV ##
n_fold = 10
size_of_fold <- floor(dim(dat_training)[1]/n_fold)
starting_validation_index <- seq(1,dim(dat_training)[1],by=size_of_fold)
starting_validation_index[n_fold+1] <- dim(dat_training)[1] ## mark end of dat

## Other params
epsilon <- 0.0001           ## for log in case of 0.
other_method <- TRUE        ## show result of other methods (glm, rpart , glmnet with partial and full features)

## Things to map/test
## (Code: U= universal to all methods, M= only change model)
##  - (U) Seed of validation [3: 17, 19, 23]
##  - (M) Log (part vs whole) vs No log transform 
##  - (U) Estimation of outside features   [Median, Group, Regression]
##  - (M) Estimation of model paramters    [none, income_group, family_size, distance_group, evac_notice_group]
##  - (U) Mapping of Prob and discount factor  [ c(1:6), c(1,.95,.9)  ]
##  - (U) Exclusion criteria (Not in mapping)  
##  - (U) Adjustment of traveling cost and stay cost (Not in mapping) 
seed <- c(17)
model_param <- c("none","income_group","prev_exp_group") #c("none","income_group","evac_notice_group")
out_feature <- c("none") #c("median","group","regression") # only post florence needs 
log_trans <- c("none","log-part") # "log-whole"
prob <- c("uniform","normal")
discount <- c(1)
mapping <- expand.grid(log_trans,model_param,out_feature,discount,prob,seed, stringsAsFactors = FALSE)
names(mapping) <- c("log_trans","model_param","out_feature","discount","prob","seed")

result_dat <- data.frame()
## Main loop for testing the models based on different mapping and others adjustments ##
## Inside the loop are cross validation.
## (Note: the code below is bad. A better way is to loop over each method and then for each method, loop over folds)
for(m in 1:dim(mapping)[1])
{
  print(c("mapping",m))
  map <- mapping[m,]
  ## Gen new dataset from mapping 
  ## All of mappings should be independent from the dataset. (Just map from text to number)
  dat_to_use <- Gen_new_data_from_map(dat = dat_training, map)
  log_transform <- (map$log_trans != "none")
  
  ## Set-up group
  ## Generate a group list for holding parameters 
  group_name <- map$model_param
  ## initialize group list for holding best util of each group ##
  group <- unique(dat_to_use[[group_name]])  ## income_group, family_size_group, distance_group
  for(g in group)
  {
    group_safety_list[[g]] <- 0
    group_noise_list[[g]] <- 0
  }
  
  ## Setup dataframe to hold each model error ##
  grid_search_err   <- data.frame()
  bayes_err         <- data.frame()
  if(other_method)
  {
    tree_latent_err   <- data.frame()
    tree_obs_err <- data.frame()
    tree_full_err <- data.frame()
    tree_all_err <- data.frame()
    net_latent_err    <- data.frame()
    net_obs_err <- data.frame()
    net_full_err <- data.frame()
    net_all_err <- data.frame()
    tree_err_df_list <- list(tree_obs_err,tree_latent_err,tree_full_err,tree_all_err)
    net_err_df_list  <- list(net_obs_err, net_latent_err, net_full_err,net_all_err)
  }
  
  ## Main loop for training/fold data ##
  for(j in 1:n_fold)
  {
    print(j)
    
    validation_index <- c(starting_validation_index[j]:starting_validation_index[j+1])
    training_temp    <- dat_to_use[-validation_index,]
    validation_temp  <- dat_to_use[validation_index,]
    ##in sample test -> not the most efficient way to do it, but preserved the overall flow the most
    if(n_fold == 1) { training_temp <- validation_temp }
    
    ###################   Grid Search #####
    ## divided the training by group
    for(g in group)
    {
      training_group <- training_temp[which(training_temp[[group_name]] == g),] 

      safety_seq <- seq(1500, 0, by = -5)
      noise_seq <- seq(1500, 0, by = -5)
      
      grid_params <- Grid_search_model(training_group,safety_seq,noise_seq,log_transform)
      
      ## save the best_safety_util of each group
      group_safety_list[g] <- grid_params$best_safety_util 
      group_noise_list[g]  <- grid_params$best_noise_util
    } 
    
    ## calculate the error of validation set ##
    validation_temp$best_safety_util <- sapply(validation_temp[[group_name]], function(x) group_safety_list[[x]])
    validation_temp$best_noise_util  <- sapply(validation_temp[[group_name]], function(x) group_noise_list[[x]])
    errors <- Model_error(validation_temp,validation_temp$best_safety_util,validation_temp$best_noise_util, total = FALSE, log_transform = log_transform)
    error_evac <- errors$error_evac
    error_stay <- errors$error_stay
    
    ## save the result ##
    total_evac_case <- sum(validation_temp$evac_decision_num == 1)
    total_stay_case <- sum(validation_temp$evac_decision_num == 0)
    f_score <- F_score(total_evac_case - error_evac, error_evac, 
                       total_stay_case - error_stay, error_stay)
    grid_error_list <- data.frame("size" = dim(validation_temp)[1],
                    "error_evac" = error_evac,"error_stay" = error_stay, "total_error" = error_evac + error_stay,
                    "recall" = f_score$recall, "precision" = f_score$precision, "f_score" = f_score$f_score)
    grid_search_err  <- rbind(grid_search_err , grid_error_list)
    
    #########################################################
    ###################   Bayesian Inference ################
    validation_temp_bayes <- validation_temp ##because of normalization 
    bayes_error_evac <- 0
    bayes_error_stay <- 0
    test_dat <- list(
      N = dim(dat_to_use)[1],
      evac_decision = dat_to_use$evac_decision_num,
      evac_decision_num = dat_to_use$evac_decision_num,
      partial_evac_cost = dat_to_use$partial_evac_cost,
      partial_stay_cost = dat_to_use$partial_stay_cost,
      safety_prob = dat_to_use$safety_prob_num,
      safety_prob_num = dat_to_use$safety_prob_num,
      group = as.numeric(dat_to_use[[group_name]]),
      L = length(unique(dat_to_use[[group_name]]))  ## L = level = number of group
    )
    if(map$log_trans == "none")
    {
      ## Normalize so that the cost is not large so that stan can fit
      mu_evac_cost <- mean(test_dat$partial_evac_cost)
      sd_evac_cost <- sd(test_dat$partial_evac_cost)
      mu_stay_cost <- mean(test_dat$partial_stay_cost)
      sd_stay_cost <- sd(test_dat$partial_stay_cost)
      test_dat$partial_evac_cost <- (test_dat$partial_evac_cost - mu_evac_cost)/sd_evac_cost
      test_dat$partial_stay_cost <- (test_dat$partial_stay_cost - mu_stay_cost)/sd_stay_cost
      validation_temp_bayes$partial_evac_cost <- (validation_temp_bayes$partial_evac_cost - mu_evac_cost)/sd_evac_cost
      validation_temp_bayes$partial_stay_cost <- (validation_temp_bayes$partial_stay_cost - mu_stay_cost)/sd_stay_cost
    }
    bayes_fit <- stan(file = "Hierachical_softmax_model.stan", data = test_dat, 
                      chain = 4, warmup = 1000, iter = 2000, cores = 2,
                      control = list(adapt_delta = 0.95))
    params <- extract(bayes_fit)
    for(l in 1:test_dat$L[1])
    {
      ## log_transform = FALSE bacause the estimated parama already in log base 10.
      temp_err <- Model_error(validation_temp_bayes[which(validation_temp_bayes[[group_name]] == as.character(l)),],
                           mean(params$safety_util[,l]), mean(params$noise_util[,l]), log_transform = FALSE, total = FALSE)
      bayes_error_evac <- bayes_error_evac + temp_err$error_evac
      bayes_error_stay <- bayes_error_stay + temp_err$error_stay
    }
    
    bayes_f_score <- F_score(total_evac_case - bayes_error_evac, bayes_error_evac,
                             total_stay_case - bayes_error_stay, bayes_error_stay)
    bayes_err_list <- data.frame("error_evac" = bayes_error_evac,"error_stay" = bayes_error_stay,
                                 "total_error" = bayes_error_evac + bayes_error_stay,
                                 "recall" = bayes_f_score$recall, "precision" = bayes_f_score$precision,
                                 "f_score" = bayes_f_score$f_score)
    bayes_err  <- rbind(bayes_err, bayes_err_list)
    
    #########################################################
    
    ## test other base line model and add the result to the dataframe  ##
    if(other_method)
    {
      for(f in 1:length(other_features_list))
      {
        ## tree section 
        temp_formula <- as.formula(paste("evac_decision_num ~ ",paste(other_features_list[[f]],collapse = "+"),sep=""))
        tree_class <- rpart(temp_formula, data = training_temp, method="class")
        tree_pred <- predict(tree_class, validation_temp, type="class")
        error_evac_tree <- sum(validation_temp$evac_decision_num == 1 & tree_pred == 0)
        error_stay_tree <- sum(validation_temp$evac_decision_num == 0 & tree_pred == 1)
        tree_f_score <- F_score(total_evac_case - error_evac_tree, error_evac_tree,
                                total_stay_case - error_stay_tree, error_stay_tree)
        tree_err_list <- data.frame("error_evac" = error_evac_tree,
                                    "error_stay" = error_stay_tree,
                                    "total_error" = (error_evac_tree + error_stay_tree),
                                    "recall"      = tree_f_score$recall,
                                    "precision"   = tree_f_score$precision,
                                    "f_score"     = tree_f_score$f_score)
        tree_err_df_list[[f]] <- rbind(tree_err_df_list[[f]],tree_err_list)
        ## logit section
        features_list <- c(other_features_list[[f]])
        # features_factor <- intersect(char_to_factor_training, features_list)
        # xlevs <- lapply(dat_to_use[,features_factor,drop=F], function(x) levels(as.factor(x)))
        x_train <- model.matrix(~ .-1, training_temp[, features_list])
        logit_net <- cv.glmnet(x_train, y = training_temp$evac_decision_num, family = "binomial")
        prediction_net <- predict(logit_net, 
                                  newx = model.matrix(~ .-1, validation_temp[,features_list]), #xlev = xlevs,
                                  type="class",s="lambda.min")
        error_evac_net <- sum(validation_temp$evac_decision_num == 1 & prediction_net==0)
        error_stay_net <- sum(validation_temp$evac_decision_num == 0 & prediction_net==1)
        net_f_score <- F_score(total_evac_case - error_evac_net, error_evac_net,
                               total_stay_case - error_stay_net, error_stay_net)
        net_err_list <- data.frame("error_evac" = error_evac_net,
                                   "error_stay" = error_stay_net,
                                   "total_error" = (error_evac_net + error_stay_net),
                                   "recall"      = net_f_score$recall,
                                   "precision"   = net_f_score$precision,
                                   "f_score"     = net_f_score$f_score)
        net_err_df_list[[f]] <- rbind(net_err_df_list[[f]],net_err_list)
      }
    }
  }
  ## At the end of main loop, we should have an error rate/matrix for each model
  
  if(other_method)
  {
    data_list <- list(grid_search_err, bayes_err,
                      tree_err_df_list[[1]],tree_err_df_list[[2]],tree_err_df_list[[3]],tree_err_df_list[[4]],
                      net_err_df_list[[1]],net_err_df_list[[2]],net_err_df_list[[3]],net_err_df_list[[4]]) 
    name_list <- list("grid_search_model","bayes_model",
                      "tree_obs","tree_latent","tree_obs+latent","tree_obs+latent+others",
                      "logit_obs","logit_latent","logit_obs+latent","logit_obs+latent+others")
  }else
  {
    data_list <- list(grid_search_err, bayes_err) 
    name_list <- list("grid_search_model","bayes_model")
  }
  ## create a result dataset
  for(i in 1:length(data_list))
  {
    d_err <- data_list[[i]]
    result_temp <- data.frame("seed" = map$seed,
                                    "out_features" = map$out_feature,
                                    "log_trans" = map$log_trans,
                                    "prob" = map$prob,
                                    "model_param" = map$model_param,
                                    "discount" = map$discount,
                                    "method"   = name_list[[i]],
                      ## The model
                      "error_evac"     = sum(d_err[["error_evac"]]), 
                      "error_stay"     = sum(d_err[["error_stay"]]), 
                      "mean_total_error" = mean(d_err[["total_error"]]/grid_search_err[["size"]]),
                      "sd_total_error" = sd(d_err[["total_error"]]/grid_search_err[["size"]]),
                      "mean_recall"    = mean(d_err[["recall"]]),
                      "sd_recall"      = sd(d_err[["recall"]]),
                      "mean_precision" = mean(d_err[["precision"]]),
                      "sd_precision"   = sd(d_err[["precision"]]),
                      "mean_f_score"   = mean(d_err[["f_score"]]),
                      "sd_f_score"     = sd(d_err[["f_score"]]))
    
    result_dat <- rbind(result_dat,result_temp) 
  }
}

result_dat
write.csv(result_dat,paste(Sys.Date(),"result_dat_post_michael.csv"))

#################################################################################################

################################ Looking at residual error ######################################
#################################################################################################

dat_to_test <- dat_to_use
dat_to_test$stay_eu <- -1*(dat_to_test$partial_stay_cost + dat_to_test$safety_prob_num*best_safety_util)
dat_to_test$evac_eu <- -1*(dat_to_test$partial_evac_cost + best_noise_util)
dat_to_test$predicted_decision <- if_else(dat_to_test$evac_eu > dat_to_test$stay_eu, 1, 0)
dat_to_test$diff_eu <- dat_to_test$evac_eu - dat_to_test$stay_eu
dat_to_test$diff_partial <- dat_to_test$partial_evac_cost - dat_to_test$partial_stay_cost
wrong_index <- which(dat_to_test$evac_decision_num != dat_to_test$predicted_decision)

View(dat_to_test[wrong_index,])

#################################################################################################

#########################  Parameter Estimation using whole dataset  ############################
#################################################################################################

dat_to_estimate <- dat_training
mapping <- 5
dat_to_estimate$safety_prob_num <- unlist(lapply(dat_to_estimate$safety_prob, 
                                            function(x) Gen_prob_from_map( prob_mappings[mapping,] ,x)))
normal_duration_map <- Gen_normal_duration_mapping(discount)
dat_to_estimate$normal_condition_return_num <- unlist(lapply(dat_to_estimate$normal_condition_return, 
                                                        function(x) normal_duration_map[[x]] ))
electricity_duration_map <- Gen_elec_duration_mapping(discount)
dat_to_estimate$electricity_lost_dur_num <- unlist(lapply(dat_to_estimate$electricity_lost_dur, 
                                                     function(x) electricity_duration_map[[x]] ))
dat_to_estimate$flood_depth_num <- unlist(lapply(dat_to_estimate$flood_depth, 
                                            function(x) Gen_flood_depth_mapping(x) ))
dat_to_estimate$partial_evac_cost <- log(dat_to_estimate$traveling_cost + epsilon , base = 10) +
  log(dat_to_estimate$safe_place_cost*dat_to_estimate$normal_condition_return_num + epsilon , base = 10)
dat_to_estimate$partial_stay_cost <- log( (dat_to_estimate$flood_cost/15)*dat_to_estimate$flood_depth_num*dat_to_estimate$normal_condition_return_num + epsilon , base=10) +
  log((dat_to_estimate$electricity_cost/5)*dat_to_estimate$electricity_lost_dur_num + epsilon , base = 10)
for(g in group)
{
  if(group_name == "none") { training_group <- dat_to_estimate }
  else { training_group <- dat_to_estimate[which(training_temp[[group_name]] == g),] }
  
  ## Grid Search for the best cost of being safety/not in danger in the training.
  best_safety_util <- 0
  best_noise_util <- 0
  best_error <- Inf ## max possible error
  
  ## some setup for optimization
  safety_seq <- seq(1500, 0, by = -5)
  noise_seq <- seq(0, 1500, by = 5)
  
  
  for( safety_util in safety_seq)
  {
    for( noise_util in noise_seq)
    {
      total_error <- Model_error(training_group, safety_util, noise_util, log_transform = log_transform)
      if(total_error < best_error)
      {
        best_safety_util <- safety_util
        best_noise_util  <- noise_util
        best_error       <- total_error
      }
    }
  }
  
  ## save the best_safety_util of each group
  if(g != "none") 
  { 
    group_safety_list[g] <- best_safety_util 
    group_noise_list[g]  <- best_noise_util
  }
} 
c(best_safety_util, best_noise_util, best_error)
#################################################################################################

############################  Parameters Estimation Using Stan  #################################
#################################################################################################

test_dat <- list(
  N = dim(dat_to_use)[1],
  evac_decision = dat_to_use$evac_decision_num,
  evac_decision_num = dat_to_use$evac_decision_num,
  partial_evac_cost = dat_to_use$partial_evac_cost,
  partial_stay_cost = dat_to_use$partial_stay_cost,
  safety_prob = dat_to_use$safety_prob_num,
  safety_prob_num = dat_to_use$safety_prob_num,
  traveling_cost = dat_to_use$traveling_cost,
  safe_palce_cost = dat_to_use$safe_place_cost,
  normal_condition_return = dat_to_use$normal_condition_return_num,
  flood_cost = dat_to_use$flood_cost,
  flood_depth = dat_to_use$flood_depth,
  electricity_cost = dat_to_use$electricity_cost,
  electricity_lost_dur = dat_to_use$electricity_lost_dur_num,
  group = as.numeric(dat_to_use$income_group),
  L = 3
)

fit1 <- stan(file = "test_hierachical_logistic.stan", data = test_dat, chain = 4, warmup = 1000, iter = 4000, cores = 2,
             control = list(adapt_delta = 0.95))
params <- extract(fit1)
test_dat <- data.frame(test_dat)
g <- 3
err <- Model_error(test_dat[which(test_dat$group == g),], 
                   mean(params$safety_util[,g]), mean(params$noise_util[,g]), log_transform = FALSE, total = FALSE)
err
(err$error_evac + err$error_stay)/dim(dat_to_use)[1]
F_score(sum(dat_to_use$evac_decision_num == 1) -  err$error_evac, err$error_evac, 
        sum(dat_to_use$evac_decision_num == 0) - err$error_stay, err$error_stay)

#################################################################################################

####################################    Across Dataset    #######################################
#################################################################################################
## Potential Source: Hypo* (TX,FL,MA) ,Pre Florence, Post Florence
## Potential Target: Post Florence, Post Michael
## (*Note: Hypo is a bit difference since we will not have all information features, importantly official notice.)
## Separate cases based on features that allow models to use.
## (Note that some features may not appear in both source and target)

## (Note: this is after exclusion.)
dat_source <- pre_florence   # post_florence 
dat_target <- post_florence  # post_michael

char_to_factor_source <- names(dat_source[,sapply(dat_source,is.character)])

only_obs <- TRUE

# dat_source$safe_place_cost <- ifelse(dat_source$place_to_evac == "Yes - The house of a friend or relative",0,dat_source$safe_place_cost)
## Set up mapping
seed <- c(17)
model_param <- c("none","income_group","evac_notice_group") #c("none","income_group","evac_notice_group")
out_feature <- c("none") #c("median","group","regression")  # c("none")# only post florence needs 
log_trans <- c("none","log-part") # "log-whole"
prob <- c("uniform","normal","mixed")
discount <- c(1,0.95)
mapping <- expand.grid(log_trans,model_param,out_feature,discount,prob,seed, stringsAsFactors = FALSE)
names(mapping) <- c("log_trans","model_param","out_feature","discount","prob","seed")

post_florence_as_source <- FALSE

result_across_dat <- data.frame()
for(m in 1:dim(mapping)[1])
{
  print(m)
  mapping_source <- mapping[m,]
  mapping_target <- mapping[m,]
  mapping_source$out_feature <- "none"    ## Only out feature for post florence
  if(post_florence_as_source) { mapping_source$out_feature <- "group" }
  
  dat_source_to_use <- Gen_new_data_from_map(dat_source,mapping_source)
  
  ## Fill in dat_target latent variable with source using observable features
  ## Latent variable: flood_cost, electricity_cost, traveling_cost, safe_place_cost
  ##                  safety_prob_num, flood_depth_num, electricity_lost_dur_nu, normal_condition_return_num
  if(only_obs)
  {
    features_temp <- c(observable_features)
    # features_factor <- intersect(char_to_factor_source,features)
    # xlevs <- lapply(dat_source[,features_factor,drop=F], function(x) levels(as.factor(x)))
    x_train <- model.matrix(~ .-1, dat_source_to_use[, features_temp])
    
    # flood_cost_glmnet       <- cv.glmnet(x_train, y = dat_source_to_use$flood_cost , alpha=1)
    # electricity_cost_glmnet <- cv.glmnet(x_train, y = dat_source_to_use$electricity_cost , alpha=1)
    # traveling_cost_glmnet   <- cv.glmnet(x_train, y = dat_source_to_use$traveling_cost, alpha=1)
    # safe_place_cost_glmnet  <- cv.glmnet(x_train, y = dat_source_to_use$safe_place_cost, alpha=1)
    # flood_depth_glmnet      <- cv.glmnet(x_train, y = dat_source_to_use$flood_depth_num, alpha=1)
    # electricity_lost_glmnet <- cv.glmnet(x_train, y = dat_source_to_use$electricity_lost_dur_num, alpha=1)
    # normal_condition_glmnet <- cv.glmnet(x_train, y = dat_source_to_use$normal_condition_return_num, alpha=1)
    # safety_prob_glmnet      <- cv.glmnet(x_train, y = dat_source_to_use$safety_prob_num)
    # partial_evac_cost_glmnet <- cv.glmnet(x_train, y = dat_source_to_use$partial_evac_cost)
    # partial_stay_cost_glmnet <- cv.glmnet(x_train, y = dat_source_to_use$partial_stay_cost)
  
    safety_prob_tree         <- rpart(as.formula(paste("safety_prob_num ~ ", paste(features_temp,collapse = "+"),sep="")),
                                      data= dat_source_to_use,method="anova")
    partial_evac_cost_tree   <- rpart(as.formula(paste("partial_evac_cost ~ ", paste(features_temp,collapse = "+"),sep="")),
                                      data=dat_source_to_use,method="anova")
    partial_stay_cost_tree   <- rpart(as.formula(paste("partial_stay_cost ~ ", paste(features_temp,collapse = "+"),sep="")),
                                      data=dat_source_to_use,method="anova")
    dat_target$safety_prob_num   <- predict(safety_prob_tree, dat_target)
    dat_target$partial_evac_cost <- predict(partial_evac_cost_tree, dat_target)
    dat_target$partial_stay_cost <- predict(partial_stay_cost_tree, dat_target)
    
    # dat_target$flood_cost       <- predict(flood_cost_glmnet,
    #                                        newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$electricity_cost <- predict(electricity_cost_glmnet,
    #                                        newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$traveling_cost   <- predict(traveling_cost_glmnet,
    #                                        newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$safe_place_cost  <- predict(safe_place_cost_glmnet,
    #                                        newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$flood_depth_num  <- predict(flood_depth_glmnet,
    #                                        newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$electricity_lost_dur_num <- predict(electricity_lost_glmnet,
    #                                            newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$normal_condition_return_num <- predict(normal_condition_glmnet,
    #                                               newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min")
    # dat_target$safety_prob_num  <- as.numeric(predict(safety_prob_glmnet,
    #                                        newx = model.matrix(~ .-1, dat_target[,features_temp]),s="lambda.min"))
    # dat_target$partial_evac_cost <- predict(partial_evac_cost_glmnet,
    #                                         newx = model.matrix(~ .-1, dat_target[,features_temp]), s= "lambda.min")
    # dat_target$partial_stay_cost <- predict(partial_stay_cost_glmnet,
    #                                         newx = model.matrix(~ .-1, dat_target[,features_temp]), s= "lambda.min")
    other_features_list <- other_features_list[1]
    dat_target_to_use <- dat_target
  }else
  {
    dat_target_to_use <- Gen_new_data_from_map(dat_target,mapping_target, skip_flag = TRUE)
  }
  log_transform <- (mapping_source$log_trans != "none")
  
  ## The model ##
  ## Only two parameters need to be estimated: safety_util, noise_util. (+ group)
  
  ##### Grid Search #####
  grid_error_evac <- 0
  grid_error_stay <- 0
  for(g in unique(dat_source_to_use[[mapping_source$model_param]]))
  {
    params <- Grid_search_model(dat_source_to_use[which(dat_source_to_use[[mapping_source$model_param]] == g),],
                                seq(1500,0,by=-5),seq(1500,0,by=-5),log_transform = log_transform)
  
    model_target_err <- Model_error(dat_target_to_use[which(dat_target_to_use[[mapping_source$model_param]] == g),],
                                  params$best_safety_util,
                                  params$best_noise_util,total=FALSE,log_transform=log_transform)
    grid_error_evac <- grid_error_evac + model_target_err$error_evac
    grid_error_stay <- grid_error_stay + model_target_err$error_stay
  }
  result_across_dat <- rbind(result_across_dat, cbind(mapping[m,],data.frame("method" = "grid_search_model", 
                                                           "error_evac" = grid_error_evac,
                                                           "error_stay" = grid_error_stay)))
  ##### Bayesian Inference #####
  dat_target_to_use_bayes <- dat_target_to_use
  temp_dat <- list(
    N = dim(dat_source_to_use)[1],
    evac_decision = dat_source_to_use$evac_decision_num,
    evac_decision_num = dat_source_to_use$evac_decision_num,
    partial_evac_cost = dat_source_to_use$partial_evac_cost,
    partial_stay_cost = dat_source_to_use$partial_stay_cost,
    safety_prob = dat_source_to_use$safety_prob_num,
    safety_prob_num = dat_source_to_use$safety_prob_num,
    group = as.numeric(dat_source_to_use[[mapping_source$model_param]]),
    L = length(unique(dat_source_to_use[[mapping_source$model_param]])),  ## L = level = number of group
    K = 7
  )
  # temp_dat$evac_decision <- ifelse(temp_dat$evac_decision == 0.05, 1, ifelse(temp_dat$evac_decision == 0.13, 2, ifelse(temp_dat$evac_decision==0.32,3,ifelse(temp_dat$evac_decision == 0.5,4,ifelse(temp_dat$evac_decision==0.68,5,ifelse(temp_dat$evac_decision==0.87,6,7))))))
  # temp_dat$evac_decision <- dat_source_to_use$evac_prob_num
  if(mapping_source$log_trans == "none")
  {
    ## Normalize so that the cost is not large so that stan can fit
    mu_evac_cost <- mean(temp_dat$partial_evac_cost) 
    sd_evac_cost <- sd(temp_dat$partial_evac_cost)
    mu_stay_cost <- mean(temp_dat$partial_stay_cost)
    sd_stay_cost <- sd(temp_dat$partial_stay_cost)
    temp_dat$partial_evac_cost <- (temp_dat$partial_evac_cost - mu_evac_cost)/(sd_evac_cost)
    temp_dat$partial_stay_cost <- (temp_dat$partial_stay_cost - mu_stay_cost)/(sd_stay_cost)
    dat_target_to_use_bayes$partial_evac_cost <- (dat_target_to_use_bayes$partial_evac_cost - mu_evac_cost)/(sd_evac_cost)
    dat_target_to_use_bayes$partial_stay_cost <- (dat_target_to_use_bayes$partial_stay_cost - mu_stay_cost)/(sd_stay_cost)
  }
  bayes_fit <- stan(file = "Hierachical_softmax_model.stan", data = temp_dat, 
                    chain = 4, warmup = 2000, iter = 4000, cores = 2,
                    control = list(adapt_delta = 0.95))
  bayes_params <- extract(bayes_fit)
  bayes_error_evac <- 0
  bayes_error_stay <- 0
  for(l in 1:temp_dat$L[1])
  {
    ## log_transform = FALSE bacause the estimated parama already in log base 10.
    temp_err <- Model_error(dat_target_to_use_bayes[which(dat_target_to_use_bayes[[mapping_source$model_param]] == as.character(l)),],
                            mean(bayes_params$safety_util[,l]),mean(bayes_params$noise_util[,l]) , log_transform = FALSE, total = FALSE)
    bayes_error_evac <- bayes_error_evac + temp_err$error_evac
    bayes_error_stay <- bayes_error_stay + temp_err$error_stay
  }
  result_across_dat <- rbind(result_across_dat, cbind(mapping[m,],data.frame("method" = "bayes_model", 
                                                           "error_evac" = bayes_error_evac,
                                                           "error_stay" = bayes_error_stay)))
  
  ##########################################################################################################################
  ## Other methods ##
  for(f in 1:length(other_features_list))
  {
    ## rpart
    temp_formula <- as.formula(paste("evac_decision_num ~ ",paste(other_features_list[[f]],collapse = "+"),sep=""))
    source_tree_class <- rpart(temp_formula, data = dat_source_to_use, method="class")
    tree_pred <- predict(source_tree_class, dat_target_to_use, type="class")
    error_evac_tree <- sum(dat_target_to_use$evac_decision_num == 1 & tree_pred == 0)
    error_stay_tree <- sum(dat_target_to_use$evac_decision_num == 0 & tree_pred == 1)
    result_across_dat <- rbind(result_across_dat, cbind(mapping[m,],data.frame("method" = paste("tree",other_features_namae_list[f],sep="_"), 
                                                             "error_evac" = error_evac_tree,
                                                             "error_stay" = error_stay_tree)))
    ## glmnet logit
    features <- c(other_features_list[[f]])
    features_factor <- intersect(char_to_factor_source, features)
    xlevs <- lapply(dat_source_to_use[,features_factor,drop=F], function(x) levels(as.factor(x)))
    x_train <- model.matrix(~ .-1, dat_source_to_use[, features])
    
    source_logit_net <- cv.glmnet(x_train, y = dat_source_to_use$evac_decision_num, family = "binomial")
    prediction_net <- predict(source_logit_net, 
                              newx = model.matrix(~ .-1, dat_target_to_use[,features],xlev = xlevs),
                              type="class",s="lambda.min")
    
    error_evac_net <- sum(dat_target_to_use$evac_decision_num == 1 & prediction_net==0)
    error_stay_net <- sum(dat_target_to_use$evac_decision_num == 0 & prediction_net==1)
    result_across_dat <- rbind(result_across_dat, cbind(mapping[m,],data.frame("method" = paste("glmnet",other_features_namae_list[f],sep="_"), 
                                                             "error_evac" = error_evac_net,
                                                             "error_stay" = error_stay_net)))
  }
}

result_across_dat$total_error <- result_across_dat$error_evac + result_across_dat$error_stay
result_across_dat
write.csv(result_across_dat,"result_dat_pre_michael_to_post_flo_obs_only_rpart.csv", row.names	= TRUE)
#################################################################################################