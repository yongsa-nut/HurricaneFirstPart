## This code is for pre processing results into a table format for the paper and supplementary. 
## Main reason for this is that the result from the main model doesn't in the good shape due to my poor coding
## In essence, the main model (grid and bayes) should have 6 different versions (log x group), but 
## the 6 versions in the main code are in the outer loop so the other methods get repeated many times.
## Running all the results again takes more time so this script is needed.

library(readr)

## Within dataset
result_pre_florence    <- read_csv("Results/2018-11-05 result_dat_pre_florence.csv")
result_post_florence_1 <- read_csv("Results/2018-11-04 result_dat_post_florence_1.csv")
result_post_florence_2 <- read_csv("Results/2018-11-05 result_dat_post_florence_2.csv")
result_post_michael    <- read_csv("Results/2018-11-05 result_dat_post_michael.csv")

result_post_florence <- rbind(result_post_florence_1, result_post_florence_2)

##Adjust total error to percentage
result_pre_florence$mean_total_error <- result_pre_florence$mean_total_error*100
result_pre_florence$sd_total_error   <- result_pre_florence$sd_total_error*100
result_post_florence$mean_total_error <- result_post_florence$mean_total_error*100
result_post_florence$sd_total_error   <- result_post_florence$sd_total_error*100
result_post_michael$mean_total_error  <- result_post_michael$mean_total_error*100
result_post_michael$sd_total_error    <- result_post_michael$sd_total_error*100


dim(result_pre_florence)
dim(result_post_florence)
dim(result_post_michael)

## Column should be each methods in the following order:
##         Grid Search    |         Bayes         |       Tree       |     Logistic     |
## None | Income | Prev   | None | Income | Prev  |                  |                  |  
## N | L| N | L | N | L  | N | L | N | L | N | L  | o | l | ol | all | o | l | ol | all |
unique(result_pre_florence$method) 

method_name <- function(value)
{
  if(value == "grid_search_model")       return("Grid")
  if(value == "bayes_model")             return("Bayes")
  if(value == "tree_obs")                return("Tree.obs")
  if(value == "tree_latent")             return("Tree.latent")
  if(value == "tree_obs+latent")         return("Tree.obs.latent")
  if(value == "tree_obs+latent+others")  return("Tree.all")
  if(value == "logit_obs")               return("Logit.obs")
  if(value == "logit_latent")            return("Logit.latent")
  if(value == "logit_obs+latent")        return("Logit.obs.latent")
  if(value == "logit_obs+latent+others") return("Logit.all")
  if(value == "glmnet_obs")               return("Logit.obs")
  if(value == "glmnet_latent")            return("Logit.latent")
  if(value == "glmnet_obs+latent")        return("Logit.obs.latent")
  if(value == "glmnet_obs+latent+others") return("Logit.all")
}
    
group_status_list <- list("none" = "None", "income_group" = "Income", "prev_exp_group" = "Prev", "evac_notice_group" = "Evac")
log_status_list <- list("none" = "No", "log-part" = "Log")
## Row should be each different mapping of the data + Filled latent features for post florence (group and regression)
##  Filled Post Florence Latent  |  Prob   | Discount  |

## Loop through the given result and add it to the given dataset

gen_result_within <- function(dat, measure, post_florence = TRUE, obs_only = FALSE)
{
  n_row <- 6
  if(post_florence) n_row <- 12
  
  if(obs_only)
  {
    dat_temp <- data.frame( "Grid None No" = c(1:n_row), "Grid None Log" = c(1:n_row), 
                            "Grid Income No" = c(1:n_row),  "Grid Income Log" = c(1:n_row),
                            "Grid Evac No" = c(1:n_row), "Grid Evac Log" = c(1:n_row),
                            "Bayes None No" = c(1:n_row), "Bayes None Log" = c(1:n_row), 
                            "Bayes Income No" = c(1:n_row),  "Bayes Income Log" = c(1:n_row),
                            "Bayes Evac No" = c(1:n_row), "Bayes Evac Log" = c(1:n_row), 
                            "Tree obs" = c(1:n_row),"Logit obs" = c(1:n_row))
  }else
  {
    dat_temp <- data.frame( "Grid None No" = c(1:n_row), "Grid None Log" = c(1:n_row), 
                          "Grid Income No" = c(1:n_row),  "Grid Income Log" = c(1:n_row),
                          "Grid Prev No" = c(1:n_row), "Grid Prev Log" = c(1:n_row),
                          "Bayes None No" = c(1:n_row), "Bayes None Log" = c(1:n_row), 
                          "Bayes Income No" = c(1:n_row),  "Bayes Income Log" = c(1:n_row),
                          "Bayes Prev No" = c(1:n_row), "Bayes Prev Log" = c(1:n_row), 
                          "Tree obs" = c(1:n_row), "Tree latent" = c(1:n_row), "Tree obs latent" = c(1:n_row), "Tree all" = c(1:n_row),
                          "Logit obs" = c(1:n_row), "Logit latent" = c(1:n_row), "Logit obs latent" = c(1:n_row), "Logit all" = c(1:n_row) )
  }
  if(post_florence)
  {
    row.names(dat_temp) <- c("group uniform 1","group uniform 0.95","group normal 1","group normal 0.95","group mixed 1", "group mixed 0.95",
                           "regression uniform 1","regression uniform 0.95","regression normal 1","regression normal 0.95","regression mixed 1", "regression mixed 0.95")
  }else
  {
    row.names(dat_temp) <- c("none uniform 1","none uniform 0.95","none normal 1","none normal 0.95","none mixed 1", "none mixed 0.95")
  }
  
  ## Loop through the whole dataset
  for(i in 1:dim(dat)[1])
  {
    temp <- dat[i,]
    ## First grab the method name
    method <- method_name(temp$method)
    if(method == "Grid" | method == "Bayes")
    {
      group_status <- group_status_list[[temp$model_param]]
      log_status <- log_status_list[[temp$log_trans]]
      method <- paste(method,group_status,log_status,sep=".")
    }
    ## Second grab the data transform
    if(temp$out_features != "median")
    {
      data_trans <- paste(temp$out_features,temp$prob,temp$discount,sep=" ")
      dat_temp[data_trans,method] <- temp[[measure]]
    }
  }

  return(dat_temp)
}

gen_result_file <- function(dat, dat_name, post_florence_flag, measurement_list, obs_flag = FALSE)
{
  for(m in measurement_list)
  {
    temp_m <- round(gen_result_within(dat,m, post_florence_flag, obs_flag),digits=2)
    
    write.csv(temp_m, file = paste("Results/",dat_name," ",m,".csv",sep=""))
  }
}

within_measurement_list <- c("mean_total_error","sd_total_error","mean_recall","sd_recall","mean_precision","sd_precision","mean_f_score","sd_f_score")

gen_result_file(result_post_florence,"post florence",TRUE,within_measurement_list)
gen_result_file(result_pre_florence,"pre florence",FALSE ,within_measurement_list)
gen_result_file(result_post_michael,"post michael",FALSE ,within_measurement_list)

###############################################################################################################################################################

result_pre_flo_to_post_flo_1 <- read_csv("Results/result_across_dat_pre_fl_to_post_flo_1.csv")
result_pre_flo_to_post_flo_2 <- read_csv("Results/result_across_dat_pre_fl_to_post_flo_2.csv")

result_pre_flo_to_post_michael <- read_csv("Results/result_across_dat_pre_fl_to_post_michael.csv")

result_post_michael_to_post_flo <- read_csv("Results/result_across_dat_post_michael_to_post_flo.csv")

result_post_flo_to_post_michael <- read_csv("Results/result_across_dat_post_flo_to_post_michael.csv")


result_pre_flo_to_post_flo <- rbind(result_pre_flo_to_post_flo_1,result_pre_flo_to_post_flo_2)
## organize results
result_pre_flo_to_post_flo$out_features <- result_pre_flo_to_post_flo$out_feature
result_pre_flo_to_post_michael$out_features <- result_pre_flo_to_post_michael$out_feature
result_post_michael_to_post_flo$out_features <- result_post_michael_to_post_flo$out_feature
result_post_flo_to_post_michael$out_features <- result_post_flo_to_post_michael$out_feature

## Calculate error
post_florence_size <- 684
post_michael_size  <- 542 

post_florence_evac <- 104
post_florence_stay <- 580

post_michael_evac  <- 104
post_michael_stay  <- 438

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

## target = post florence
result_pre_flo_to_post_flo$percentage_error      <- 100*(result_pre_flo_to_post_flo$total_error/post_florence_size)
result_post_michael_to_post_flo$percentage_error <- 100*(result_post_michael_to_post_flo$total_error/post_florence_size)

F_pre_flo_to_post_flo <- mapply(F_score, post_florence_evac - result_pre_flo_to_post_flo$error_evac, result_pre_flo_to_post_flo$error_evac,
                                 post_florence_stay - result_pre_flo_to_post_flo$error_stay, result_pre_flo_to_post_flo$error_stay)
result_pre_flo_to_post_flo$recall    <- unlist(F_pre_flo_to_post_flo[1,])
result_pre_flo_to_post_flo$precision <- unlist(F_pre_flo_to_post_flo[2,])
result_pre_flo_to_post_flo$f_score   <- unlist(F_pre_flo_to_post_flo[3,])

F_post_mi_to_post_flo <- mapply(F_score,post_florence_evac - result_post_michael_to_post_flo$error_evac,result_post_michael_to_post_flo$error_evac,
                                 post_florence_stay - result_post_michael_to_post_flo$error_stay,result_post_michael_to_post_flo$error_stay)
result_post_michael_to_post_flo$recall    <- unlist(F_post_mi_to_post_flo[1,])
result_post_michael_to_post_flo$precision <- unlist(F_post_mi_to_post_flo[2,])
result_post_michael_to_post_flo$f_score   <- unlist(F_post_mi_to_post_flo[3,])

## target = post michael
result_pre_flo_to_post_michael$percentage_error  <- 100*(result_pre_flo_to_post_michael$total_error/post_michael_size)
result_post_flo_to_post_michael$percentage_error <- 100*(result_post_flo_to_post_michael$total_error/post_michael_size) 

F_pre_flo_to_post_mi <- mapply(F_score, post_michael_evac - result_pre_flo_to_post_michael$error_evac, result_pre_flo_to_post_michael$error_evac,
                               post_michael_stay - result_pre_flo_to_post_michael$error_stay, result_pre_flo_to_post_michael$error_stay)

result_pre_flo_to_post_michael$recall    <- unlist(F_pre_flo_to_post_mi[1,])
result_pre_flo_to_post_michael$precision <- unlist(F_pre_flo_to_post_mi[2,])
result_pre_flo_to_post_michael$f_score   <- unlist(F_pre_flo_to_post_mi[3,])

F_post_flo_to_post_mi <- mapply(F_score, post_michael_evac - result_post_flo_to_post_michael$error_evac, result_post_flo_to_post_michael$error_evac,
                                result_post_flo_to_post_michael$error_stay, result_post_flo_to_post_michael$error_stay)

result_post_flo_to_post_michael$recall    <- unlist(F_post_flo_to_post_mi[1,])
result_post_flo_to_post_michael$precision <- unlist(F_post_flo_to_post_mi[2,])
result_post_flo_to_post_michael$f_score   <- unlist(F_post_flo_to_post_mi[3,])


gen_result_within(result_pre_flo_to_post_flo,"percentage_error",TRUE)

across_measurement_list <- c("percentage_error","recall","precision","f_score" )

gen_result_file(result_pre_flo_to_post_flo,"pre florence to post florence", TRUE ,across_measurement_list)
gen_result_file(result_post_michael_to_post_flo,"post michael to post florence", TRUE ,across_measurement_list)
gen_result_file(result_pre_flo_to_post_michael,"pre florence to post michael", FALSE ,across_measurement_list)
gen_result_file(result_post_flo_to_post_michael,"post florence to post michael", TRUE ,across_measurement_list)

###############################################################################################################################################################

result_pre_flo_to_post_flo_obs <- read_csv("Results/result_dat_pre_flo_to_post_flo_obs_only_rpart.csv")

result_pre_flo_to_post_michael_obs <- read_csv("Results/result_dat_pre_flo_to_post_michael_obs_only_rpart.csv")

result_post_michael_to_post_flo_obs <- read_csv("Results/result_dat_post_michael_to_post_flo_obs_only_rpart.csv")

result_post_flo_to_post_michael_obs <- read_csv("Results/result_dat_post_flo_to_post_michael_obs_only_rpart.csv")


## organize results
result_pre_flo_to_post_flo_obs$out_features <- result_pre_flo_to_post_flo_obs$out_feature
result_pre_flo_to_post_michael_obs$out_features <- result_pre_flo_to_post_michael_obs$out_feature
result_post_michael_to_post_flo_obs$out_features <- result_post_michael_to_post_flo_obs$out_feature
result_post_flo_to_post_michael_obs$out_features <- result_post_flo_to_post_michael_obs$out_feature

## target = post florence
result_pre_flo_to_post_flo_obs$percentage_error      <- 100*(result_pre_flo_to_post_flo_obs$total_error/post_florence_size)
result_post_michael_to_post_flo_obs$percentage_error <- 100*(result_post_michael_to_post_flo_obs$total_error/post_florence_size)

F_pre_flo_to_post_flo_obs <- mapply(F_score, post_florence_evac - result_pre_flo_to_post_flo_obs$error_evac, result_pre_flo_to_post_flo_obs$error_evac,
                                post_florence_stay - result_pre_flo_to_post_flo_obs$error_stay, result_pre_flo_to_post_flo_obs$error_stay)
result_pre_flo_to_post_flo_obs$recall    <- unlist(F_pre_flo_to_post_flo_obs[1,])
result_pre_flo_to_post_flo_obs$precision <- unlist(F_pre_flo_to_post_flo_obs[2,])
result_pre_flo_to_post_flo_obs$f_score   <- unlist(F_pre_flo_to_post_flo_obs[3,])

F_post_mi_to_post_flo_obs <- mapply(F_score,post_florence_evac - result_post_michael_to_post_flo_obs$error_evac,result_post_michael_to_post_flo_obs$error_evac,
                                post_florence_stay - result_post_michael_to_post_flo_obs$error_stay,result_post_michael_to_post_flo_obs$error_stay)
result_post_michael_to_post_flo_obs$recall    <- unlist(F_post_mi_to_post_flo_obs[1,])
result_post_michael_to_post_flo_obs$precision <- unlist(F_post_mi_to_post_flo_obs[2,])
result_post_michael_to_post_flo_obs$f_score   <- unlist(F_post_mi_to_post_flo_obs[3,])

## target = post michael
result_pre_flo_to_post_michael_obs$percentage_error  <- 100*(result_pre_flo_to_post_michael_obs$total_error/post_michael_size)
result_post_flo_to_post_michael_obs$percentage_error <- 100*(result_post_flo_to_post_michael_obs$total_error/post_michael_size) 

F_pre_flo_to_post_mi_obs <- mapply(F_score, post_michael_evac - result_pre_flo_to_post_michael_obs$error_evac, result_pre_flo_to_post_michael_obs$error_evac,
                               post_michael_stay - result_pre_flo_to_post_michael_obs$error_stay, result_pre_flo_to_post_michael_obs$error_stay)

result_pre_flo_to_post_michael_obs$recall    <- unlist(F_pre_flo_to_post_mi_obs[1,])
result_pre_flo_to_post_michael_obs$precision <- unlist(F_pre_flo_to_post_mi_obs[2,])
result_pre_flo_to_post_michael_obs$f_score   <- unlist(F_pre_flo_to_post_mi_obs[3,])

F_post_flo_to_post_mi_obs <- mapply(F_score, post_michael_evac - result_post_flo_to_post_michael_obs$error_evac, result_post_flo_to_post_michael_obs$error_evac,
                                result_post_flo_to_post_michael_obs$error_stay, result_post_flo_to_post_michael_obs$error_stay)

result_post_flo_to_post_michael_obs$recall    <- unlist(F_post_flo_to_post_mi_obs[1,])
result_post_flo_to_post_michael_obs$precision <- unlist(F_post_flo_to_post_mi_obs[2,])
result_post_flo_to_post_michael_obs$f_score   <- unlist(F_post_flo_to_post_mi_obs[3,])


gen_result_within(result_pre_flo_to_post_flo_obs,"percentage_error",FALSE,obs_only = TRUE)

across_measurement_list <- c("percentage_error","recall","precision","f_score" )

gen_result_file(result_pre_flo_to_post_flo_obs,"pre florence to post florence obs only", FALSE ,across_measurement_list, obs_flag = TRUE)
gen_result_file(result_post_michael_to_post_flo_obs,"post michael to post florence obs only", FALSE ,across_measurement_list, obs_flag = TRUE)
gen_result_file(result_pre_flo_to_post_michael_obs,"pre florence to post michael obs only", FALSE ,across_measurement_list, obs_flag = TRUE)
gen_result_file(result_post_flo_to_post_michael_obs,"post florence to post michael obs only", FALSE ,across_measurement_list, obs_flag = TRUE)



