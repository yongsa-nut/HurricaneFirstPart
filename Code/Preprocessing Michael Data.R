## Preprocessing Michael data both pre and post
## Mainly chaning parameter name to be consistant with other dataset

## Load all the files
library(readr)
library(zipcode)

#### Adding child_0_5, child_6_12, child_13_18, elder_65_up
count_age_range <- function(lower,upper,subj)
{
  fam_size <- as.numeric(subj[which(names(subj) == "Q16")]) #Q16 == family size
  remain_member <- fam_size - 1 ## exclude the subject 
  ## living alone
  if(remain_member == 0) { return(0) }
  ## max allow number of family is 10
  if(remain_member > 9) { remain_member <- 9 }
  # q17_index = starting index of q17 + jumpindex [2*(n*n-1)/2]
  q17_index <- which(names(subj) == "Q17_1#1_1_1") + (remain_member * (remain_member-1)) 
  # test in rare case of missing data even though it's force response question
  if(is.na(subj[q17_index])) return(NA)
  # grab all the index
  age_list <- as.numeric( subj[q17_index : (q17_index+remain_member-1)] )
  #adjust age that is in calendar year
  cur_year <- 2018
  for(j in 1:length(age_list))
  {
    if(age_list[j] > 1900)  age_list[j] <- cur_year - age_list[j]
  }
  count <- sum( age_list >= lower & age_list <=upper)
  return(count)
}
adding_child_elder <- function(dat)
{
  temp0_5 <- c()
  temp6_12 <- c()
  temp13_18 <- c()
  temp65_up <- c()
  for(i in 1:dim(dat)[1])
  {
    temp0_5   <- c(temp0_5,count_age_range(0,5,dat[i,]))
    temp6_12  <- c(temp6_12,count_age_range(6,12,dat[i,]))
    temp13_18 <- c(temp13_18,count_age_range(13,18,dat[i,]))
    temp65_up    <- c(temp65_up,count_age_range(65,120,dat[i,]))
  }
  dat$child_0_5 <- temp0_5
  dat$child_6_12 <- temp6_12
  dat$child_13_18 <- temp13_18
  dat$elder_65_up <- temp65_up
  return(dat)
}


post_michael <- read_csv("Data/Post Hurricane Survey - Hurricane Michael.csv")

dat <- post_michael

dat <- dat[3:dim(post_michael)[1],]
dat <- dat[which(dat$Finished == "True"),]
dim(dat)

# Construct new dataframe

dat <- adding_child_elder(dat)

dat_michael <- data.frame( 
  ## response characteristic
  "duration"  = as.numeric(dat$`Duration (in seconds)`),
  "EndDate"   = dat$EndDate,
  "MTurkCode" = as.numeric(dat$MTurkCode),
  "zipcode" = dat$Q9,
  
  ## Demoghahic 
  "age"               = as.numeric(dat$Q1),
  "gender"            = dat$Q2,
  "employment"        = dat$Q3, ## new features for post michael
  "edu"               = dat$Q4,
  "income"            = dat$Q5,
  "house_structure"   = dat$Q7,
  "is_owned"          = dat$Q8,
  "city"              = dat$QCity_Q9,
  "state"             = dat$QState_Q9,
  "years_in_resident" = dat$Q10,
  "distance_to_coast" = dat$Q11,
  "num_vehicle"       = dat$Q12,
  "has_pet"           = dat$Q13,
  "type_of_pet"       = dat$Q14,
  "importance_of_pet" = dat$Q15,
  "family_size"       = as.numeric(dat$Q16),
  "child_0_5"         = as.factor(dat$child_0_5),
  "child_6_12"        = as.factor(dat$child_6_12),
  "child_13_18"       = as.factor(dat$child_13_18),
  "elder_65_up"       = as.factor(dat$elder_65_up),
  "special_need_med"  = dat$Q18,
  "special_need_moving" = dat$Q19,
  
  ## social influence
  "contribute_final_decision" = dat$Q20,
  "household_discussion"      = dat$Q21,
  "household_suggestion"       = dat$Q22,
  "outside_family_discussion" = dat$Q23,
  "outside_family_suggestion"  = dat$Q24,
  "outside_friend_discussion" = dat$Q25,
  "outside_friend_suggestion"  = dat$Q26,
  "neighbors_observation"     = dat$Q27,
  "neighbors_doing"       = dat$Q28,
  
  ## prev. experience and preparation
  "prev_exp"             = dat$Q31,
  "prev_exp_last_time"   = dat$Q32,
  "prev_exp_highest_cat" = dat$Q33,
  "prev_exp_notice"      = dat$Q34,
  "prev_exp_decision"    = dat$Q35,
  "prev_exp_same_choice" = dat$Q36,
  
  ## probability estimation 
  "safety_prob"   = dat$Q43,
  
  ## numerical estimation
  "flood_depth"             = dat$Q51,
  "electricity_lost_dur"    = dat$Q52,   ## Have to more fine gain
  "normal_condition_return" = dat$Q53,
  "flood_cost"              = as.numeric(dat$Q54),
  "electricity_cost"        = as.numeric(dat$Q55),
  "hurricane_category"      = dat$Q50,
  
  ## info section
  "received_TV_Radio"    = dat$Q77,
  "TVR_dmg_done"         = dat$Q78_1,
  "TVR_forecasted_dmg"   = dat$Q78_2,
  "TVR_causualities"     = dat$Q78_3,
  "TVR_traffic_jams"     = dat$Q78_4,
  "TVR_preparing"        = dat$Q78_5,
  "TVR_stay"             = dat$Q78_6,
  "TVR_evac"             = dat$Q78_7,
  
  "received_social_med"  = dat$Q79,
  "SM_dmg_done"          = dat$Q80_1,
  "SM_forecasted_dmg"    = dat$Q80_2,
  "SM_causualities"      = dat$Q80_3,
  "SM_traffic_jams"      = dat$Q80_4,
  "SM_preparing"         = dat$Q80_5,
  "SM_stay"              = dat$Q80_6,
  "SM_evac"              = dat$Q80_7,
  
  ## post section
  
  "emergency_request_before" = dat$Q105_1,
  "emergency_request_during" = dat$Q105_2,
  "emergency_request_after"  = dat$Q105_3,
  "first_hear_florence"      = dat$Q81,
  "official_notice"          = dat$Q83,
  "notice_type"              = as.character(dat$Q84),
  "notice_when"              = dat$Q85,
  "evac_decision"            = dat$Q86,
  "same_choice"              = dat$Q106,
  
  ## Post - evac
  "date_evac"   = dat$Q87,
  "time_evac"   = dat$Q88,
  "how_evac"    = dat$Q89,
  "where_evac"  = dat$Q90,
  "how_far_evac" = dat$Q91,
  "traveling_cost_evac" = dat$Q92,
  "place_cost_evac" = dat$Q93,
  "have_returned_home" = dat$Q94,
  "when_returned_home" = dat$Q95,
  
  ## Post - stay
  "prepare_stay"        = dat$Q100,
  "when_prepare_stay"   = dat$Q101,
  "amount_prepare_stay" = dat$Q102,
  "traveling_est_stay"  = dat$Q103,
  "place_cost_est_stay" = dat$Q104
)

dat_michael$mandatory_evac_notice <- ifelse(grepl("Mandatory evacuation",dat_michael$notice_type,fixed=TRUE),1,0)
dat_michael$voluntary_evac_notice <- ifelse(grepl("Voluntary (Suggested) evacuation",dat_michael$notice_type,fixed=TRUE),1,0)
dat_michael$mandatory_stay_notice <- ifelse(grepl("Mandatory stay",dat_michael$notice_type,fixed=TRUE),1,0)
dat_michael$voluntary_stay_notice <- ifelse(grepl("Voluntary (Suggested) stay",dat_michael$notice_type,fixed=TRUE),1,0)

## Cleaning end date: date format YYYY:DD:MM HH:mm:ss
dat_michael$EndDateTime <- substring(dat_michael$EndDate,12,20)
dat_michael$EndDate <- substring(dat_michael$EndDate,1,10)

## Adding longitude variables
## Have to exclude other states here so that all data points have proper longitude and latitude.
data("zipcode")
dat_michael <- dat_michael[which(dat_michael$state == "GA" | dat_michael$state == "FL"),]
dat_michael$longitude <- unlist(sapply(dat_michael$zipcode, function(x) zipcode$longitude[zipcode$zip == x]))
dat_michael$latitude <- sapply(dat_michael$zipcode, function(x) zipcode$latitude[zipcode$zip == x])

write.csv(dat_michael,"pre-processed post michael.csv",row.names=FALSE)


