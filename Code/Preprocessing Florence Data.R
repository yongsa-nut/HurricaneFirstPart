## Preprocessing Florence data both pre and post
## Steps:
##   1) Merge all the separate post surveys into one and adjust the wording to be the same (office notice Q)
##      1.1) Transform all the Q## into meaningful named features
##   2) Merge all the pre and post subjects into one pre and one post with MTurkID, Date, Code
##   3) Mark subjects that are in both pre and post version in the post version
##   4) Generate a new dataset consists of subjects that are in both versions
##      4.1) Format: feature_pre and feature_post 
## List of mistakes: 
##   1) Typo in remain/stay (inconsistent wording of choices) [Need to adjust]
##   2) Hurricane Lane instead of Hurricane Florence (House structure and zipcode Question) [Don't need to adjust] 

## Load all the files
library(readr)
pre_florence <- read_csv("Hurricane Project 2018/Data/Pre-Hurricane Survey - Hurricane Florence.csv")
post_florence1 <- read_csv("Hurricane Project 2018/Data/Post-Hurricane Survey - Hurricane Florence.csv")
post_florence2 <- read_csv("Hurricane Project 2018/Data/Post-Hurricane Survey - Hurricane Florence - Fixed Minor Typo.csv")
post_florence3 <- read_csv("Hurricane Project 2018/Data/Post-Hurricane Survey - Hurricane Florence - Fixed Minor Typo 2.csv")

pre_florence <- pre_florence[3:dim(pre_florence)[1],]
post_florence1 <- post_florence1[3:dim(post_florence1)[1],]
post_florence2 <- post_florence2[3:dim(post_florence2)[1],]
post_florence3 <- post_florence3[3:dim(post_florence3)[1],]

## Total size of each dataset. The number includes in-progress questionnaire too
dim(pre_florence)[1]
dim(post_florence1)[1]
dim(post_florence2)[1]
dim(post_florence3)[1]

## include only finished questionnaire
is_include <- function(dat)
{
  result <- ((dat$Finished == "True"))
  print(sum(dat$Finished!="True"))
  return(result)
}

pre_florence <- pre_florence[which(is_include(pre_florence)),]
post_florence1 <- post_florence1[which(is_include(post_florence1)),]
post_florence2 <- post_florence2[which(is_include(post_florence2)),]
post_florence3 <- post_florence3[which(is_include(post_florence3)),]

dim(pre_florence)[1]
dim(post_florence1)[1]
dim(post_florence2)[1]
dim(post_florence3)[1]

## Construct new dataframe for Pre and Post Hurricane

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

pre_florence <- adding_child_elder(pre_florence)
post_florence1 <- adding_child_elder(post_florence1)
post_florence2 <- adding_child_elder(post_florence2)
post_florence3 <- adding_child_elder(post_florence3)

dat_pre <- data.frame(
  ## response characteristic
  "duration"  = as.numeric(pre_florence$`Duration (in seconds)`),
  "EndDate"   = pre_florence$EndDate,
  "MTurkCode" = as.numeric(pre_florence$MTurkCode),
  "zipcode" = pre_florence$Q9,
  
  ## Demoghahic 
  "age"               = as.numeric(pre_florence$Q1),
  "gender"            = pre_florence$Q2,
  "edu"               = pre_florence$Q4,
  "income"            = pre_florence$Q5,
  "house_structure"   = pre_florence$Q7,
  "is_owned"          = pre_florence$Q8,
  "city"              = pre_florence$QCity_Q9,
  "state"             = pre_florence$QState_Q9,
  "years_in_resident" = pre_florence$Q10,
  "distance_to_coast" = pre_florence$Q11,
  "num_vehicle"       = pre_florence$Q12,
  "has_pet"           = pre_florence$Q13,
  "type_of_pet"       = pre_florence$Q14,
  "importance_of_pet" = pre_florence$Q15,
  "family_size"       = as.numeric(pre_florence$Q16),
  "child_0_5"         = as.factor(pre_florence$child_0_5),
  "child_6_12"        = as.factor(pre_florence$child_6_12),
  "child_13_18"       = as.factor(pre_florence$child_13_18),
  "elder_65_up"       = as.factor(pre_florence$elder_65_up),
  "special_need_med"  = pre_florence$Q18,
  "special_need_moving" = pre_florence$Q19,
  
  ## social influence
  "contribute_final_decision" = pre_florence$Q20,
  "household_discussion"      = pre_florence$Q21,
  "household_influence"       = pre_florence$Q22,
  "outside_family_discussion" = pre_florence$Q23,
  "outside_family_influence"  = pre_florence$Q24,
  "outside_friend_discussion" = pre_florence$Q25,
  "outside_friend_influence"  = pre_florence$Q26,
  "neighbors_observation"     = pre_florence$Q27,
  "neighbors_influence"       = pre_florence$Q28,
  
  ## prev. experience and preparation
  "place_to_evac"        = pre_florence$Q29,
  "how_to_evac"          = pre_florence$Q56,
  "info_source"          = pre_florence$Q30,
  "prev_exp"             = pre_florence$Q31,
  "prev_exp_last_time"   = pre_florence$Q32,
  "prev_exp_highest_cat" = pre_florence$Q33,
  "prev_exp_notice"      = pre_florence$Q34,
  "prev_exp_decision"    = pre_florence$Q35,
  "prev_exp_same_choice" = pre_florence$Q36,
  
  ## utility estimation
  "flood_cost"           = as.numeric(pre_florence$Q37),
  "electricity_cost"     = as.numeric(pre_florence$Q38),
  "shelter_cost"         = as.numeric(pre_florence$Q39),
  "traveling_cost"       = as.numeric(pre_florence$Q40),
  "safe_place_cost"      = as.numeric(pre_florence$Q41),
  
  ## probability estimation 
  "evac_prob"     = pre_florence$Q42,
  "safety_prob"   = pre_florence$Q43,
  "supplier_prob" = pre_florence$Q44,
  "job_lost_prob" = pre_florence$Q45,
  
  ## numerical estimation
  "flood_depth"             = pre_florence$Q51,
  "electricity_lost_dur"    = pre_florence$Q52,
  "normal_condition_return" = pre_florence$Q53,
  "damage_est"              = as.numeric(pre_florence$Q55),
  "hurricane_category" = pre_florence$Q50,
  
  ## info section
  "received_TV_Radio"    = pre_florence$Q78,
  "TVR_dmg_done"         = pre_florence$Q79_1,
  "TVR_forecasted_dmg"   = pre_florence$Q79_2,
  "TVR_causualities"     = pre_florence$Q79_3,
  "TVR_traffic_jams"     = pre_florence$Q79_4,
  "TVR_preparing"        = pre_florence$Q79_5,
  "TVR_stay"             = pre_florence$Q79_6,
  "TVR_evac"             = pre_florence$Q79_7,
  
  "received_social_med"  = pre_florence$Q81,
  "SM_dmg_done"          = pre_florence$Q85_1,
  "SM_forecasted_dmg"    = pre_florence$Q85_2,
  "SM_causualities"      = pre_florence$Q85_3,
  "SM_traffic_jams"      = pre_florence$Q85_4,
  "SM_preparing"         = pre_florence$Q85_5,
  "SM_stay"              = pre_florence$Q85_6,
  "SM_evac"              = pre_florence$Q85_7,
  
  ## pre section
  "heard_florence"       = pre_florence$Q73,
  "when_first_hear"      = pre_florence$Q74,
  "how_first_hear"       = pre_florence$Q75,
  "official_notice"      = pre_florence$Q76,
  "notice_type"          = pre_florence$Q77
)

gen_post_dat <- function(dat)
{
  dat_temp <- data.frame( 
    ## response characteristic
    "duration"  = as.numeric(dat$`Duration (in seconds)`),
    "EndDate"   = dat$EndDate,
    "MTurkCode" = as.numeric(dat$MTurkCode),
    "zipcode" = dat$Q9,
    
    ## Demoghahic 
    "age"               = as.numeric(dat$Q1),
    "gender"            = dat$Q2,
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
    "household_agreement"       = dat$Q22,
    "outside_family_discussion" = dat$Q23,
    "outside_family_agreement"  = dat$Q24,
    "outside_friend_discussion" = dat$Q25,
    "outside_friend_agreement"  = dat$Q26,
    "neighbors_observation"     = dat$Q27,
    "neighbors_agreement"       = dat$Q28,
    
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
    "electricity_lost_dur"    = dat$Q52,
    "normal_condition_return" = dat$Q53,
    "damage_est"              = as.numeric(dat$Q55),
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
    
    ## Post - stay
    "prepare_stay"        = dat$Q100,
    "when_prepare_stay"   = dat$Q101,
    "amount_prepare_stay" = dat$Q102,
    "traveling_est_stay"  = dat$Q103,
    "place_cost_est_stay" = dat$Q104
  )
  return(dat_temp)
}

dat_post1 <- gen_post_dat(post_florence1)
dat_post2 <- gen_post_dat(post_florence2)
dat_post3 <- gen_post_dat(post_florence3)

#### fixing post1 wording on notice type: replace remain to stay
dat_post1$notice_type <- gsub("remain","stay",dat_post1$notice_type)

dat_post <- rbind(dat_post1,dat_post2,dat_post3)

#### Separate 4 notice types into 4 features
#### Types: Mandatory evacutation notice, Voluntary (Suggested) evacuation notice, 
####        Voluntary (Suggested) stay notice, Mandatory stay notice
dat_pre$mandatory_evac_notice <- ifelse(grepl("Mandatory evacuation",dat_pre$notice_type,fixed=TRUE),1,0)
dat_pre$voluntary_evac_notice <- ifelse(grepl("Voluntary (Suggested) evacuation",dat_pre$notice_type,fixed=TRUE),1,0)
dat_pre$mandatory_stay_notice <- ifelse(grepl("Mandatory stay",dat_pre$notice_type,fixed=TRUE),1,0)
dat_pre$voluntary_stay_notice <- ifelse(grepl("Voluntary (Suggested) stay",dat_pre$notice_type,fixed=TRUE),1,0)

dat_post$mandatory_evac_notice <- ifelse(grepl("Mandatory evacuation",dat_post$notice_type,fixed=TRUE),1,0)
dat_post$voluntary_evac_notice <- ifelse(grepl("Voluntary (Suggested) evacuation",dat_post$notice_type,fixed=TRUE),1,0)
dat_post$mandatory_stay_notice <- ifelse(grepl("Mandatory stay",dat_post$notice_type,fixed=TRUE),1,0)
dat_post$voluntary_stay_notice <- ifelse(grepl("Voluntary (Suggested) stay",dat_post$notice_type,fixed=TRUE),1,0)

###### compare with table(dat_post$notice_type) (pass)

#### Cleaning end date: date format YYYY:DD:MM HH:mm:ss
dat_post$EndDateTime <- substring(dat_post$EndDate,12,20)
dat_post$EndDate <- substring(dat_post$EndDate,1,10)

## Construct new dataframe for MTurkID
pre_florence_worker_1 <- read_csv("Hurricane Project 2018/MTurk ID Files/pre florence worker 1.csv")
pre_florence_worker_2 <- read_csv("Hurricane Project 2018/MTurk ID Files/pre florence worker 2.csv")
post_florence_worker_1 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 1.csv")
post_florence_worker_2 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 2.csv")
post_florence_worker_3 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 3.csv")
post_florence_worker_4 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 4.csv")
post_florence_worker_5 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 5.csv")
post_florence_worker_6 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 6.csv")
post_florence_worker_7 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 7.csv")
post_florence_worker_8 <- read_csv("Hurricane Project 2018/MTurk ID Files/post florence worker 8.csv")

pre_florence_worker <- rbind(pre_florence_worker_1,pre_florence_worker_2)
post_florence_worker <- rbind(post_florence_worker_1,post_florence_worker_2,post_florence_worker_3,post_florence_worker_4,
                              post_florence_worker_5,post_florence_worker_6,post_florence_worker_7,post_florence_worker_8)
dat_pre_worker <- data.frame(
  "WorkerID" = pre_florence_worker$WorkerId,
  "MTurkCode" = as.numeric(pre_florence_worker$Answer.surveycode)
  ,stringsAsFactors=FALSE
)
dat_post_worker <- data.frame(
  "WorkerID" = post_florence_worker$WorkerId,
  "MTurkCode" = as.numeric(post_florence_worker$Answer.surveycode)
  ,stringsAsFactors=FALSE
)

#### (check no repeat code) [happen with p = 1/1000000]
length(unique(dat_pre_worker$MTurkCode)) == dim(dat_pre_worker)[1]
length(unique(dat_post_worker$MTurkCode)) == dim(dat_post_worker)[1]

## Mark subjects that are in pre & post in post version
#### How: find match WorkerID and lik WorkerID to response using MTurkCode
both_pre_code <- c()
both_post_code <- c()

dat_pre_size <- c(1:dim(dat_pre_worker)[1])
dat_post_size <- c(1:dim(dat_post_worker)[1])

for(d_pre in dat_pre_size)
{
  for(d_post in dat_post_size)
  {
    if(dat_pre_worker$WorkerID[d_pre] == dat_post_worker$WorkerID[d_post])
    {
      both_pre_code <- c(both_pre_code, dat_pre_worker$MTurkCode[d_pre])
      both_post_code <- c(both_post_code,dat_post_worker$MTurkCode[d_post] )
      break
    }
  }
}

length(both_pre_code) 
length(both_post_code)

## Generate a new dataframe of only subjects in both 

dat_pre_size <- dim(dat_pre)[1]
dat_post_size <- dim(dat_post)[1]

dat_both_pre_post <- data.frame()
for(i in 1:length(both_pre_code))
{
  pre_index <- -1
  post_index <- -1
  ## search pre code
  for(j in 1:dat_pre_size)
  {
    if(both_pre_code[i] == dat_pre$MTurkCode[j])
    {
      pre_index <- j
      break
    }
  }
  ## search post code
  for(k in 1:dat_post_size)
  {
    if(both_post_code[i] == dat_post$MTurkCode[k])
    {
      post_index <- k
      break
    }
  }
  ## merge the two and add to the dataset
  if(pre_index != -1 & post_index != -1)
  {
    pre_subject <- dat_pre[pre_index,]
    post_subject <- dat_post[post_index,]
    names(pre_subject) <- paste("pre",names(pre_subject),sep="_")
    names(post_subject) <- paste("post",names(post_subject),sep="_")
    temp_subject <- cbind(pre_subject,post_subject)
    dat_both_pre_post <- rbind(dat_both_pre_post,temp_subject)
  }
}

## Save three files: pre, post, and both.

write.csv(dat_pre, file  = "Hurricane Project 2018/Data/pre-processed Pre Florence.csv")
write.csv(dat_post, file = "Hurricane Project 2018/Data/pre-processed Post Florence.csv")
write.csv(dat_both_pre_post, file = "Hurricane Project 2018/Data/pre-processed Both Pre Post Florence.csv")

