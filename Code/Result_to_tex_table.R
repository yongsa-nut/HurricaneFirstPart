library(readr)

dat_mean <- read.csv("Results/post michael to post florence obs only f_score.csv")

dat_sd   <- read.csv("Results/post michael sd_f_score.csv")

sd_flag <- FALSE


for(i in 1:dim(dat_mean)[1])
{
  temp_m <- as.character(dat_mean[i,-1])
  str_m <- ""
  for(j in 1:length(temp_m))
  {
    str_m <- paste(str_m,temp_m[j],sep="&")
  }
  print(str_m)
  
  if(sd_flag)
  {
    temp_sd <- as.character(dat_sd[i,-1])
    str_sd <- ""
    for(j in 1:length(temp_sd))
    {
      str_sd <- paste(str_sd,temp_sd[j],sep="&")
    }
    print(str_sd)
  }
}

