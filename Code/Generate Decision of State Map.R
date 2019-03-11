library(ggplot2)
library(maps)
library(mapdata)
library(zipcode)
library(readr)

data("zipcode")

plot_evac_map <- function(dat, state_name)
{
  ## setting up map data for plot
  states <- map_data("state")
  state <- subset(states, region == state_name)
  counties <- map_data("county")
  county <- subset(counties, region == state_name)
  
  ## cleaning zipcode
  dat$zipcode <- as.numeric(substr(dat$zipcode,1,5))
  
  ## creating a summary dataset
  dat_to_plot <- data.frame(table(dat$zipcode))
  names(dat_to_plot)[2] <- "num_subject"
  dat_to_plot$evac_portion <- sapply(dat_to_plot$Var1, 
                                      function(x) 
                                        sum(ifelse(dat$evac_decision[dat$zipcode == x]=="Yes",1,0)))
  dat_to_plot$evac_portion <- dat_to_plot$evac_portion/dat_to_plot$num_subject
  
  ## add lat and long based on zipcode
  dat_to_plot$lat  <- sapply(dat_to_plot$Var1, function(x) ifelse(length(zipcode$latitude[zipcode$zip == x])==0,0,zipcode$latitude[zipcode$zip == x]) )
  dat_to_plot$long <- sapply(dat_to_plot$Var1, function(x) ifelse(length(zipcode$longitude[zipcode$zip == x])==0,0,zipcode$longitude[zipcode$zip == x]) )
  dat_to_plot <- subset(dat_to_plot, (long != 0 | lat !=0))
  ## plotting
  base <- ggplot() +  geom_polygon(data = state, aes(x=long, y=lat, group=group), color = "black", fill = "gray") + 
    geom_polygon(data = county, aes(x=long, y=lat, group=group), fill = NA, color = "white") +
    geom_polygon(data = state, aes(x=long, y=lat, group=group),color = "black", fill = NA) + # get the state border back on top 
    coord_fixed(1.3)
  
  title <- paste(tools::toTitleCase(state_name)," Map (N=",dim(dat)[1],")",sep = "") 
  
  base + geom_point(data = dat_to_plot, aes(x=long, y = lat, color=evac_portion, size = num_subject)) + 
    scale_color_gradient2(low = "red",mid="yellow",high="blue",midpoint=0.5) + 
    scale_size_continuous(breaks = c(1:max(dat_to_plot$num_subject)), range = c(1, max(dat_to_plot$num_subject)*0.7)) + ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size=40, vjust = 1))
}


post_florence <- read_csv("Data/pre-processed post florence.csv")

post_florence_nc <- subset(post_florence, state == "NC")
post_florence_sc <- subset(post_florence, state == "SC")

plot_evac_map(post_florence_nc, "north carolina")
plot_evac_map(post_florence_sc, "south carolina")

post_michael <- read_csv("Data/pre-processed post michael.csv")

post_michael_ga <- subset(post_michael, state == "GA")
post_michael_fl <- subset(post_michael, state == "FL")
post_michael_fl_north <- subset(post_michael_fl, longitude < -84)

plot_evac_map(post_michael_ga, "georgia")
plot_evac_map(post_michael_fl, "florida")
plot_evac_map(post_michael_fl_north,"florida")


######################################################################
irma <- read_csv("Data/df_irma_ready_regression.csv")

irma$zipcode <- irma$preIrma_zipcode
irma$evac_decision <- ifelse(irma$evac_decision == 1, "Yes","No")

plot_evac_map(irma,"florida")

harvey <- read_csv("Data/df_Harvey_ready_regression.csv")

harvey$zipcode <- harvey$preHarvey_zipcode
harvey$evac_decision <- ifelse(harvey$evac_decision == 1 , "Yes","No")
harvey <- subset(harvey, city == "TX")

plot_evac_map(harvey, "texas")

