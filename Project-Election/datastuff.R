library(readr)
X4_US_elections <- read_table("E:/EPFL/AppStat/AppStat/Data/4_US_elections.txt")
Election_data = X4_US_elections
Election_data$Region = rep(0, nrow(Election_data))
Northeast = c(7 ,8 ,19, 20, 21, 29, 30, 32, 38, 39, 45, 48)
South = c(1, 4, 9, 10, 17, 18, 24, 33, 36, 40, 42, 43, 46)
Midwest = c(13 ,14 ,15 ,16 ,22 ,23 ,25 ,27 ,34 ,35 ,41 ,49)
West = c(2 ,3 ,5 ,6 ,11 ,12 ,26 ,28 ,31 ,37 ,44 ,47 ,50)
Election_data$Region[which(Election_data$state %in% West)] = 'West'
Election_data$Region[which(Election_data$state %in% Midwest)] = 'Midwest'
Election_data$Region[which(Election_data$state %in% South)] = 'South'
Election_data$Region[which(Election_data$state %in% Northeast)] = 'Northeast'



