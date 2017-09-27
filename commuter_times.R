library(jsonlite)

census_api_key <- "YOUR API KEY HERE"

#get acs 2016 data for travel time to work and population for all urban areas
vars <- paste('B08303_', sprintf('%03i', seq(1, 13)), 'E', sep='')
vars <- c("B01003_001E", vars)
variable_list <- paste0(vars, collapse =",")

url <- paste0("https://api.census.gov/data/2016/acs/acs1?get=NAME,", variable_list, "&for=urban+area:*&key=", census_api_key)
commuter_data <- fromJSON(url)
commuter_data_frame <- as.data.frame(commuter_data, stringsAsFactors = F)

names(commuter_data_frame) <- c("location_name", "total_population", "total_commuters",
                                "less_than_5_min", "5_to_9_min", "10_to_14_min", "15_to_19_min",
                                "20_to_24_min", "25_to_29_min", "30_to_34_min", "35_to_39_min",
                                "40_to_44_min", "45_to_59_min", "60_to_89_min", "90_or_more_min",
                                "urban_area")
commuter_data_frame <- commuter_data_frame[2:nrow(commuter_data_frame), ]

for(i in 2:15) {
  commuter_data_frame[,i] <- as.integer(commuter_data_frame[,i])
}

#remove urban areas with a population less than 65,000. Only gets rid of 6 rows
commuter_data_frame <- subset(commuter_data_frame, commuter_data_frame$total_population>64999)

#create the median 
#for each row, figure out which commuter bin our median commuter belongs to
#first we just get the number (in line) of the median commuter
commuter_data_frame$median_commuter_number <- round(commuter_data_frame$total_commuters / 2)

#then we figure out which bin he belongs by comparing the median_commuter_name to the cummulative sum of commuters working from shortest to longest commute times
#is there a way to do this with apply instead of nested loops?

median_commute_bins <- c()
median_commute_bin_number <- c()

for (j in 1:nrow(commuter_data_frame)) {
  i <- 4
  median_number <- commuter_data_frame[j,17]
  median_here <- (commuter_data_frame[j,4] > median_number)
  
  while(median_here == FALSE) {
    i = i + 1
    median_here <- (sum(commuter_data_frame[j,4:i]) > median_number)
  }
  
  median_commute_bins <- c(median_commute_bins,names(commuter_data_frame)[i])
  median_commute_bin_number <- c(median_commute_bin_number, i)
}


commuter_data_frame$median_commute_bins  <- median_commute_bins 
commuter_data_frame$median_commute_bin_number  <- median_commute_bin_number


# because commute times are variable and the majority bins are only 5 minutes wide, 
# I will leave the median commute times as 5 minute ranges. For Tracy, CA, because the range is 45 to 59 minutes, 
# I will estimate a smaller range.

# add percentage with commute time 60 minutes and higher
commuter_data_frame$percent_more_than_hour <- round((commuter_data_frame$`60_to_89_min` + commuter_data_frame$`90_or_more_min`)/commuter_data_frame$total_commuters * 100)

# order commuter_data_frame according to median commute bin and percentage with commute time 60 min + 
commuter_data_frame <- commuter_data_frame[order(-commuter_data_frame$percent_more_than_hour, -commuter_data_frame$median_commute_bin_number),]

#get acs 2016 data for travel time to work and population for all urban areas
vars <- paste('B08303_', sprintf('%03i', seq(1, 13)), 'E', sep='')
vars <- c("B01003_001E", vars)
variable_list <- paste0(vars, collapse =",")

url <- paste0("https://api.census.gov/data/2016/acs/acs1?get=NAME,", variable_list, "&for=urban+area:*&key=", census_api_key)
commuter_data <- fromJSON(url)
commuter_data_frame <- as.data.frame(commuter_data, stringsAsFactors = F)

names(commuter_data_frame) <- c("location_name", "total_population", "total_commuters",
                                "less_than_5_min", "5_to_9_min", "10_to_14_min", "15_to_19_min",
                                "20_to_24_min", "25_to_29_min", "30_to_34_min", "35_to_39_min",
                                "40_to_44_min", "45_to_59_min", "60_to_89_min", "90_or_more_min",
                                "urban_area")
commuter_data_frame <- commuter_data_frame[2:nrow(commuter_data_frame), ]

for(i in 2:15) {
  commuter_data_frame[,i] <- as.integer(commuter_data_frame[,i])
}

#remove urban areas with a population less than 65,000. Only gets rid of 6 rows
commuter_data_frame <- subset(commuter_data_frame, commuter_data_frame$total_population>64999)

#create the median 
#for each row, figure out which commuter bin our median commuter belongs to
#first we just get the number (in line) of the median commuter
commuter_data_frame$median_commuter_number <- round((commuter_data_frame$total_commuters +1) / 2)

#then we figure out which bin he belongs by comparing the median_commuter_name to the cummulative sum of commuters working from shortest to longest commute times
#is there a way to do this with apply instead of nested loops?

median_commute_bins <- c()
median_commute_bin_number <- c()

for (j in 1:nrow(commuter_data_frame)) {
  i <- 4
  median_number <- commuter_data_frame[j,17]
  median_here <- (commuter_data_frame[j,4] > median_number)
  
  while(median_here == FALSE) {
    i = i + 1
    median_here <- (sum(commuter_data_frame[j,4:i]) > median_number)
  }
  
  median_commute_bins <- c(median_commute_bins,names(commuter_data_frame)[i])
  median_commute_bin_number <- c(median_commute_bin_number, i)
}


commuter_data_frame$median_commute_bins  <- median_commute_bins 
commuter_data_frame$median_commute_bin_number  <- median_commute_bin_number


# because commute times are variable and the majority bins are only 5 minutes wide, 
# I will leave the median commute times as 4 minute ranges. For Tracy, CA, because the range is 45 to 59 minutes, 
# I will estimate a smaller range. Using this formula for median: https://www.mathstips.com/statistics/median-for-discrete-and-continuous-frequency-type.html, 
# I determined that that the estimate median for Cary is 47 minutes, which will be marked as a range from 45 to 49 (+- 2 minutes
# from the estimated median for uniformity) 

# add percentage with commute time 60 minutes and higher
commuter_data_frame$percent_more_than_hour <- round((commuter_data_frame$`60_to_89_min` + commuter_data_frame$`90_or_more_min`)/commuter_data_frame$total_commuters * 100)
commuter_data_frame$percent_more_than_90 <- round((commuter_data_frame$`90_or_more_min`)/commuter_data_frame$total_commuters * 100)
# order commuter_data_frame according to median commute bin and percentage with commute time 60 min + 
commuter_data_frame <- commuter_data_frame[order(-commuter_data_frame$percent_more_than_hour, -commuter_data_frame$median_commute_bin_number),]
write.csv(commuter_data_frame, file = "commuter_data.csv")
