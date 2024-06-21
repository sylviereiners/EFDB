##Wrangling the remainders##

correctedremainder <- remainingrows %>%
  filter(grepl("^3\\.B", IPCC.2006.Source.Sink.Category, ignore.case = TRUE))


##Focus on TEMPERATE MOUNTAIN SYSTEM##
remaintempmountsys <- correctedremainder %>%
  filter(apply(., 1, function(row) any(grepl("mountain system", 
                                             row, ignore.case=TRUE))))
      ##nothing##
tempbymountainsystem <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("mountain system", 
                                             row, ignore.case=TRUE))))
      #what didnt make it##
remaintempbymtnsys <- anti_join(temperate_subset, tempbymountainsystem, 
                        by = names(temperate_subset))

##Focus on TEMPERATE CONTINENTAL FOREST##
remaintempcontfor <- correctedremainder %>%
  filter(apply(., 1, function(row) any(grepl("continental forest", 
                                             row, ignore.case=TRUE))))
      ##nothing##
tempbycontinentalforest <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("continental forest", 
                                             row, ignore.case=TRUE))))
      #what didnt make it##
tempmntsysandtempcontfor <- bind_rows(tempbymountainsystem, tempbycontinentalforest)
remaintempbymntsysANDcontfor <- anti_join(temperate_subset, tempmntsysandtempcontfor, 
                        by = names(temperate_subset))

##Focus on TEMPERATE DESERT##
remaintempdesert <- correctedremainder %>%
  filter(apply(., 1, function(row) any(grepl("desert", 
                                             row, ignore.case=TRUE))))
      ## 3 rows, could be##
tempbydesert <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("desert", 
                                             row, ignore.case=TRUE))))
      ## only 1##
      ##what didnt make it##
tempmntsyscontfordes <- bind_rows(tempbymountainsystem, tempbycontinentalforest, 
                                  tempbydesert)
remaintempmntsyscontfordes <- anti_join(temperate_subset, tempmntsyscontfordes, 
                                          by = names(temperate_subset))

##Focus on TEMPERATE OCEANIC FOREST##
remaintempocfor <- correctedremainder %>%
  filter(apply(., 1, function(row) any(grepl("oceanic forest", 
                                             row, ignore.case=TRUE))))
      ##nothing##
tempbyocfor <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("oceanic forest", 
                                             row, ignore.case=TRUE))))
tempfour <- bind_rows(tempmntsyscontfordes, tempbyocfor)
remainfour <-  anti_join(temperate_subset, tempfour, 
                         by = names(temperate_subset))

##Focus on TEMPERATE STEPPE##
remaintempsteppe <- correctedremainder %>%
  filter(apply(., 1, function(row) any(grepl("steppe", 
                                             row, ignore.case=TRUE))))
      ##nothing##
tempbysteppe <- temperate_subset %>%
  filter(apply(., 1, function(row) any(grepl("steppe", 
                                             row, ignore.case=TRUE))))
tempALL <- bind_rows(tempfour, tempbysteppe)
remainALLtemp <-  anti_join(temperate_subset, tempALL, 
                         by = names(temperate_subset))
      ## 305/512 classified! ##
write.csv(tempALL, file = "classifiedtemperate.csv", row.names = FALSE)
