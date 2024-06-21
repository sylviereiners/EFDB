EFDBbasedata <- read.csv("/Users/sylviereiners/Downloads/EFDB_output.csv")
library(dplyr)
CO2EFDB <- subset(EFDBbasedata, Gas == "CARBON DIOXIDE")
row_ranges <- c(2030:2274,
                4672:7701,
                8717:11003,
                13588:13597,
                13599:13774,
                13877:14222,
                14462:14539,
                15299:15418,
                15514:15619,
                15950:16300,
                16404:16474,
                16840:16865,
                17013:17129,
                17613:17889,
                18158:18191,
                18517:19352,
                19693:19926,
                20034:20149,
                20243:21330)
co23bEFDB <- CO2EFDB %>% slice(row_ranges)

##Tropical##
tropicalandrainfall_subset <- co23bEFDB %>% 
  filter(apply(., 1, function(row) {
    any(grepl("tropical|rainfall", row, ignore.case=TRUE)) & 
      !any(grepl("subtropical", row, ignore.case=TRUE))
  }))
      ##vs##
tropical_subset <- co23bEFDB %>% 
  filter(apply(., 1, function(row) {
    any(grepl("tropical", row, ignore.case=TRUE)) & 
      !any(grepl("subtropical", row, ignore.case=TRUE))
  }))
      ##What got included with rainfall?##
rainfallfromtropical <- anti_join(tropicalandrainfall_subset, justtropical_subset, 
                                  by = names(tropicalandrainfall_subset))
      ##Do NOT use "rainfall" for tropical##


##Subtropical##
subtropical_subset <- co23bEFDB %>% 
  filter(apply(., 1, function(row) any(grepl("subtropical", 
                                             row, ignore.case=TRUE))))
      ##Looks pretty good, no obvious outliers##

##Temperate##
temeperatewithkeywords_subset <- co23bEFDB %>% 
  filter(apply(., 1, function(row) any(grepl("temperate|coniferous|broadlead|fir|evergreen|deciduous", 
                                             row, ignore.case=TRUE))))
      ##vs##
temperate_subset <- co23bEFDB %>%
  filter(apply(., 1, function(row)any(grepl("temperate",
                                            row, ignore.case=TRUE))))
      ##Do NOT use keywords for temperate##

##Boreal##
boreal_subset <- co23bEFDB %>% 
  filter(apply(., 1, function(row) any(grepl("boreal", 
                                             row, ignore.case=TRUE))))
##Polar##
polar_subset <- co23bEFDB %>% 
  filter(apply(., 1, function(row) any(grepl("polar|<10º", 
                                             row, ignore.case=TRUE))))
##Combined/remaining##
combinedsubsets <- bind_rows(tropical_subset, 
                             subtropical_subset, 
                             temperate_subset, 
                             boreal_subset, 
                             polar_subset)
combinedsubsets <- combinedsubsets %>% distinct()
remainingrows <- anti_join(co23bEFDB, combinedsubsets, by = names(co23bEFDB))
## 1672 rows were grabbed by the keywords, 2578 remain ##

##Identifying overlap##
tropsubtropOL <- semi_join(tropical_subset, subtropical_subset)
troptempOL <- semi_join(tropical_subset, temperate_subset)
tropborealOL <- semi_join(tropical_subset, boreal_subset)
troppolarOL <- semi_join(tropical_subset, polar_subset)
subtroptempOL <- semi_join(subtropical_subset, temperate_subset)
subtropborealOL <- semi_join(subtropical_subset, boreal_subset)
subtroppolarOL <- semi_join(subtropborealOL, polar_subset)
tempborealOL <- semi_join(temperate_subset, boreal_subset)
temppolarOL <- semi_join(temperate_subset, polar_subset)
borealpolarOL <- semi_join(boreal_subset, polar_subset)
##Now - look over remainder to further filter it, look over OL dfs to find problems##

