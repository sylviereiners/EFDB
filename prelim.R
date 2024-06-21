read.csv("/Users/sylviereiners/Downloads/EFDB_output.csv", header = TRUE)

setwd("/Users/sylviereiners/Downloads/EFDB_output.csv")
itbasedata <- read_csv("EFDB_output.csv")
getwd
dir.exists("/Users/sylviereiners/Downloads/EFDB_output.csv")
EFDBbasedata <- read.csv("/Users/sylviereiners/Downloads/EFDB_output.csv")

library(dplyr)
EFDBbasedata <- arrange(EFDBbasedata, EFDBbasedata$V3)
threeEFDB <- EFDBbasedata[(12719:20218), ]

names(threeEFDB)[names(threeEFDB) == 'V1'] <- 'EF_ID'
names(threeEFDB)[names(threeEFDB) == 'V2'] <- 'IPCC_1996'
names(threeEFDB)[names(threeEFDB) == 'V3'] <- 'IPCC_2006'
names(threeEFDB)[names(threeEFDB) == 'V4'] <- 'GAS'
names(threeEFDB)[names(threeEFDB) == 'V5'] <- 'FUEL_1996'
names(threeEFDB)[names(threeEFDB) == 'V6'] <- 'FUEL_2006'
names(threeEFDB)[names(threeEFDB) == 'V7'] <- 'C_POOL'
names(threeEFDB)[names(threeEFDB) == 'V8'] <- 'TYPE_OF_PARAMETER'
names(threeEFDB)[names(threeEFDB) == 'V9'] <- 'DESCRIPTION'
names(threeEFDB)[names(threeEFDB) == 'V10'] <- 'TECHNOLOGIES_PRACTICES'
names(threeEFDB)[names(threeEFDB) == 'V11'] <- 'PARAMETERS_CONDITIONS'
names(threeEFDB)[names(threeEFDB) == 'V12'] <- 'REGION_REGIONAL_CONDITIONS'
names(threeEFDB)[names(threeEFDB) == 'V13'] <- 'ABATEMENT_CONTROL_TECHNOLOGIES'
names(threeEFDB)[names(threeEFDB) == 'V14'] <- 'OTHER_PROPERTIES'
names(threeEFDB)[names(threeEFDB) == 'V15'] <- 'VALUE'
names(threeEFDB)[names(threeEFDB) == 'V16'] <- 'UNIT'
names(threeEFDB)[names(threeEFDB) == 'V17'] <- 'EQUATION'
names(threeEFDB)[names(threeEFDB) == 'V18'] <- 'IPCC_WORKSHEET'
names(threeEFDB)[names(threeEFDB) == 'V19'] <- 'TECHNICAL_REFERENCE'
names(threeEFDB)[names(threeEFDB) == 'V20'] <- 'SOURCE_OF_DATA'
names(threeEFDB)[names(threeEFDB) == 'V21'] <- 'DATA_PROVIDER'

unique(threeEFDB$IPCC_2006)
unique(threeEFDB$IPCC_1996)
unique(threeEFDB$GAS)
unique(threeEFDB$FUEL_1996)
unique(threeEFDB$FUEL_2006)
unique(threeEFDB$C_POOL)
unique(threeEFDB$TYPE_OF_PARAMETER)
unique(threeEFDB$DESCRIPTION)
unique(threeEFDB$TECHNOLOGIES_PRACTICES)
unique(threeEFDB$PARAMETERS_CONDITIONS)
unique(threeEFDB$REGION_REGIONAL_CONDITIONS)
unique(threeEFDB$OTHER_PROPERTIES)

CO2threeEFDB <- subset(threeEFDB, Gas == "CARBON DIOXIDE")
unique(CO2threeEFDB$GAS)
library(data.table)
head(CO2threeEFDB,2)
class(CO2threeEFDB)
library(dplyr)
unique(CO2threeEFDB$UNIT)


##Good for filtering by keyword##
##POLAR##
library(dplyr)
logical_matrix <- apply(CO2threeEFDB, 2, function(column) 
  grepl("polar", column, ignore.case=TRUE))
polar_rows <- CO2threeEFDB[apply(logical_matrix,1,any),]

##TROPICAL - minus subtropical##
char_matrix <- as.matrix(CO2threeEFDB)
logical_matrix_trop <- apply(char_matrix, 2, function(column) 
  grepl("tropical", column, ignore.case = TRUE))
logical_matrix_subtrop <- apply(char_matrix, 2, function(column) 
  grepl("subtropical", column, ignore.case = TRUE))
logical_matrix_tropical <- logical_matrix_trop & !logical_matrix_subtrop
tropical_rows <- CO2threeEFDB[apply(logical_matrix_tropical, 1, any), ]

##SUBTROPICAL##
subtropical_rows <- CO2threeEFDB[apply(logical_matrix_subtrop, 1, any), ]

##TEMPERATE##
logical_matrix_temp <- apply(CO2threeEFDB, 2, function(column) 
  grepl("temperate", column, ignore.case=TRUE))
temperate_rows <- CO2threeEFDB[apply(logical_matrix,1,any),]

##BOREAL##
logical_matrix_boreal <- apply(CO2threeEFDB, 2, function(column) 
  grepl("boreal", column, ignore.case=TRUE))
boreal_rows <- CO2threeEFDB[apply(logical_matrix_boreal,1,any),]


##Check for overlap, (manually?) place rows in correct subset##
tropical_indices <- as.numeric(rownames(tropical_rows))
subtropical_indices <- as.numeric(rownames(subtropical_rows))
temperate_indices <- as.numeric(rownames(temperate_rows))
boreal_indices <- as.numeric(rownames(boreal_rows))
polar_indices <- as.numeric(rownames(polar_rows))

overlap_tropbysubtrop <- intersect(tropical_indices, subtropical_indices)
overlap_tropbytemp <- intersect(tropical_indices, temperate_indices)
overlap_tropbyboreal <- intersect(tropical_indices, boreal_indices)
overlap_tropbypolar <- intersect(tropical_indices, polar_indices)
overlap_subtropbytemp <- intersect(subtropical_indices, temperate_indices)
overlap_subtropbyboreal <- intersect(subtropical_indices, boreal_indices)
overlap_subtropbypolar <- intersect(subtropical_indices, polar_indices)
overlap_tempbyboreal <- intersect(temperate_indices, boreal_indices)
overlap_tempbypolar <- intersect(temperate_indices, polar_indices)
overlap_borealbypolar <- intersect(boreal_indices, polar_indices)

overlaps_matrix <- matrix(c(
  length(overlap_tropbysubtrop), length(overlap_tropbytemp), 
  length(overlap_tropbyboreal), length(overlap_tropbypolar),
  length(overlap_subtropbytemp), length(overlap_subtropbyboreal), 
  length(overlap_subtropbypolar),length(overlap_tempbyboreal), 
  length(overlap_tempbypolar),length(overlap_borealbypolar)
), nrow = 1)

overlaps_df <- as.data.frame(overlaps_matrix)

colnames(overlaps_df) <- c("Tropical/Subtropical", "Tropical/Temperate", 
                           "Tropical/Boreal", "Tropical/Polar",
                           "Subtropical/Temperate", "Subtropical/Boreal", 
                           "Subtropical/Polar", "Temperate/Boreal", 
                           "Temperate/Polar", "Boreal/Polar")
##I'm a little stuck here. I need to make 4 new data frames listing the rows that contain overlap.##
##From here, I want to create a data frame that includes all the rows from the original that were not met by keywords.##

filtered_rows <- c(
  as.vector(rownames(tropical_rows)),
  as.vector(rownames(subtropical_rows)),
  as.vector(rownames(temperate_rows)),
  as.vector(rownames(boreal_rows)),
  as.vector(rownames(polar_rows))
)
rowsexcludedfromsubset <- CO2threeEFDB %>% filter(!row.names(.) %in% filtered_rows)
##This could work. I need to think of more keywords to add for subsetting but the code works.##
