# Import California Department of Education (CDE) enrollment data files
# 
# Date files and file specification available here: https://www.cde.ca.gov/ds/sd/sd/filesenr.asp
# 
# This script will work for enrollment files from 2007-08 school year and later
#
# This script imports enrollment data files from individual school years and combines them into a single data frame

# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyr, dplyr)

# Set working directory
setwd("C:/Github/SimilarSchoolComparison/Data/Enrollment")

# Create list to store data frames
l_dataframes <- list()

# List of files in the working folder
l_files <- dir()

# Import each file into a data frame
for (i in 1:length(l_files)) {
  
  l_dataframes[[i]] <- l_files[i] %>%
    
                          # Import file
                          read.csv(header = TRUE
                                   , sep = "\t"
                                   , colClasses = c(rep("character", 6)
                                                    , rep("integer", 17))) %>%
                          
                          # Parse school year from file name
                          mutate(FILE_NAME = l_files[i])
}

# Create single data frame
df_enr <- bind_rows(l_dataframes)

# Clean up
rm(i, l_dataframes, l_files)
gc()


df_enr_long <- df_enr %>%
  
  pivot_longer(KDGN:ADULT, names_to = "GRADE_SPAN", values_to = "COUNT") %>%
  
  # Add ethnic text variable
  mutate(ETHNIC_TXT = case_when(
      ETHNIC == "0" ~ "Not Reported"
      , ETHNIC == "1" ~ "American Indian or Alaska Native"
      , ETHNIC == "2" ~ "Asian"
      , ETHNIC == "3" ~ "Pacific Islander"
      , ETHNIC == "4" ~ "Filipino"
      , ETHNIC == "5" ~ "Hispanic or Latino"
      , ETHNIC == "6" ~ "African American"
      , ETHNIC == "7" ~ "White"
      , ETHNIC == "9" ~ "Two or More Races"),
    
    School_Year_2digit = as.numeric(substring(FILE_NAME, 4, 5)),
    
    SCHOOL_YEAR = paste0(School_Year_2digit,
                         "-",
                         School_Year_2digit + 1)) %>%
  
  # Drop file name
  dplyr::select(-FILE_NAME, -School_Year_2digit)
  