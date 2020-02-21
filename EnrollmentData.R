# Import California Department of Education (CDE) enrollment data files
# 
# Date files and file specification available here: https://www.cde.ca.gov/ds/sd/sd/filesenr.asp
# 
# This script will work for enrollment files from 2007-08 school year and later
#
# This script imports enrollment data files from individual school years and combines them into a tidy data set

#Clear console
cat("\014") 

#Clear memory
rm(list=ls())
gc()

# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyr, dplyr, zip)

# Create vector with school years for looping 
v_schoolyears <- c("2007-08", "2008-09", "2009-10", "2010-11", "2011-12", 
                   "2012-13", "2013-14", "2016-17", "2017-18", "2018-19")

# Urls
v_url_begin <- "http://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear="
v_url_end <- "&cCat=Enrollment&cPage=filesenr.asp"

# Download data files
for (i in 1:length(v_schoolyears)) {
  
  download.file(url = paste0(v_url_begin, v_schoolyears[i], v_url_end),
                destfile = paste0("enrollment_", v_schoolyears[i], ".txt"))
  
}

# Create list to store data frames
l_dataframes <- list()

# Import each file into a data frame
for (i in 1:length(v_schoolyears)) {
  
  l_dataframes[[i]] <- paste0("enrollment_", v_schoolyears[i],".txt") %>%
    
                          # Import file
                          read.csv(header = TRUE
                                   , sep = "\t"
                                   , colClasses = c(rep("character", 6) # first 6 columns are text
                                                    , rep("integer", 17))) %>% # next 17 columns are numeric
                          
                          # Add school year column
                          mutate(SCHOOL_YEAR = v_schoolyears[i])
}

# Delete stored files
for (i in 1:length(v_schoolyears)) {
  
  file.remove(paste0("enrollment_", v_schoolyears[i], ".txt"))
  
}
gc()

# Create single data frame
df_enrollment <- bind_rows(l_dataframes)

# Clean up
rm(i, l_dataframes)
gc()

# Pivot to long, tidy format
df_enrollment_long <- df_enrollment %>%
  
  pivot_longer(KDGN:ADULT, names_to = "GRADE_LEVEL", values_to = "COUNT") %>%
  
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
      , ETHNIC == "9" ~ "Two or More Races")) %>%
  
  # Reorder columns
  select(CDS_CODE:SCHOOL,
         SCHOOL_YEAR,
         ETHNIC,
         ETHNIC_TXT,
         GENDER,
         GRADE_LEVEL,
         COUNT)

# Export in RDS format
saveRDS(df_enrollment_long, "enrollment_by_school.rds")

# Export CSV
data.table::fwrite(df_enrollment_long,
                   file = "enrollment_by_school.csv",
                   row.names = FALSE)
# Compress CSV
zip(zipfile = "enrollment_by_school.zip",
    files = "enrollment_by_school.csv")

# Delete CSV file
file.remove("enrollment_by_school.csv")