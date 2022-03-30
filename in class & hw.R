## INSTALL PACKAGES
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

## IMPORT DATA
linelist_raw <- import("/Users/staceyshe/Desktop/PPD/S1S2/620 Wed/wk3/raw/linelist_raw.xlsx")

## SEE FIRST 50 ROWS OF DATA
head(50)

## GET AN OVERVIEW OF THE ENTIRE DATAFRAME 
skimr::skim(linelist_raw)
names(linelist_raw)

## AUTOMATIC CLEANING COLUMN NAMES
linelist <- linelist_raw %>% 
  janitor::clean_names()
names(linelist)

## MANUAL NAME CLEANING
# Cleaning 'pipe' chain (starts with raw data and pipes it through cleaning steps)
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)

# Rename by column position
  rename(newNameForFirstColumn  = 1,
         newNameForSecondColumn = 2)

  linelist_raw %>% 
    select(
      date_infection       = `infection_date`,    # rename and KEEP ONLY these columns
      date_hospitalisation = `hosp_date`)
# !!!!Error in UseMethod("rename") : 
# no applicable method for 'rename' applied to an object of class "c('double', 'numeric')"
  
linelist_raw <- openxlsx::readWorkbook("/Users/staceyshe/Desktop/PPD/S1S2/620 Wed/wk3/raw/linelist_raw.xlsx", fillMergedCells = TRUE)

## SELECT OR REORDER COLUMNS
names(linelist)
# linelist dataset is piped through select() command, and names() prints just the column names
linelist %>% 
  select(case_id, date_onset, date_hospitalisation, fever) %>% 
  names()  # display the column names
## [1] "case_id"              "date_onset"           "date_hospitalisation" "fever"

# move date_onset and date_hospitalisation to beginning
linelist %>% 
  select(date_onset, date_hospitalisation, everything()) %>% 
  names()

# select columns that are class Numeric
linelist %>% 
  select(where(is.numeric)) %>% 
  names()
# select columns containing certain characters
linelist %>% 
  select(contains("date")) %>% 
  names()
# searched for multiple character matches
linelist %>% 
  select(matches("onset|hosp|fev")) %>%   # note the OR symbol "|"
  names()
# Only one of these columns exists, but no error is produced and the code continues without stopping your cleaning chain.
linelist %>% 
  select(any_of(c("date_onset", "village_origin", "village_detection", "village_residence", "village_travel"))) %>% 
  names()

## REMOVE COLUMNS
linelist %>% 
  select(-c(date_onset, fever:vomit)) %>% # remove date_onset and all columns from fever to vomit
  names()
# can also remove a column using base R syntax, by defining it as NULL
linelist$date_onset <- NULL   # deletes column with base R syntax 

# Stand alone
# Create a new linelist with id and age-related columns
linelist_age <- select(linelist, case_id, contains("age"))
# display the column names
names(linelist_age)

# Add to the pipe chain
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
# begin cleaning pipe chain
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
# remove column
select(-c(row_num, merged_header,x28))


## DUPLICATION
linelist <- linelist %>% 
  distinct()
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
# begin cleaning pipe chain
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
# de-duplicate
distinct()
# !!ERROR!! Column `x28` doesn't exist???... But it does exist... 


## COLUMN CREATION AND TRANSFORMATION

#New columns
linelist <- linelist %>% 
  mutate(new_col = 10)
# OR reference values in other columns, to perform calculations.
# a new column bmi is created to hold the Body Mass Index (BMI)
linelist <- linelist %>% 
  mutate(bmi = wt_kg / (ht_cm/100)^2)

new_col <- linelist %>%                       
  mutate(
    new_var_dup    = case_id,             # new column = duplicate/copy another existing column
    new_var_static = 7,                   # new column = all values the same
    new_var_static = new_var_static + 5,  # you can overwrite a column, and it can be a calculation using other variables
    new_var_paste  = stringr::str_glue("{hospital} on ({date_hospitalisation})") # new column = pasting together values from other columns
  ) %>% 
  select(case_id, hospital, date_hospitalisation, contains("new"))        # show only new columns, for demonstration purposes

## CONVERT COLUMN CLASS
class(linelist$age)
#[1] "character"
class(linelist$date_onset)
#[1] "character"
linelist <- linelist %>% 
  mutate(age = as.numeric(age))

## GROUPED DATA
# age normalized to mean of ALL rows
linelist %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

# age normalized to mean of hospital group
linelist %>% 
  group_by(hospital) %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

## TRANSFORM MULTIPLE COLUMNS

linelist <- linelist %>% 
  mutate(across(.cols = c(temp, ht_cm, wt_kg), .fns = as.character))
#to change all columns to character class
linelist <- linelist %>% 
  mutate(across(.cols = everything(), .fns = as.character))
#to change all columns contains the string “date” to character class
linelist <- linelist %>% 
  mutate(across(.cols = contains("date"), .fns = as.character))
# mutating the columns that are currently class POSIXct and convert them to a normal class date
linelist <- linelist %>% 
  mutate(across(.cols = where(is.POSIXct), .fns = as.Date))

# coalesce
village_detection <- c("a", "b", NA,  NA)
village_residence <- c("a", "c", "a", "d")

village <- coalesce(village_detection, village_residence)
village    # print
# OR provide data frame columns
linelist <- linelist %>% 
  mutate(village = coalesce(village_detection, village_residence))
#!!! ERROR!!! `village` must be size 6611 or 1, not 4.

# Cumulative math
sum(c(2,4,15,10))     # returns only one number
# [1] 31
cumsum(c(2,4,15,10))  # returns the cumulative sum at each step
# [1]  2  6 21 31

cumulative_case_counts <- linelist %>%  # begin with case linelist
  count(date_onset) %>%                 # count of rows per day, as column 'n'   
  mutate(cumulative_cases = cumsum(n))  # new column, of the cumulative sum at each row
# See the first 10 rows
head(cumulative_case_counts, 10)

# Using base R
# Creating new BMI column
linelist$bmi = linelist$wt_kg / (linelist$ht_cm / 100) ^ 2
# !!! ERROR!!! Error in linelist$ht_cm/100 : non-numeric argument to binary operator

# Add to pipe chain
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
# begin cleaning pipe chain
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
# add new column
mutate(bmi = wt_kg / (ht_cm/100)^2) %>% 
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age))

#!!! ERROR!!! Column `x28` doesn't exist.


## RECODE VALUES

# fix incorrect values                   # old value       # new value
linelist <- linelist %>% 
  mutate(date_onset = recode(date_onset, "2014-14-15" = "2014-04-15"))
# another example re-coding multiple values within one column
table(linelist$hospital, useNA = "always")  # print table of all unique values, including missing  

linelist <- linelist %>% 
  mutate(hospital = recode(hospital,
                           # for reference: OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  ))

table(linelist$hospital, useNA = "always")

# Simple logic
# Example: change gender of one specific observation to "Female" 
linelist <- linelist %>% 
  mutate(gender = replace(gender, case_id == "2195", "Female"))
#OR
linelist$gender[linelist$case_id == "2195"] <- "Female"

# Change source value of Missing and Nonmissing values
linelist <- linelist %>% 
  mutate(source_known = ifelse(!is.na(source), "known", "unknown"))

# Create a date of death column, which is NA if patient has not died.
linelist <- linelist %>% 
  mutate(date_death = if_else(outcome == "Death", date_outcome, NA_real_))
# !!! ERROR!!! `false` must be a character vector, not a double vector.
# !!! Avoid stringing together many ifelse commands… use case_when() instead! 
# case_when() is much easier to read and you’ll make fewer errors.

# Complex logic
linelist <- linelist %>% 
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age is given in years
    age_unit == "months" ~ age/12,    # if age is given in months
    is.na(age_unit)      ~ age,       # if age unit is missing, assume years
    TRUE                 ~ NA_real_)) # any other circumstance, assign missing
# !!! ERROR!!!
#in `mutate()`:! Problem while computing `age_years = case_when(...)`.
#Caused by error in `age / 12`:! non-numeric argument to binary operator

#Missing values
#dplyr function
linelist <- linelist %>% 
  mutate(hospital = replace_na(hospital, "Missing"))

#forcats package
linelist %>% 
  mutate(hospital = fct_explicit_na(hospital))

#To convert a specific value to NA
linelist <- linelist %>% 
  mutate(hospital = na_if(hospital, "Missing"))

# Use replace() or case_when() for logic criteria (e.g. “all values > 99”)
# Convert temperatures above 40 to NA 
linelist <- linelist %>% 
  mutate(temp = replace(temp, temp > 40, NA))
# Convert onset dates earlier than 1 Jan 2000 to missing
linelist <- linelist %>% 
  mutate(date_onset = replace(date_onset, date_onset > as.Date("2000-01-01"), NA))


#Add to pipe chain
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
# begin cleaning pipe chain
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # add column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%     
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED

# clean values of hospital column
mutate(hospital = recode(hospital,
                         # OLD = NEW
                         "Mitylira Hopital"  = "Military Hospital",
                         "Mitylira Hospital" = "Military Hospital",
                         "Military Hopital"  = "Military Hospital",
                         "Port Hopital"      = "Port Hospital",
                         "Central Hopital"   = "Central Hospital",
                         "other"             = "Other",
                         "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
)) %>% 
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_))


## NUMERIC CATEGORIES
#check the class of the linelist variable age
class(linelist$age_years)
# examine the distribution
hist(linelist$age_years)
#!!! Error in hist.default(linelist$age_years) : 'x' must be numeric

summary(linelist$age_years, na.rm=T)

# With ceiling set to TRUE
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70),
      ceiling = TRUE)) # 70 is ceiling, all above become NA

# show table
table(linelist$age_cat, useNA = "always")
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      lower = 0,
      upper = 100,
      by = 10))

# show table
table(linelist$age_cat, useNA = "always")

# cut
linelist <- linelist %>% 
  
  # cut() creates age_cat, automatically of class Factor      
  mutate(age_cat = cut(
    age_years,
    breaks = c(0, 5, 10, 15, 20, 30, 50, 70, 100),          
    right = FALSE,
    include.lowest = TRUE,        
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-49", "50-69", "70-100")),
    
    # make missing values explicit
    age_cat = fct_explicit_na(
      age_cat,
      na_level = "Missing age")  # you can specify the label
  )    

# table to view counts
table(linelist$age_cat, useNA = "always")


# Quickly make breaks and labels
# Make break points from 0 to 90 by 5
age_seq = seq(from = 0, to = 90, by = 5)
age_seq

# Make labels for the above categories, assuming default cut() settings
age_labels = paste0(age_seq + 1, "-", age_seq + 5)
age_labels

# check that both vectors are the same length
length(age_seq) == length(age_labels)

# Quantile breaks
quantile(linelist$age_years,               # specify numeric vector to work on
         probs = c(0, .25, .50, .75, .90, .95),   # specify the percentiles you want
         na.rm = TRUE)                            # ignore missing values 

linelist %>%                                # begin with linelist
  mutate(deciles = cut(age_years,           # create new column decile as cut() on column age_years
                       breaks = quantile(                      # define cut breaks using quantile()
                         age_years,                               # operate on age_years
                         probs = seq(0, 1, by = 0.1),             # 0.0 to 1.0 by 0.1
                         na.rm = TRUE),                           # ignore missing values
                       include.lowest = TRUE)) %>%             # for cut() include age 0
  janitor::tabyl(deciles)                   # pipe to table to display

# Evenly-sized groups
# make groups with ntile()
ntile_data <- linelist %>% 
  mutate(even_groups = ntile(age_years, 10))

# make table of counts and proportions by group
ntile_table <- ntile_data %>% 
  janitor::tabyl(even_groups)

# attach min/max values to demonstrate ranges
ntile_ranges <- ntile_data %>% 
  group_by(even_groups) %>% 
  summarise(
    min = min(age_years, na.rm=T),
    max = max(age_years, na.rm=T)
  )

# combine and print - note that values are present in multiple groups
left_join(ntile_table, ntile_ranges, by = "even_groups")

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # add column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%     
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
  
  # clean values of hospital column
  mutate(hospital = recode(hospital,
                           # OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  )) %>% 
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  ###################################################   
mutate(
  # age categories: custom
  age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70)),
  
  # age categories: 0 to 85 by 5s
  age_cat5 = epikit::age_categories(age_years, breakers = seq(0, 85, 5)))


## ADD ROWS
linelist <- linelist %>% 
  add_row(row_num = 666,
          case_id = "abc",
          generation = 4,
          `infection date` = as.Date("2020-10-10"),
          .before = 2)


## FILTER ROWS
linelist <- linelist %>% 
  filter(gender == "f")   # keep only rows where gender is equal to "f"
linelist %>% 
  drop_na(case_id, age_years)  # drop rows with missing values for case_id or age_years
# Filter by row number
# View first 100 rows
linelist %>% head(100)     # or use tail() to see the n last rows

# Show row 5 only
linelist %>% filter(row_number() == 5)

# View rows 2 through 20, and three specific columns
linelist %>% filter(row_number() %in% 2:20) %>% select(date_onset, outcome, age)

#Examine the data
hist(linelist$date_onset, breaks = 50)

table(Hospital  = linelist$hospital,                     # hospital name
      YearOnset = lubridate::year(linelist$date_onset),  # year of date_onset
      useNA     = "always")                              # show missing values

linelist <- linelist %>% 
  # keep rows where onset is after 1 June 2013 OR where onset is missing and it was a hospital OTHER than Hospital A or B
  filter(date_onset > as.Date("2013-06-01") | (is.na(date_onset) & !hospital %in% c("Hospital A", "Hospital B")))

nrow(linelist)

#Standalone
# dataframe <- filter(dataframe, condition(s) for rows to keep)
linelist <- filter(linelist, !is.na(case_id))
linelist <- linelist[!is.na(case_id), ]

#Quickly review records
View(linelist)
# OR
View(linelist[linelist$case_id %in% c("11f8ea", "76b97a", "47a5f5"), c("date_onset", "date_hospitalisation")])

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
# begin cleaning pipe chain
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # add column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%     
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
  
  # clean values of hospital column
  mutate(hospital = recode(hospital,
                           # OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  )) %>% 
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_)) %>% 
  
  mutate(
    # age categories: custom
    age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70)),
    
    # age categories: 0 to 85 by 5s
    age_cat5 = epikit::age_categories(age_years, breakers = seq(0, 85, 5))) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED

filter(
  # keep only rows where case_id is not missing
  !is.na(case_id),  
  
  # also filter to keep only the second outbreak
  date_onset > as.Date("2013-06-01") | (is.na(date_onset) & !hospital %in% c("Hospital A", "Hospital B")))


## ROW-WISE CALCULATIONS
linelist %>%
  rowwise() %>%
  mutate(num_symptoms = sum(c(fever, chills, cough, aches, vomit) == "yes")) %>% 
  ungroup() %>% 
  select(fever, chills, cough, aches, vomit, num_symptoms) # for display

linelist %>%
  rowwise() %>%
  mutate(num_NA_dates = sum(is.na(c_across(contains("date"))))) %>% 
  ungroup() %>% 
  select(num_NA_dates, contains("date")) # for display

linelist %>%
  rowwise() %>%
  mutate(latest_date = max(c_across(contains("date")), na.rm=T)) %>% 
  ungroup() %>% 
  select(latest_date, contains("date"))  # for display

#use max() to get the latest or most recent date for each row
linelist %>%
  rowwise() %>%
  mutate(latest_date = max(c_across(contains("date")), na.rm=T)) %>% 
  ungroup() %>% 
  select(latest_date, contains("date"))  # for display

## ARRANGE AND SORT
linelist %>% 
  arrange(hospital, desc(date_onset))


