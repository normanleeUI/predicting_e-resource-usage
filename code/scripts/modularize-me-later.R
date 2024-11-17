# load packages ####
library(tidyverse)
library(DBI)
library(readxl)
library(cli)

# load raw data ####
az_database_list <- read.csv('raw_data/az_database_list.csv')
platform_usage <- read.csv('raw_data/platform-usage_by_fy-and-platform.csv')
research_outputs <- read.csv('raw_data/research-outputs-by-ay-and-college.csv')

spring_enrollment <- read_xlsx('raw_data/spring-enrollment_by_college-major-and-semester.xlsx')
fall_enrollment <- read_xlsx('raw_data/fall-enrollment_by_college-major-and-semester.xlsx')
grant_funding <- read_xlsx('raw_data/funding-reports-combined_by_college-and-fy.xlsx')

# explore data ####
View(az_database_list)
for (i in colnames(az_database_list)){
  class <- class(az_database_list[[i]])
  cat(col_blue(i), ': ', col_red(class), "\n")
}

View(fall_enrollment)
for (i in colnames(fall_enrollment)){
  class <- class(fall_enrollment[[i]])
  cat(col_blue(i), ': ', col_red(class), "\n")
}

View(spring_enrollment)
for (i in colnames(spring_enrollment)){
  class <- class(spring_enrollment[[i]])
  cat(col_blue(i), ': ', col_red(class), "\n")
}

View(grant_funding)
for (i in colnames(grant_funding)){
  class <- class(grant_funding[[i]])
  cat(col_blue(i), ': ', col_red(class), "\n")
}

View(platform_usage)
for (i in colnames(platform_usage)){
  class <- class(platform_usage[[i]])
  cat(col_blue(i), ': ', col_red(class), "\n")
}

View(research_outputs)
for (i in colnames(research_outputs)){
  class <- class(research_outputs[[i]])
  cat(col_blue(i), ': ', col_red(class), "\n")
}

# remove unnecessary columns and rename remaining ones ####
az_database_list <- select(az_database_list,
                           id = "ID",
                           name = "Name",
                           description = "Description",
                           date_created = "Created",
                           vendor = "Vendor",
                           subjects = "Subjects")

fall_enrollment <- select(fall_enrollment,
                          college = "College",
                          term = "Term Description",
                          enrollment = "Count (Data suppression) 1")

spring_enrollment <- select(spring_enrollment,
                            college = "College",
                            term = "Term Description",
                            enrollment = "Count (Data suppression) 1")

grant_funding <- select(grant_funding,
                        "fiscal-year",
                        "college",
                        grant_proposals = "Number of proposals",
                        grant_funding = "research expenditures")

## concatenate "organization.parent.unit.name" columns and drop them in addition to removing/renaming ####

research_outputs <- research_outputs %>%
  mutate(unit_affiliations = apply(research_outputs[, 9:16], 1, function(row) list(row[row != "" & !is.na(row)]))) %>% 
  select(asset_id = "Asset.Id",
         pub_year = "Asset.Published.Date..String.",
         type = "Asset.Type",
         unit_affiliations)
