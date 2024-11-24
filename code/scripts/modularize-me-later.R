# load packages ####
library(tidyverse)
library(DBI)
library(readxl)
library(cli)
library(lubridate)
library(stringr)

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
                        fiscal_year = "fiscal-year",
                        "college",
                        grant_proposals = "Number of proposals",
                        grant_funding = "research expenditures")

platform_usage <- select(platform_usage,
                         fiscal_year = "Usage.Date.Fiscal.Year",
                         platform = "Normalized.Platform",
                         usage = "Usage.Measures.Total",
                         unique_titles = "Title.Identifier.Count")

## concatenate "organization.parent.unit.name" columns and drop them in addition to removing/renaming ####

research_outputs <- research_outputs %>%
  mutate(unit_affiliations = apply(research_outputs[, 9:16], 1, function(row) list(row[row != "" & !is.na(row)]))) %>% 
  select(asset_id = "Asset.Id",
         pub_year = "Asset.Published.Date..String.",
         type = "Asset.Type",
         unit_affiliations)

# sum enrollment numbers ####

spring_enrollment_sums <- spring_enrollment %>% 
  group_by(term, college) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

view(spring_enrollment_sums)

fall_enrollment_sums <- fall_enrollment %>% 
  group_by(term, college) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

view(fall_enrollment_sums)

# count research outputs by academic unit and fiscal year ####

## clean pub_year column ####
# cases:
# yyyy | possible regular expression: ^\d{4}$
# NULL and ""
# dd/mm/yyyy (doesn't always include leading 0's) | ^([1-9]|[12][0-9]|3[01])/([1-9]|1[12])/(19|20)\d{2}$
# mm/dd/yyyy (doesn't always include leading 0's) | ^([1-9]|1[12])/([1-9]|[12][0-9]|3[01])/(19|20)\d{2}$
# yy-mmm (3 alphanumeric characters for month abbreviation; doesn't always inclued leading 0's) | ^\\d{1,2}-[a-zA-Z]{3}$
# mmm-yy (3 alphanumeric characters for month abbreviation; doesn't always inclued leading 0's) | ^[a-zA-Z]{3}-\\d{1,2}$



research_outputs <- research_outputs %>% 
  mutate(
    pub_year_cleaned = case_when(
      str_detect(pub_year, "^\\d{4}$") ~ mdy(paste0('01/01/',pub_year)),
    
      str_detect(pub_year, "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/(\\d{4}$)") ~ dmy(pub_year),
    
      str_detect(pub_year, "^([1-9]|1[12])/([1-9]|[12][0-9]|3[01])/(19|20)\\d{2}$") ~ mdy(pub_year),
    
      str_detect(pub_year, "^\\d{1,2}-[a-zA-Z]{3}$") ~ ymd(
        paste0(
          str_replace(str_extract(pub_year, "\\d{1,2}"),
                          "\\d{1,2}",
                          paste0('20',str_extract(pub_year, "\\d{1,2}"),'-',str_extract(pub_year, '[a-zA-Z]{3}'),'-','01')))),
    
      str_detect(pub_year, "^[a-zA-Z]{3}-\\d{1,2}$") ~ ymd(
        paste0(
          str_replace(str_extract(pub_year, "\\d{1,2}"),
                      "\\d{1,2}",
                      paste0('20',str_extract(pub_year, "\\d{1,2}"),'-',str_extract(pub_year, '[a-zA-Z]{3}'),'-','01')))),
    
    TRUE ~ as_date(pub_year),
    
  ),
  
  needs_fy_sim = case_when(
    str_detect(pub_year, "^\\d{4}$") ~ TRUE,
    str_detect(pub_year, "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/(\\d{4}$)") ~ FALSE,
    str_detect(pub_year, "^([1-9]|1[12])/([1-9]|[12][0-9]|3[01])/(19|20)\\d{2}$") ~ FALSE,
    str_detect(pub_year, "^\\d{1,2}-[a-zA-Z]{3}$") ~ FALSE,
    str_detect(pub_year, "^[a-zA-Z]{3}-\\d{1,2}$") ~ FALSE
  )
)


## assign fiscal years; randomly generate if necessary ####

research_outputs <- research_outputs %>% 
  mutate(fiscal_year = case_when(
    needs_fy_sim == TRUE ~ case_when(
      sample(1:2, n(), replace = TRUE) == 1 ~ as.integer(pub_year),
      sample(1:2, n(), replace = TRUE) == 2 ~ as.integer(pub_year)+1
    ),
    
    
  ))

view(research_outputs)

## add a modifier that divides the "effort" of the publication by the number of unique academic units listed as authors

