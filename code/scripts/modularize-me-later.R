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
  mutate(unit_affiliations =
           lapply(1:nrow(research_outputs), function(i){
             row <- research_outputs[i, 9:16] # should find some way to avoid hardcoding these in the future
             unique(row[row != "" & !is.na(row)]) # didn't see any empty strings in there but just in case.
             })
         ) %>% 
  select(asset_id = "Asset.Id",
         pub_year = "Asset.Published.Date..String.",
         type = "Asset.Type",
         unit_affiliations)

# pre-process research outputs #### 
## clean pub_year column ####
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
      TRUE ~ as_date(pub_year)
  ),
  
  needs_fy_sim = case_when(
    str_detect(pub_year, "^\\d{4}$") ~ TRUE,
    str_detect(pub_year, "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/(\\d{4}$)") ~ FALSE,
    str_detect(pub_year, "^([1-9]|1[12])/([1-9]|[12][0-9]|3[01])/(19|20)\\d{2}$") ~ FALSE,
    str_detect(pub_year, "^\\d{1,2}-[a-zA-Z]{3}$") ~ FALSE,
    str_detect(pub_year, "^[a-zA-Z]{3}-\\d{1,2}$") ~ FALSE
  )
)

## assign fiscal years; randomly generate if necessary; remove entries without a date ####
research_outputs <- research_outputs %>% 
  mutate(
    random_number = runif(n()), # need to declare this as an intermediate variable beforehand because otherwise if you generate a number >0.5 in the <0.5 case you get an N/A and vice versa.
    fiscal_year = case_when(
      needs_fy_sim == TRUE & random_number < 0.5 ~ as.integer(year(pub_year_cleaned)),
      needs_fy_sim == TRUE & random_number >= 0.5 ~ as.integer(year(pub_year_cleaned))-1,
      needs_fy_sim == FALSE & month(pub_year_cleaned) >= 7 ~ as.integer(year(pub_year_cleaned)),
      needs_fy_sim == FALSE & month(pub_year_cleaned) < 7 ~ as.integer(year(pub_year_cleaned))-1
    )) %>% 
  na.omit(research_outputs$pub_year_cleaned)

## add a modifier that divides the "effort" of the publication by the number of unique academic units listed as authors
research_outputs <- research_outputs %>%
  mutate(
    count_unique_affiliations = sapply(research_outputs$unit_affiliations, function(i) length(i)),
    effort_per_affiliation = 1/count_unique_affiliations) # sapply is remarkably convenient here because its coercion into vector automatically lines up the data types to do division.

## count research outputs by academic unit and fiscal year ####
### get asset "effort" for each individual asset ####
research_output_counts <- data.frame(
  fy_college = 
    unlist( # need to lapply and unlist rather than something else b/c unit affiliations can be a length>2 vector, so the paste0 and lapply combo creates nested lists.
      lapply(1:nrow(research_outputs), function(i){
        paste0(research_outputs$fiscal_year[i], # really gotta nail down the difference between single and double brackets; never really sure which one to use.
               research_outputs$unit_affiliations[[i]])
    })
      ),
  
  output_effort_units = 
    unlist(
      sapply(1:nrow(research_outputs), function(i){
        rep(research_outputs$effort_per_affiliation[i], length(research_outputs$unit_affiliations[[i]]))
    })
  ),
  
  asset_id =
    unlist(
      sapply(1:nrow(research_outputs), function(i){
        rep(research_outputs$asset_id[i], length(research_outputs$unit_affiliations[[i]]))
      })
    )
  )

# debug
research_output_debug <- left_join(research_output_counts, research_outputs, by = "asset_id") %>% 
  select(asset_id,
         fy_college,
         pub_year_cleaned,
         unit_affiliations,
         count_unique_affiliations,
         output_effort_units,
         effort_per_affiliation) %>% 
  mutate(
    effort_match = case_when(
      output_effort_units == effort_per_affiliation ~ 'GOOD',
      output_effort_units != effort_per_affiliation ~ 'BAD',
      TRUE ~ 'SOMETHING ELSE'
    ),
    
    affiliations_match = case_when(
      sapply(unit_affiliations, length) == count_unique_affiliations ~ 'GOOD',
      sapply(unit_affiliations, length) != count_unique_affiliations ~ 'BAD',
      TRUE ~ 'SOMETHING ELSE'
      ),
    
    fy_date_match = case_when(
    as.integer(str_sub(fy_college, start = 1, end = 4)) == year(pub_year_cleaned) ~ 'GOOD',
    year(pub_year_cleaned) - as.integer(str_sub(fy_college, start = 1, end = 4)) == 1 ~ 'GOOD',
    year(pub_year_cleaned) - as.integer(str_sub(fy_college, start = 1, end = 4)) != 1|0 ~ 'BAD',
    TRUE ~ 'SOMETHING ELSE'
  )
  )

### sum efforts based on fy & coll id's ####

# sum enrollment numbers ####
spring_enrollment_sums <- spring_enrollment %>% 
  group_by(term, college) %>%  # group_by is not as mysterious as it first appears, works just like sorting in SQL.
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

fall_enrollment_sums <- fall_enrollment %>% 
  group_by(term, college) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))