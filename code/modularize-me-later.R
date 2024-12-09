# load packages ####
library(tidyverse)
library(DBI)
library(readxl)
library(cli)
library(lubridate)
library(stringr)
library(uuid)
library(ggplot2)

# load raw data ####
az_database_list <- read.csv('raw_data/az_database_list.csv')
az_database_usage <- read.csv('raw_data/database_usage_by_month.csv')
research_outputs <- read.csv('raw_data/research-outputs-by-ay-and-college.csv')

spring_enrollment <- read_xlsx('raw_data/spring-enrollment_by_college-major-and-semester.xlsx')
fall_enrollment <- read_xlsx('raw_data/fall-enrollment_by_college-major-and-semester.xlsx')
grant_funding <- read_xlsx('raw_data/funding-reports-combined_by_college-and-fy.xlsx')

# explore data ####
# View(az_database_list)
# for (i in colnames(az_database_list)){
#   class <- class(az_database_list[[i]])
#   cat(col_blue(i), ': ', col_red(class), "\n")
# }
# 
# # View(az_database_usage)
# for (i in colnames(az_database_usage)){
#   class <- class(az_database_usage[[i]])
#   cat(col_blue(i), ': ', col_red(class), "\n")
# }
# View(fall_enrollment)
# for (i in colnames(fall_enrollment)){
#   class <- class(fall_enrollment[[i]])
#   cat(col_blue(i), ': ', col_red(class), "\n")
# }
# 
# View(spring_enrollment)
# for (i in colnames(spring_enrollment)){
#   class <- class(spring_enrollment[[i]])
#   cat(col_blue(i), ': ', col_red(class), "\n")
# }
# 
# View(grant_funding)
# for (i in colnames(grant_funding)){
#   class <- class(grant_funding[[i]])
#   cat(col_blue(i), ': ', col_red(class), "\n")
# }
# 
# 
# View(research_outputs)
# for (i in colnames(research_outputs)){
#   class <- class(research_outputs[[i]])
#   cat(col_blue(i), ': ', col_red(class), "\n")
# }

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
                        college,
                        grant_proposals = "Number of proposals",
                        grant_funding = "research expenditures")

## concatenate "organization.parent.unit.name" columns and drop them in addition to removing/renaming ####
# print(unique(unlist(research_outputs[9:16])))

research_outputs <- research_outputs %>%
  mutate(unit_affiliations =
           lapply(1:nrow(research_outputs), function(i){
             row <- research_outputs[i, 9:16] # should find some way to avoid hardcoding these in the future
             unique(row[row != ""])
             })
         ) %>% 
  select(asset_id = "Asset.Id",
         pub_year = "Asset.Published.Date..String.",
         type = "Asset.Type",
         unit_affiliations)

# debug
# print(unique(unlist(research_outputs$unit_affiliations)))

# pre-process research outputs #### 
## clean pub_year column ####
# print(unique(research_outputs$pub_year))
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
        paste0(research_outputs$fiscal_year[i], # I got it! Single brackets return a subset of the element, double-brackets extract a value based on the index number provided. Single brackets look like they operate similarly to double-brackets in things like lapply because you are generally subsetting based on individual index values, which looks like extraction.
               research_outputs$unit_affiliations[[i]]) # another thing that sometimes confuses you about subsetting is lapply's and the like already iterate through the lists! you don't have to tell them how to do that by subsetting or extracting from the object you're already iterating over
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
# print(table(research_output_counts$fy_college)[table(research_output_counts$fy_college) > 1])

# research_output_debug <- left_join(research_output_counts, research_outputs, by = "asset_id") %>% 
#   select(asset_id,
#          fy_college,
#          pub_year_cleaned,
#          unit_affiliations,
#          count_unique_affiliations,
#          output_effort_units,
#          effort_per_affiliation) %>% 
#   mutate(
#     effort_match = case_when(
#       output_effort_units == effort_per_affiliation ~ 'GOOD',
#       output_effort_units != effort_per_affiliation ~ 'BAD',
#       TRUE ~ NA
#     ),
#     
#     affiliations_match = case_when(
#       sapply(unit_affiliations, length) == count_unique_affiliations ~ 'GOOD',
#       sapply(unit_affiliations, length) != count_unique_affiliations ~ 'BAD',
#       TRUE ~ NA
#       ),
#     
#     fy_date_match = case_when(
#     as.integer(str_sub(fy_college, start = 1, end = 4)) == year(pub_year_cleaned) ~ 'GOOD',
#     year(pub_year_cleaned) - as.integer(str_sub(fy_college, start = 1, end = 4)) == 1 ~ 'GOOD',
#     year(pub_year_cleaned) - as.integer(str_sub(fy_college, start = 1, end = 4)) != 1|0 ~ 'BAD',
#     TRUE ~ NA
#   )
#   )

# view(filter(research_output_debug, effort_match == "BAD" | affiliations_match == "BAD" | fy_college == "BAD"))

### sum efforts based on fy & coll id's ####
research_output_counts <- research_output_counts %>% 
  group_by(fy_college) %>% 
  summarize(total_outputs = sum(output_effort_units, na.rm = TRUE))

# sum enrollment numbers ####
spring_enrollment_sums <- spring_enrollment %>% 
  group_by(term, college) %>%  # group_by is not as mysterious as it first appears, works just like sorting in SQL.
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

fall_enrollment_sums <- fall_enrollment %>% 
  group_by(term, college) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

# generate remaining fy & college ID's ####
## enrollment ####
# print(unique(fall_enrollment_sums$college))
fall_enrollment_sums <- fall_enrollment_sums %>% 
  mutate(fy_college = case_when(
      str_detect(college, "Agricultural & Life Sciences") ~ paste0(str_sub(term, start = -4, end = -1),'cals'),
      str_detect(college, "Art & Architecture") ~ paste0(str_sub(term, start = -4, end = -1),'caa'),
      str_detect(college, "Business & Economics") ~ paste0(str_sub(term, start = -4, end = -1),'cbe'),
      str_detect(college, "Education, Health & Human Sci|WWAMI") ~ paste0(str_sub(term, start = -4, end = -1),'cehhs'),
      str_detect(college, "Engineering") ~ paste0(str_sub(term, start = -4, end = -1),'coe'),
      str_detect(college, "Letters Arts & Social Sciences") ~ paste0(str_sub(term, start = -4, end = -1),'class'),
      str_detect(college, "Law") ~ paste0(str_sub(term, start = -4, end = -1),'law'),
      str_detect(college, "Natural Resources") ~ paste0(str_sub(term, start = -4, end = -1),'cnr'),
      str_detect(college, "^Science$") ~ paste0(str_sub(term, start = -4, end = -1),'cos'),
      TRUE ~ NA
    )
  ) %>% 
  group_by(fy_college) %>% 
  summarize(total_enrollment = sum(total_enrollment, na.rm =  TRUE))

# print(unique(spring_enrollment_sums$college))
spring_enrollment_sums <- spring_enrollment_sums %>% 
  mutate(fy_college = case_when(
    str_detect(college, "Agricultural & Life Sciences") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'cals'),
    str_detect(college, "Art & Architecture") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'caa'),
    str_detect(college, "Business & Economics") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'cbe'),
    str_detect(college, "Education, Health & Human Sci|WWAMI") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'cehhs'),
    str_detect(college, "Engineering") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'coe'),
    str_detect(college, "Letters Arts & Social Sciences") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'class'),
    str_detect(college, "Law") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'law'),
    str_detect(college, "Natural Resources") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'cnr'),
    str_detect(college, "^Science$") ~ paste0(as.integer(str_sub(term, start = -4, end = -1))-1,'cos'),
    TRUE ~ NA
  )
  ) %>% 
  group_by(fy_college) %>% 
  summarize(total_enrollment = sum(total_enrollment, na.rm =  TRUE))

# debug
# view(filter(spring_enrollment_sums, is.na(fy_college)))
# view(filter(fall_enrollment_sums, is.na(fy_college)))
# print(table(spring_enrollment_sums$fy_college)[table(spring_enrollment_sums$fy_college) > 1])
# print(table(fall_enrollment_sums$fy_college)[table(fall_enrollment_sums$fy_college) > 1])

## grant funding ####
# unique_funding_recipients <- data.frame(
#   recipients = sort(unique(grant_funding$college),decreasing = FALSE, na.last = TRUE))
# View(unique_funding_recipients)

grant_funding <- grant_funding %>% 
  mutate(fy_college = case_when(
    str_detect(college, regex("Agricultural &|and Life Sciences", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'cals'),
    str_detect(college, regex("Letters, Arts &|and Social Sciences", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'class'),
    str_detect(college, regex("Art &|and Architecture", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'caa'),
    str_detect(college, regex("Business &|and Economics", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'cbe'),
    str_detect(college, regex("Education|WWAMI|Health", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'cehhs'),
    str_detect(college, regex("Engineering", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'coe'),
    str_detect(college, regex("law", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'law'),
    str_detect(college, regex("Natural Resources", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'cnr'),
    str_detect(college, regex("College of Science", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'cos'),
    TRUE ~ NA
  )) %>% group_by(fy_college) %>% 
  summarize(grant_funding = sum(grant_funding, na.rm =  TRUE),
            grant_proposals = sum(as.integer(grant_proposals), na.rm = TRUE)) %>%
  na.omit(fy_college)

#debug
# view(filter(grant_funding, is.na(fy_college)))
# print(table(grant_funding$fy_college)[table(grant_funding$fy_college) > 1])

# pre-process database usage statistics ####
database_stats <- left_join(az_database_usage, az_database_list, by = join_by(ID == id))

database_stats <- database_stats %>% # the mutate below is my great, poorly automated shame, don't look at it.
  mutate(total_views_fy15 = rowSums(database_stats[,12:23]),
         total_views_fy16 = rowSums(database_stats[,24:35]),
         total_views_fy17 = rowSums(database_stats[,36:47]),
         total_views_fy18 = rowSums(database_stats[,48:59]),
         total_views_fy19 = rowSums(database_stats[,60:71]),
         total_views_fy20 = rowSums(database_stats[,72:83]),
         total_views_fy21 = rowSums(database_stats[,84:95]),
         total_views_fy22 = rowSums(database_stats[,96:107]),
         total_views_fy23 = rowSums(database_stats[,108:119]),
         remaining_views = rowSums(database_stats[,c(5:11,120:125)]),
         ) %>% 
  select(
    id = "ID",
    database_name = "Name",
    subjects,
    total_views = "Total",
    total_views_fy15,
    total_views_fy16,
    total_views_fy17,
    total_views_fy18,
    total_views_fy19,
    total_views_fy20,
    total_views_fy21,
    total_views_fy22,
    # total_views_fy23,
    # remaining_views,
  )
# debug
# database_stats <- database_stats %>% 
#   mutate(math_check = rowSums(database_stats[,5:14]) == total_views) %>% 
#   filter(math_check == FALSE)

database_stats <- database_stats %>% 
  mutate(relevant_colleges = lapply(1:nrow(database_stats), function(i){
    college_list = c(
      case_when(str_detect(subjects[i], regex("Business|Economics", ignore_case = TRUE)) ~ "cbe"),
      case_when(str_detect(subjects[i], regex("Computing|Electronics|Engineering|Chemistry|environmental", ignore_case = TRUE)) ~ "coe"),
      case_when(str_detect(subjects[i], regex("Education|Health|Medicine|curriculum", ignore_case = TRUE)) ~ "cehhs"),
      case_when(str_detect(subjects[i], regex("history|philosophy|religious|literature|anthropology|sociology|political|psychology|international|language", ignore_case = TRUE)) ~ "class"),
      case_when(str_detect(subjects[i], regex("architecture|music|literature", ignore_case = TRUE)) ~ "caa"),
      case_when(str_detect(subjects[i], regex("agricultural|family|geospatial|veterinary", ignore_case = TRUE)) ~ "cals"),
      case_when(str_detect(subjects[i], regex("physics|earth|chemistry|biology|geography|geology|geospatial|mathematics", ignore_case = TRUE)) ~ "cos"),
      case_when(str_detect(subjects[i], regex("geospatial|forestry|wildlife|fisheries|geography|environmental", ignore_case = TRUE)) ~ "cnr"),
      case_when(str_detect(subjects[i], regex("law", ignore_case = TRUE)) ~ "law"))
    
      college_list <- college_list[!is.na(college_list)]
  }
  ), relevant_fycolls = lapply(1:length(relevant_colleges), function(x){
    fiscal_year = c(2015:2022)
    unlist(lapply(relevant_colleges[[x]], function(y) paste0(fiscal_year,y)))})) %>%
  filter(database_name != "[Deleted]") %>%
  filter(relevant_colleges != "character(0)") %>% 
  select(-total_views_fy23) # only have grant funding data up to fy22
  
# probably not necessary
# transposed_database_stats = pivot_longer(database_stats, cols = total_views_fy16:total_views_fy22) %>%
#   select(id,
#          database_name,
#          relevant_colleges,
#          fiscal_year = "name",
#          views = "value") %>%
#   mutate(fiscal_year = paste0("20",str_sub(fiscal_year, -2, -1)),
#          fy_college = lapply(1:length(relevant_colleges), function(x){
#            lapply(relevant_colleges[[x]], function(y) paste0(fiscal_year[[x]],y))
#          }))

# join university variable data together ####

# check range of fy_college
# print(sort(na.omit(spring_enrollment_sums$fy_college))[1])
# print(sort(na.omit(spring_enrollment_sums$fy_college))[length(na.omit(spring_enrollment_sums$fy_college))])
# print(sort(na.omit(fall_enrollment_sums$fy_college))[1])
# print(sort(na.omit(fall_enrollment_sums$fy_college))[length(na.omit(fall_enrollment_sums$fy_college))])
# print(sort(na.omit(research_output_counts$fy_college))[1])
# print(sort(na.omit(research_output_counts$fy_college))[length(na.omit(research_output_counts$fy_college))])
# print(sort(na.omit(grant_funding$fy_college))[1])
# print(sort(na.omit(grant_funding$fy_college))[length(na.omit(grant_funding$fy_college))])

university_variables <- full_join(spring_enrollment_sums, fall_enrollment_sums, by = "fy_college") %>%
  full_join(research_output_counts, by = "fy_college") %>%
  full_join(grant_funding, by = "fy_college") %>%
  na.omit(fy_college) %>% 
  mutate(total_enrollment = total_enrollment.x+total_enrollment.y) %>% 
  select(-total_enrollment.x,
         -total_enrollment.y)

# create database - fycoll matches data frame ####
database_fycoll_combos <- data.frame(
  database_id = unlist(sapply(1:nrow(database_stats), function(i){
    rep(database_stats$id[i], length(database_stats$relevant_fycolls[[i]]))
  })),
  relevant_fycolls = unlist(sapply(1:nrow(database_stats), function(i) database_stats$relevant_fycolls))) %>%
    mutate(combo_id = UUIDgenerate(n=length(relevant_fycolls), output = "string"))


# store in a SQL database ####
predicting_resource_usage_db <- dbConnect(RSQLite::SQLite(), "processed_data/predicting_resource_usage.db")

dbExecute(predicting_resource_usage_db, "CREATE TABLE university_variables (
          fy_college varchar(9) NOT NULL,
          total_outputs float NOT NULL,
          grant_funding float NOT NULL,
          grant_proposals integer NOT NULL,
          total_enrollment integer NOT NULL,
          PRIMARY KEY (fy_college)
          );")

dbExecute(predicting_resource_usage_db, "CREATE TABLE resource_variables (
          database_id varchar(8) NOT NULL,
          database_name varchar(100) NOT NULL,
          database_subjects varchar(100),
          total_database_views integer,
          total_views_fy15 integer,
          total_views_fy16 integer,
          total_views_fy17 integer,
          total_views_fy18 integer,
          total_views_fy19 integer,
          total_views_fy20 integer,
          total_views_fy21 integer,
          total_views_fy22 integer,,
          PRIMARY KEY (database_id)
);")

dbExecute(predicting_resource_usage_db, "CREATE TABLE database_fycoll_combos (
          combo_id varchar(50),
          database_id varchar(8) NOT NULL,
          relevant_fycolls varchar(9) NOT NULL,
          PRIMARY KEY (combo_id),
          FOREIGN KEY (relevant_fycolls) REFERENCES university_variables(fy_college),
          FOREIGN KEY (database_id) REFERENCES resource_variables(database_id)
);")

# final clean-up of data.frames
view(university_variables)
view(database_stats)
view(database_fycoll_combos)

database_stats <- database_stats %>% 
  select(
    database_id = "id",
    database_name,
    database_subjects = "subjects",
    total_database_views = "total_views",
    total_views_fy15,
    total_views_fy16,
    total_views_fy17,
    total_views_fy18,
    total_views_fy19,
    total_views_fy20,
    total_views_fy21,
    total_views_fy22,
    # relevant_colleges
  )

dbWriteTable(predicting_resource_usage_db, "university_variables", university_variables, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "resource_variables", database_stats, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "database_fycoll_combos", database_fycoll_combos, overwrite = TRUE)

#check content of tables
# add more thorough debug here
fromsql_univ_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM university_variables;")
fromsql_resource_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM resource_variables;")
fromsql_combos <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_fycoll_combos;")

# visualize ####
