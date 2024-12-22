# research outputs ####

# concatenate "organization.parent.unit.name" columns; rename and remove columns as necessary
# print(unique(unlist(research_outputs[9:16]))) # used to determine whether columns contained "", NA's, NULLs, or other values
# print(unique(research_outputs$pub_year)) # get a sense of different date formats in the dataframe
research_outputs <- research_outputs %>%
  mutate(unit_affiliations =
           lapply(1:nrow(research_outputs), function(i){
             unique(research_outputs[i, 9:16][research_outputs[i, 9:16] != ""]) # != condition based on print statement above
           })
  ) %>% 
  select(asset_id = "Asset.Id",
         pub_year = "Asset.Published.Date..String.",
         type = "Asset.Type",
         unit_affiliations) %>% 
  mutate( # clean pub year column; identify rows where fiscal year needs to be simulated
    pub_year_cleaned = case_when( # use regex to parse different date formats into lubridate friendly strings
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
    
    needs_fy_sim = case_when( # for publications where only "YYYY" dates are provided there isn't enough info to determine fy, so must be simulated
      str_detect(pub_year, "^\\d{4}$") ~ TRUE,
      str_detect(pub_year, "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/(\\d{4}$)") ~ FALSE,
      str_detect(pub_year, "^([1-9]|1[12])/([1-9]|[12][0-9]|3[01])/(19|20)\\d{2}$") ~ FALSE,
      str_detect(pub_year, "^\\d{1,2}-[a-zA-Z]{3}$") ~ FALSE,
      str_detect(pub_year, "^[a-zA-Z]{3}-\\d{1,2}$") ~ FALSE
    )
  ) %>% 
  mutate(  # assign fiscal years; randomly generate if necessary; remove entries without a date
    random_number = runif(n()), # generate a column of random numbers
    fiscal_year = case_when(
      needs_fy_sim == TRUE & random_number < 0.5 ~ as.integer(year(pub_year_cleaned)), # randomly generate a fiscal year based on whether a randomly generated number is above or below 0.5
      needs_fy_sim == TRUE & random_number >= 0.5 ~ as.integer(year(pub_year_cleaned))-1,
      needs_fy_sim == FALSE & month(pub_year_cleaned) >= 7 ~ as.integer(year(pub_year_cleaned)), # else, just extract month from lubridate dates, using a July - June fiscal year
      needs_fy_sim == FALSE & month(pub_year_cleaned) < 7 ~ as.integer(year(pub_year_cleaned))-1
    )) %>% 
  mutate( # don't want to double-count publications if the have multiple academic units associated with them, so divide the "effort" of each publication between the units that produced it
    count_unique_affiliations = sapply(unit_affiliations, function(i) length(i)),
    effort_per_affiliation = 1/count_unique_affiliations) %>% 
  na.omit(pub_year_cleaned) %>% # remove research outputs without dates
  select(-random_number, # remove columns that are no longer necessary
         -pub_year,
         -needs_fy_sim)

###
# debug
# print(unique(unlist(research_outputs$unit_affiliations)))
###

# create dataframe to count research output "effort" by academic unit and fiscal year
research_output_counts <- data.frame(
  fy_college = 
    unlist( 
      lapply(1:nrow(research_outputs), function(i){
        paste0(research_outputs$fiscal_year[[i]], 
               research_outputs$unit_affiliations[[i]])
      })
    ),
  
  output_effort_units = 
    unlist(
      sapply(1:nrow(research_outputs), function(i){
        rep(research_outputs$effort_per_affiliation[[i]], length(research_outputs$unit_affiliations[[i]])) # since an fy_college value is created for each of the units affiliated with a single research output, we have to ascribe the research output's "effort" value as many times as there are academic units
      })
    ),
  
  asset_id =
    unlist(
      sapply(1:nrow(research_outputs), function(i){
        rep(research_outputs$asset_id[[i]], length(research_outputs$unit_affiliations[[i]]))
      })
    )
) %>% 
  group_by(fy_college) %>% 
  summarize(total_outputs = sum(output_effort_units, na.rm = TRUE)) # sum efforts based on fy & coll id's

###
# debug - performed before final summing
# print(table(research_output_counts$fy_college)[table(research_output_counts$fy_college) > 1])
# research_output_debug <- left_join(research_output_counts, research_outputs, by = "asset_id") %>% 
#   select(asset_id,
#          fy_college,
#          pub_year_cleaned,
#          unit_affiliations,
#          output_effort_units,
#          effort_per_affiliation) %>% 
#   mutate(
#     effort_match = case_when(
#       output_effort_units == effort_per_affiliation ~ 'GOOD',
#       output_effort_units != effort_per_affiliation ~ 'BAD',
#       TRUE ~ NA
#     ),
#     fy_date_match = case_when(
#     as.integer(str_sub(fy_college, start = 1, end = 4)) == year(pub_year_cleaned) ~ 'GOOD',
#     year(pub_year_cleaned) - as.integer(str_sub(fy_college, start = 1, end = 4)) == 1 ~ 'GOOD',
#     year(pub_year_cleaned) - as.integer(str_sub(fy_college, start = 1, end = 4)) != 1|0 ~ 'BAD',
#     TRUE ~ NA
#   )
#   )

# view(filter(research_output_debug, effort_match == "BAD" | affiliations_match == "BAD" | fy_college == "BAD"))
###

# enrollment numbers ####
# generate fy and college values and modify data frames so they sum enrollments for each college and fiscal year

# print(unique(fall_enrollment$college)) # check to make sure college names are consistent
fall_enrollment <- fall_enrollment %>%  
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
  summarize(enrollment = sum(enrollment, na.rm =  TRUE)) # generate data frame that sums enrollment numbers

# print(unique(spring_enrollment$college))
spring_enrollment <- spring_enrollment %>%  
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
  summarize(enrollment = sum(enrollment, na.rm =  TRUE)) # generate data frame that sums enrollment numbers

###
# debug
# view(filter(spring_enrollment, is.na(fy_college)))
# view(filter(fall_enrollment, is.na(fy_college)))
# print(table(spring_enrollment$fy_college)[table(spring_enrollment$fy_college) > 1])
# print(table(fall_enrollment$fy_college)[table(fall_enrollment$fy_college) > 1])
###

# check to see if funding recipient names are consistent
# unique_funding_recipients <- data.frame(
#   recipients = sort(unique(grant_funding$college),decreasing = FALSE, na.last = TRUE))
# View(unique_funding_recipients)

# grant funding ####

grant_funding <- grant_funding %>% # generate fiscal year and college id's, sum funding and grant proposals, omit units other than the ones we intend to track
  mutate(fy_college = case_when(
    str_detect(college, regex("Agricultural &|and Life Sciences", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'cals'),
    str_detect(college, regex("Letters, Arts &|and Social Sci", ignore_case = TRUE)) ~ paste0('20',fiscal_year,'class'),
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

###
#debug
# view(filter(grant_funding, is.na(fy_college)))
# print(table(grant_funding$fy_college)[table(grant_funding$fy_college) > 1])
###

# print(data.frame(
#   index = 1:ncol(az_database_usage),
#   column_name = names(az_database_usage)
# ))

# az_database_usage <- left_join(az_database_usage, az_database_list, by = join_by(ID == id))
az_database_usage <- az_database_usage %>% 
  mutate(total_views_fy15 = rowSums(az_database_usage[,12:23]),
         total_views_fy16 = rowSums(az_database_usage[,24:35]),
         total_views_fy17 = rowSums(az_database_usage[,36:47]),
         total_views_fy18 = rowSums(az_database_usage[,48:59]),
         total_views_fy19 = rowSums(az_database_usage[,60:71]),
         total_views_fy20 = rowSums(az_database_usage[,72:83]),
         total_views_fy21 = rowSums(az_database_usage[,84:95]),
         total_views_fy22 = rowSums(az_database_usage[,96:107]),
         total_views_fy23 = rowSums(az_database_usage[,108:119]),
         remaining_views = rowSums(az_database_usage[,c(5:11,120:125)]),
  ) %>% 
  select(
    database_id = "ID",
    # database_name = "Name",
    # database_subjects = "subjects",
    # total_database_views = "Total",
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
  ) %>% 
  pivot_longer(cols = total_views_fy15:total_views_fy22) %>%
  mutate(fy = str_sub(name, -2, -1)) %>%
  select(-name,
         views = "value")
az_database_usage <- az_database_usage %>% 
  mutate(view_id = UUIDgenerate(n=nrow(az_database_usage), output = "string"))
    # select(database,
    #        database_name,
    #        relevant_colleges,
    #        fiscal_year = "name",
    #        views = "value") %>%
    # mutate(fiscal_year = paste0("20",str_sub(fiscal_year, -2, -1)),
    #        fy_college = lapply(1:length(relevant_colleges), function(x){
    #          lapply(relevant_colleges[[x]], function(y) paste0(fiscal_year[[x]],y))
    #        }))

az_database_list <- az_database_list %>% 
  mutate(relevant_colleges =lapply(1:nrow(az_database_list), function(i){
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
  })) %>% 
  # relevant_fycolls = lapply(1:length(relevant_colleges), function(x){
  #   fiscal_year = c(2015:2022)
  #   unlist(lapply(relevant_colleges[[x]], function(y) paste0(fiscal_year,y)))})) %>%
  filter(name != "[Deleted]") %>%
  filter(relevant_colleges != "character(0)") %>% 
  select(database_id = "id",
         database_name = "name",
         description,
         subjects,
         relevant_colleges)

# debug
# az_database_usage <- az_database_usage %>% 
#   mutate(math_check = rowSums(az_database_usage[,5:14]) == total_views) %>% 
#   filter(math_check == FALSE)
