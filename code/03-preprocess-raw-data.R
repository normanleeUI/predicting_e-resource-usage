# research outputs ####

# concatenate "organization.parent.unit.name" columns; rename and remove columns as necessary

# print(unique(unlist(research_outputs[9:16])))
research_outputs <- research_outputs %>%
  mutate(unit_affiliations =
           lapply(1:nrow(research_outputs), function(i){
             row <- research_outputs[i, 9:16]
             unique(row[row != ""])
           })
  ) %>% 
  select(asset_id = "Asset.Id",
         pub_year = "Asset.Published.Date..String.",
         type = "Asset.Type",
         unit_affiliations)
###
# debug
# print(unique(unlist(research_outputs$unit_affiliations)))
###

# clean pub year column; identify rows where fiscal year needs to be simulated

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

###
# debug
###

# assign fiscal years; randomly generate if necessary; remove entries without a date
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

# add a modifier that divides the "effort" of the publication by the number of unique academic units
research_outputs <- research_outputs %>%
  mutate(
    count_unique_affiliations = sapply(research_outputs$unit_affiliations, function(i) length(i)),
    effort_per_affiliation = 1/count_unique_affiliations)

# create dataframe to count research outputs by academic unit and fiscal year
research_output_counts <- data.frame(
  fy_college = 
    unlist( 
      lapply(1:nrow(research_outputs), function(i){
        paste0(research_outputs$fiscal_year[i], 
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

###
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
###

# sum efforts based on fy & coll id's
research_output_counts <- research_output_counts %>% 
  group_by(fy_college) %>% 
  summarize(total_outputs = sum(output_effort_units, na.rm = TRUE))

# enrollment numbers ####
# generate data frames that sum enrollment numbers for each college
spring_enrollment_sums <- spring_enrollment %>% 
  group_by(term, college) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

fall_enrollment_sums <- fall_enrollment %>% 
  group_by(term, college) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE))

# generate remaining fy & college ID's ####

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

###
# debug
# view(filter(spring_enrollment_sums, is.na(fy_college)))
# view(filter(fall_enrollment_sums, is.na(fy_college)))
# print(table(spring_enrollment_sums$fy_college)[table(spring_enrollment_sums$fy_college) > 1])
# print(table(fall_enrollment_sums$fy_college)[table(fall_enrollment_sums$fy_college) > 1])
###

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

###
#debug
# view(filter(grant_funding, is.na(fy_college)))
# print(table(grant_funding$fy_college)[table(grant_funding$fy_college) > 1])
###