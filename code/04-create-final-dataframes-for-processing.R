# check range of fy_college
# print(sort(na.omit(spring_enrollment_sums$fy_college))[1])
# print(sort(na.omit(spring_enrollment_sums$fy_college))[length(na.omit(spring_enrollment_sums$fy_college))])
# print(sort(na.omit(fall_enrollment_sums$fy_college))[1])
# print(sort(na.omit(fall_enrollment_sums$fy_college))[length(na.omit(fall_enrollment_sums$fy_college))])
# print(sort(na.omit(research_output_counts$fy_college))[1])
# print(sort(na.omit(research_output_counts$fy_college))[length(na.omit(research_output_counts$fy_college))])
# print(sort(na.omit(grant_funding$fy_college))[1])
# print(sort(na.omit(grant_funding$fy_college))[length(na.omit(grant_funding$fy_college))])

university_variables <- full_join(spring_enrollment, fall_enrollment, by = "fy_college") %>%
  full_join(research_output_counts, by = "fy_college") %>%
  full_join(grant_funding, by = "fy_college") %>%
  na.omit(fy_college) %>% 
  mutate(total_enrollment = enrollment.x+enrollment.y,
         fy = str_sub(fy_college, 1, 4),
         college = str_sub(fy_college, 5)) %>% 
  select(-enrollment.x,
         -enrollment.y)

database_coll_combos <- data.frame(
  database_id = unlist(sapply(1:nrow(az_database_list), function(i){
    rep(az_database_list$database_id[i], length(az_database_list$relevant_colleges[[i]]))
  })),
  relevant_colleges = unlist(sapply(1:nrow(az_database_list), function(i) az_database_list$relevant_colleges[[i]])))

database_coll_combos <- database_coll_combos %>% 
  mutate(combo_id = UUIDgenerate(n=nrow(database_coll_combos), output = "string"))

az_database_list <- az_database_list %>% 
  select(-relevant_colleges
)

# clean up environment ####
rm(fall_enrollment)
rm(spring_enrollment)
rm(research_outputs)
rm(grant_funding)
