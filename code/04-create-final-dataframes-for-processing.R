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
  mutate(total_enrollment = enrollment.x+enrollment.y) %>% 
  select(-enrollment.x,
         -enrollment.y)

database_stats <- left_join(az_database_usage, az_database_list, by = join_by(ID == id))
database_stats <- database_stats %>% 
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
    database_id = "ID",
    database_name = "Name",
    database_subjects = "subjects",
    total_database_views = "Total",
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
  mutate(relevant_colleges =lapply(1:nrow(database_stats), function(i){
    college_list = c(
      case_when(str_detect(database_subjects[i], regex("Business|Economics", ignore_case = TRUE)) ~ "cbe"),
      case_when(str_detect(database_subjects[i], regex("Computing|Electronics|Engineering|Chemistry|environmental", ignore_case = TRUE)) ~ "coe"),
      case_when(str_detect(database_subjects[i], regex("Education|Health|Medicine|curriculum", ignore_case = TRUE)) ~ "cehhs"),
      case_when(str_detect(database_subjects[i], regex("history|philosophy|religious|literature|anthropology|sociology|political|psychology|international|language", ignore_case = TRUE)) ~ "class"),
      case_when(str_detect(database_subjects[i], regex("architecture|music|literature", ignore_case = TRUE)) ~ "caa"),
      case_when(str_detect(database_subjects[i], regex("agricultural|family|geospatial|veterinary", ignore_case = TRUE)) ~ "cals"),
      case_when(str_detect(database_subjects[i], regex("physics|earth|chemistry|biology|geography|geology|geospatial|mathematics", ignore_case = TRUE)) ~ "cos"),
      case_when(str_detect(database_subjects[i], regex("geospatial|forestry|wildlife|fisheries|geography|environmental", ignore_case = TRUE)) ~ "cnr"),
      case_when(str_detect(database_subjects[i], regex("law", ignore_case = TRUE)) ~ "law"))
    
    college_list <- college_list[!is.na(college_list)]
  }),
  relevant_fycolls = lapply(1:length(relevant_colleges), function(x){
    fiscal_year = c(2015:2022)
    unlist(lapply(relevant_colleges[[x]], function(y) paste0(fiscal_year,y)))})) %>%
  filter(database_name != "[Deleted]") %>%
  filter(relevant_colleges != "character(0)")

# debug
# database_stats <- database_stats %>% 
#   mutate(math_check = rowSums(database_stats[,5:14]) == total_views) %>% 
#   filter(math_check == FALSE)

database_fycoll_combos <- data.frame(
  database_id = unlist(sapply(1:nrow(database_stats), function(i){
    rep(database_stats$database_id[i], length(database_stats$relevant_fycolls[[i]]))
  })),
  relevant_fycolls = unlist(sapply(1:nrow(database_stats), function(i) database_stats$relevant_fycolls)),
  combo_id = UUIDgenerate(output = "string"))

database_stats <- database_stats %>% 
  select(-relevant_colleges,
          -relevant_fycolls
)