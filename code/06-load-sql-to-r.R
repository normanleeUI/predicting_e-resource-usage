# get data
predicting_resource_usage_db <- dbConnect(RSQLite::SQLite(), "processed_data/predicting_resource_usage.db")

univ_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM university_variables;")
database_use <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_usage;")
database_info <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_info;")
combos <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_coll_combos;")

# join data
all_data <- full_join(database_use, database_info, by = join_by(database_id)) %>%
  full_join(combos, by = join_by(database_id)) %>% 
  mutate(fy_college = paste0("20",fy,relevant_colleges)) %>% 
  left_join(univ_vars, by = join_by(fy_college)) %>% 
  select(-fy.x,
         -view_id,
         -description,
         -subjects,
         -combo_id,
         -relevant_colleges,
         fy = "fy.y")
