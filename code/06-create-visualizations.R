# get data ####
predicting_resource_usage_db <- dbConnect(RSQLite::SQLite(), "processed_data/predicting_resource_usage.db")
univ_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM university_variables;")
database_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM resource_variables;")
combos <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_fycoll_combos;")
all_data <- left_join(combos, univ_vars, join_by(relevant_fycolls == fy_college)) %>% 
  left_join(database_vars, join_by(database_id))


# visualize ####

ggplot(data = filter(univ_vars, str_detect(fy_college, "coe")),
       mapping = aes(x = fy_college,y = total_outputs)) +
  geom_point()



