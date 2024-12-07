# get data ####
predicting_resource_usage_db <- dbConnect(RSQLite::SQLite(), "processed_data/predicting_resource_usage.db")
univ_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM university_variables;")
database_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM resource_variables;")
combos <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_fycoll_combos;")

# visualize ####

ggplot(data = filter(univ_vars, str_detect(fy_college, "coe")),
       mapping = aes(x = str_sub(fy_college, 1, 4),y = total_outputs)) +
  geom_point() + 
  labs(x = "Fiscal Year",
       y = "Research Output (effort units)") +
  ggtitle("Research Output \"Effort\": College of Engineering")

