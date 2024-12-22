# get data ####
predicting_resource_usage_db <- dbConnect(RSQLite::SQLite(), "processed_data/predicting_resource_usage.db")
univ_vars <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM university_variables;")
database_use <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_usage;")
database_info <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_info;")
combos <- dbGetQuery(predicting_resource_usage_db, "SELECT * FROM database_coll_combos;")
