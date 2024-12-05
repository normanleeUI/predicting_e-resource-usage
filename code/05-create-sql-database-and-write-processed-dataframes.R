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

dbWriteTable(predicting_resource_usage_db, "university_variables", university_variables, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "resource_variables", database_stats, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "database_fycoll_combos", database_fycoll_combos, overwrite = TRUE)