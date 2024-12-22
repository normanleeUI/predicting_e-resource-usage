predicting_resource_usage_db <- dbConnect(RSQLite::SQLite(), "processed_data/predicting_resource_usage.db")

# Get list of all tables
tables <- dbListTables(predicting_resource_usage_db)

# Drop each table
for (table in tables) {
  dbExecute(predicting_resource_usage_db, sprintf("DROP TABLE IF EXISTS %s", table))
}

dbExecute(predicting_resource_usage_db, "CREATE TABLE university_variables (
          fy_college varchar(9) NOT NULL,
          total_outputs float NOT NULL,
          grant_funding float NOT NULL,
          grant_proposals integer NOT NULL,
          total_enrollment integer NOT NULL,
          fy integer NOT NULL,
          college varchar(5) NOT NULL,
          PRIMARY KEY (fy_college),
          FOREIGN KEY (college) REFERENCES database_coll_combos(relevant_colls)
          );")

# dbExecute(predicting_resource_usage_db, "CREATE TABLE resource_variables (
#           database_id varchar(8) NOT NULL,
#           database_name varchar(100) NOT NULL,
#           database_subjects varchar(100),
#           total_database_views integer,
#           total_views_fy15 integer,
#           total_views_fy16 integer,
#           total_views_fy17 integer,
#           total_views_fy18 integer,
#           total_views_fy19 integer,
#           total_views_fy20 integer,
#           total_views_fy21 integer,
#           total_views_fy22 integer,,
#           PRIMARY KEY (database_id)
# );")

dbExecute(predicting_resource_usage_db, "CREATE TABLE database_usage (
          database_id varchar(8) NOT NULL,
          database_name varchar(100) NOT NULL,
          fy integer,
          views integer,
          view_id varchar(50) NOT NULL,
          PRIMARY KEY (view_id),
          FOREIGN KEY (database_id) REFERENCES database_info(database_id)
);")

dbExecute(predicting_resource_usage_db, "CREATE TABLE database_info (
          database_id varchar(8) NOT NULL,
          database_name varchar(100) NOT NULL,
          database_subjects varchar(100),
          PRIMARY KEY (database_id),
          FOREIGN KEY (database_id) REFERENCES database_info(database_usage),
          FOREIGN KEY (database_id) REFERENCES database_coll_combos(database_id)
);")

dbExecute(predicting_resource_usage_db, "CREATE TABLE database_coll_combos (
          combo_id varchar(50),
          database_id varchar(8) NOT NULL,
          relevant_colls varchar(5) NOT NULL,
          PRIMARY KEY (combo_id),
          FOREIGN KEY (database_id) REFERENCES database_info(database_id)
);")

# write data to db tables
dbWriteTable(predicting_resource_usage_db, "university_variables", university_variables, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "database_info", az_database_list, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "database_usage", az_database_usage, overwrite = TRUE)
dbWriteTable(predicting_resource_usage_db, "database_coll_combos", database_coll_combos, overwrite = TRUE)

# clean environment
rm(table)
rm(tables)