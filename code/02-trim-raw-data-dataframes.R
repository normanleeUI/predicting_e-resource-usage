az_database_list <- select(az_database_list,
                           id = "ID",
                           name = "Name",
                           description = "Description",
                           date_created = "Created",
                           vendor = "Vendor",
                           subjects = "Subjects")

fall_enrollment <- select(fall_enrollment,
                          college = "College",
                          term = "Term Description",
                          enrollment = "Count (Data suppression) 1")

spring_enrollment <- select(spring_enrollment,
                            college = "College",
                            term = "Term Description",
                            enrollment = "Count (Data suppression) 1")

grant_funding <- select(grant_funding,
                        fiscal_year = "fiscal-year",
                        college,
                        grant_proposals = "Number of proposals",
                        grant_funding = "research expenditures")