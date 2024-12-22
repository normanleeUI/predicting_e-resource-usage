# visualize ####

# CoE outputs by fiscal year
ggplot(data = filter(univ_vars, str_detect(fy_college, "coe")),
       aes(x = str_sub(fy_college, 1, 4),y = total_outputs)) +
  geom_point() + 
  labs(x = "Fiscal Year",
       y = "Research Output (effort units)") +
  ggtitle("Research Output \"Effort\": College of Engineering")

# Web of Science usage by fiscal year
dat <- filter(database_vars, str_detect(database_name, "Web of Science"))
dat_long <- pivot_longer(dat, cols = c(5:12))
ggplot(data = dat_long,
       aes(x = str_sub(name, -2, -1), y = value)) +
  geom_point() + 
  labs(x = "Fiscal Year",
       y = "Database Views") +
  ggtitle("Web of Science views by Fiscal Year")

# Web of Science usage and College enrollment
dat <- filter(all_data, str_detect(database_name, "Web of Science") & str_detect(relevant_fycolls, "coe"))
dat_long <- filter(pivot_longer(dat, cols = c(5:12)), as.integer(str_sub(relevant_fycolls, 3, 4)) == str_sub(name, -2, -1))
# view(dat_long)

ggplot(dat_long, aes(x = total_enrollment,
                     y = value,
                     label = paste0('FY',str_sub(relevant_fycolls, 3, 4)))) +
  geom_line() +
  geom_point() +
  geom_text_repel(size = 2.5,
                  force_pull = 3,
                  force = 5) + 
  labs(x = "Total Enrollment",
       y = "Database Views")+
  ggtitle("Web of Science usage by CoE Enrollment (FY15:22)")

## Conclusion
