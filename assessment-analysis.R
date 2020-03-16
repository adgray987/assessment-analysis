### assessment-analysis.R
library(tidyverse)
library(viridisLite)

assessment_file_path <- "[path to report csv]"

assessment <- read_csv(assessment_file_path, skip = 2)

assessment <- read_csv(assessment_file_path, skip = 2, guess_max = nrow(assessment))

#Get total assessments
#Since there is a many-to-many relationship between resources and assessments,
#assessments with n resources attached will be counted n times
at <- nrow(assessment)

#----

#Sum the observations in the "Existing Description" section

#Administrative/custodial control
count_appraisal <- assessment %>%
  count(appraisal) %>% 
  select(count = n, "Appraisal" = appraisal) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Internal") %>%
  mutate(portion = round((count / at), 2))

count_deed_of_gift <- assessment %>%
  count(deed_of_gift) %>% 
  select(count = n, "Deed of Gift" = deed_of_gift) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Internal") %>% 
  mutate(portion = round((count / at), 2))

count_control_file <- assessment %>%
  count(control_file) %>% 
  select(count = n, "Control File" = control_file) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Internal") %>%
  mutate(portion = round((count / at), 2))

count_accession_report <- assessment %>%
  count(accession_report) %>% 
  select(count = n, "ASpace Accession Record" = accession_report) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>%
  add_column(control_type = "Internal") %>%
  mutate(portion = round((count / at), 2))

#Combine the counts for administrative/custodial control
count_total_admin <- bind_rows(count_appraisal, count_deed_of_gift, count_control_file, 
                            count_accession_report) %>% 
  filter(exists == "Yes") %>% 
  arrange(desc(count))

#Plot the counts for 'Existing Description'
admin_chart <- ggplot(count_total_admin, aes(x = reorder(type, -portion), y= portion, fill = exists)) +
  ggtitle("Administrative and Custodial Control") +
  geom_bar(aes(y = portion), stat = "identity", width = .50, show.legend = FALSE) +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = .50), 
        axis.text.x = element_text(angle = 90, vjust = .50, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = element_blank(), y = element_blank())

#----
#Intellectual control
count_catalog_record <- assessment %>%
  count(catalog_record) %>% 
  select(count = n, "Catalog Record" = catalog_record) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>%
  add_column(control_type = "Public") %>%
  mutate(portion = round((count / at), 2))

count_finding_aid_ead <- assessment %>%
  count(finding_aid_ead) %>% 
  select(count = n, "EAD Finding Aid" = finding_aid_ead) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Public") %>%
  mutate(portion = round((count / at), 2))

count_container_list <- assessment %>% 
  count(container_list) %>% 
  select(count = n, "Container List" = container_list) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Public") %>%
  mutate(portion = round((count / at), 2))

count_finding_aid_paper <- assessment %>% 
  count(finding_aid_paper) %>%
  select(count = n, "Paper Finding Aid" = finding_aid_paper) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Public") %>%
  mutate(portion = round((count / at), 2))

count_finding_aid_spreadsheet <- assessment %>% 
  count(finding_aid_spreadsheet) %>%
  select(count = n, "Spreadsheet Finding Aid" = finding_aid_spreadsheet) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Public") %>%
  mutate(portion = round((count / at), 2))

count_finding_aid_word <- assessment %>% 
  count(finding_aid_word) %>%
  select(count = n, "Word Finding Aid" = finding_aid_word) %>% 
  pivot_longer(-count, names_to = "type", values_to = "exists") %>% 
  add_column(control_type = "Public") %>%
  mutate(portion = round((count / at), 2))

#Combine the counts for intellectual control
count_total_intel <- bind_rows(count_catalog_record, 
                            count_finding_aid_ead, count_container_list, 
                            count_finding_aid_paper, 
                            count_finding_aid_spreadsheet,
                            count_finding_aid_word) %>% 
  filter(exists == "Yes") %>% 
  arrange(desc(count))

#Plot the counts for intellectual control
intel_chart <- ggplot(count_total_intel, aes(x = reorder(type, -portion), y= portion, fill = exists)) +
  ggtitle("Existing Description\nand Intellectual Control") +
  geom_bar(aes(y = portion), stat = "identity", width = .50, show.legend = FALSE) +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(hjust = .50), 
        axis.text.x = element_text(angle = 90, vjust = .50, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = element_blank(), y = element_blank())

#----
#Sum the observations in the "Ratings" section

count_int_access <- assessment %>%
  filter(!is.na(intellectual_access_description_rating)) %>% 
  count(intellectual_access_description_rating) %>% 
  select(count = n, "intellectual\naccess" = intellectual_access_description_rating) %>% 
  pivot_longer(-count, names_to = "category", values_to = "rating") %>% 
  mutate(portion = round((count / at), 2))

count_doc_qual <- assessment %>%
  select(documentation_quality_rating) %>% 
  filter(!is.na(documentation_quality_rating)) %>% 
  count(documentation_quality_rating) %>% 
  select(count = n, "documentation\nquality" = documentation_quality_rating) %>% 
  pivot_longer(-count, names_to = "category", values_to = "rating") %>% 
  mutate(portion = round((count / at), 2))

count_interest <- assessment %>%
  select(interest_rating) %>% 
  filter(!is.na(interest_rating)) %>% 
  count(interest_rating) %>% 
  select(count = n, "interest" = interest_rating) %>% 
  pivot_longer(-count, names_to = "category", values_to = "rating") %>% 
  mutate(portion = round((count / at), 2))

count_phys_acc <- assessment %>%
  select(physical_access_arrangement_rating) %>% 
  filter(!is.na(physical_access_arrangement_rating)) %>% 
  count(physical_access_arrangement_rating) %>% 
  select(count = n, "physical\naccess" = physical_access_arrangement_rating) %>% 
  pivot_longer(-count, names_to = "category", values_to = "rating") %>% 
  mutate(portion = round((count / at), 2))

count_phys_cond <- assessment %>%
  select(physical_condition_rating) %>% 
  filter(!is.na(physical_condition_rating)) %>% 
  count(physical_condition_rating) %>% 
  select(count = n, "physical\ncondition" = physical_condition_rating) %>% 
  pivot_longer(-count, names_to = "category", values_to = "rating") %>% 
  mutate(portion = round((count / at), 2))

count_housing_quality <- assessment %>%
  select(housing_quality_rating) %>% 
  filter(!is.na(housing_quality_rating)) %>% 
  count(housing_quality_rating) %>% 
  select(count = n, "housing\nquality" = housing_quality_rating) %>% 
  pivot_longer(-count, names_to = "category", values_to = "rating") %>% 
  mutate(portion = round((count / at), 2))

#Combine the counts for ratings
total_ratings <- bind_rows(count_interest, count_doc_qual, 
                                 count_int_access, count_phys_cond, 
                                 count_housing_quality, count_phys_acc)
  

#Plot the counts for ratings
ratings_chart <- ggplot(total_ratings, 
                        aes(fill = factor(rating), x = category, y = count)) +
  geom_bar(position = position_fill(), stat = "identity") +
  scale_fill_viridis_d(name = "Rating", direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Appraisal and Access Ratings") +
  theme(plot.title = element_text(hjust = .50), 
        axis.text.x = element_text(angle = 90, vjust = .50, hjust = 1)) +
  guides(fill = guide_legend()) +
  labs(x = element_blank(), y = element_blank())

#----
#Save data to file
dir.create("[path for data]")
setwd("[path for output data]")
write_csv(count_total_intel, "intellectual_control.csv")
write_csv(count_total_admin, "admin_control.csv")
write_csv(total_ratings, "assessment_ratings.csv")

#Save figures to file
dir.create("[path for figs]")
setwd("[path for figs]")
ggsave("intel_chart.png", intel_chart)
ggsave("admin_chart.png", admin_chart)
ggsave("ratings_chart.png", ratings_chart)
