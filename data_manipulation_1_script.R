library(tidyverse)
library(taRifx)
library(writexl)

### Loading dataset and getting % passed and total passed

pssa_scores_19_original <- read.csv("C:\\Users\\micha\\Documents\\Spring 2022\\CAS 138T\\Persuasive Essay\\Visualizations\\Data Sources\\pssa_scores_19.csv") %>%
  filter(grepl("SD", District.Name))

pssa_scores_19_original[is.na(pssa_scores_19_original)] = 0

omitted_districts <- c("Bryn Athyn SD, no public schools", "Duquesne City SD, does not offer an 8th grade", "Wilkinsburg Borough SD, does not offer an 8th grade")

pssa_scores_19_original$Percent.Passed <- pssa_scores_19_original$Percent.Proficient + pssa_scores_19_original$Percent.Advanced
pssa_scores_19_original$Total.Passed <- (pssa_scores_19_original$Percent.Passed/100) * pssa_scores_19_original$Number.Scored

### Getting district level data for ELA, Grade 8

### Filtering data

pssa_scores_19_ela_original <- filter(pssa_scores_19_original, 
                                      Subject == "English Language Arts" & Group == "All Students" & Grade == "8")

### Getting sums by school district

total_test_takers_ela <- by(pssa_scores_19_ela_original$Number.Scored, 
                            pssa_scores_19_ela_original$District.Name, sum)
total_test_takers_ela <- as.data.frame(total_test_takers_ela)

total_passed_ela <- by(pssa_scores_19_ela_original$Total.Passed, 
                       pssa_scores_19_ela_original$District.Name, sum)
total_passed_ela <- as.numeric(total_passed_ela)

percent_passed_ela <- total_passed_ela / total_test_takers_ela$value

passed_table_ela <- cbind(total_test_takers_ela, total_passed_ela, percent_passed_ela)

names(passed_table_ela)[names(passed_table_ela) == "IDX1"] <- "school_district"
names(passed_table_ela)[names(passed_table_ela) == "value"] <- "total_test_takers_ela"

### Trying to figure out why 3 districts were missing from test scores.
### Districts listed in the "omitted_districts" value set do offer 8th grade for various reasons, and thus are
### excluded from the analysis

### write_xlsx(passed_table_ela,"C:\\Users\\micha\\Documents\\Spring 2022\\CAS 138T\\Persuasive Essay\\table.xlsx")

### Getting district level data for Science, Grade 8

### Filtering data

pssa_scores_19_science_original <- filter(pssa_scores_19_original, 
                                      Subject == "Science" & Group == "All Students" & Grade == "8")

### Getting sums by school district

total_test_takers_science <- by(pssa_scores_19_science_original$Number.Scored, 
                                pssa_scores_19_science_original$District.Name, sum)
total_test_takers_science <- as.data.frame(total_test_takers_science)

total_passed_science <- by(pssa_scores_19_science_original$Total.Passed, 
                           pssa_scores_19_science_original$District.Name, sum)
total_passed_science <- as.numeric(total_passed_science)

percent_passed_science <- total_passed_science / total_test_takers_science$value

passed_table_science <- cbind(total_test_takers_science, total_passed_science, percent_passed_science)

names(passed_table_science)[names(passed_table_science) == "IDX1"] <- "school_district"
names(passed_table_science)[names(passed_table_science) == "value"] <- "total_test_takers_science"

### Getting district level data for Math, Grade 8

### Filtering data

pssa_scores_19_math_original <- filter(pssa_scores_19_original, 
                                          Subject == "Math" & Group == "All Students" & Grade == "8")

### Getting sums by school district

total_test_takers_math <- by(pssa_scores_19_math_original$Number.Scored, 
                             pssa_scores_19_math_original$District.Name, sum)
total_test_takers_math <- as.data.frame(total_test_takers_math)

total_passed_math <- by(pssa_scores_19_math_original$Total.Passed, pssa_scores_19_math_original$District.Name, sum)
total_passed_math <- as.numeric(total_passed_math)

percent_passed_math <- total_passed_math / total_test_takers_math$value

passed_table_math <- cbind(total_test_takers_math, total_passed_math, percent_passed_math)

names(passed_table_math)[names(passed_table_math) == "IDX1"] <- "school_district"
names(passed_table_math)[names(passed_table_math) == "value"] <- "total_test_takers_math"

### Bringing it all together
### For some reason, I had to do a very roundabout method of getting the district variable as a factor and
### the percent passed as numbers.

school_districts <- as.character(passed_table_ela$school_district)

passed_table_total <- as.data.frame(cbind(school_districts, percent_passed_ela, percent_passed_math, 
                                          percent_passed_science))
passed_table_total$school_districts <- as.factor(passed_table_total$school_districts)
passed_table_total$percent_passed_ela <- as.numeric(passed_table_total$percent_passed_ela)
passed_table_total$percent_passed_math <- as.numeric(passed_table_total$percent_passed_math)
passed_table_total$percent_passed_science <- as.numeric(passed_table_total$percent_passed_science)

### In order to calculate a rough estimate of an "adequacy cost", I am finding the states that scored in the top
### half of all 3 PSSA tests and determining a median AIE per WADM for that sub-group

tophalf_ela <- passed_table_ela %>%
  slice_max(order_by = percent_passed_ela, n = 250)

tophalf_math <- passed_table_math %>%
  slice_max(order_by = percent_passed_math, n = 250)

tophalf_science <- passed_table_science %>%
  slice_max(order_by = percent_passed_science, n = 250)

passed_table_total$top_half_of_state_all <- 
  ifelse(passed_table_total$percent_passed_ela >= min(tophalf_ela$percent_passed_ela) & 
           passed_table_total$percent_passed_math >= min(tophalf_math$percent_passed_math) & 
           passed_table_total$percent_passed_science >= min(tophalf_science$percent_passed_science), "TRUE", "FALSE")

### Bringing in the expenditure table

expenditures_2018_19 <- read.csv("C:\\Users\\micha\\Documents\\Spring 2022\\CAS 138T\\Persuasive Essay\\Visualizations\\Data Sources\\afr_expenditures_2018_19.csv")

expenditures_2018_19$AIE.per.WADM <- as.numeric(expenditures_2018_19$AIE.per.WADM)

expenditures_and_scores <- cbind(passed_table_total, expenditures_2018_19$AIE.per.WADM)

success_cost_table <- filter(expenditures_and_scores, top_half_of_state_all == "TRUE")

median(success_cost_table$`expenditures_2018_19$AIE.per.WADM`)

### Median is $13,100.22

expenditures_and_scores$distance_from_median_cost <- expenditures_and_scores$`expenditures_2018_19$AIE.per.WADM` - 10321.85


