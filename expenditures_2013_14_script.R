library(tidyverse)

expenditures_2013_14 <- read.csv("C:\\Users\\micha\\Documents\\Spring 2022\\CAS 138T\\Persuasive Essay\\Visualizations\\Data Sources\\afr_expenditures_2013_14.csv")

as.numeric(expenditures_2013_14$Total.Revenue.per.ADM)
mean(expenditures_2013_14$Total.Revenue.per.ADM)
