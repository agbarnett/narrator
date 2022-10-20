# 3_journal_variation.R
# look at variations in phrases by journals
# August 2022
library(dplyr)

## get the data
load('data/for_analysis.RData') # from 2_concatenate_processed_data.R

# mean spelling errors by journal
by_journal = group_by(abstracts, jabbrv) %>%
  summarise(n = n(),
            male = mean(male),
            female = mean(female),
            spelling = mean(spelling)) %>%
  filter(n > 100) # at least 100 abstracts

# to here
hist(by_journal$male)
