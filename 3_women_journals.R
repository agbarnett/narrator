# 3_women_journals.R
# look at when journals for women started
# August 2022
library(dplyr)
library(ggplot2)
library(tidyr)

## get the abstract data
load('data/for_analysis.RData') # from 2_concatenate_processed_data.R
## get the journal
load('data/0_journals.RData') # from 0_journals.R

# add journal information to abstract
abstracts = left_join(abstracts, journals, by='jabbrv')
# how many are missing the journal title
prop.table(table(is.na(abstracts$jtitle)))

## Section 1: look at when journals started ###

## get journal time ranges
time_ranges = group_by(abstracts, jabbrv, jtitle) %>%
  summarise(n = n(),
            start = min(date),
            end = max(date)) %>%
  ungroup() %>%
  filter(n > 50) # with at least 50 abstracts

# now look for women in title
women_words = c('women','woman','mother','obstetrics','perinatal','breastfeeding','menopause','pregnancy','gynecologic','maternal','midwifery','breast cancer','femme','mujer','frau','female','gender','menstrual')
women_pattern = paste(women_words, collapse='|')
time_ranges = mutate(time_ranges, 
                     women = str_detect(tolower(jtitle), pattern = women_pattern),
                     women = case_when(
                       women == TRUE ~ 'Yes',
                       women == FALSE ~ 'No'),
                     women = ifelse(is.na(women),'Missing',women))
# now plot time that these journals appeared
cum_plot = ggplot(time_ranges, aes(x=start, col=women))+
  stat_ecdf(geom = "step", size=1.05)+
  scale_color_manual('Journal title\nincluding women', values=c('darkorange2','darkseagreen2','grey33'))+
  theme_bw()+
  theme(legend.position = c(0.15, 0.8))
cum_plot

## Section 2: look for journals with high female proportions ###
load('data/for_analysis.RData') # from 2_concatenate_processed_data.R
stats = mutate(abstracts, female = 100* female / n.words) %>% # per 100 words
  group_by(jabbrv) %>%
  summarise(n = n(),
            mean = mean(female)) %>%
  filter(n>50) %>%
  arrange(-mean) 

## Section 3: which journals had a big increase in the female score between 1980 and 2000
load('data/for_analysis.RData') # from 2_concatenate_processed_data.R
# look at the difference between 1990 and 1980
difference = mutate(abstracts,
                    female = 100* female / n.words, # per 100 words
                   year = as.numeric(format(date,'%Y'))) %>%
  filter(year %in% c(1980, 1981, 2000, 2001)) %>% # two years early and two years later
  mutate(early = year < 1990) %>%
  group_by(jabbrv, early) %>%
  summarise(n = n(),
            mean = mean(female)) %>%
  filter(n > 10) %>% # at least this many abstracts 
  pivot_wider(values_from = 'mean', names_from= 'early') %>%
  mutate(diff = `FALSE` - `TRUE`) %>%
  arrange(-diff) %>% # largest differences at top
  ungroup()

## look at changes
filter(difference, is.na(`TRUE`)) %>% # journals that were missing in 1980s (so may have started in this time)
  summarise(n = n(),
            mean = mean(`FALSE`)) # female words in 2000s
filter(difference, is.na(`FALSE`)) %>% # journals that were missing in 2000s (so may have closed down)
  summarise(n = n(),
            mean = mean(`TRUE`)) # female words in 1980s
