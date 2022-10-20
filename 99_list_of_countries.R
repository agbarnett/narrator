# 99_list_of_countries.R
# list of countries for affiliation matching
# data from https://www.worldatlas.com/articles/names-of-countries-in-their-own-languages.html
# with acronyms added by me
# August 2022
library(janitor)
library(readxl)
library(dplyr)
library(stringr)

# get excel data
countries = read_excel('data/country_list.xlsx') %>%
  clean_names() %>%
  mutate(country_name_english = str_squish(country_name_english))

# make alternative versions for each country
country_list = NULL
for (k in 1:nrow(countries)){
  # simple version
  frame = data.frame(english = countries$country_name_english[k]) %>% mutate(match = english)
  country_list = bind_rows(country_list, frame)
  # alternative versions
  versions = str_split(countries$country_name_native_tongue[k], pattern=',')[[1]]
  for (v in versions){
    frame = data.frame(english = countries$country_name_english[k], match = str_squish(v))
    country_list = bind_rows(country_list, frame)
  }
}

# remove duplicates
country_list = unique(country_list)

## add US universities
us_unis = read.csv('data/us_universities.csv', header = FALSE) 
names(us_unis) = 'match'
us_unis = mutate(us_unis, english = 'United States')
country_list = bind_rows(country_list, us_unis)

## add US state codes
us_states = read.csv('data/us_state_codes.csv', header = FALSE) 
names(us_states) = 'match'
us_states = mutate(us_states,  
                   match = str_squish(match),
                   english = 'United States')
country_list = bind_rows(country_list, us_states)

## add UK universities
uk_unis = read.csv('data/uk_universities.csv', header = FALSE) 
names(uk_unis) = 'match'
uk_unis = mutate(uk_unis, 
                 match = str_squish(match),
                 english = 'United Kingdom')
country_list = bind_rows(country_list, uk_unis)

## add Canadian universities
can_unis = read.csv('data/can_universities.csv', header = FALSE) 
names(can_unis) = 'match'
can_unis = mutate(can_unis,  
                  match = str_squish(match),
                  english = 'Canada')
country_list = bind_rows(country_list, can_unis)

## add Australian universities
aus_unis = read.csv('data/aus_universities.csv', header = FALSE) 
names(aus_unis) = 'match'
aus_unis = mutate(aus_unis, 
                  match = str_squish(match),
                  english = 'Australia')
country_list = bind_rows(country_list, aus_unis)

## add South Korean universities
sk_unis = read.csv('data/sk_universities.csv', header = FALSE) 
names(sk_unis) = 'match'
sk_unis = mutate(sk_unis, 
                  match = str_squish(match),
                  english = 'South Korea')
country_list = bind_rows(country_list, sk_unis)

## add optional full-stop for St (Saint)
country_list = mutate(country_list,
                      match = str_replace_all(match, pattern='St\\. ', replacement = 'St\\.? '),
                      match = str_replace_all(match, pattern='St ', replacement = 'St\\.? '))

# create whole words
country_list = mutate(country_list, 
                      match_words = paste('\\b', match, '\\b', sep=''))
# 
save(country_list, file='data/countries.RData')
