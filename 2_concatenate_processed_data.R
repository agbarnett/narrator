# 2_concatenate_processed_data.R
# concatenate the processed data into one large file
# use processed data from lyra
# August 2022
source('99_combine_article_types.R') # for combining article types
library(dplyr)
library(forcats)
library(stringr)

# move to HPC and list the files
setwd("//hpc-fs/barnetta/acronym") # move to HPC
to.combine = dir('processed') # find the files to combine
# alternative version
alt = str_detect(to.combine, 'alt')
alt.files = to.combine[alt]
to.combine = to.combine[!alt]
#
cat('There are ', length(to.combine), ' original files.\n', sep='')
cat('There are ', length(alt.files), ' alternative files.\n', sep='')
max_files = 1114
all = paste('pubmed.', sprintf('%04d', 1:max_files), '.RData', sep='')
setdiff(all, to.combine) # files that are missing

# loop to combine
abstracts = ex.abstracts = NULL # start with empty data
for (file in to.combine){
  # get original and alternative files and merge
  alt.file = str_replace(file, pattern='pubmed', replacement='pubmed.alt')
  load(paste('processed/', alt.file, sep='')) # from 1_process_pubmed_alternative.R
  alternative = abstract.data
  load(paste('processed/', file, sep='')) # from 1_process_pubmed.R
  abstract.data = full_join(abstract.data, alternative, by='pmid') %>%
    mutate(
      sentences = ifelse(sentences==0, 1, sentences), # avoid a few zeros
      words_per_sentence = n.words / sentences)
  
  # date was not always in date format:
  if(class(abstract.data$date) == 'numeric'){ abstract.data = mutate(abstract.data, date=as.Date(date, origin='1970-01-01'))} 
  
  ## add `numbers` to excluded abstracts
  # a) not english
  non_english = numbers$start - numbers$post.non.english
  median_date = median(abstract.data$date) # use median date from the same batch
  if(non_english > 0){
    this.ex = data.frame(number = 1:non_english, date = median_date, reason = 'Not in English') # add as frequency; missing type
    ex.abstracts = bind_rows(ex.abstracts, this.ex)
  }
  # b) empty abstract
  no_abstract = numbers$post.non.english - numbers$post.empty.abstract
  if(no_abstract > 0){
    this.ex = data.frame(number = 1:no_abstract, date = median_date, reason = 'No abstract') # add as frequency
    ex.abstracts = bind_rows(ex.abstracts, this.ex)
  }
  
  ## remove abstracts with 5 or fewer words
  fewer.5 = filter(abstract.data, n.words <= 5) %>%
    select(pmid, date, type) %>%
    mutate(reason = '5 words or fewer')
  if(nrow(fewer.5) > 0){
    ex.abstracts = bind_rows(ex.abstracts, fewer.5)
  }
  abstract.data = filter(abstract.data, n.words > 5)
  
  ##
  if(is.null(abstract.data)==FALSE) {abstracts = bind_rows(abstracts, abstract.data)}
  if(is.null(excluded.abstracts)==FALSE) {ex.abstracts = bind_rows(ex.abstracts, excluded.abstracts)}
  # tidy up to avoid results being carried forward into empty results
  remove(abstract.data, excluded.abstracts)
}
excluded.abstracts = ex.abstracts # rename

## remove duplicates in abstracts
dups = duplicated(abstracts$pmid)
n.duplicates = sum(dups)
to.remove = unique(abstracts$pmid[dups])
# add to exclusions
ex.duplicates = filter(abstracts, pmid %in% to.remove) %>%
  group_by(pmid) %>%
  slice(1) %>%
  select(pmid, date, type) %>%
  mutate(reason = 'Duplicate') %>%
  ungroup()
excluded.abstracts = bind_rows(excluded.abstracts, ex.duplicates)
abstracts = filter(abstracts, !pmid %in% to.remove) # remove from data

## remove empty document types
no.type = filter(abstracts, is.na(type) == TRUE) %>%
  select(pmid, date, type) %>%
  mutate(reason = 'No article type')
excluded.abstracts = bind_rows(excluded.abstracts, no.type)
# now remove from data
abstracts = filter(abstracts, !pmid %in% no.type$pmid) 

## combine article types to create more manageable list
abstracts = combine.types(abstracts) # from 99_combine_article_types.R
excluded.abstracts = combine.types(excluded.abstracts)

## exclude prior to 1950, just not enough papers
exclusion.date = as.Date('1950-01-01')
## add to exclusions
ex.abstracts = filter(abstracts, date < exclusion.date) %>%
  select(pmid, date, type) %>%
  mutate(reason = 'Pre 1950')
excluded.abstracts = bind_rows(excluded.abstracts, ex.abstracts)
## now exclude from titles and abstracts
abstracts = filter(abstracts, date >= exclusion.date)

# save
setwd("U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/text.mining/narrator")
save(abstracts, excluded.abstracts, file='data/for_analysis.RData')
