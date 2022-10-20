# 99_find_abstract_for_figure.R
# find a good abstract to use in a figure to show the data extraction process
# July 2022
library(dplyr)
library(stringr)
source('99_key_characters_words_phrases.R')

# find an abstract that has everything
filter(abstract.data,
       hedging > 0,
       signposting  > 0,
       narrator == TRUE,
       noun_chunks > 0,
       (male>0 | female>0)) %>%
  sample_n(1)

# look at example and find exa
example_pmid = 20643696 
infile = 'raw/unprocessed.pubmed.baseline.0666.RData'
load(infile)
abstract = filter(raw_pubmed, pmid == example_pmid) %>% pull(abstract)

## hedging
index = str_locate_all(tolower(abstract), pattern = to_search_hedging)[[1]]
for (k in 1:nrow(index)){
  cat(str_sub(abstract, start=index[k,1], end=index[k,2]), '\n')
}

## signposting
index = str_locate_all(tolower(abstract), pattern = to_search_signposting)[[1]]
for (k in 1:nrow(index)){
  cat(str_sub(abstract, start=index[k,1], end=index[k,2]), '\n')
}

## narrator
index = str_locate_all(abstract, pattern = to_search_narrator)[[1]]
for (k in 1:nrow(index)){
  cat(str_sub(abstract, start=index[k,1], end=index[k,2]), '\n')
}

## noun chunks
annotated <- udpipe_annotate(ud_model, x = abstract)
annotated_frame = as.data.frame(annotated) 
for_chunks = filter(annotated_frame,
                    !(upos=='PUNCT' & str_detect(xpos, pattern="'|`")), # remove quotes before counting noun chunks
                    !(upos=='PUNCT' & xpos=='HYPH')) # remove hyphens
pasted = paste(for_chunks$upos, collapse='/')
index = str_locate_all(pasted, pattern='NOUN/NOUN/NOUN')[[1]]
str_count(str_sub(pasted, 1, index[1,1]),'/') # word count for start of noun chunk

## gender
index = str_locate_all(abstract, pattern = to_search_male)[[1]]
for (k in 1:nrow(index)){
  cat(str_sub(abstract, start=index[k,1], end=index[k,2]), '\n')
}
