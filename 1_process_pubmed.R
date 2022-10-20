# 1_process_pubmed.R
# process the pubmed data to extract text features from the abstract
# moved to lyra
# August 2022
library(udpipe) # for noun strings
ud_model <- udpipe_download_model(language = "english-ewt")
ud_model <- udpipe_load_model(ud_model$file_model)
library(stringr)
library(dplyr)
library(tidyr)
source('99_main_function_abstract.R') # main function for abstracts
source('99_key_characters_words_phrases.R')

# load data for further processing.
files_to_loop = dir('raw', pattern='baseline')
number = 0
for (file in files_to_loop){  # TEMPORARY
  
  number = number + 1
  if(number%%10 == 0){cat(paste('Up to number', number, '\r'))} # progress bar

  ## file names
  # check if the processed file already exists
  file_number = str_remove_all(file, 'unprocessed\\.pubmed\\.baseline\\.|\\.RData')
  if(length(dir('processed', pattern = file_number) > 0)){
    next 
  }
  infile = paste('raw/', file, sep='') # from 0_read_pubmed_api.R
  load(infile)
  
  ## process the papers in a large loop
  abstract.data = excluded.abstracts = NULL # start with empty data sets
  for (k in 1:nrow(raw_pubmed)){ # loop through abstracts
    
    # abstracts
    abstract.empty = FALSE
    # don't even start if abstract is empty or is very short
    if(is.na(raw_pubmed$abstract[k]) == TRUE | raw_pubmed$abstract[k]=='' | raw_pubmed$abstract[k]==' ' | 
       tolower(raw_pubmed$abstract[k])=='n/a' | tolower(raw_pubmed$abstract[k])=='n/a.'|
       tolower(raw_pubmed$abstract[k])=='no abstract available' | tolower(raw_pubmed$abstract[k])=='no abstract available.'){abstract.empty = TRUE}
    if(abstract.empty==FALSE){
      n.words = str_count(raw_pubmed$abstract[k], ' ') # rough word count
      if(is.na(n.words) == TRUE){n.words = 0}
      if(n.words <= 10 & raw_pubmed$type[k] == 'Published Erratum'){abstract.empty = TRUE} # short abstracts with errata are usually just a citation/note
      if(n.words <= 10 & str_detect(string=raw_pubmed$abstract[k], pattern='This corrects the article')) {abstract.empty = TRUE} # alternative search for errata (in case they are listed as type 'journal article')
    }
    if(abstract.empty == TRUE){
      this.exclude = data.frame(pmid=raw_pubmed$pmid[k], date=raw_pubmed$date[k], type=raw_pubmed$type[k], reason='No abstract', stringsAsFactors = FALSE)
      excluded.abstracts = bind_rows(excluded.abstracts, this.exclude) 
    }
    if(abstract.empty == FALSE){
      aresults = process_abstract(indata=raw_pubmed, k=k) # using the main function 99_main_function_abstract.R
      # concatenate the data
      abstract.data = bind_rows(abstract.data, aresults) 
      remove(aresults) # tidy up
    }
    
    #
    if(k%%500 == 0){cat(paste('Up to abstract', k, '\r'))} # progress bar
  }
  
  # replace NAs with zeros for estimates from Universal part-of-speech tag
  tag_vars = c('ADJ', 'ADP', 'ADV', 'AUX', 'CCONJ', 'DET', 'NOUN', 'NUM', 'PART', 'PRON', 'PROPN', 'PUNCT', 'SCONJ', 'VERB', 'SYM', 'X', 'INTJ')
  abstract.data = mutate_at(abstract.data,
                             vars(all_of(tag_vars)),  replace_na, 0)
  
  # check if sum of tags is much greater than words
  tag_vars = tag_vars[tag_vars !='PUNCT'] # remove punctuation
  check = group_by(abstract.data, pmid, n.words) %>%
    select(pmid, n.words, all_of(tag_vars)) %>%
    pivot_longer(cols = all_of(tag_vars)) %>%
    summarise(count = sum(value)) %>%
    filter(count > n.words*1.05)

  # save
  outfile = paste('processed/pubmed.', file_number, '.RData', sep='') # 
  save(abstract.data, excluded.abstracts, file=outfile)
  
}

# create random checks
output_file = 'checks.docx'
rmarkdown::render(input = '99_random_checks.Rmd', 
                  output_format='word_document',
                  output_file = output_file)
