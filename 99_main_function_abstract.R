# 99_main_function_abstract.R
# main function to extract the text features from abstracts
# some code copied from https://github.com/agbarnett/stats_section/blob/master/code/plosone/5_process_stats_section.R
# used by 1_process_pubmed.R
# July 2022

## Section 2: function ##
process_abstract = function(indata, k){

# process the abstract
abstract = indata$abstract[k]
#abstract = str_replace_all(abstract, pattern = to.replace, replacement = ' ') # remove fractions, superscripts, etc; use space so that sub/super-script and word are separated
abstract = str_replace_all(abstract, pattern = bogus.acronyms.abstract, replacement = ' ') # replaced and not added to word count

# replace common symbols
abstract = str_replace_all(string=abstract, pattern=' = ', replacement = ' equal to ') 
abstract = str_replace_all(string=abstract, pattern=' < ', replacement = ' less than ') 
abstract = str_replace_all(string=abstract, pattern='&lt;', replacement = ' less than ') # add spaces because fixed by str_squish
abstract = str_replace_all(string=abstract, pattern=' > ', replacement = ' greater than ')
abstract = str_replace_all(string=abstract, pattern='&gt;', replacement = ' greater than ')
abstract = str_replace_all(string=abstract, pattern=' & ', replacement = ' and ')
abstract = str_replace_all(string=abstract, pattern='&amp;', replacement = ' and ')
abstract = str_replace_all(string=abstract, pattern="\\s*(±)\\s*|\\s*(\\+/-+)\\s*", replacement = " plus or minus ")

# replace slash, not working!
#abstract = str_replace_all(string=abstract, pattern='\\\"', replacement = '"')
# does it matter, what does it look like below??

# turned off
#stats_section = stats_section %>% mutate(text_data_clean = replace_symbol(text_data_clean))

# remove double spaces; and spaces at start and end
abstract = str_squish(abstract)
abstract = str_replace_all(abstract, pattern='\\( ', '\\(')
abstract = str_replace_all(abstract, pattern=' \\)', '\\)')

# remove publication date onwards
is_pub_date = str_locate(pattern = 'Expected final online publication date', string=abstract)
if(any(!is.na(is_pub_date))){
  abstract = str_sub(abstract, 1, is_pub_date[1,1] - 1)
}

## writing features
# a) hedging - lower case
hedging = str_count(tolower(abstract), pattern = to_search_hedging)
# b) Signposting - lower case 
signposting = str_count(tolower(abstract), pattern = to_search_signposting)
# c) narrator - case sensitive so do not use `tolower`
narrator = str_detect(abstract, pattern = to_search_narrator) # just a yes/no
# d) noun chunks
annotated <- udpipe_annotate(ud_model, x = abstract)
annotated_frame = as.data.frame(annotated) 
for_chunks = filter(annotated_frame,
                    !(upos=='PUNCT' & str_detect(xpos, pattern="'|`")), # remove quotes before counting noun chunks
                    !(upos=='PUNCT' & xpos=='HYPH')) # remove hyphens
pasted = paste(for_chunks$upos, collapse='/')
noun_chunks = str_count(pasted, pattern='NOUN/NOUN/NOUN')
# e) count all types Universal part-of-speech tag (see https://universaldependencies.org/u/pos/index.html)
universal = group_by(annotated_frame, upos) %>%
  tally() %>%
  pivot_wider(values_from = 'n', names_from = 'upos')
# f) copyright - used to find errors
copyright = str_count(tolower(abstract), pattern = to_search_copyright) # 
# g) male gender - lower case
male = str_count(tolower(abstract), pattern = to_search_male) # 
# h) female gender - lower case
female = str_count(tolower(abstract), pattern = to_search_female) # 
# i) misspellings - lower case
spelling = str_count(tolower(abstract), pattern = to_search_spelling) # 
# j) negative control
control = str_count(tolower(abstract), pattern = to_search_negative_control) # 
# count words
n.words = str_count(abstract, '\\w+')

# flag if abstract appears to include a citation
find.jabbrv = str_detect(string = abstract, pattern = indata$jabbrv[k])[[1]] # is the first author's name in the abstract
find.name = str_detect(string = abstract, pattern = paste('\\b', indata$first.author[k], '\\b', sep = ''))[[1]] # is the first author's name in the abstract
citation = find.jabbrv & find.name

## return the results
tframe = data.frame(pmid = indata$pmid[k], 
          date = indata$date[k], 
          type = indata$type[k], 
          jabbrv = indata$jabbrv[k], 
          n.authors = indata$n.authors[k],
          country = indata$country[k],
          n.words = n.words, 
          hedging = hedging,
          signposting = signposting,
          narrator = narrator,
          noun_chunks = noun_chunks, 
          male = male, 
          female = female,
          spelling = spelling,
		  control = control,
          citation = citation, 
          copyright = copyright,
          stringsAsFactors = FALSE) %>%
  bind_cols(universal) # add universal counts
if(nrow(tframe)!=1){cat('error, wrong number of rows', indata$pmid[k], '.\n', sep='')}
return(tframe)

}
