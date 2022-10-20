# 0_read_pubmed_api.R
# read data from pubmed into R using batch queries
# May 2022
library(XML)
library(dplyr)
library(textclean) # for replace_non_ascii
library(stringr)
library(easyPubMed) # for new_PM_df which imports XML into R
source('99_table_articles_byAuth_adapted.R') # use my adapted versions of this code
source('99_article_to_df_adapted.R') # faster without author data
source('98_key_not_sharing.R') # for my key

## create search query to find relevant abstracts
# download in batch, see https://www.data-pulse.com/projects/Rlibs/vignettes/easyPubMed_01_getting_started.html
year = 2022 # TO DO, loop through years
new_query = paste("hasabstract AND ", year,"[PDAT]", sep='') # with an abstract
# creates 
out = batch_pubmed_download(pubmed_query_string = new_query, 
                            dest_dir = 'xml', # subfolder
                            format = "xml", 
                            api_key = my_ncbi_key,
                            batch_size = 4900, # max is 5000
                            encoding = "UTF8",
                            dest_file_prefix = paste(year, '_', sep=''))

# loop through batches
batches = dir('xml', pattern = paste('^', year,'_', sep='')) # find all the batches for this year in the subfolder

# import the XML file into R - takes a while
for (batch in batches){
  file = paste('xml/', batch, sep='') # in subfolder
  source('0_run_read.R')
}
 
