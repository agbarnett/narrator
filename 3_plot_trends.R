# 3_plot_trends.R
# runs the trend plot for all journals combined and specific journals
# September 2022
library(rmarkdown)
library(stringr)

## get the data
load('data/for_analysis.RData') # from 2_concatenate_processed_data.R

### Part 1: overall trends ###

# overall
export = TRUE # export figures
legend_position = c(0.80, 0.15) # for male/female legend (bottom-right)
render(input = '3_plot_trends.Rmd',
       output_format = 'word_document',
       output_file = '3_plot_trends_overall.docx')

### Part 2: trends by journal ###

# journals to plot trends for:
journals = read.table(header=TRUE, sep=',', text='
journal,jabbrv
Nature,Nature
Science,Science
Transactions of Proceedings of the Royal Society B,Proc Biol Sci
Journal of Biological Chemistry,J Biol Chem
JACS (Journal of the American Chemical Society),J Am Chem Soc')
## loop through journals
export = FALSE # do not export graphs
legend_position = c(0.80, 0.80) # for male/female legend (top-right)
for (k in 1:nrow(journals)){
  load('data/for_analysis.RData') # from 2_concatenate_processed_data.R
  abstracts = filter(abstracts, jabbrv == journals$jabbrv[k])
  this_journal_no_space = str_replace_all(journals$jabbrv[k], pattern = ' ', replacement = '_')
  outfile = paste('3_plot_trends_', this_journal_no_space, '.docx', sep='')
  render(input = '3_plot_trends.Rmd',
       output_format = 'word_document',
       output_file = outfile)
}

# not on pubmed:
# Journal of Zoology,J Zool
# not enough papers:
# Plant Sciences,Plant Sci

### Part 3: trends by country ###

load('data/for_analysis.RData') # re-load; from 2_concatenate_processed_data.R
export = FALSE
render(input = '3_plot_trends_country.Rmd',
       output_format = 'word_document',
       output_file = '3_plot_trends_country.docx')
