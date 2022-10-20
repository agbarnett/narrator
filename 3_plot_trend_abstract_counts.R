# 3_plot_trend_abstract_counts.R
# plot the number of included abstracts over time
# included in 3_plot_trends.Rmd
# July 2022
library(ggplot2)
library(gridExtra) # for grid.arrange
library(dplyr)
library(tidyr)

# get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R

## counts by year
count_abtracts = mutate(abstracts, 
                      year = as.numeric(format(date, '%Y'))) %>%
  group_by(year) %>%
  summarise(n = n(), nj = length(unique(jabbrv))) %>%
  ungroup()

# plot
for_plot = filter(count_abtracts, year >= 1900)
breaks = c(10,100,1000,10000,100000,800000)
cplot = ggplot(data=for_plot, aes(x=year, y=n))+
  geom_line(size=1.04)+
  xlab('Year')+
  ylab('Number of abstracts')+
  theme_bw()+
  scale_y_log10(breaks=breaks, labels = function(x) format(x, big.mark=',', scientific=FALSE))+
  theme(panel.grid.minor = element_blank())
outfile = 'figures/abstract.counts.jpg'
jpeg(outfile, width=5.5, height=3.75, units='in', res=400, quality=100)
print(cplot)
dev.off()
