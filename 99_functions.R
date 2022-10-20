# 99_functions.R
# June 2022

# function to round with trailing zeros
roundz  = function(x, digits=0){formatC( round( x, digits ), format='f', digits=digits)}

# function to get the annual statistics and plot trends over time
make_trend_plot = function(indata, 
                           label = NULL,
                           category_labels = NULL,
                           legend_position = NULL, # optional legend position
                           add_ggtitle = FALSE, # add ggtitle
                           start_y_axis = NULL, # start the y-axis at zero?
                           min_abstracts = 50, # minimum number of abstracts for plot
                           outcome){
  # colour palettes
  cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",'skyblue','dark red') # added a few more
  colors_two_outcomes = c("#FFC125", "#9F79EE")
  
  # plotting multiple outcomes?
  multiple = FALSE
  if(length(outcome) > 1){
    multiple = TRUE
  }
  
  # label
  if(is.null(label) == TRUE){label = outcome[1]}
  
  # rename outcome variable
  for (k in 1:length(outcome)){
    index = names(indata) == outcome[k]
    names(indata)[index] = paste('outcome', k, sep='')
  }
  # counts or binary outcome
  if(class(indata$outcome1) != 'logical'){
    ylabel = 'Per 100 words'
    # make rate per 100 words
    indata = mutate(indata,
                    year = as.numeric(format(date, '%Y')),
                    dependent1 = 100*(outcome1 / n.words))
    if(multiple==TRUE){ # add second variable
      indata = mutate(indata, dependent2 = 100*(outcome2 / n.words))
    }
    make_percent = FALSE
  }
  if(class(indata$outcome) == 'logical'){
    ylabel = 'Percentage of abstracts'
    # not standardised per 100
    indata = mutate(indata,
                    year = as.numeric(format(date, '%Y')),
                    dependent1 = as.numeric(outcome1))
    make_percent = TRUE
  }
  
  ## plot 1: all article types combined
  if(multiple==FALSE){
    to_plot = group_by(indata, year) %>%
      summarise(n = n(),
                sd1 = sd(dependent1),
                mean1 = mean(dependent1)) 
  }
  if(multiple==TRUE){
    to_plot = group_by(indata, year) %>%
      summarise(n = n(),
                sd1 = sd(dependent1),
                mean1 = mean(dependent1),
                sd2 = sd(dependent2),
                mean2 = mean(dependent2)) 
  }
  to_plot = mutate(to_plot,
      z = qt(0.975, df = n-1),
      sem1 = sd1/sqrt(n),
      lower1 = mean1 - (z*sem1), # confidence interval for the mean
      upper1 = mean1 + (z*sem1),
      lower1 = ifelse(lower1<0, 0, lower1))
  if(multiple == TRUE){
    to_plot = mutate(to_plot,
       sem2 = sd2/sqrt(n),
       lower2 = mean2 - (z*sem2), # confidence interval for the mean
       upper2 = mean2 + (z*sem2),
       lower2 = ifelse(lower2<0, 0, lower2))
  }
  
  # convert to percent
  if(make_percent == TRUE){
    to_plot = mutate(to_plot, 
                     mean1 = mean1*100,
                     lower1 = lower1*100,
                     upper1 = upper1*100)
    if(multiple == TRUE){
      to_plot = mutate(to_plot, 
                       mean2 = mean2*100,
                       lower2 = lower2*100,
                       upper2 = upper2*100)
    }
  }
  
  # transform from wide to long for multiple outcomes
  to_plot_long = select(to_plot, year, mean1, lower1, upper1) %>%
    rename('mean' = 'mean1',
           'lower' = 'lower1',
           'upper' = 'upper1') %>%
    mutate(outnum = 1)
  if(multiple==TRUE){
    to_plot_long2 = select(to_plot, year, mean2, lower2, upper2) %>%
      rename('mean' = 'mean2',
             'lower' = 'lower2',
             'upper' = 'upper2') %>%
      mutate(outnum = 2)
    to_plot_long = bind_rows(to_plot_long, to_plot_long2) %>%
      mutate(outlabel = factor(outnum, levels=1:length(outcome), labels=outcome))
  }
  
  # plot
  if(multiple==FALSE){
  cplot = ggplot(data=to_plot_long, aes(x=year, y=mean, ymin=lower, ymax=upper))+
    geom_ribbon(alpha=0.2)+
    geom_line(size=1.04)+
    xlab('Year')+
    ylab(ylabel)+
    theme_bw()+
    theme(panel.grid.minor = element_blank())
  }
  if(multiple==TRUE){
    cplot = ggplot(data=to_plot_long, aes(x=year, y=mean, ymin=lower, ymax=upper, col=outlabel))+
      geom_ribbon(alpha=0.2)+
      geom_line(size=1.04)+
      scale_color_manual(NULL, values=colors_two_outcomes)+
      xlab('Year')+
      ylab(ylabel)+
      theme_bw()+
      theme(panel.grid.minor = element_blank(),
            legend.position = 'top')
  }
  if(is.null(category_labels) == FALSE){
    cplot = cplot + scale_color_manual(NULL, values=colors_two_outcomes, labels = category_labels)
  }
  if(add_ggtitle == TRUE){
    cplot = cplot + ggtitle(str_to_title(label))
  }
  if(is.null(start_y_axis) == FALSE){
    cplot = cplot + scale_y_continuous(limits=c(start_y_axis, NA))
  }
  if(is.null(legend_position) == FALSE){
    cplot = cplot + theme(legend.position = legend_position)
  }

  ## plot 2: stratified by article type
  if(multiple==FALSE){
    to_plot = group_by(indata, year, type) %>%
      summarise(n = n(),
                sd1 = sd(dependent1),
                mean1 = mean(dependent1)) 
  }
  if(multiple==TRUE){
    to_plot = group_by(indata, year, type) %>%
      summarise(n = n(),
                sd1 = sd(dependent1),
                mean1 = mean(dependent1),
                sd2 = sd(dependent2),
                mean2 = mean(dependent2)) 
  }
  to_plot = filter(to_plot, n >= min_abstracts) %>% # remove years with less than this many abstracts
    mutate(z = qt(0.975, df = n-1),
           sem1 = sd1/sqrt(n),
           lower1 = mean1 - (z*sem1), # confidence interval for the mean
           upper1 = mean1 + (z*sem1))
  if(multiple==TRUE){
    to_plot = mutate(to_plot, 
             sem2 = sd2/sqrt(n),
             lower2 = mean2 - (z*sem2), # confidence interval for the mean
             upper2 = mean2 + (z*sem2))
  }
  # convert to percent
  if(make_percent == TRUE){
    to_plot = mutate(to_plot, 
                     mean1 = mean1*100,
                     lower1 = lower1*100,
                     upper1 = upper1*100)
    if(multiple==TRUE){
      to_plot = mutate(to_plot, 
                       mean2 = mean2*100,
                       lower2 = lower2*100,
                       upper2 = upper2*100)
    }
  }
  
  # transform from wide to long for multiple outcomes
  to_plot_long = select(to_plot, type, year, mean1, lower1, upper1) %>%
    rename('mean' = 'mean1',
           'lower' = 'lower1',
           'upper' = 'upper1') %>%
    mutate(outnum = 1)
  if(multiple==TRUE){
    to_plot_long2 = select(to_plot, type, year, mean2, lower2, upper2) %>%
      rename('mean' = 'mean2',
             'lower' = 'lower2',
             'upper' = 'upper2') %>%
      mutate(outnum = 2)
    if(is.null(category_labels)==TRUE){
      to_plot_long = bind_rows(to_plot_long, to_plot_long2) %>%
        mutate(outlabel = factor(outnum, levels=1:length(outcome), labels=outcome))
    }
    if(is.null(category_labels)==FALSE){
      to_plot_long = bind_rows(to_plot_long, to_plot_long2) %>%
        mutate(outlabel = factor(outnum, levels=1:length(outcome), labels=category_labels))
    }
  }

  # plot
  if(multiple==FALSE){
    splot = ggplot(data=to_plot_long, aes(x=year, y=mean, col=factor(type)))+
      scale_color_manual(NULL, values=cbPalette)+
      geom_line(size=1.04)+
      xlab('Year')+
      ylab(ylabel)+
      theme_bw()+
      theme(panel.grid.minor = element_blank())
  }
  if(multiple==TRUE){
    splot = ggplot(data=to_plot_long, aes(x=year, y=mean, col=factor(type)))+
      scale_color_manual(NULL, values=cbPalette)+
      geom_line(size=1.04)+
      xlab('Year')+
      ylab(ylabel)+
      theme_bw()+
      theme(panel.grid.minor = element_blank())+
      facet_wrap(~outlabel, nrow = 2)
  }
  if(add_ggtitle == TRUE){
    splot = splot + ggtitle(str_to_title(label))
  }
  if(is.null(start_y_axis) == FALSE){
    cplot = cplot + scale_y_continuous(limits=c(start_y_axis, NA))
  }
  
  # return
  to_return = list()
  to_return$combined = cplot
  to_return$strata = splot
  return(to_return)
}


# function to get the annual statistics and plot trends over time
# version by country
make_trend_plot_country = function(indata, 
                           label = NULL,
                           add_ggtitle = FALSE, # add ggtitle
                           start_y_axis = NULL, # start the y-axis at zero?
                           min_abstracts = 50, # minimum number of abstracts for plot
                           outcome){
  # colour palettes
  cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",'skyblue','dark red') # added a few more
  colors_two_outcomes = c("#FFC125", "#9F79EE")
  
  # label
  if(is.null(label) == TRUE){label = outcome[1]}
  
  # rename outcome variable
  for (k in 1:length(outcome)){
    index = names(indata) == outcome[k]
    names(indata)[index] = paste('outcome', k, sep='')
  }
  # counts or binary outcome
  if(class(indata$outcome1) != 'logical'){
    ylabel = 'Per 100 words'
    # make rate per 100 words
    indata = mutate(indata,
                    year = as.numeric(format(date, '%Y')),
                    dependent = 100*(outcome1 / n.words))
    make_percent = FALSE
  }
  if(class(indata$outcome) == 'logical'){
    ylabel = 'Percentage of abstracts'
    # not standardised per 100
    indata = mutate(indata,
                    year = as.numeric(format(date, '%Y')),
                    dependent = as.numeric(outcome1))
    make_percent = TRUE
  }
  
  ## plot by country
  to_plot = group_by(indata, year, country) %>%
    summarise(n = n(),
              sd = sd(dependent),
              mean = mean(dependent)) %>%
    ungroup()
  to_plot = to_plot = filter(to_plot, n >= min_abstracts) %>% # remove years with less than this many abstracts
    mutate(z = qt(0.975, df = n-1),
           sem = sd/sqrt(n),
           lower = mean - (z*sem), # confidence interval for the mean
           upper = mean + (z*sem),
           lower = ifelse(lower<0, 0, lower))

  # convert to percent
  if(make_percent == TRUE){
    to_plot = mutate(to_plot, 
                     mean = mean*100,
                     lower = lower*100,
                     upper = upper*100)
  }
  
  # plot
  cplot = ggplot(data=to_plot, aes(x=year, y=mean, col=factor(country)))+
      geom_line(size=1.04)+
    scale_color_manual(NULL, values=colours)+
      xlab('Year')+
      ylab(ylabel)+
      theme_bw()+
      theme(panel.grid.minor = element_blank(),
            legend.position = 'right')
  if(add_ggtitle == TRUE){
    cplot = cplot + ggtitle(str_to_title(label))
  }
  if(is.null(start_y_axis) == FALSE){
    cplot = cplot + scale_y_continuous(limits=c(start_y_axis, NA))
  }

  # return
  return(cplot)
}

