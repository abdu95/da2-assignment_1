#######################
##  DA2 and Coding   ##
##    Assignment     ##
##    Analysis of    ##
## registered case & ##
##  registered death ##
##                   ##
##      NO. 3        ##
##   Analysis of     ##
##    the data       ##
#######################

# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)

# the file path 
my_file <- "../data/clean/covid_pop_09_11_2020_clean.csv"

# make dataframe from a file
df <- read_csv(my_file)

# drop extreme values
df <- subset(df, df$population < 300000000)


#' 4 (c) Check all your variables - with the help of histograms, summary statistics and checking extreme values - 
#' and make a conscious decision on which observation(s) to drop.

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()


summary( df )

# df %>% filter(!complete.cases(df))

#' 4. f) Check and report your distributions for y and x variables: 
#' use histograms and summary statistics table (mean, median, min, max, standard deviation)

summary_stats_death <- df %>% summarise(
  mean = mean(death),
  median = median(death),
  std = sd(death),
  min = min(death),
  max = max(death))

summary_stats_confirmed <- df %>% summarise(
  mean = mean(confirmed),
  median = median(confirmed),
  std = sd(confirmed),
  min = min(confirmed),
  max = max(confirmed))

#' (g) Check the possible different ln transformation for the variables with plotting 
#' different scatterplots with lo(w)ess. 
#' Make a substantive and statistical reasoning, where and when to use ln transformation. 
#' You do not need to fit any model here, only use statistical reasoning based on the graphs.
#' i. Take care when it is possible to make ln transformation: 
#' you may need to drop or change some variables.


######
# Check basic scatter-plot:
#     No_of_death = alpha + beta * No_of_confirmed
#
# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#
# No_of_death - No_of_confirmed: level-level model without scaling
# model is not capturing the data very well. model does not fit the data
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020)",y = "Number of registered death") 


# changing the scale for No_of_death for checking log-transformation: level y - log x 
# looks good
# log log and level log
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020, ln scale)",y = "Number of registered death") +
  scale_x_continuous( trans = log_trans() )

# changing the scale for No_of_death for checking log-transformation: log y - level x 
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020)",y = "Number of registered death (ln scale)") +
  scale_y_continuous( trans = log_trans() )

# changing the scale for No_of_death and No_of_confirmed for checking log-transformation: log y - log x
ggplot( df , aes(x = confirmed, y = death ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020, ln scale)",y = "Number of registered death (ln scale)") +
  scale_x_continuous( trans = log_trans() )+
  scale_y_continuous( trans = log_trans() )


####
# Conclusions:

# - Substantive: unit of measurement
# - Statistical: avoiding long-right tail, more symmetric



