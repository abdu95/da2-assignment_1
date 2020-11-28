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

####
# 
# Quick check on all HISTOGRAMS
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

######
# Check basic scatter-plot:
#     No_of_death = alpha + beta * No_of_confirmed
#
# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#
# No_of_death - No_of_confirmed: level-level model without scaling
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
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# changing the scale for No_of_death for checking log-transformation: log y - level x 
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020)",y = "Number of registered death (ln scale)") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# changing the scale for No_of_death and No_of_confirmed for checking log-transformation: log y - log x
ggplot( df , aes(x = confirmed, y = death ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020, ln scale)",y = "Number of registered death (ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans() )


####
# Conclusions:

# - Substantive: unit of measurement
# - Statistical: avoiding long-right tail, more symmetric



