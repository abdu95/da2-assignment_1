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

# Call the data from file
my_file <- "../data/clean/covid_pop_09_11_2020_clean.csv"
df <- read_csv(my_file)


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

what <- select(df, df$population < 300000000)
what <- subset(df, df$population < 300000000)
######
# Check basic scatter-plot:
#     No_of_death = alpha + beta * No_of_cases
#
# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#
# 1) death - No_of_cases: level-level model without scaling
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case (31 October 2020)",y = "Number of registered death") 
