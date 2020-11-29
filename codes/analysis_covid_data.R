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

# Add log of confirmed cases and log of number of death
df <- df %>% 
  mutate(ln_confirmed = log(confirmed),
         ln_death = log(death))

######
# Make some models:
#     reg1: ln_death = alpha + beta * ln_confirmed
#     reg2: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2
#     reg3: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2 + beta_3 * ln_confirmed^3
# weighted-ols:
#     reg4: ln_death = alpha + beta * ln_confirmed, weights: population

# exclude countries with negative logs
df <- df %>% filter( ln_death >= 0)


# First model: Simple Linear Regression for ln_confirmed - ln_death
reg1 <- lm_robust(ln_death ~ ln_confirmed, data = df, se_type = "HC2")
reg1
# Summary statistics
summary(reg1)
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# To handle polynomials: 
# Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_confirmed_sq = ln_confirmed^2,
                     ln_death_sq = ln_death^2)


# Second model: Quadratic (linear) regression for ln_confirmed - ln_death
reg2 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )


# Third model: Piecewise linear spline regression

# 1st define the cutoff for gdp per capita
cutoff <- c(8,12,14) # or use c(10, 50)
# 2nd we use a log transformation -> cutoff needs to be transformed as well
# Use simple regression with the lspline function
reg3 <- lm_robust(ln_death ~ lspline( ln_confirmed , cutoff), data = df )
summary( reg3 )
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) +
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x, cutoff) , method = lm , color = 'red' )


# Fourth model: Weighted linear regression, using population as weights.
reg4 <- lm_robust(ln_death ~ ln_confirmed, data = df , weights = population)
summary( reg4 )

ggplot(data = df, aes(x = ln_confirmed, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 15)) +
  labs(x = "Number of confirmed cases (ln scale) ",y = "Number of death cases, (ln scale)")

#####
# Creating model summary with texreg

data_out <- "../out/"

htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("GDP total - linear","GDP total - quadratic","GDP total - cubic",
                                "GDP/capita - linear","GDP/capita - quadratic","GDP/capita - PLS",
                                "GDP/capita - weighted linear"),
         caption = "Modelling life expectancy and different wealth measures of countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

