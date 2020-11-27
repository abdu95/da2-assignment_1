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


# Call the data from file
my_file <- "../data/clean/covid_pop_09_11_2020_clean.csv"
df <- read_csv(my_file)
summary( df )
