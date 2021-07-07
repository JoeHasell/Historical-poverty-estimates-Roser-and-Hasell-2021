library(tidyverse)


# LOAD PREPARED GDP, POPULATION AND GINI DATA -----

load("Manipulated data/GDP_pop_bm_years.Rda")
load("Manipulated data/gini_bm_years.Rda")

# select scenario and data source and merge GDP and gini datasets

gini_source<- "baseline_GCIP_income_vZ"

# select the gini data series
gini_data <- gini_bm_years %>%
  rename(gini = !!gini_source) %>%
  select(Country, Benchmark_year, gini)

# select the data for the selected GDP multiplier
GDP_data<- GDP_pop_bm_years %>%
  select(Country, Benchmark_year, GDP.per.capita, Population, Region)

# merge gini and GDP data and select year
GDP_and_ginis<-left_join(GDP_data, gini_data) %>%
  filter(Benchmark_year == 1980) %>%
  ungroup()




# FIND P25, P50 AND P75 OF GLOBAL DISTRIBUTION IN YEAR ------


# This function calculates poverty headcounts on a dataset for a given 
  # pov threshold. It is used subsequently to find quantiles of the distribution.
test_povline<- function(df, poverty_threshold) {
# Calculate standard deviation from Gini (on assumption of log-normality)

#   Gini = 2 * phi(sd/sqrt(2)) - 1
# ...where phi(z) is the cumulative standard normal
#
# -> sd = sqrt(2) * invnormal((Gini +1)/2)

# R's inverse standard normal CDF is given by: qnorm(p, mean=0, sd=1)

  df<- df %>%
  mutate(sd = 2^(1/2) * qnorm(((gini + 1)/2), 0 , 1))

# calculate mu (mean of ln(X))

#   mean of a log normal is e^(mu + (sd^2)/2)
# -> mu = ln(mean) - (sd^2)/2

  df<- df %>%
  mutate(mu = log(GDP.per.capita) - (sd^2)/2)


# Calculate share below poverty line:
# log normal cumulative dist defined given by:
#  F(x) = p = P(x < X) = normal((ln(`x') - `mu')/ `sd')

  df<- df %>%
  mutate(poverty_rate = pnorm((log(poverty_threshold * 365) - mu) / sd) * 100,# fraction below threshold
         poverty_threshold = paste0("$", poverty_threshold, "_per_day"
         ))

  return(df)
}



# Run function iteratively, adjusting poverty_line to find a given quartile:
poverty_line<- 5.20

check<- test_povline(GDP_and_ginis, poverty_line) 
world_poor_check<- check %>%
  summarise(pov_rate = weighted.mean(poverty_rate,Population))


# In 1980:
# P75 is $23.77
# P50 is $6.48
# P25 is $3.05

# Povcal global share on less than $1.90 a day in 1981 was 42.48%, and in 
# 1982 was 42.08%. A National accounts poverty line of $5.20 yields a poverty
# rate of 43.04% in 1980 - roughly a continuation of the same linear trend.

