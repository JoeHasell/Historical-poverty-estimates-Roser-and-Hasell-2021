library(tidyverse)

# Our benchmark years
  # Set so as to coincide as much as possible with the benchmarks used in
    # the source data.

benchmarks<- c(1820, 1850, 1870, 
               1890, 1910, 1929, 
               1950, 1960, 1970, 
               1980, 1990, 2000,
               2005, 2010, 2017)




# IMPORT ORIGINAL DATA -----

# country regions
df_regions<- read.csv("Original data/Country regions.csv")


# Load and tidy Maddison 2020
fp <- "Manipulated data/mpd2020 (Reformatted and country names standardized).csv"
df_MDP<- read.csv(fp)

df_MDP[df_MDP == 0] <- NA # Some NAs for population and GDP per cap get imported as zeros

# Separate regional data out from country data
MDP_region_entities<- c("East Asia",
                        "South and South-East Asia",
                        "Eastern Europe",
                        "Latin America",
                        "Middle East",
                        "Sub-Sahara Africa",
                        "Western Europe",
                        "Western Offshoots",
                        "World")

df_MDP_regions<- df_MDP %>% # store region data here
  filter(Country %in% MDP_region_entities)

df_MDP<- df_MDP %>% # now with only country data
  filter(!Country %in% MDP_region_entities)


   


# de la Ecosura growth figures for African countries

# read data
fp<-"Manipulated data/de la Ecosura (2013) growth estimates for African countries (country names standardized).csv"
df_Ecosura<- read.csv(fp)

# This dataset provides growth rates by period. But these periods do not
# map onto our benchmark years.
# We calculate average growth between our benchmarks by aggregating across
# subperiods available in the de la Ecosura (2013) data.


# 1870-1890 benchmarks

  # calculate total growth over the two available subperiods
df_Ecosura<- df_Ecosura %>%
  mutate(Index_1870_1880 = 1 * (1 + Growth.rate.1870.1880/100)^10)

df_Ecosura<- df_Ecosura %>%
  mutate(Index_1870_1890 = Index_1870_1880 * (1 + Growth.rate.1880.1890/100)^10)

#  Calculate (1+r)
# GDP_t = GDP_0 x (1 + r)^t
# -> (1 + r)^t = GDP_t/GDP_0
# -> (1+r) = (GDP_t/GDP_0)^(1/t)

df_Ecosura<- df_Ecosura %>%
  mutate(average_growth_1870 = (Index_1870_1890/1)^(1/20))


# 1890-1910 benchmarks
  # NB the de la Ecosura data uses 1913 as a benchmark. We use this as a
  # proxy for 1910.

  # calculate total growth over the two available subperiods
df_Ecosura<- df_Ecosura %>%
  mutate(Index_1890_1900 = 1 * (1 + Growth.rate.1890.1900/100)^10)

df_Ecosura<- df_Ecosura %>%
  mutate(Index_1890_1913 = Index_1890_1900 * (1 + Growth.rate.1900.1913/100)^13)

df_Ecosura<- df_Ecosura %>%
  mutate(average_growth_1890 = (Index_1890_1913/1)^(1/23))


# 1910-1929 benchmarks
  # NB the de la Ecosura data uses 1913 as a benchmark. We use this as a
  # proxy for 1910.

df_Ecosura<- df_Ecosura %>%
  mutate(average_growth_1910 = 1 + Growth.rate.1913.1929/100)


# 1929-1950 benchmarks

df_Ecosura<- df_Ecosura %>%
  mutate(Index_1929_1938 = 1 * (1 + Growth.rate.1929.1938/100)^9)

df_Ecosura<- df_Ecosura %>%
  mutate(Index_1929_1950 = Index_1929_1938 * (1 + Growth.rate.1938.1950/100)^12)

df_Ecosura<- df_Ecosura %>%
  mutate(average_growth_1929 = (Index_1929_1950/1)^(1/21))

# Select needed vars
df_Ecosura<-  select(df_Ecosura, c(Country, 
                                   average_growth_1870,
                                   average_growth_1890,
                                   average_growth_1910,
                                   average_growth_1929))

# convert to long format ready to match with MDP observations for benchmark years
df_Ecosura<-gather(df_Ecosura, Benchmark_year, Ecosura_growth_rate, 
                   average_growth_1870:average_growth_1929)

# Make a numerical year var from the gathered column names
df_Ecosura<- df_Ecosura %>%
  mutate(Benchmark_year = as.integer(sub("average_growth_", "", Benchmark_year)))


# INTERPOLATE MISSING MDP DATA ---------

# Make an empty df of country-benchmark years
countries <- as.data.frame(unique(df_MDP$Country)) %>% 
  slice(rep(1:n(), each = length(benchmarks)))

long_benchmarks<- as.data.frame(rep(benchmarks,
                                    times=length(unique(df_MDP$Country))))

GDP_pop_bm_years<- cbind(countries,long_benchmarks)
names(GDP_pop_bm_years)<-c("Country","Benchmark_year")


# For GDP per cap and population, interpolate values forwards for 
  # benchmark years based on growth rates.

for(interp_varname in c("GDP.per.capita", "Population")){

# rename variable to be interpolated
df_MDP_int_var<- rename(df_MDP, interp_var = !!as.symbol(interp_varname))


# make df of country, year and interp_var (dropping NAs), with leading values
  # of interp_var and the year
df_MDP_int_var<- df_MDP_int_var %>%
  arrange(Country, Year) %>%
  select(Country, Year, interp_var) %>%
  filter(!is.na(interp_var)) %>%
  group_by(Country) %>%
  mutate(interp_var_lead = lead(interp_var, n = 1, default = NA),
         interp_var_lead_year = lead(Year, n = 1, default = NA))


# Calculate growth rates between observation and lead

# Var_t = Var_0 x (1 + r)^t
# -> (1 + r)^t = Var_t/Var_0
# -> (1+r) = (Var_t/Var_0)^(1/t)

df_MDP_int_var<- df_MDP_int_var %>%
  mutate(growth = (interp_var_lead/interp_var)^
                      (1/(interp_var_lead_year - Year)))
        

# For each benchmark year (apart form the first), find the closest 
  # observation prior to the benchmark and use this to predict the 
  # benchmark year value

for(bm in benchmarks[-(1)]) {
    
    df_bm<- df_MDP_int_var %>%
      filter(Year < bm) %>%
      group_by(Country) %>% 
      top_n(1, Year)
    
    df_bm<- df_bm %>%
      mutate(predicted_var = interp_var * growth^(bm-Year),
             Benchmark_year = bm) %>%
      select(Country, Benchmark_year, predicted_var)
    
    if(bm==benchmarks[2]){ # If the first (2nd in the vector)...
      df_predicted_var<- df_bm # store as a new df
    } else{
      df_predicted_var<- rbind(df_predicted_var,df_bm) # else append to aggregate
    }
}


# Merge in original and interpolated data to country_benchmarks df,
  # noting which observations are original and which interpolated

  # Merge in original observations 
df_MDP_int_var<- df_MDP_int_var %>%
  select(Country, Year, interp_var) %>%
  rename(Benchmark_year = Year)

GDP_pop_bm_years<- left_join(GDP_pop_bm_years, df_MDP_int_var)

# Note if observations are original

GDP_pop_bm_years<- GDP_pop_bm_years %>%
  mutate(interp_var_source = NA) %>%
  mutate(interp_var_source = replace(interp_var_source,
                                    !is.na(interp_var),
                                      "MDP"))

# Merge in the predicted values - (note that the 'predicted' values equal the 
# actual values provided in MDP where MDP provides an observation for the BM 
# year. But there are actual values for the first BM year that have not
# been predicted. 

GDP_pop_bm_years<- left_join(GDP_pop_bm_years, df_predicted_var)

# Note data is interpolated  
GDP_pop_bm_years<- GDP_pop_bm_years %>%
  mutate(interp_var_source = replace(interp_var_source,
                                     (is.na(interp_var) & !is.na(predicted_var)),
                                      "MDP Interpolated"))

# Replace any missing original values with interpolated values 
GDP_pop_bm_years<- GDP_pop_bm_years %>%
  mutate(interp_var = replace(interp_var,
                              is.na(interp_var),
                              GDP_pop_bm_years[
                                is.na(GDP_pop_bm_years$interp_var),
                                "predicted_var"]))


# Before exiting the loop over variables to be interpolated,
    # rename the temporary 'interp_var' variables to their actual names
GDP_pop_bm_years<- GDP_pop_bm_years %>%
  select(-predicted_var) %>%
  rename(!!interp_varname := interp_var,
         !!paste0(interp_varname,"_source") := interp_var_source )

}

# AFRICAN COUNTRIES: EXTRAPOLATE 1870-1950 USING DE LA ECOSURA GROWTH RATES ----

# merge in de la Ecosura growth rates
GDP_pop_bm_years<- left_join(GDP_pop_bm_years, df_Ecosura)

# Extrapolate GDP per cap for previous benchmark iteratively
for(i in 1:4){
  
  GDP_pop_bm_years<- GDP_pop_bm_years%>% 
    group_by(Country) %>% 
    mutate(GDP_lead = lead(GDP.per.capita, n = 1, default = NA),
           GDP_lead_year = lead(Benchmark_year, n = 1, default = NA))
  
  #  Growth rate calc
  # GDP_t = GDP_0 x (1 + r)^t
  # GDP_0 = GDP_t / (1+r)^t
  
  # Predict GDP per cap for the previous period
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Predicted.GDP.per.capita = GDP_lead / 
             Ecosura_growth_rate^(GDP_lead_year - Benchmark_year))
  
  # Note that this observation is (about to be) extrapolated in this way
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita_source = replace(GDP.per.capita_source,
                    (is.na(GDP.per.capita) & !is.na(Predicted.GDP.per.capita)),
                    "Extrapolated with de la Ecosura"))
  
  # Swap in the predicted value where currently missing, using dplyr coalesce
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita = coalesce(GDP.per.capita,Predicted.GDP.per.capita))
  
  # (Repeated to iterate back over the four periods)
}

# Drop unneeded variables
GDP_pop_bm_years<- select(GDP_pop_bm_years, 
                               c(Country, Benchmark_year, GDP.per.capita, Population, 
                                 GDP.per.capita_source, Population_source))



# EXTRAPOLATE REMAINING MISSING DATA -----
  # Based on growth rates observed in region or bloc

# Merge in regions data

GDP_pop_bm_years<- left_join(GDP_pop_bm_years, df_regions)

# drop individual countries of Czechoslovakia, as handled as aggregate
GDP_pop_bm_years<- GDP_pop_bm_years %>%
  filter(!Country %in% c("Czechia", "Slovakia"))


# Calculate regional pop and gdp per cap avg growth rates ------

# Calculate individual entity pop and GDP growth rates

growth_rates<- GDP_pop_bm_years %>%
  select(Country, Benchmark_year, GDP.per.capita, 
               Population, Region, Region_USSR_Yugo) %>%
  group_by(Country) %>% 
  mutate(GDP_lead = lead(GDP.per.capita, n = 1, default = NA),
         GDP_lead_year = lead(Benchmark_year, n = 1, default = NA),
         Pop_lead = lead(Population, n = 1, default = NA),
         Pop_lead_year = lead(Benchmark_year, n = 1, default = NA))


#  Growth rate
# GDP_t = GDP_0 x (1 + r)^t
# -> (1 + r)^t = GDP_t/GDP_0
# -> (1+r) = (GDP_t/GDP_0)^(1/t)

growth_rates<- growth_rates %>%
  mutate(GDPpercap_growth = (GDP_lead/GDP.per.capita)^
           (1/(GDP_lead_year - Benchmark_year)))

growth_rates<- growth_rates %>%
  mutate(Pop_growth = (Pop_lead/Population)^(
    1/(Pop_lead_year - Benchmark_year)))

# # Inspect number of growth rate observations for each region
# growth_rate_count<- growth_rates %>%
#   group_by(Region_USSR_Yugo, Benchmark_year) %>%
#   summarise(non_na_pop_count = sum(!is.na(Pop_growth)),
#             non_na_GDP_count = sum(!is.na(GDPpercap_growth)))


# Calculate the average rates across regions, according to two regional
  # definitions:

# First we will apply a regional definition such that former USSR or Yugoslavia
# member states are extrapolated based on the growth rates in the aggregate bloc

Avg_region_USSR_Yugo_growth_rates<- growth_rates %>%
  group_by(Region_USSR_Yugo, Benchmark_year) %>%
  summarise(region_USSR_Yugo_GDP_growth = mean(GDPpercap_growth, na.rm=TRUE),
            region_USSR_Yugo_pop_growth = mean(Pop_growth, na.rm=TRUE))

  # swap the region labels between the member states and the aggregate, so
    # as to match aggregate bloc growth rates to individual members
  
      # drop the averages derived across individual bloc states -
        # (i.e. those labeled with USSR and Yugoslavia regions)
Avg_region_USSR_Yugo_growth_rates<- Avg_region_USSR_Yugo_growth_rates %>%
  filter(!(Region_USSR_Yugo %in% c("USSR", "Yugoslavia")))

      # re-label the rates of the aggregate block with the region label 
        # given to the individual states, so that they will be extrapolated
        # on the basis of the aggregate bloc growth rate
Avg_region_USSR_Yugo_growth_rates<- Avg_region_USSR_Yugo_growth_rates %>%
  mutate(Region_USSR_Yugo = replace(Region_USSR_Yugo,
                                    Region_USSR_Yugo == "Aggregate USSR",
                                    "USSR"),
         Region_USSR_Yugo = replace(Region_USSR_Yugo,
                                    Region_USSR_Yugo == "Aggregate FY",
                                    "Yugoslavia"))

# Second, a regional definition where these two blocs are part of EECA
# region (to extrapolate for even earlier periods)
Avg_region_growth_rates<- growth_rates %>%
  group_by(Region, Benchmark_year) %>%
  summarise(region_GDP_growth = mean(GDPpercap_growth, na.rm=TRUE),
            region_pop_growth = mean(Pop_growth, na.rm=TRUE))


# Merge in regional average growth rates to extrapolate earlier years -----
# First the regions with USSR and Yugo separate growth figures from the
  # aggregate block,then any remaining blanks are extrapolated based on 
  # the regions definitions with USSR and Yugo former states forming part
  # of wider EECA region.

GDP_pop_bm_years<- merge(GDP_pop_bm_years, Avg_region_USSR_Yugo_growth_rates, all=TRUE)

GDP_pop_bm_years<- merge(GDP_pop_bm_years, Avg_region_growth_rates, all=TRUE)


# Loop to extrapolate pop and gdp per cap back based on regional averages

GDP_pop_bm_years<- arrange(GDP_pop_bm_years, Country, Benchmark_year)


# First for the regions using separate USSR/Yugo aggregate rates
for(i in 1:15){ # a loop to move back across benchmark years
  
  GDP_pop_bm_years<- GDP_pop_bm_years%>% 
    group_by(Country) %>% 
    mutate(GDP_lead = lead(GDP.per.capita, n = 1, default = NA),
           GDP_lead_year = lead(Benchmark_year, n = 1, default = NA),
           Pop_lead = lead(Population, n = 1, default = NA),
           Pop_lead_year = lead(Benchmark_year, n = 1, default = NA))
  
  #  Growth rate calc
  # GDP_t = GDP_0 x (1 + r)^t
  # GDP_0 = GDP_t / (1+r)^t
  
  # Extrapolate GDP per cap and population for the previous period
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Predicted.GDP.per.capita = GDP_lead / 
             region_USSR_Yugo_GDP_growth^(GDP_lead_year - Benchmark_year))
  
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Predicted.Population = Pop_lead / 
             region_USSR_Yugo_pop_growth^(Pop_lead_year - Benchmark_year))
  
  
  # Note that this observation is extrapolated in this way
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita_source = replace(GDP.per.capita_source,
                                        (is.na(GDP.per.capita) & !is.na(Predicted.GDP.per.capita) & 
                                           (USSR_dummy + Yugoslavia_dummy)>0),
                                        "Extrapolated using aggregate bloc growth rate"))
  
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita_source = replace(GDP.per.capita_source,
                                        (is.na(GDP.per.capita) & !is.na(Predicted.GDP.per.capita) & 
                                           (USSR_dummy + Yugoslavia_dummy)==0),
                                        "Extrapolated using average regional growth rate"))
  
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Population_source = replace(Population_source,
                                       (is.na(Population) & !is.na(Predicted.Population) & 
                                          (USSR_dummy + Yugoslavia_dummy)>0),
                                       "Extrapolated using aggregate bloc growth rate"))
  
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Population_source = replace(Population_source,
                                       (is.na(Population) & !is.na(Predicted.Population) & 
                                          (USSR_dummy + Yugoslavia_dummy)==0),
                                       "Extrapolated using average regional growth rate"))
  
  
  # Swap in the predicted value where currently missing
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita = coalesce(GDP.per.capita, Predicted.GDP.per.capita),
           Population = coalesce(Population, Predicted.Population))
  
  # (Repeated many times to iterate back over the whole time series)
}


# Then for regions applying whole EECA region average to former USSR/Yugo states
for(i in 1:15){ # a loop to move back across benchmark years
  
  GDP_pop_bm_years<- GDP_pop_bm_years%>% 
    group_by(Country) %>% 
    mutate(GDP_lead = lead(GDP.per.capita, n = 1, default = NA),
           GDP_lead_year = lead(Benchmark_year, n = 1, default = NA),
           Pop_lead = lead(Population, n = 1, default = NA),
           Pop_lead_year = lead(Benchmark_year, n = 1, default = NA))
  
  #  Growth rate calc
  # GDP_t = GDP_0 x (1 + r)^t
  # GDP_0 = GDP_t / (1+r)^t
  
  # Predict GDP per cap and population for the previous period
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Predicted.GDP.per.capita = GDP_lead / 
             region_GDP_growth^(GDP_lead_year - Benchmark_year))
  
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Predicted.Population = Pop_lead / 
             region_pop_growth^(Pop_lead_year - Benchmark_year))
  
  
  # Note that this observation is extrapolated in this way
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita_source = replace(GDP.per.capita_source,
                                        (is.na(GDP.per.capita) & !is.na(Predicted.GDP.per.capita)),
                                        "Extrapolated using average regional growth rate"))
  
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(Population_source = replace(Population_source,
                                       (is.na(Population) & !is.na(Predicted.Population)),
                                       "Extrapolated using average regional growth rate"))
  
  # Swap in the predicted value where currently missing
  GDP_pop_bm_years<- GDP_pop_bm_years %>%
    mutate(GDP.per.capita = coalesce(GDP.per.capita, Predicted.GDP.per.capita),
           Population = coalesce(Population, Predicted.Population))
  
  
  # (Repeated many times to iterate back over the whole time series)
}



# Select needed variables
GDP_pop_bm_years <- GDP_pop_bm_years %>%
  select(Country, Benchmark_year, Region, Region_USSR_Yugo, 
          GDP.per.capita, GDP.per.capita_source,
          Population, Population_source)

# Drop USSR and Yugoslavia aggregate entities (all calculations just based
  # on former individual states from here on out.)
GDP_pop_bm_years <- GDP_pop_bm_years %>%
  filter(!(Country %in% c("Former USSR", "Former Yugoslavia")))


# SAVE ------

save(GDP_pop_bm_years, file="Manipulated data/GDP_pop_bm_years.Rda")
