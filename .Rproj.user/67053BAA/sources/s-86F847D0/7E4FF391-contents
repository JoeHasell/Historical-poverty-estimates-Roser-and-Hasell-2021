library(tidyverse)

# Functions
source("Scripts/Functions/keep_closest.R")

# Our benchmark years
# Set so as to coincide as much as possible with the benchmarks used in
# the source data.

benchmarks<- c(1820, 1850, 1870, 
               1890, 1910, 1929, 
               1950, 1960, 1970, 
               1980, 1990, 2000,
               2005, 2010, 2017)



# country regions
df_regions<- read.csv("Original data/Country regions.csv")


# IMPORT AND PREPARE VAN ZANDEN ET AL DATA -----

# import
fp <- "Manipulated data/van Zanden IncomeInequality_Broad (country names standardized).csv"
df_vanZan<- read.csv(fp) %>%
  rename(vZ_gini = Gini,
         Benchmark_year = Year) %>%
  mutate(vZ_gini = vZ_gini/100) # Gini as fraction


# Van Zanden data has Slovakia and Czech Rep separately, but Maddison GDP data 
  # has them combined as Czechoslovakia. I apply the Slovakia Ginis to 
  # Czechoslovakia since they are more complete, and then drop the Czech Rep 
  # figures

df_vanZan<- df_vanZan %>%
  mutate(Country = replace(Country,
                                     Country == "Slovakia",
                                     "Czechoslovakia")) %>%
  filter(!Country == "Czechia")


# POVCAL DATA -----

fp<- "Manipulated data/Povcal data/Povcal Ginis for benchmark years.csv"
povcal_ginis<- read.csv(fp) %>%
  select(Country, Benchmark_year, Povcal_gini) %>%
  mutate(Povcal_gini = ifelse(Povcal_gini==-1, NA, Povcal_gini))

# some missing data is recorded as '-1'


# adjust some entities to map onto Maddison 
  # (Sudan -> Former sudan and Czech Rep -> Czechoslovakia)
povcal_ginis<- povcal_ginis %>%
  mutate(Country = replace(Country,
                           Country == "Sudan",
                           "Former Sudan"),
         Country = replace(Country,
                           Country == "Czech Republic",
                           "Czechoslovakia"))

# GCIP ------

  # income
fp<- "Manipulated data/GCIP income-inequalityexcel (country names standardized).csv"
GCIP_income<- read.csv(fp) %>%
  select(country, year, gini) %>%
  rename(Country = country,
         Year = year,
         GCIP_income_gini = gini)

  # consumption
fp<- "Manipulated data/GCIP consumption-inequalityexcel (country names standardized).csv"
GCIP_consumption<- read.csv(fp) %>%
  select(country, year, gini) %>%
  rename(Country = country,
         Year = year,
         GCIP_consumption_gini = gini)

CGIP_ginis<- merge(GCIP_income, GCIP_consumption, all = TRUE)


# Adjust some entities to match Maddison: Set Former Sudan ginis as 'Sudan'. 
# Set Czechoslovakia ginis as Czech Rep (Slovakia is dropped when merged later)
CGIP_ginis<- CGIP_ginis %>%
  mutate(Country = replace(Country, # Assume that entire series refers to Former Sudan
                           Country == "Sudan",
                           "Former Sudan"),
         Country = replace(Country,
                           Country == "Czechia",
                           "Czechoslovakia"))


# Whereas van Zanden data and Povcal data already aligns with benchmark years,
  # here we select the closest year in GCIP data. This function maps the nearest
  # observations (within 3 years) to the benchmark years.
  # In fact, GCIP is fully inter/extrapolated already. However it only runs to 
  # 2014-15. So in practice this function is only making matches with tolerance
  # for our final benchmark year, 2017. All the rest are exact matches.

GCIP_benchmarks<- keep_closest(CGIP_ginis, benchmarks, 3)

GCIP_benchmarks<- GCIP_benchmarks %>%
  rename(Benchmark_year = Target_Year) %>%
  select(-Original_Year)


# MERGE SOURCES -------

all_ginis<- merge(df_vanZan, povcal_ginis, all = TRUE )
all_ginis<- merge(all_ginis, GCIP_benchmarks, all = TRUE )


# CONSTRUCT THREE SERIES FROM SOURCES ------

# 1) baseline: GCIP income plus van Zanden where missing (i.e. before 1960)

  # copy over original observations in GCIP_income, noting original source 
all_ginis<- all_ginis %>% 
  mutate(baseline_GCIP_income_vZ = GCIP_income_gini,
         baseline_GCIP_income_vZ_source = ifelse(!is.na(GCIP_income_gini),
                                                 "GCIP_income_gini_original",
                                                 NA))
  

# Add in van Zanden where missing, noting original source 
all_ginis<- all_ginis %>% 
  mutate(baseline_GCIP_income_vZ_source = ifelse(
            (is.na(baseline_GCIP_income_vZ) & !is.na(vZ_gini)),
              "vZ_gini_original",
              baseline_GCIP_income_vZ_source),
         baseline_GCIP_income_vZ = ifelse(
           (is.na(baseline_GCIP_income_vZ) & !is.na(vZ_gini)),
            vZ_gini,
            baseline_GCIP_income_vZ))


# 2) As (1) but using GCIP consumption data


# copy over original observations in GCIP_consumption, noting original source 
all_ginis<- all_ginis %>% 
  mutate(alt_GCIP_consumption_vZ = GCIP_consumption_gini,
         alt_GCIP_consumption_vZ_source = ifelse(!is.na(GCIP_consumption_gini),
                                                 "GCIP_consumption_gini_original",
                                                 NA))


# Add in van Zanden where missing, noting original source 
all_ginis<- all_ginis %>% 
  mutate(alt_GCIP_consumption_vZ_source = ifelse(
    (is.na(alt_GCIP_consumption_vZ) & !is.na(vZ_gini)),
    "vZ_gini_original",
    alt_GCIP_consumption_vZ_source),
    alt_GCIP_consumption_vZ = ifelse(
      (is.na(alt_GCIP_consumption_vZ) & !is.na(vZ_gini)),
      vZ_gini,
      alt_GCIP_consumption_vZ))

# 3) As (1) but using Povcal

# copy over original observations in Povcal, noting original source 
all_ginis<- all_ginis %>% 
  mutate(alt_Povcal_vZ = Povcal_gini,
         alt_Povcal_vZ_source = ifelse(!is.na(Povcal_gini),
                                                 "Povcal_gini_original",
                                                 NA))


# Add in van Zanden where missing, noting original source 
all_ginis<- all_ginis %>% 
  mutate(alt_Povcal_vZ_source = ifelse(
    (is.na(alt_Povcal_vZ) & !is.na(vZ_gini)),
    "vZ_gini_original",
    alt_Povcal_vZ_source),
    alt_Povcal_vZ = ifelse(
      (is.na(alt_Povcal_vZ) & !is.na(vZ_gini)),
      vZ_gini,
      alt_Povcal_vZ))




# Map onto complete country-benchmark_year list -----

# Make an empty df of country-benchmark years (according to Maddison countries, 
  # listed in the region mapping csv -  but drop the aggregate USSR and Yugo
  # blocs, which are dealt with as individual countries and Czech and Slovakia,
  # which are grouped together)
countries <- data.frame("Country" = df_regions$Country) %>%
  filter(!Country %in% c("Former USSR", "Former Yugoslavia",
                         "Slovakia", "Czechia")) %>% 
  slice(rep(1:n(), each = length(benchmarks)))

long_benchmarks<- data.frame("Benchmark_year" = rep(benchmarks,
                                    times=length(unique(countries$Country))))

gini_bm_years<- cbind(countries,long_benchmarks)


# merge three collated series into empty countyr-benchmark list
all_ginis<- all_ginis %>%
  select(Country, Benchmark_year, 
         baseline_GCIP_income_vZ, baseline_GCIP_income_vZ_source,
         alt_GCIP_consumption_vZ, alt_GCIP_consumption_vZ_source,
         alt_Povcal_vZ, alt_Povcal_vZ_source)

gini_bm_years<- left_join(gini_bm_years,all_ginis )


# INTER/EXTRAPOLATE MISSING OBSERVATIONS --------


# Calculate regional averages for each series -----

# merge in regions
gini_bm_years<- left_join(gini_bm_years, df_regions)


# Calculate regional mean for each gini series and benchmark year, according
# to two region categorizations: one where countries of former USSR and 
# Yugoslavia form their on respective regions, and one where both are part of
# EECA.

series_list<- c("baseline_GCIP_income_vZ", "alt_GCIP_consumption_vZ", 
                "alt_Povcal_vZ")

  # regions with USSR and Yugo separate from EECA
for(s in series_list){
  
  region_avg_var_name<- paste0(s, "_region_avg_USSR_Yugo")
  
  region_avg<- gini_bm_years %>% 
    group_by(Region_USSR_Yugo, Benchmark_year) %>%
    summarise(!!region_avg_var_name := mean(eval(as.name(s)), na.rm=TRUE))
  
  gini_bm_years<- left_join(gini_bm_years, region_avg)
  
}

  # regions with USSR and Yugo included in EECA
for(s in series_list){
  
  region_avg_var_name<- paste0(s, "_region_avg")
  
  region_avg<- gini_bm_years %>% 
    group_by(Region, Benchmark_year) %>%
    summarise(!!region_avg_var_name := mean(eval(as.name(s)), na.rm=TRUE))
  
  gini_bm_years<- left_join(gini_bm_years, region_avg)
  
}

# Add in regional averages to Gini series where missing ------
  # Two passes: first the regions with separate USSR and Yugo, then the regions
  # with these two blocs combined with EECA

for(s in series_list){

    s_source<- paste0(s,"_source")
  s_region_avg_USSR_Yugo<- paste0(s,"_region_avg_USSR_Yugo")
  s_region_avg<- paste0(s,"_region_avg")
  
  # First pass: USSR Yugo as separate regions...
gini_bm_years<- gini_bm_years %>% 
  mutate(!!sym(s_source) := ifelse(
    (is.na(!!sym(s)) & !is.na(!!sym(s_region_avg_USSR_Yugo))),
    "regional average",
    !!sym(s_source)),
    !!sym(s) := ifelse(
      (is.na(!!sym(s)) & !is.na(!!sym(s_region_avg_USSR_Yugo))),
      !!sym(s_region_avg_USSR_Yugo),
     !!sym(s)))


  # Second pass: USSR and Yugo included in EECA...
gini_bm_years<- gini_bm_years %>% 
  mutate(!!sym(s_source) := ifelse(
    (is.na(!!sym(s)) & !is.na(!!sym(s_region_avg))),
    "regional average",
    !!sym(s_source)),
    !!sym(s) := ifelse(
      (is.na(!!sym(s)) & !is.na(!!sym(s_region_avg))),
      !!sym(s_region_avg),
      !!sym(s)))
}


# SELECT FINAL VARS AND SAVE ------

gini_bm_years<- gini_bm_years %>% 
  select(Country, Benchmark_year,
         baseline_GCIP_income_vZ, baseline_GCIP_income_vZ_source,
         alt_GCIP_consumption_vZ, alt_GCIP_consumption_vZ_source,
         alt_Povcal_vZ, alt_Povcal_vZ_source)

save(gini_bm_years, file="Manipulated data/gini_bm_years.Rda")
