library(tidyverse)


# LOAD PREPARED GDP, POPULATION AND GINI DATA -----

load("Manipulated data/GDP_pop_bm_years.Rda")
load("Manipulated data/gini_bm_years.Rda")

options(scipen = 999) # This just stops the data being presented in scientific notation


# SET POV LINES, GINI MULTIPLIERS, GDP MULTIPLIERS AND GINI SOURCES -----

# Poverty lines
poverty_thresholds <- c(1, 2,5,10, 20, 30) # Main lines

poverty_thresholds <- c(poverty_thresholds,3.05, 6.48,23.77) # P25, P50 and P75 in 1980
    # See the script 'Find quartiles of global distribution in 1980.R'

poverty_thresholds <- c(poverty_thresholds, 1.90,3.20, 5.50,10, 30) # povcal comparisons

poverty_thresholds <- c(poverty_thresholds,5.2) # Line to match on to Povcal 1.90

poverty_thresholds<- unique(poverty_thresholds) # in case any repeated pov lines

# GDP multipliers
GDP_multipliers<- c(0.666, 1, 1.333)

# gini multipliers
Gini_multipliers<- c(0.666, 1, 1.333)

# Gini sources series - the name of the gini variables under 3 different constructions
Gini_sources<- c("baseline_GCIP_income_vZ", "alt_GCIP_consumption_vZ", 
                 "alt_Povcal_vZ")

# ESTIMATE POVERTY RATES ----
  # for each pov line, for each GDP multiplier, for each gini multiplier,
  # and each gini source data series.... Stored as a 4D list

countries_pov_estimates<- list()
regions_pov_estimates<- list()
world_pov_estimates<- list()


# for each gini source data series
for(gini_source in Gini_sources) {
  
  # filter the inequality data for the selected gini source
   selected_ginis <- gini_bm_years %>%
  rename(gini = !!gini_source) %>% # standardise the var name
     select(Country, Benchmark_year, gini)
  
     # create lists to store the various gini multiplier scenarios 
  gini_multiplier_level_countries<- list()
  gini_multiplier_level_world<- list()
  gini_multiplier_level_regions<- list()
  
  # for each gini multiplier
  for(gini_multiplier in Gini_multipliers) {

      # multiply gini by multiplier, but capped at min and max of actual series
    selected_multiplied_ginis <- selected_ginis %>%
      mutate(gini = ifelse(gini*gini_multiplier<min(gini), min(gini),
                          ifelse(gini*gini_multiplier>max(gini), max(gini),
                                 gini*gini_multiplier)))
    
    
    min_gini <- selected_ginis %>%
      summarise(mi_gini = min(gini)) %>%
      unique() %>%
      as.numeric()
    
    max_gini <- selected_ginis %>%
      summarise(ma_gini = max(gini)) %>%
      unique() %>%
      as.numeric()
    
    print(paste0("Gini source: ", gini_source,
                 "min Gini: ", min_gini,
                 "max Gini: ", max_gini))
    
    # create lists to store the various GDP multiplier scenarios 
    GDP_multiplier_level_countries<- list()
    GDP_multiplier_level_world<- list()
    GDP_multiplier_level_regions<- list()
    
    # for each GDP multiplier
    for(GDP_multiplier in GDP_multipliers) {
      
      selected_multiplied_GDP<- GDP_pop_bm_years %>%
        select(Country, Benchmark_year, GDP.per.capita, Population, Region) %>%
        mutate(GDP.per.capita = GDP.per.capita * GDP_multiplier)

      GDP_and_ginis<-left_join(selected_multiplied_GDP, selected_multiplied_ginis)
     
      # create lists to store the various poverty thresholds
      poverty_threshold_level_countries<- list()
      poverty_threshold_level_world<- list()
      poverty_threshold_level_regions<- list()
      
        # For each poverty threshold
       for(poverty_threshold in poverty_thresholds) {
    
        # Define lognormal distribution paramaters, mu and sd
        
        # Calculate standard deviation from Gini (on assumption of log-normality)
        
        #   Gini = 2 * phi(sd/sqrt(2)) - 1
        # ...where phi(z) is the cumulative standard normal
        # 
        # -> sd = sqrt(2) * invnormal((Gini +1)/2)
        
        # R's inverse standard normal CDF is given by: qnorm(p, mean=0, sd=1)
        
        country_pov_rates<- GDP_and_ginis %>%
          mutate(sd = 2^(1/2) * qnorm(((gini + 1)/2), 0 , 1))
        
        # calculate mu (mean of ln(X))
        
        #   mean of a log normal is e^(mu + (sd^2)/2)
        # -> mu = ln(mean) - (sd^2)/2
        
        country_pov_rates<- country_pov_rates %>%
          mutate(mu = log(GDP.per.capita) - (sd^2)/2)
        
        
        # Calculate share below poverty line:
        # log normal cumulative dist defined given by:
        #  F(x) = p = P(x < X) = normal((ln(`x') - `mu')/ `sd')
        
        country_pov_rates<- country_pov_rates %>%
          mutate(poverty_rate = pnorm((log(poverty_threshold * 365) - mu) 
                                        / sd) * 100) # fraction below threshold
        
        
        # World poverty
        
        world_poverty_rate<- country_pov_rates %>%
          group_by(Benchmark_year) %>%
          summarise(poverty_rate = weighted.mean(poverty_rate,Population),
                    GDP.per.capita = weighted.mean(GDP.per.capita,Population),
                    Population = sum(Population))
        
        
        # Regional headcount (numbers), headcount ratio and share out of world pop 
        # get world pop and merge to benchmark year
        
        regional_breakdown<- country_pov_rates %>%
          mutate(Region = replace(Region,
                                  Country == "China",
                                  "China"),
                 Region = replace(Region,
                                  Region == "EAP",
                                  "EAP excl. China"),
                 Region = replace(Region,
                                  Country == "India",
                                  "India"), 
                 Region = replace(Region,
                                  Region == "SA",
                                  "SA excl. India")) %>%
          group_by(Benchmark_year, Region) %>%
          summarise(regional_pov_rate = weighted.mean(poverty_rate,Population),
                    regional_population = sum(Population)) %>%
          mutate(number_in_poverty = (regional_pov_rate/100) * regional_population)
        
        # Calculate share in world population
        
        world_pop<- world_poverty_rate %>%
          ungroup() %>%
          select(Benchmark_year, Population)%>%
          rename(World_population = Population) %>%
          unique()
        
        regional_breakdown<- merge(regional_breakdown, world_pop)
        
        regional_breakdown<- regional_breakdown %>%
          mutate(poor_share_in_world_pop = number_in_poverty/World_population*100)
        
        # store in list (deepest level)    
        poverty_threshold_level_countries[[paste0(poverty_threshold,"_per_day")]]<- country_pov_rates
        poverty_threshold_level_world[[paste0(poverty_threshold,"_per_day")]]<- world_poverty_rate
        poverty_threshold_level_regions[[paste0(poverty_threshold,"_per_day")]]<- regional_breakdown
        
      }
      
      GDP_multiplier_level_countries[[paste0("GDP X ", GDP_multiplier)]]<- poverty_threshold_level_countries   
      GDP_multiplier_level_world[[paste0("GDP X ", GDP_multiplier)]]<- poverty_threshold_level_world  
      GDP_multiplier_level_regions[[paste0("GDP X ", GDP_multiplier)]]<- poverty_threshold_level_regions 
      
      
    }
    
    gini_multiplier_level_countries[[paste0("Gini X ", gini_multiplier)]]<- GDP_multiplier_level_countries   
    gini_multiplier_level_world[[paste0("Gini X ", gini_multiplier)]]<- GDP_multiplier_level_world  
    gini_multiplier_level_regions[[paste0("Gini X ", gini_multiplier)]]<- GDP_multiplier_level_regions 
    
    
  } 
  

  
  countries_pov_estimates[[gini_source]]<- gini_multiplier_level_countries   
  world_pov_estimates[[gini_source]]<- gini_multiplier_level_world  
  regions_pov_estimates[[gini_source]]<- gini_multiplier_level_regions 
  
    
}


# UNPACK WORLD POV ESTIMATES INTO A SINGLE DATAFRAME ------

  # a list of of the pov estimates stored in list format
country_world_region_pov_estimates_list<- list("countries" = countries_pov_estimates,
                                               "world" = world_pov_estimates,
                                               "regions" = regions_pov_estimates)

  # somewhere to store the unpacked dfs
unpacked_pov_estiamtes_under_different_scenarios<- list()

# # GDP multipliers
GDP_multipliers<- c(0.666, 1, 1.333)
# 
# # gini multipliers
Gini_multipliers<- c(0.666, 1, 1.333)
# 
# # Gini sources series - the name of the gini variables under 3 different constructions
Gini_sources<- c("baseline_GCIP_income_vZ", "alt_GCIP_consumption_vZ",
                 "alt_Povcal_vZ")

for (ent_level in c("countries", "world", "regions")){

    pov_estimates_entity_level<- country_world_region_pov_estimates_list[[ent_level]]
  
# for each scenario  at each level, aggregate into a single data frame.

for(gini_srce in Gini_sources){
  
  source_level<- pov_estimates_entity_level[[gini_srce]]
  
  for(gini_multip in Gini_multipliers){
    
    gini_multip_level<- source_level[[paste0("Gini X ", gini_multip)]]
    
    for(GDP_multip in GDP_multipliers){
      
      GDP_multip_level<- gini_multip_level[[paste0("GDP X ", GDP_multip)]]
      
      
      
      for(threshold in poverty_thresholds){

        df_new_threshold<- GDP_multip_level[[paste0(threshold,"_per_day")]] %>%
          mutate(poverty_threshold = threshold )
        
        if(threshold == poverty_thresholds[1]){ #if first, store in new aggregate df
          df_threshold<- df_new_threshold
        } else { #else append to aggregate df
          df_threshold<- rbind(df_new_threshold,
                               df_threshold)
        }
      }

            # pass up to a df aggrgeateing across the higher list level
      df_newGDP_multiplier<- df_threshold %>%
        mutate(GDP_multiplier = GDP_multip)
      
      if(GDP_multip == GDP_multipliers[1]){ #if first, store in new aggregate df
        
        df_GDP_multiplier<- df_newGDP_multiplier
      } else { #else append to aggregate df
        df_GDP_multiplier<- rbind(df_newGDP_multiplier,
                                  df_GDP_multiplier)
      }
      
    }

    df_newgini_multiplier<- df_GDP_multiplier %>%
      mutate(gini_multiplier = gini_multip)
    
    if(gini_multip == Gini_multipliers[1]){ #if first, store in new aggregate df
      df_gini_multiplier<- df_newgini_multiplier
    } else { #else append to aggregate df
      df_gini_multiplier<- rbind(df_newgini_multiplier,
                                 df_gini_multiplier)
    }
    
  }
  
  df_newall_data<- df_gini_multiplier %>%
    mutate(gini_source = gini_srce)
  
  if(gini_srce == Gini_sources[1]){ #if first, store in new aggregate df
    df_all_data<- df_newall_data %>%
      mutate(gini_source = gini_srce)
  } else { #else append to aggregate df
    df_all_data<- rbind(df_newall_data,
                        df_all_data)
  }
  
  
  
}

# store resulting aggregate df for each entity level in a list 
unpacked_pov_estiamtes_under_different_scenarios[[ent_level]]<- df_all_data

}


# SAVE -----

save(unpacked_pov_estiamtes_under_different_scenarios, file = "Manipulated data/Poverty estimates under different scenarios (country, world and region data stored separately in list).Rda")

