

library(tidyverse)

library(svglite)
library(patchwork) # This is for improved chart faceting


# COMPARE WITH MOATSOS AND B&M -------

# Load our global estimates -----
fp<- "Outputs/Upload files for Our World in Data/Reconstruction of historical global extreme poverty rates, 1820-2017.csv"

OWID_ests<- read.csv(fp) %>%
  select(year, poverty_rate) %>%
  rename(Year = year,
         Roser_Hasell = poverty_rate)

# Load Moatsos data (also includes B&M) -----
fp<- "Manipulated data/Moatsos (2021) clean.csv"

Moatsos_data<-  read.csv(fp) %>%
  select(Year, CBN, DAD, Bourguignon...Morrisson) %>%
  rename(Moatsos_CBN = CBN,
         Moatsos_WB = DAD,
         B_and_M = Bourguignon...Morrisson) %>%
   mutate_at(vars(Moatsos_CBN:B_and_M),
            .funs = funs(. * 100)) #express as percent


# Merge together and plot ----

all_data<- merge(OWID_ests, Moatsos_data, all = TRUE) %>%
  gather(source, poverty_rate, -(Year)) %>%
  drop_na()

ggplot(all_data, aes(x = Year, y = poverty_rate, colour = source )) +
  geom_path() +
  ylim(0, 100) +
  theme_light()

ggsave("Outputs/Chart images/compare_with_Moatsos.svg")

# VIZ COMPARE PROGRESS ON P25, P50 AND P75 (IN 1980/1) WITH POVCAL -----

# Load Povcal figures ----

fp<- "/Users/joehasell/Documents/OWID/Growth and Inequality/Historical GDP-poverty estimates/Dec 2020 Poverty paper revision/Manipulation/Povcal comparisons/"

# Global P25 in 1981
pov_line<- 1.19
povcal_P25<- read.csv(paste0(fp,"region_data_", pov_line,".csv"))
povcal_P25<-povcal_P25 %>%
  mutate(threshold = "P25")

# Global P50 in 1981
pov_line<- 2.4
povcal_P50<- read.csv(paste0(fp,"region_data_", pov_line,".csv"))
povcal_P50<-povcal_P50 %>%
  mutate(threshold = "P50")

# Global P75 in 1981
pov_line<- 10.04
povcal_P75<- read.csv(paste0(fp,"region_data_", pov_line,".csv"))
povcal_P75<-povcal_P75 %>%
  mutate(threshold = "P75")

# Stack the quartile data 
povcal_quartile_data<- rbind(povcal_P25, povcal_P50)
povcal_quartile_data<- rbind(povcal_quartile_data, povcal_P75)

# prepare global headcounts
povcal_quartile_data<- povcal_quartile_data %>%
  filter(regiontitle == "World Total") %>%
  select(c(year, threshold, headcount)) %>%
  rename(poverty_rate = headcount) %>%
  mutate(poverty_rate = poverty_rate*100,  #express as %
         data_source = "World Bank estimates (survey approach)")

#  Select national accounts estiamtes for comparison and append to povcal ----
historical_quartile_data<- world_poverty_rate %>%
  ungroup() %>%
  filter(Gini_adjustment == "x_1",
         GDP_multiplier == "x_1",
         gini_series == "GCIP_income",
         Benchmark_year >= 1980) %>%
  rename(year = Benchmark_year) %>%
  select(year, poverty_threshold, poverty_rate) %>%
  mutate(threshold = "",
         threshold = replace(threshold,
                             poverty_threshold == "$3.06_per_day", # P25 in 1980 is $3.06
                             "P25"),
         threshold = replace(threshold,
                             poverty_threshold == "$6.5_per_day", # P50 in 1980 is $6.50
                             "P50"),
         threshold = replace(threshold,
                             poverty_threshold == "$23.75_per_day", # P75 in 1980 is $23.75
                             "P75"),
         data_source = "Own estimates (national accounts approach)") %>%
  filter(threshold %in% c("P25","P50","P75")) %>%
  select(-poverty_threshold)


compare_quartile_data<- rbind(povcal_quartile_data, historical_quartile_data)


compare_quartile_data_chart<- ggplot(compare_quartile_data, aes(x = year, y = poverty_rate)) +
  geom_line(aes(colour = threshold, linetype = data_source)) +
  geom_point(aes(colour = threshold)) +
  theme_light()

compare_quartile_data_chart

fp<- "/Users/joehasell/Documents/OWID/Growth and Inequality/Historical GDP-poverty estimates/Dec 2020 Poverty paper revision/Graphics/"
ggsave(file=paste0(fp, "Compare quartile pov rates for WB and NA from 1980.svg"), plot=compare_quartile_data_chart, width=16, height=8)


# CHINA - IMPACT OF INEQUALITY ON POV ESTIAMTES  ------

# 1820
data_1820<- data.frame("gini_scenario" = c("Actual estimated inequality", 
                                           "Low inequality (Gini = 0.25)",
                                           "High inequality (Gini = 0.65)"),
                       "gini" = c(0.4485430,
                                  0.25,
                                  0.65))

data_1820<- data_1820 %>%
  mutate(GDP_per_cap = 882,
         year = 1820)


# 2017 
data_2017<- data.frame("gini_scenario" = c("Actual estimated inequality", 
                                           "Low inequality (Gini = 0.25)",
                                           "High inequality (Gini = 0.65)"),
                       "gini" = c(0.5294172,
                                  0.25,
                                  0.65))

data_2017<- data_2017 %>%
  mutate(GDP_per_cap = 12734,
         year = 2017)


china_scenarios<- rbind(data_1820, data_2017)

china_scenarios<- china_scenarios %>%
  mutate(sd = 2^(1/2) * qnorm(((gini + 1)/2), 0 , 1),
         mu = log(GDP_per_cap) - (sd^2)/2,
         poverty_rate_2_per_day = pnorm((log(2*365) - mu) / sd) * 100,
         poverty_rate_5_per_day = pnorm((log(5*365) - mu) / sd) * 100)


china_scenarios_distributions<- data.frame("year" = integer(),
                                           "income" = numeric(),
                                           "density" = numeric(),
                                           "gini_scenario" = character())

income<- seq(100,100000,10) # The range of incomes to plot

for(y in c(1820,2017)){
  
  for(g_scen in c("Low inequality (Gini = 0.25)",
                  "Actual estimated inequality",
                  "High inequality (Gini = 0.65)")) {
    
    # g_scen<- "Actual estimated inequality"
    # y<- 2017
    # 
    lnorm_params<- china_scenarios %>%
      filter(gini_scenario == g_scen,
             year == y) %>%
      select(mu,sd)
    
    
    density<- dlnorm(income,lnorm_params$mu,lnorm_params$sd) * income
    # Multiply by income in order to scale to a log x-axis
    
    new_scenario<- data.frame(income, density)
    new_scenario<- new_scenario %>%
      mutate(year = y,
             gini_scenario = g_scen)
    
    
    china_scenarios_distributions<- rbind(china_scenarios_distributions,
                                          new_scenario )
    
  } 
  
}

china_scenarios_distributions<-  china_scenarios_distributions %>%
  mutate(gini_scenario = factor(gini_scenario, 
                                levels=c("Low inequality (Gini = 0.25)",
                                         "Actual estimated inequality",
                                         "High inequality (Gini = 0.65)"))) 


china_scenarios_plot<- ggplot(data=china_scenarios_distributions, aes(alpha = 0.4)) +
  geom_ribbon(aes(x = log(income) ,
                  ymin=0, ymax=density, fill = as.factor(year))) +
  geom_line(aes(x = log(income), y = density, colour = as.factor(year) )) +
  facet_grid(gini_scenario ~ .) +
  geom_vline(xintercept = c(log(1*365),
                            log(2*365),
                            log(5*365), 
                            log(10*365), 
                            log(20*365),
                            log(50*365),
                            log(100*365),
                            log(200*365),
                            log(882),
                            log(12734))) +
  scale_x_continuous(breaks=log(c(100,200,500,1000,2000,5000,10000,20000,50000,100000)),
                     labels = c("100","200","500","1000","2000","5000","10000","20000","50000","100000")) +
  theme_light() +
  ggtitle(paste0("China income distribution in 1820 and 2017 under different inequality scenarios"))

china_scenarios_plot

fp<- "/Users/joehasell/Documents/OWID/Growth and Inequality/Historical GDP-poverty estimates/Dec 2020 Poverty paper revision/Graphics/"
ggsave(file=paste0(fp, "China scenarios.svg"), plot=china_scenarios_plot, width=10, height=8)




# ALTERNATIVE SCENARIOS/ROBUSTNESS -----


# Load country, region and world poverty estiamtes under different scenarios and source data
load("Manipulated data/Poverty estimates under different scenarios (country, world and region data stored separately in list).Rda")


# Vary Gini source ------


# Set pov line
# (this must be selected from the range of lines
# for which poverty rates have been calculated earlier in the script
# chain. See 'Estimate poverty counts.R')

threshold<- c(2, 5,10,20)


# grab series from the collated pov estimates
Gini_sources<- unpacked_pov_estiamtes_under_different_scenarios[["world"]] %>%
  filter(GDP_multiplier ==  1,
         gini_multiplier == 1,
         poverty_threshold %in% threshold) %>%
  mutate(poverty_rate = poverty_rate/100)


compare_gini_source<- ggplot(Gini_sources, aes(x = Benchmark_year, y = poverty_rate, colour = as.factor(gini_source))) +
  geom_line()  +
  facet_wrap(~ poverty_threshold) + 
  theme_light() + 
  scale_y_continuous(labels = scales::percent) 


ggsave(file="Outputs/Chart images/Compare gini source.svg", 
       plot=compare_gini_source)




# Vary Gini ------

# Set pov line
# (this must be selected from the range of lines
# for which poverty rates have been calculated earlier in the script
# chain. See 'Estimate poverty counts.R')

threshold<- c(2, 5,10,20)

# grab series from the collated pov estimates
Gini_scenarios<- unpacked_pov_estiamtes_under_different_scenarios[["world"]] %>%
  filter(gini_source == "baseline_GCIP_income_vZ",
         GDP_multiplier ==  1,
         poverty_threshold %in% threshold) %>%
  mutate(poverty_rate = poverty_rate/100)

compare_gini_scenarios<- ggplot(Gini_scenarios, aes(x = Benchmark_year, y = poverty_rate, colour = as.factor(gini_multiplier))) +
  geom_line() +
  facet_wrap(~ poverty_threshold) + 
  theme_light() + 
  scale_y_continuous(labels = scales::percent) 

ggsave(file="Outputs/Chart images/Compare gini scenarios.svg", 
       plot=compare_gini_scenarios)

# Vary GDP ------

# Set pov line
# (this must be selected from the range of lines
# for which poverty rates have been calculated earlier in the script
# chain. See 'Estimate poverty counts.R')

threshold<- c(2, 5,10,20)


# grab series from the collated pov estimates
GDP_scenarios<- unpacked_pov_estiamtes_under_different_scenarios[["world"]] %>%
  filter(gini_source == "baseline_GCIP_income_vZ",
         gini_multiplier == 1,
         poverty_threshold %in% threshold) %>%
  mutate(poverty_rate = poverty_rate/100)


compare_GDP_scenarios<- ggplot(GDP_scenarios, aes(x = Benchmark_year, y = poverty_rate, colour = as.factor(GDP_multiplier))) +
  geom_line() +
  facet_wrap(~ poverty_threshold) + 
  theme_light() + 
  scale_y_continuous(labels = scales::percent) 

ggsave(file="Outputs/Chart images/Compare GDP scenarios.svg", 
       plot=compare_GDP_scenarios)


# INEQUALITY POSSIBILITY FRONTIER -----

# Cited from Milanovic, Lindert and Williamson (2008) define a minimum
# subsistence level of 300 in 1990 GK PPPs and an inequality frontier 
# given by the equation:

# G* = (a - 1)/a 
# where G is the maximum possilbe Gini and a is the ratio of the average
# income and the subsistence income.

# $300 in 1990 GK is roughly equal to $520 in 2011 PPPs
# This is calculated by adjusting according to CPI inflation in the US 
# between 1990 and 2011. See https://data.oecd.org/price/inflation-cpi.htm
# US CPI was 94.9 in 2011 and 55.1 in 1990 (2015=100).
#  300 * 94.9/55.1  = 517

# But on the other hand we see from Ravallion (2016) that the consumption 'floor'
# is much lower than this, more like $1 a day = $350 (in 2011 prices)

low_s<- 500
high_s<- 350

average <- seq(300,30000,10)

G_frontier_high<- (average/high_s - 1)/(average/high_s) 
G_frontier_low<- (average/low_s - 1)/(average/low_s)

G_frontiers<- data.frame(average, G_frontier_high, G_frontier_low)

G_frontiers[G_frontiers<0] <- NA # no Ginis below 0 allowed


# Prepare scatter of actual data

GDP_gini<- collated_scenarios %>%
  filter(gini_series == "GCIP_income",
         Gini_adjustment == "x_1",
         GDP_multiplier == "x_1",
         Gini_source %in% c("van_Zanden", "GCIP_income"),
         GDP_per_cap_source == "MDP") %>%
  select(Benchmark_year, Gini, GDP.per.capita) %>%
  unique() # drops the duplicates from different poverty rates


ggplot(G_frontiers, aes(x=average)) +
  geom_line(aes(y=G_frontier_high)) + 
  geom_line(aes(y=G_frontier_low)) +
  geom_point(data = GDP_gini, aes(y = Gini, x = GDP.per.capita,
                                  colour = as.factor(Benchmark_year)),
             alpha=0.6) +
  scale_x_log10()

impossible_ginis<- GDP_gini %>%
  mutate(G_frontier_high = (GDP.per.capita/high_s - 1)/(GDP.per.capita/high_s), 
         G_frontier_low = (GDP.per.capita/low_s - 1)/(GDP.per.capita/low_s))

low_impossible<- impossible_ginis %>%
  filter(Gini>G_frontier_low)%>%
  count(Benchmark_year)

high_impossible<- impossible_ginis %>%
  filter(Gini>G_frontier_high)%>%
  count(Benchmark_year)
