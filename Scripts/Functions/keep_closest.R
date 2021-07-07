library(tidyverse)


# This is a function to select data around a vector of target years,
# within a tolerance. Year variable must be 'Year', Country must be 'Country'.
keep_closest<- function(DF, target, tolerance) {
  
  # Try matching target(s) with actual year
  DF$Original_Year<-DF$Year
  collect<- DF[DF$Year %in% target,]
  collect$Country_Year <- paste0(collect$Country,collect$Year)
  
  for(i in 1:tolerance){
    
    # Shift year forward and try match again
    new<- DF
    new$Year<- new$Year+i
    new<- new[(new$Year %in% target),] #Match?
    
    # No double matches allowed (based on country-year)
    new$Country_Year<- paste0(new$Country,new$Year)
    new<-new[!(new$Country_Year %in% collect$Country_Year),]
    
    # Add any new matches to main collection
    collect<- rbind(collect,new)
    
    # Shift year backward and try match again
    new<- DF
    new$Year<- new$Year-i
    new<- new[(new$Year %in% target),] #Match?
    
    # No double matches allowed (based on country-year)
    new$Country_Year<- paste0(new$Country,new$Year)
    new<-new[!(new$Country_Year %in% collect$Country_Year),]
    
    # Add any new matches to main collection
    collect<- rbind(collect,new)
    
    
  }
  
  collect<- select(collect, -Country_Year)
  collect<- dplyr::rename(collect, Target_Year= Year)
  return(collect)
}