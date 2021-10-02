Project 1
================
Jennifer Relihan

-   [Requirements](#requirements)
-   [API Interaction Functions](#api-interaction-functions)
    -   [`country`](#country)
    -   [`all_countries`](#all_countries)
    -   [`country_cases`](#country_cases)
    -   [`live_cases`](#live_cases)
    -   [`world_totals`](#world_totals)
    -   [`day_one`](#day_one)
-   [Data Exploration](#data-exploration)
    -   [Percent of deaths by province in the United
        States.](#percent-of-deaths-by-province-in-the-united-states)

# Requirements

To use the functions for interacting with the COVID API, I used the
following packages:  
\* `httr` \* `jsonlite`

In addition to those packages, I used the following packages in the rest
of the document:  
\* `rmarkdown` \* `base` \* `tidyverse` \* `dplyr` \* `magrittr`

# API Interaction Functions

Here are the functions to interact with the Covid API.

### `country`

#### This function returns a data.frame with data on all countries. You can also return just the rows for a single country by adding the country name or ID.

``` r
country <- function(Country_name="all"){
  #Get the data
  get_json <- fromJSON("https://api.covid19api.com/summary")
  #select the data.frame
  country_df <- get_json$Countries[,1:11]
  #if country doesn't equal "all", check country name
  if (Country_name != "all"){
    #if country name is in the country column, subset for those rows
    if (Country_name %in% country_df$Country) {
      country_df <- country_df %>%
          filter(Country == Country_name)
    }
    #if country id is in the ID column, subset for those rows
    else if (Country_name %in% country_df$CountryCode){
      country_df <- country_df %>%
        filter(CountryCode == Country_name)
    }
    else {
      message <- paste("ERROR: Argument for Country or CountryCode was not found in the Country/CountryCode columns. Try Country('all') to find the country you're looking for.")
      stop(message)
    }
  }
  #Do nothing if the country equals "all"
  else{
    
  }
  #Return data frame
  return(country_df)
}
```

### `all_countries`

#### Returns a list of the countries available to select from

``` r
all_countries <- function(type="all"){
all_countries_df <- fromJSON("https://api.covid19api.com/countries")
if (type != "all"){
      message <- paste("ERROR: Argument for type was not found. Try type('all') to get the list of countries.")
      stop(message)
    }
  #Do nothing if the type equals "all"
  else{
    
  }
  #Return data frame
  return(all_countries_df)
}
```

### `country_cases`

#### Returns the number of cases for confirmed, active, deaths, and recovered for the country of interest (user specified).The default is “united-states”. Put a “-” in between each part of the country name if necessary.

``` r
country_cases <- function(type="united-states"){
  url_country_cases_df <- paste0("https://api.covid19api.com/total/country/",type)
  country_cases_df <- fromJSON(url_usa_cases_df) 
  return(country_cases_df)
}
```

### `live_cases`

#### Returns all live cases for a country of interest. The default is “united-states”. Put a “-” in between each part of the country name if necessary. You can also specify a date in the format “2021-07-12T00:00:00Z” and the Province.

``` r
live_cases <- function(type="united-states", date="all", province="all" ){
  url_live_cases_df <- paste0("https://api.covid19api.com/live/country/",type)
  live_cases_df <- fromJSON(url_live_cases_df) 
  if (date != "all"){
    #if date is in the Date column, subset for those rows
    if (date %in% live_cases_df$Date) {
      live_cases_df <- live_cases_df %>%
          filter(Date == date)
    }
  }
    if (province != "all"){
    #if province is in the Province column, subset for those rows
    if (province %in% live_cases_df$Province) {
      live_cases_df <- live_cases_df %>%
          filter(Province == province)
    }
    else {
      message <- paste("ERROR: Argument for province or date was not found in the columns.")
      stop(message)
    }
  }
  #Do nothing if the province equals "all"
  else{
    
  }
  #Return data frame  
  return(live_cases_df)
    }
```

### `world_totals`

#### Returns the world total for confirmed, deaths, and recovered cases

``` r
world_totals <- function(type="all"){
world_totals_df <- fromJSON("https://api.covid19api.com/world/total")
if (type != "all"){
      message <- paste("ERROR: Argument for type was not found. Try type('all') to get the dataset.")
      stop(message)
    }
  #Do nothing if the type equals "all"
  else{
    
  }
  #Return data frame
  return(as.data.frame(world_totals_df))
}
```

### `day_one`

#### Returns all cases by case type for a country from the first recorded case. Cases must be one of: confirmed, recovered, deaths. Formatting for type is either by Country name or slug (South Africa or south-africa)

``` r
day_one <- function(type="south-africa", case="confirmed" ){
  url_day_one_df <- paste0("https://api.covid19api.com/dayone/country/",type,"/","status","/",case)
  day_one_df <- fromJSON(url_day_one_df) 
  #Return data frame
  return(day_one_df)
}
```

# Data Exploration

Pulling in data.

``` r
#list of all countries available
country_list_df<- all_countries(type="all")
#All countries data
all_country_df<- country(Country_name = "all")
# Current cases in the US as of 10-04-2021
current_cases_df<- live_cases(type="united-states",date = "2021-10-04T00:00:00Z", province = "all")
```

Creating new variables: \#\#\# Percent of confirmed cases by province in
the United States.

``` r
#Changing integer columns to numeric
current_cases_df[,9:12]<- lapply(current_cases_df[,9:12],as.numeric)
#Sum of the confirmed cases
sum_confirmed <- sum(current_cases_df$Confirmed)
#Percent of confirmed cases by province
current_cases_df <- current_cases_df %>% mutate(Percent_Confirmed= (Confirmed/sum_confirmed)*100)
#Rounding percent confirmed to min amount of digits
current_cases_df[,14]<- round(current_cases_df$Percent_Confirmed, digits = 2)
```

### Percent of deaths by province in the United States.

``` r
# Sum of deaths
sum_deaths <- sum(current_cases_df$Deaths)
# Percent of deaths by province
current_cases_df <- current_cases_df %>% mutate(Percent_Deaths=(Deaths/sum_deaths)*100)
#Rounding percent deaths to min amount of digits
current_cases_df[,15]<- round(current_cases_df$Percent_Deaths, digits = 2)
```
