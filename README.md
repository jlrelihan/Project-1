Project 1
================
Jennifer Relihan

-   [Requirements](#requirements)
    -   [In addition to those packages, I used the following packages in
        the rest of the
        document:](#in-addition-to-those-packages-i-used-the-following-packages-in-the-rest-of-the-document)
-   [API Interaction Functions](#api-interaction-functions)
    -   [This function returns a data.frame with data on all countries.
        You can also return just the rows for a single country by adding
        the country name or
        ID.](#this-function-returns-a-dataframe-with-data-on-all-countries-you-can-also-return-just-the-rows-for-a-single-country-by-adding-the-country-name-or-id)
    -   [Returns a list of the countries available to select
        from](#returns-a-list-of-the-countries-available-to-select-from)
    -   [Returns the number of cases for confirmed, active, deaths, and
        recovered for the country of interest (user specified).The
        default is “united-states”. Put a “-” in between each part of
        the country name if
        necessary.](#returns-the-number-of-cases-for-confirmed-active-deaths-and-recovered-for-the-country-of-interest-user-specifiedthe-default-is-united-states-put-a---in-between-each-part-of-the-country-name-if-necessary)
    -   [Returns all live cases for a country of interest. The default
        is “united-states”. Put a “-” in between each part of the
        country name if necessary. You can also specify a date in the
        format “2021-07-12T00:00:00Z” and the
        Province.](#returns-all-live-cases-for-a-country-of-interest-the-default-is-united-states-put-a---in-between-each-part-of-the-country-name-if-necessary-you-can-also-specify-a-date-in-the-format-2021-07-12t000000z-and-the-province)
    -   [Returns the world total for confirmed, deaths, and recovered
        cases](#returns-the-world-total-for-confirmed-deaths-and-recovered-cases)
    -   [Returns all cases by case type for a country from the first
        recorded case. Cases must be one of: confirmed, recovered,
        deaths.](#returns-all-cases-by-case-type-for-a-country-from-the-first-recorded-case-cases-must-be-one-of-confirmed-recovered-deaths)

# Requirements

To use the functions for interacting with the COVID API, I used the
following packages:  
\* `httr` \* `jsonlite`

## In addition to those packages, I used the following packages in the rest of the document:

    * `rmarkdown`
    * `base`
    * `tidyverse`
    * `dplyr`

Setting up working directory and reading in required packages.

# API Interaction Functions

Here are the functions to interact with the Covid API.

#### `country`

### This function returns a data.frame with data on all countries. You can also return just the rows for a single country by adding the country name or ID.

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

#### `all_countries`

### Returns a list of the countries available to select from

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

#### `country_cases`

### Returns the number of cases for confirmed, active, deaths, and recovered for the country of interest (user specified).The default is “united-states”. Put a “-” in between each part of the country name if necessary.

``` r
country_cases <- function(type="united-states"){
  url_country_cases_df <- paste0("https://api.covid19api.com/total/country/",type)
  country_cases_df <- fromJSON(url_usa_cases_df) 
  return(country_cases_df)
}
```

#### `live_cases`

### Returns all live cases for a country of interest. The default is “united-states”. Put a “-” in between each part of the country name if necessary. You can also specify a date in the format “2021-07-12T00:00:00Z” and the Province.

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

#### `world_totals`

### Returns the world total for confirmed, deaths, and recovered cases

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

#### `day_one`

### Returns all cases by case type for a country from the first recorded case. Cases must be one of: confirmed, recovered, deaths.

``` r
day_one <- function(type="south-africa", case="confirmed" ){
  url_day_one_df <- paste0("https://api.covid19api.com/dayone/country/",type,"/","status","/",case)
  day_one_df <- fromJSON(url_day_one_df) 
  #Return data frame
  return(day_one_df)
}
```
