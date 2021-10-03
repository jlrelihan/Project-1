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

### Returns the number of cases for confirmed, active, deaths, and recovered for the country of interest (user specified).The default is “united-states”. Put a “-” in between each part of the country name if necessary.

``` r
usa_cases <- function(type="united-states"){
  url_usa_cases_df <- paste0("https://api.covid19api.com/total/country/",type)
  usa_cases_df <- fromJSON(url_usa_cases_df) 
  return(usa_cases_df)
}
```
