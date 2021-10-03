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
    -   [Creating new variables:](#creating-new-variables)
        -   [Variable 1: Percent of confirmed cases by province in the
            United
            States.](#variable-1-percent-of-confirmed-cases-by-province-in-the-united-states)
        -   [Variable 2: Percent of deaths by province in the United
            States.](#variable-2-percent-of-deaths-by-province-in-the-united-states)
    -   [Summary:](#summary)
        -   [Contingency Tables:](#contingency-tables)
        -   [Numerical Summaries](#numerical-summaries)
    -   [Plots:](#plots)
        -   [Bar Plot](#bar-plot)
        -   [Histogram](#histogram)
        -   [Box Plot](#box-plot)
        -   [Scatter Plot](#scatter-plot)
        -   [General Plots](#general-plots)

# Requirements

To use the functions for interacting with the COVID API, I used the
following packages:  
\* `httr` \* `jsonlite`

In addition to those packages, I used the following packages in the rest
of the document:  
\* `rmarkdown` \* `base` \* `tidyverse` \* `dplyr` \* `magrittr` \*
`ggplot2`

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
#sub-setting data
current_cases_df_select_states <- current_cases_df %>% filter(Province == "Massachusetts" | Province == "New York"|Province == "North Carolina"|Province == "Texas"|Province == "California")
```

## Creating new variables:

### Variable 1: Percent of confirmed cases by province in the United States.

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

### Variable 2: Percent of deaths by province in the United States.

``` r
# Sum of deaths
sum_deaths <- sum(current_cases_df$Deaths)
# Percent of deaths by province
current_cases_df <- current_cases_df %>% mutate(Percent_Deaths=(Deaths/sum_deaths)*100)
#Rounding percent deaths to min amount of digits
current_cases_df[,15]<- round(current_cases_df$Percent_Deaths, digits = 2)
```

## Summary:

### Contingency Tables:

``` r
#Showing how many rows for each state
table(current_cases_df$Province)
```

    ## 
    ##                  Alabama                   Alaska                  Arizona 
    ##                        1                        1                        1 
    ##                 Arkansas               California                 Colorado 
    ##                        1                        1                        1 
    ##              Connecticut                 Delaware     District of Columbia 
    ##                        1                        1                        1 
    ##                  Florida                  Georgia                     Guam 
    ##                        1                        1                        1 
    ##                   Hawaii                    Idaho                 Illinois 
    ##                        1                        1                        1 
    ##                  Indiana                     Iowa                   Kansas 
    ##                        1                        1                        1 
    ##                 Kentucky                Louisiana                    Maine 
    ##                        1                        1                        1 
    ##                 Maryland            Massachusetts                 Michigan 
    ##                        1                        1                        1 
    ##                Minnesota              Mississippi                 Missouri 
    ##                        1                        1                        1 
    ##                  Montana                 Nebraska                   Nevada 
    ##                        1                        1                        1 
    ##            New Hampshire               New Jersey               New Mexico 
    ##                        1                        1                        1 
    ##                 New York           North Carolina             North Dakota 
    ##                        1                        1                        1 
    ## Northern Mariana Islands                     Ohio                 Oklahoma 
    ##                        1                        1                        1 
    ##                   Oregon             Pennsylvania              Puerto Rico 
    ##                        1                        1                        1 
    ##             Rhode Island           South Carolina             South Dakota 
    ##                        1                        1                        1 
    ##                Tennessee                    Texas                     Utah 
    ##                        1                        1                        1 
    ##                  Vermont           Virgin Islands                 Virginia 
    ##                        1                        1                        1 
    ##               Washington            West Virginia                Wisconsin 
    ##                        1                        1                        1 
    ##                  Wyoming 
    ##                        1

``` r
# Showing how many rows for each Country
table(all_country_df$Country)
```

    ## 
    ##                     Afghanistan                         Albania 
    ##                               1                               1 
    ##                         Algeria                         Andorra 
    ##                               1                               1 
    ##                          Angola             Antigua and Barbuda 
    ##                               1                               1 
    ##                       Argentina                         Armenia 
    ##                               1                               1 
    ##                       Australia                         Austria 
    ##                               1                               1 
    ##                      Azerbaijan                         Bahamas 
    ##                               1                               1 
    ##                         Bahrain                      Bangladesh 
    ##                               1                               1 
    ##                        Barbados                         Belarus 
    ##                               1                               1 
    ##                         Belgium                          Belize 
    ##                               1                               1 
    ##                           Benin                          Bhutan 
    ##                               1                               1 
    ##                         Bolivia          Bosnia and Herzegovina 
    ##                               1                               1 
    ##                        Botswana                          Brazil 
    ##                               1                               1 
    ##               Brunei Darussalam                        Bulgaria 
    ##                               1                               1 
    ##                    Burkina Faso                         Burundi 
    ##                               1                               1 
    ##                        Cambodia                        Cameroon 
    ##                               1                               1 
    ##                          Canada                      Cape Verde 
    ##                               1                               1 
    ##        Central African Republic                            Chad 
    ##                               1                               1 
    ##                           Chile                           China 
    ##                               1                               1 
    ##                        Colombia                         Comoros 
    ##                               1                               1 
    ##             Congo (Brazzaville)                Congo (Kinshasa) 
    ##                               1                               1 
    ##                      Costa Rica                   Côte d'Ivoire 
    ##                               1                               1 
    ##                         Croatia                            Cuba 
    ##                               1                               1 
    ##                          Cyprus                  Czech Republic 
    ##                               1                               1 
    ##                         Denmark                        Djibouti 
    ##                               1                               1 
    ##                        Dominica              Dominican Republic 
    ##                               1                               1 
    ##                         Ecuador                           Egypt 
    ##                               1                               1 
    ##                     El Salvador               Equatorial Guinea 
    ##                               1                               1 
    ##                         Eritrea                         Estonia 
    ##                               1                               1 
    ##                        Ethiopia                            Fiji 
    ##                               1                               1 
    ##                         Finland                          France 
    ##                               1                               1 
    ##                           Gabon                          Gambia 
    ##                               1                               1 
    ##                         Georgia                         Germany 
    ##                               1                               1 
    ##                           Ghana                          Greece 
    ##                               1                               1 
    ##                         Grenada                       Guatemala 
    ##                               1                               1 
    ##                          Guinea                   Guinea-Bissau 
    ##                               1                               1 
    ##                          Guyana                           Haiti 
    ##                               1                               1 
    ##   Holy See (Vatican City State)                        Honduras 
    ##                               1                               1 
    ##                         Hungary                         Iceland 
    ##                               1                               1 
    ##                           India                       Indonesia 
    ##                               1                               1 
    ##       Iran, Islamic Republic of                            Iraq 
    ##                               1                               1 
    ##                         Ireland                          Israel 
    ##                               1                               1 
    ##                           Italy                         Jamaica 
    ##                               1                               1 
    ##                           Japan                          Jordan 
    ##                               1                               1 
    ##                      Kazakhstan                           Kenya 
    ##                               1                               1 
    ##                        Kiribati                   Korea (South) 
    ##                               1                               1 
    ##                          Kuwait                      Kyrgyzstan 
    ##                               1                               1 
    ##                         Lao PDR                          Latvia 
    ##                               1                               1 
    ##                         Lebanon                         Lesotho 
    ##                               1                               1 
    ##                         Liberia                           Libya 
    ##                               1                               1 
    ##                   Liechtenstein                       Lithuania 
    ##                               1                               1 
    ##                      Luxembourg          Macedonia, Republic of 
    ##                               1                               1 
    ##                      Madagascar                          Malawi 
    ##                               1                               1 
    ##                        Malaysia                        Maldives 
    ##                               1                               1 
    ##                            Mali                           Malta 
    ##                               1                               1 
    ##                Marshall Islands                      Mauritania 
    ##                               1                               1 
    ##                       Mauritius                          Mexico 
    ##                               1                               1 
    ## Micronesia, Federated States of                         Moldova 
    ##                               1                               1 
    ##                          Monaco                        Mongolia 
    ##                               1                               1 
    ##                      Montenegro                         Morocco 
    ##                               1                               1 
    ##                      Mozambique                         Myanmar 
    ##                               1                               1 
    ##                         Namibia                           Nepal 
    ##                               1                               1 
    ##                     Netherlands                     New Zealand 
    ##                               1                               1 
    ##                       Nicaragua                           Niger 
    ##                               1                               1 
    ##                         Nigeria                          Norway 
    ##                               1                               1 
    ##                            Oman                        Pakistan 
    ##                               1                               1 
    ##                           Palau           Palestinian Territory 
    ##                               1                               1 
    ##                          Panama                Papua New Guinea 
    ##                               1                               1 
    ##                        Paraguay                            Peru 
    ##                               1                               1 
    ##                     Philippines                          Poland 
    ##                               1                               1 
    ##                        Portugal                           Qatar 
    ##                               1                               1 
    ##              Republic of Kosovo                         Romania 
    ##                               1                               1 
    ##              Russian Federation                          Rwanda 
    ##                               1                               1 
    ##           Saint Kitts and Nevis                     Saint Lucia 
    ##                               1                               1 
    ##    Saint Vincent and Grenadines                           Samoa 
    ##                               1                               1 
    ##                      San Marino           Sao Tome and Principe 
    ##                               1                               1 
    ##                    Saudi Arabia                         Senegal 
    ##                               1                               1 
    ##                          Serbia                      Seychelles 
    ##                               1                               1 
    ##                    Sierra Leone                       Singapore 
    ##                               1                               1 
    ##                        Slovakia                        Slovenia 
    ##                               1                               1 
    ##                 Solomon Islands                         Somalia 
    ##                               1                               1 
    ##                    South Africa                     South Sudan 
    ##                               1                               1 
    ##                           Spain                       Sri Lanka 
    ##                               1                               1 
    ##                           Sudan                        Suriname 
    ##                               1                               1 
    ##                       Swaziland                          Sweden 
    ##                               1                               1 
    ##                     Switzerland    Syrian Arab Republic (Syria) 
    ##                               1                               1 
    ##       Taiwan, Republic of China                      Tajikistan 
    ##                               1                               1 
    ##    Tanzania, United Republic of                        Thailand 
    ##                               1                               1 
    ##                     Timor-Leste                            Togo 
    ##                               1                               1 
    ##             Trinidad and Tobago                         Tunisia 
    ##                               1                               1 
    ##                          Turkey                          Uganda 
    ##                               1                               1 
    ##                         Ukraine            United Arab Emirates 
    ##                               1                               1 
    ##                  United Kingdom        United States of America 
    ##                               1                               1 
    ##                         Uruguay                      Uzbekistan 
    ##                               1                               1 
    ##                         Vanuatu Venezuela (Bolivarian Republic) 
    ##                               1                               1 
    ##                        Viet Nam                           Yemen 
    ##                               1                               1 
    ##                          Zambia                        Zimbabwe 
    ##                               1                               1

### Numerical Summaries

``` r
# Mean of Current Deaths in the U.S. 
mean(current_cases_df$Deaths)
```

    ## [1] 12748.53

``` r
# Summary of current confirmed cases in the U.S.
summary(current_cases_df$Confirmed)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     270  162362  510209  794233  917655 4732419

``` r
# Quantiles of current confirmed cases in the U.S.
quantile(current_cases_df$Confirmed)
```

    ##      0%     25%     50%     75%    100% 
    ##     270  162362  510209  917655 4732419

## Plots:

### Bar Plot

``` r
bar <- ggplot(current_cases_df, aes(x=Province,y=Confirmed))
bar + geom_col(aes(fill=Confirmed)) + theme_gray() + labs(x="State",y="Confirmed Cases", title = "Confirmed Cases in the United States") + guides(x=guide_axis(angle=90))
```

![](README_files/figure-gfm/unnamed-chunk-198-1.jpeg)<!-- -->

### Histogram

``` r
his <- ggplot(current_cases_df, aes(x=Deaths))
his + geom_histogram(fill="darkslategray") + theme_gray() + labs(x="Number of Deaths",y="Count", title = "Count of States Number of Deaths")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-199-1.jpeg)<!-- -->

### Box Plot

``` r
box <- ggplot(current_cases_df, aes(x=Country,y=Deaths))
box + geom_boxplot(fill="grey") + theme_gray() + labs(x="Country",y="Death Count", title = "Count of Number of Deaths")
```

![](README_files/figure-gfm/unnamed-chunk-200-1.jpeg)<!-- -->

### Scatter Plot

### General Plots
