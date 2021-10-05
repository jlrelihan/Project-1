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
        -   [Bar Plot: Here I graphed each State and the number of
            confirmed cases colored by case
            count.](#bar-plot-here-i-graphed-each-state-and-the-number-of-confirmed-cases-colored-by-case-count)
        -   [Histogram: Here I graphed a histogram showing the frequency
            distribution of
            deaths.](#histogram-here-i-graphed-a-histogram-showing-the-frequency-distribution-of-deaths)
        -   [Box Plot: Here I graphed the distribution of the United
            States death count. You can clearly see three outliers that
            are represented in the first bar
            chart.](#box-plot-here-i-graphed-the-distribution-of-the-united-states-death-count-you-can-clearly-see-three-outliers-that-are-represented-in-the-first-bar-chart)
        -   [Scatter Plot: Here you can see the relationship between
            confirmed cases and deaths in the
            U.S.](#scatter-plot-here-you-can-see-the-relationship-between-confirmed-cases-and-deaths-in-the-us)
        -   [ECDF - Emipirical Cumulative Distribution
            Function](#ecdf---emipirical-cumulative-distribution-function)

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
print(table(current_cases_df$Province))
```

    ## 
    ##                  Alabama                   Alaska                  Arizona                 Arkansas               California                 Colorado 
    ##                        1                        1                        1                        1                        1                        1 
    ##              Connecticut                 Delaware     District of Columbia                  Florida                  Georgia                     Guam 
    ##                        1                        1                        1                        1                        1                        1 
    ##                   Hawaii                    Idaho                 Illinois                  Indiana                     Iowa                   Kansas 
    ##                        1                        1                        1                        1                        1                        1 
    ##                 Kentucky                Louisiana                    Maine                 Maryland            Massachusetts                 Michigan 
    ##                        1                        1                        1                        1                        1                        1 
    ##                Minnesota              Mississippi                 Missouri                  Montana                 Nebraska                   Nevada 
    ##                        1                        1                        1                        1                        1                        1 
    ##            New Hampshire               New Jersey               New Mexico                 New York           North Carolina             North Dakota 
    ##                        1                        1                        1                        1                        1                        1 
    ## Northern Mariana Islands                     Ohio                 Oklahoma                   Oregon             Pennsylvania              Puerto Rico 
    ##                        1                        1                        1                        1                        1                        1 
    ##             Rhode Island           South Carolina             South Dakota                Tennessee                    Texas                     Utah 
    ##                        1                        1                        1                        1                        1                        1 
    ##                  Vermont           Virgin Islands                 Virginia               Washington            West Virginia                Wisconsin 
    ##                        1                        1                        1                        1                        1                        1 
    ##                  Wyoming 
    ##                        1

``` r
# Showing how many rows for each Country
print(table(all_country_df$Country))
```

    ## 
    ##                     Afghanistan                         Albania                         Algeria                         Andorra                          Angola 
    ##                               1                               1                               1                               1                               1 
    ##             Antigua and Barbuda                       Argentina                         Armenia                       Australia                         Austria 
    ##                               1                               1                               1                               1                               1 
    ##                      Azerbaijan                         Bahamas                         Bahrain                      Bangladesh                        Barbados 
    ##                               1                               1                               1                               1                               1 
    ##                         Belarus                         Belgium                          Belize                           Benin                          Bhutan 
    ##                               1                               1                               1                               1                               1 
    ##                         Bolivia          Bosnia and Herzegovina                        Botswana                          Brazil               Brunei Darussalam 
    ##                               1                               1                               1                               1                               1 
    ##                        Bulgaria                    Burkina Faso                         Burundi                        Cambodia                        Cameroon 
    ##                               1                               1                               1                               1                               1 
    ##                          Canada                      Cape Verde        Central African Republic                            Chad                           Chile 
    ##                               1                               1                               1                               1                               1 
    ##                           China                        Colombia                         Comoros             Congo (Brazzaville)                Congo (Kinshasa) 
    ##                               1                               1                               1                               1                               1 
    ##                      Costa Rica                   Côte d'Ivoire                         Croatia                            Cuba                          Cyprus 
    ##                               1                               1                               1                               1                               1 
    ##                  Czech Republic                         Denmark                        Djibouti                        Dominica              Dominican Republic 
    ##                               1                               1                               1                               1                               1 
    ##                         Ecuador                           Egypt                     El Salvador               Equatorial Guinea                         Eritrea 
    ##                               1                               1                               1                               1                               1 
    ##                         Estonia                        Ethiopia                            Fiji                         Finland                          France 
    ##                               1                               1                               1                               1                               1 
    ##                           Gabon                          Gambia                         Georgia                         Germany                           Ghana 
    ##                               1                               1                               1                               1                               1 
    ##                          Greece                         Grenada                       Guatemala                          Guinea                   Guinea-Bissau 
    ##                               1                               1                               1                               1                               1 
    ##                          Guyana                           Haiti   Holy See (Vatican City State)                        Honduras                         Hungary 
    ##                               1                               1                               1                               1                               1 
    ##                         Iceland                           India                       Indonesia       Iran, Islamic Republic of                            Iraq 
    ##                               1                               1                               1                               1                               1 
    ##                         Ireland                          Israel                           Italy                         Jamaica                           Japan 
    ##                               1                               1                               1                               1                               1 
    ##                          Jordan                      Kazakhstan                           Kenya                        Kiribati                   Korea (South) 
    ##                               1                               1                               1                               1                               1 
    ##                          Kuwait                      Kyrgyzstan                         Lao PDR                          Latvia                         Lebanon 
    ##                               1                               1                               1                               1                               1 
    ##                         Lesotho                         Liberia                           Libya                   Liechtenstein                       Lithuania 
    ##                               1                               1                               1                               1                               1 
    ##                      Luxembourg          Macedonia, Republic of                      Madagascar                          Malawi                        Malaysia 
    ##                               1                               1                               1                               1                               1 
    ##                        Maldives                            Mali                           Malta                Marshall Islands                      Mauritania 
    ##                               1                               1                               1                               1                               1 
    ##                       Mauritius                          Mexico Micronesia, Federated States of                         Moldova                          Monaco 
    ##                               1                               1                               1                               1                               1 
    ##                        Mongolia                      Montenegro                         Morocco                      Mozambique                         Myanmar 
    ##                               1                               1                               1                               1                               1 
    ##                         Namibia                           Nepal                     Netherlands                     New Zealand                       Nicaragua 
    ##                               1                               1                               1                               1                               1 
    ##                           Niger                         Nigeria                          Norway                            Oman                        Pakistan 
    ##                               1                               1                               1                               1                               1 
    ##                           Palau           Palestinian Territory                          Panama                Papua New Guinea                        Paraguay 
    ##                               1                               1                               1                               1                               1 
    ##                            Peru                     Philippines                          Poland                        Portugal                           Qatar 
    ##                               1                               1                               1                               1                               1 
    ##              Republic of Kosovo                         Romania              Russian Federation                          Rwanda           Saint Kitts and Nevis 
    ##                               1                               1                               1                               1                               1 
    ##                     Saint Lucia    Saint Vincent and Grenadines                           Samoa                      San Marino           Sao Tome and Principe 
    ##                               1                               1                               1                               1                               1 
    ##                    Saudi Arabia                         Senegal                          Serbia                      Seychelles                    Sierra Leone 
    ##                               1                               1                               1                               1                               1 
    ##                       Singapore                        Slovakia                        Slovenia                 Solomon Islands                         Somalia 
    ##                               1                               1                               1                               1                               1 
    ##                    South Africa                     South Sudan                           Spain                       Sri Lanka                           Sudan 
    ##                               1                               1                               1                               1                               1 
    ##                        Suriname                       Swaziland                          Sweden                     Switzerland    Syrian Arab Republic (Syria) 
    ##                               1                               1                               1                               1                               1 
    ##       Taiwan, Republic of China                      Tajikistan    Tanzania, United Republic of                        Thailand                     Timor-Leste 
    ##                               1                               1                               1                               1                               1 
    ##                            Togo             Trinidad and Tobago                         Tunisia                          Turkey                          Uganda 
    ##                               1                               1                               1                               1                               1 
    ##                         Ukraine            United Arab Emirates                  United Kingdom        United States of America                         Uruguay 
    ##                               1                               1                               1                               1                               1 
    ##                      Uzbekistan                         Vanuatu Venezuela (Bolivarian Republic)                        Viet Nam                           Yemen 
    ##                               1                               1                               1                               1                               1 
    ##                          Zambia                        Zimbabwe 
    ##                               1                               1

### Numerical Summaries

``` r
# Mean of Current Deaths in the U.S. 
print(mean(current_cases_df$Deaths))
```

    ## [1] 12748.53

``` r
# Summary of current confirmed cases in the U.S.
print(summary(current_cases_df$Confirmed))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     270  162362  510209  794233  917655 4732419

``` r
# Quantiles of current confirmed cases in the U.S.
print(quantile(current_cases_df$Confirmed))
```

    ##      0%     25%     50%     75%    100% 
    ##     270  162362  510209  917655 4732419

## Plots:

### Bar Plot: Here I graphed each State and the number of confirmed cases colored by case count.

``` r
bar <- ggplot(current_cases_df, aes(x=Province,y=Confirmed)) + geom_col(aes(fill=Confirmed)) + theme_gray() + labs(x="State",y="Confirmed Cases", title = "Confirmed Cases in the United States") + guides(x=guide_axis(angle=90))
print(bar)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.jpeg)<!-- -->

### Histogram: Here I graphed a histogram showing the frequency distribution of deaths.

``` r
his <- ggplot(current_cases_df, aes(x=Deaths)) + geom_histogram(fill="darkslategray") + theme_gray() + labs(x="Number of Deaths",y="Count", title = "Count of States Number of Deaths")
print(his)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-14-1.jpeg)<!-- -->

### Box Plot: Here I graphed the distribution of the United States death count. You can clearly see three outliers that are represented in the first bar chart.

``` r
box <- ggplot(current_cases_df, aes(x=Country,y=Deaths)) + geom_boxplot(fill="grey") + theme_gray() + labs(x="Country",y="Death Count", title = "Count of Number of Deaths")
print(box)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.jpeg)<!-- -->

### Scatter Plot: Here you can see the relationship between confirmed cases and deaths in the U.S.

``` r
correlation <- cor(current_cases_df$Confirmed,current_cases_df$Deaths)
scatter <- ggplot(current_cases_df, aes(x=Confirmed, y=Deaths)) + geom_point()+ theme_gray() + labs(x="Confirmed Cases",y="Death Count", title = "Relation of Confirmed Cases to Death Count by Province") + geom_text(x=4e+06, y=10000, size=5, label=paste0("Correlation = ", round(correlation,2)))
print(scatter)
```

![](README_files/figure-gfm/unnamed-chunk-16-1.jpeg)<!-- -->

### ECDF - Emipirical Cumulative Distribution Function

``` r
ECDF <- ggplot(current_cases_df_select_states, aes(x=Deaths)) + stat_ecdf(geom="step") + theme_gray() + labs(x="Number of Deaths",y="Count", title = "ECDF Chart") + ylab("ECDF")
print(ECDF)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.jpeg)<!-- -->
