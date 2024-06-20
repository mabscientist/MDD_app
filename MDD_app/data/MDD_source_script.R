
# setup -------------------------------------------------------------------

# load required packages:

library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)

# country polygon setup --------------------------------------------------

# load, modify, assign key country polygons
countries_sf <- 
  
  # read in country polygons file from Natural Earth
  
  read_sf('data/countries.geojson') %>% 
  rename(country = ADMIN) %>% 
  
  # initial name changing; see below
  
  mutate(country =
           str_replace(country,
                       ' and ',
                       ' & ')) %>% 
  
  # basically this is why I have a source script
  # change names in sf to names that match MDD format
  
  mutate(country = case_when(
    country == 'United States of America' ~ 'United States',
    country == 'The Bahamas' ~ 'Bahamas',
    country == 'United Republic of Tanzania' ~ 'Tanzania',
    country == 'Sao Tome & Principe' ~ 'São Tomé & Príncipe',
    country == 'Saint Barthelemy' ~ 'Saint Barthélemy',
    country == 'Ivory Coast' ~ "Côte d'Ivoire",
    country == 'Republic of Congo' ~ 'Republic of the Congo',
    country == 'Falkland Islands' ~ 'Falkland Islands (Malvinas)',
    country == 'Federated States of Micronesia' ~ 'Micronesia',
    country == 'Guinea Bissau' ~ 'Guinea-Bissau',
    country == 'Pitcairn Islands' ~ 'Pitcairn',
    country == 'South Georgia & South Sandwich Islands' ~ 'South Georgia & the South Sandwich Islands',
    country == 'Republic of Serbia' ~ 'Serbia',
    country == 'Cape Verde' ~ 'Cabo Verde',
    country == 'Swaziland' ~ 'Eswatini',
    country == 'Macedonia' ~ 'North Macedonia',
    
    TRUE ~ as.character(country))) %>% 
  
  # transform to equal area CRS for static maps
  
  st_transform(crs = 8857)

# I don't like this projection but it's tmap/leaflet's so I need this too:

countries_tmap <-
  countries_sf %>% 
  
  # transform to necessary crs
  
  st_transform(crs = 3857)

# not using this for country names but to link countries to continents
continent_key <-
  read_csv('data/country_continent_key.csv') %>% 
  
  # yes they're not all countries but simplifying column name:
  
  rename(country = country_territory)

# MDD setup -------------------------------------------------------

MDD_tbl <-
  read_csv('data/MDD_6.20.2024.csv') %>% 
  
  # don't include domestic species or recently extinct species
  # don't include the one species with no country distribution data
  
  filter(
    extinct == 0,
    domestic == 0,
    !is.na(countryDistribution)) %>%
  
  # select relevant columns only as tidily as possible
  
  select(sciName,
         order,
         authyear = authoritySpeciesYear,
         long = typeLocalityLongitude,
         lat = typeLocalityLatitude,
         countryDistribution,
         iucn = iucnStatus)  %>% 
  
  # convert from camel case to all lowercase
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  # need to make countries into tidyish data
  # this makes lists of countries
  
  mutate(
    
    # split old column into vector on | delimiter
    
    countries = str_split(countrydistribution, pattern = "\\|"),
    
    # dropping original column
    
    .keep = "unused") 

# create tibble of only species with type locality long/lat data:

points <-
  MDD_tbl %>% 
  
  # filter for non-NA longitude values
  
  filter(!is.na(long)) %>% 
  
  # filter for non-NA latitude values
  
  filter(!is.na(lat)) %>% 

  # filter for uncertain lat/lon values
  
  filter(!grepl("\\(", long)) %>% 
  filter(!grepl("\\(", lat)) %>% 
  
  # make into shapefile
  
  st_as_sf(
    coords = c('long', 'lat'),
    crs = 4326) %>% 
  
  # set to leaflet/tmap projection:
  
  st_transform(crs = 3857)


# MDD tidying ---------------------------------------------------------

# make separate tibble of endemics only; inherently tidy

endemics <-
  MDD_tbl %>% 
  
  # filter to species with just one country in distribution
  
  filter(
    lengths(countries) == 1) %>% 
  
  # now just 1 per cell but still in list format; unlist
  
  mutate(country =
           unlist(countries),
         
         # drop old column
         
         .keep = "unused")

# creates lenient, long version of MDD
# distrib. in country is known OR vagrant/hypothesized:

MDD_lenient <-
  MDD_tbl %>% 
  
  # rename countries column since it will soon only have 1 per cell
  
  rename(country = countries) %>% 
  
  # unnest to make tidy
  
  unnest_longer(country) %>% 
  
  # remove blanks
  
  filter(!country == '') %>% 
  
  # replace countries column with one with same name
  
  mutate(country =
           
           # remove question marks at the end of countries; keep entries
           
           str_replace(country, '\\?', ''))

rm(MDD_tbl)

# I use this to quickly calculate percentage threatened & endemics later
# Makes summary tibble of species richness per country:

totals <- 
  MDD_lenient %>% 
  
  # calculate # species per country:
  
  group_by(country) %>% 
  summarize(total = n()) 

# function & color for graphs------------------------------------------------

# palette and hex codes from RColorBrewer
# split continents indicate transcontinental countries e.g., Indonesia

top_colors <-
  set_names(
    c('#66c2a5',
      '#fc8d62',
      '#8da0cb',
      '#e78ac3',
      '#a6d854',
      '#ffd92f',
      '#e5c494',
      '#b3b3b3'),
    nm = c('Europe',
           'Asia',
           'Africa',
           'Oceania',
           'North America',
           'South America',
           'Asia|Europe',
           'Asia|Oceania')) 

# makes graph of top 10 countries of #species by continent
# use to make consistent graphs throughout project

make_top_10 <- function(country_tbl){
  country_tbl %>% 
    
    # calculate # of species per country
    
    group_by(country) %>% 
    summarize(n = n()) %>% 
    
    # take top 10 countries
    
    arrange(desc(n)) %>% 
    slice(1:10) %>% 
    
    # arrange in ascending order
    
    arrange(n) %>% 
    
    # join to continents
    
    inner_join(continent_key, 
               by = 'country') %>% 
    
    # refactor to make graph less top heavy later
    
    mutate(country = 
             factor(country) %>% 
             fct_reorder(desc(n))) %>% 
    
    # plot:
    
    ggplot(aes(x = country,
               y = n,
               fill = continent)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = top_colors) +
    scale_y_continuous(expand=c(0,0))+
    coord_flip() +
    labs(x = 'Country',
         y = 'Number of Mammal Species',
         fill = 'Continent') +
    theme_minimal()}
