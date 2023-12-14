library(tidyverse)

gapminder_data <- read_csv("gapminder_data.csv")

summarize(gapminder_data, averagelifeExp = mean(lifeExp))

gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp))

# %>%
# shift + control + m

gapminder_data_summarized <- gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))

#danger!
gapminder_data <- gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(recent_year = max(year))

gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(average = mean(lifeExp))

#what is the average GDP per capita for the first year in the dataset?
gapminder_data %>% 
  summarize(recent_year = min(year))
#what is the average life expectancy for the first year in the data set?
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarize(average = mean(lifeExp))
#what is the avg GSP per capita for the first year in the dataset cont?
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarize(average = mean(gdpPercap))
#another way
gapminder_data %>%
  filter(year == min(year)) %>% 
  summarize(average = mean(gdpPercap))
#yet another way
my_object <- gapminder_data %>% 
  summarize(first_year=min(year))

gapminder_data %>% 
  filter(year == pull(my_object)) %>% 
  summarize(average = mean(gdpPercap))

#find life expectancy for every single year
# group_by command

gapminder_data %>% 
  group_by(year) %>% 
  summarize(average = mean(lifeExp))

#calculate average life expectancy by continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average = mean(lifeExp))

#create new column that calculates new life exp for each continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(aver = mean(lifeExp), min = min(lifeExp))

#mutate() creates new columns with existing data
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap)

#multiply by constants e.g., make new column representing population in millions
gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popInMillions = pop/1000000)

#select() subset or pull out specific portions of our data you're interested in
#only want to see population and year values
gapminder_data %>% 
  select(pop, year)
# - means what you're subtracting from dataset
gapminder_data %>% 
  select(-continent)
# use select() command to produce a dataframe that has only the columns country, continent, year, and life exp
gapminder_data %>% 
  select(country, continent, year, lifeExp)
gapminder_data %>% 
  select(-pop, -gdpPercap)
#select has helper functions built into it to help you filter through data
#pick through certain features of column name
gapminder_data %>% 
  select(year, starts_with('c'))
gapminder_data %>% 
  select(ends_with("p"))

?select
gapminder_data %>% 
  select(year, starts_with('con'))

#transfer between wide and long dataset
#pivot wider and pivot longer

#pivot wider
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

# new dataset
getwd()
gapminder_data_2007 <- read_csv("gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

temp <- read_csv("co2-un-data.csv")

#skip = 
read_csv('co2-un-data.csv', skip = 1)

#remedy column names; tell R what to call them 
co2_emissions_dirty <- read_csv("co2-un-data.csv", skip = 2, 
                                col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions_dirty

#another way to deal with warnings
read_csv("co2-un-data.csv", skip = 1) %>% 
  rename(country = ...2)

#pull out specific columns
co2_emissions_dirty %>% 
  select(country, year, series, value)

#couple issues in series; different measurements
#really long titles in series; we should make them shorter so we don't have long sentences
#shorten them to "total emissions" and "per capita emissions" using we code function
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))

#use pivot wider to spread out series and value
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value)

#get values out of dataset that are closest to 2007
#first get count per year - 2005 and 2010 are primary candidates
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  count(year)

#pick 2005
#filter out data for just 2005 and drop out year column
co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) 

#save co2 emissions into new object now that it is filtered and clean!
co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#which column do we want to use to merge two datasets together? COUNTRY!
#country is our "key"; may run into some problems with not matching perfectly with other columns or missing data in other places though
#gap data 2007 and co2 emissions
#use "join"s
inner_join(gapminder_data, co2_emissions)
#inner_join joined it automatically by country bc it recognized it as shared across both datasets

#this gives us same result, but we specify this way (in case there are multiple ways to join, you can use this)
inner_join(gapminder_data, co2_emissions, by = "country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() + 
  labs(x = "GDP (per capita)", y = "CO2 emitted (per capita)", title = "Strong association between a nation's GDP and CO2 production")
