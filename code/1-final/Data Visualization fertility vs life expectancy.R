#some question wording may be from HarvardX staff
#fertility vs life expectancy
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
names(gapminder)
gapminder %>% filter(continent == "Africa" & year == 2012) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

#now with color for diff regions of africa
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(continent == "Africa" & year == 2012) %>%
  ggplot(aes(fertility, life_expectancy, color = region)) +
  geom_point()

#select only countries in africa in 2012 with fertility 3 or less & life exp at least
#70
library(dplyr)
library(dslabs)
data(gapminder)
df <- gapminder %>% filter(continent == "Africa" & year == 2012 & 
                             fertility <= 3 & life_expectancy >= 70) %>%
  select(country, region)

#make a table to filter if vietnam, US and between 1960 & 2010
library(dplyr)
library(dslabs)
data(gapminder)
tab <- filter(gapminder, country %in% c("United States", "Vietnam") 
              & year >= 1960 & year <= 2010)
#graph it
p <- tab %>% ggplot(aes(year, life_expectancy, color = country)) + geom_line()
p

#same thing for cambodia
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(country == "Cambodia" & year >= 1960 & 
                       year <= 2010) %>% ggplot(aes(year, life_expectancy)) + 
  geom_line()

#africa gdp per day
library(dplyr)
library(dslabs)
data(gapminder)
daydollars <- mutate(gapminder, dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year == 2010 & gdp != "NA")
#plot smooth density plot
daydollars %>% ggplot(aes(dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2")
#faceted sdp
library(ggplot2)
library(dslabs)
data(gapminder)
daydollars2 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(1970, 2010) & gdp != "NA") %>%
  ggplot(aes(dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .)
daydollars2

#faceted sdp & stacked histogram by region
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day, fill = region)) +
  geom_density(bw = 0.5, position = "stack") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .) +
  geom_histogram(position = "stack")

#scatter plot of dollars per day vs. infant mortality
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_Africa_2010 <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year == 2010 & !is.na(gdp)) 
gapminder_Africa_2010 %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point()
gapminder_Africa_2010

#transform x axis to base log2 base scale
gapminder_Africa_2010 %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point()
scale_x_continuous(trans = "log2")

#add labels for country names  
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, 
                                     color = region, label = country)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  geom_text()

#facet grid by year
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_africa_19702010 <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(gdp) & !is.na(infant_mortality))
gapminder_africa_19702010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_point()+
  geom_text()+
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .)

#take a look at fertility
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_africa_19702010 <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(gdp) & !is.na(fertility))
gapminder_africa_19702010 %>% 
  ggplot(aes(dollars_per_day, fertility, color = region, label = country)) +
  geom_point()+
  geom_text()+
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .)