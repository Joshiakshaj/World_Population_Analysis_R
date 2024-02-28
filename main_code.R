library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(hms)
library(ggplot2)
library(reshape2)
library(ggpie)
library(plotly)
library(plotrix)
library(gridExtra)

#importing the dataset and viewing the column names
world_pop <- read.csv("C:/Users/aksha/Downloads/Data_Analysis_Projects/Wolrd_Population_DA/world_population.csv")
colnames(world_pop)

#renaming the column names for the dataset
colnames(world_pop)[6] <- "population_2022"
colnames(world_pop)[7] <- "population_2020"
colnames(world_pop)[8] <- "population_2015"
colnames(world_pop)[9] <- "population_2010"
colnames(world_pop)[10] <- "population_2000"
colnames(world_pop)[11] <- "population_1990"
colnames(world_pop)[12] <- "population_1980"
colnames(world_pop)[13] <- "population_1970"
colnames(world_pop)[14] <- "square_area"
colnames(world_pop)[15] <- "pop_density"
colnames(world_pop)[16] <- "growth_rate"
colnames(world_pop)[17] <- "world_pop_percent"

#checkiing the column names and checking the summary of the dataset
colnames(world_pop)

sum(is.na(world_pop))

str(world_pop)

###Visualizing The Number of Countries Per Continent###

# number of countries per continent
country_count <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(Count = n())

countries_per_continent <-ggplot(country_count, aes(x=Continent, y=Count, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= Count), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Countries Count', title = 'Countries per Continent')+
  theme(axis.text.x = element_text(angle = 45))
countries_per_continent

# visualizing the countries count as a pie chart
fig <- plot_ly(type='pie', labels=country_count$Continent, values=country_count$Count, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = 'Countries per Continent')
fig

#Total Population by Year by Continent#
#YEAR 2022

pop_2022 <- world_pop %>%
  group_by(Continent)%>%
  summarise(total_pop = sum(population_2022)) %>%
  arrange (desc(total_pop))

pop2022 <- ggplot(pop_2022, aes(x=Continent, y=total_pop , fill = Continent)) + geom_col(position = "dodge") + geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (2022)')+
  theme(axis.text.x = element_text(angle = 45))
pop2022

# 2020
pop_2020 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_2020)) %>%
  arrange(desc(total_pop))

pop2020 <- ggplot(pop_2020, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (2020)')+
  theme(axis.text.x = element_text(angle = 45))
pop2020

# 2015
pop_2015 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_2015)) %>%
  arrange(desc(total_pop))

pop2015 <- ggplot(pop_2015, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (2015)')+
  theme(axis.text.x = element_text(angle = 45))
pop2015

# 2010
pop_2010 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_2010)) %>%
  arrange(desc(total_pop))

pop2010 <- ggplot(pop_2010, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (2010)')+
  theme(axis.text.x = element_text(angle = 45))
pop2010

# 2000
pop_2000 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_2000)) %>%
  arrange(desc(total_pop))

pop2000 <- ggplot(pop_2000, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (2000)')+
  theme(axis.text.x = element_text(angle = 45))
pop2000

# 1990
pop_1990 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_1990)) %>%
  arrange(desc(total_pop))

pop1990 <- ggplot(pop_1990, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (1990)')+
  theme(axis.text.x = element_text(angle = 45))

# 1980
pop_1980 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_1980)) %>%
  arrange(desc(total_pop))

pop1980 <- ggplot(pop_1980, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (1980)')+
  theme(axis.text.x = element_text(angle = 45))

# 1970
pop_1970 <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop = sum(population_1970)) %>%
  arrange(desc(total_pop))

pop1970 <- ggplot(pop_1970, aes(x=Continent, y=total_pop, fill = Continent)) + geom_col(position = "dodge") +
  geom_text(aes(label= total_pop), vjust=0, size=3) +
  labs(x= 'Continent', y = 'Total Population', title = 'Total Population by Continent by Year (1970)')+
  theme(axis.text.x = element_text(angle = 45))

pop1990
pop1980
pop1970

#ggarrange(pop1990, pop1980, pop1970 + rremove("x.text"), labels = c("1990", "1980", "1970"),ncol = 2, nrow = 2)

# Total population trend by continent
# Performing some data manipulation to create a new table to place the continents as columns and the total population as rows
pop_combined <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop_2022 = sum(population_2022), total_pop_2020 = sum(population_2020),
            total_pop_2015 = sum(population_2015), total_pop_2010 = sum(population_2010),
            total_pop_2000 = sum(population_2000), total_pop_1990 = sum(population_1990),
            total_pop_1980 = sum(population_1980), total_pop_1970 = sum(population_1970)) 

pop_combined <- t(select(pop_combined, "total_pop_2022", "total_pop_2020", "total_pop_2015", "total_pop_2010",
                         "total_pop_2000", "total_pop_1990", "total_pop_1980", "total_pop_1970"))

colnames(pop_combined) <- c("Africa", "Asia", "Europe", "North_America", "Oceania", "South_America")
pop_coombined_new <- cbind(Years = rownames(pop_combined), pop_combined)
rownames(pop_coombined_new)<- 1:nrow(pop_coombined_new)
colnames(pop_coombined_new) <- c("Years","Africa", "Asia", "Europe", "North_America", "Oceania", "South_America")
pop_coombined_new <- as.data.frame(pop_coombined_new)

# Total population trend by continent
# Africa
Africa <- ggplot(pop_coombined_new, aes(x=Years, y=reorder(Years,Africa), fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Africa), vjust=0, size=3) +
  labs(x= 'Years', y = 'Africa', title = 'Total Population Trend for Africa')+
  theme(axis.text.x = element_text(angle = 45))

# Asia
Asia <-ggplot(pop_coombined_new, aes(x=Years, y=Asia, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Asia), vjust=0, size=3) +
  labs(x= 'Years', y = 'Asia', title = 'Total Population Trend for Asia')+
  theme(axis.text.x = element_text(angle = 45))

# Europe
Europe <-ggplot(pop_coombined_new, aes(x=Years, y=Europe, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Europe), vjust=0, size=3) +
  labs(x= 'Years', y = 'Europe', title = 'Total Population Trend for Europe')+
  theme(axis.text.x = element_text(angle = 45))

# North America
North_America <-ggplot(pop_coombined_new, aes(x=Years, y=North_America, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= North_America), vjust=0, size=3) +
  labs(x= 'Years', y = 'North America', title = 'Total Population Trend for North America')+
  theme(axis.text.x = element_text(angle = 45))

# Oceania
Oceania <- ggplot(pop_coombined_new, aes(x=Years, y=Oceania, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Oceania), vjust=0, size=3) +
  labs(x= 'Years', y = 'Oceania', title = 'Total Population Trend for Oceania')+
  theme(axis.text.x = element_text(angle = 45))

# South America
South_America <- ggplot(pop_coombined_new, aes(x=Years, y=South_America, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= South_America), vjust=0, size=3) +
  labs(x= 'Years', y = 'South America', title = 'Total Population Trend for South America')+
  theme(axis.text.x = element_text(angle = 45))

#tried to stack the charts with these codes, they did not look good.
#grid.arrange(Asia, Europe, ncol=1)
#plot_grid(Asia, Europe, labels = "AUTO")

Africa
Asia
Europe
North_America
Oceania
South_America

# top 10 countries with largest area in square km
large_area <- world_pop %>% 
  group_by(Country, square_area) %>% 
  arrange(desc(square_area)) %>% 
  select("Country","square_area") %>% 
  head(10)
large_area

#Visualizingthe countries on a map
large_area$Country[large_area$Country == "United States"] <- "USA"
colnames(large_area) <- c('region', 'square_area')

#loading the map dataset from ggplot and joining my table with the map data
mapdata <- map_data("world")
#View(mapdata)
mapdata <- left_join(mapdata, large_area, by="region")
View(mapdata)

#filtering rows with NA in the squae area column out
mapdata1 <- mapdata %>% 
  filter(!is.na(mapdata$square_area))
View(mapdata1)

dff <- mapdata1 %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat),
            group = group)
map2 <- ggplot(mapdata1, aes(long, lat, group = group))+
  geom_polygon(aes(fill = square_area ), color = "white") +
  scale_fill_viridis_c(name='Square Area', option = "D") +
  coord_equal(ratio = 2) +
  theme_bw() +
  geom_text(data = dff, 
            aes(long, lat, label = region, vjust =0.5, hjust=1,group = group), 
            size = 3, color = "red")
#ggplotly(map2) this slows the notebook 
map2

#`summarise()` has grouped output by 'region'. You can override using the
#'.groups` argument.

largest_countries <- ggplot(large_area, aes(x=region, y=square_area, fill = region)) + geom_col(position = "dodge") +
  geom_text(aes(label= square_area), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Square Area km^2', title = 'Countries with Highest Square Area')+
  theme(axis.text.x = element_text(angle = 45))
largest_countries

# top countries with the least area in square km
small_area <-world_pop %>% 
  group_by(Country, square_area) %>% 
  arrange(square_area) %>% 
  select("Country","square_area") %>% 
  head(10)

small_countries <- ggplot(small_area, aes(x=Country, y=square_area, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= square_area), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Square Area km^2', title = 'Countries with Lowest Square Area')+
  theme(axis.text.x = element_text(angle = 45))
small_countries

# total square area by continent
tot_area <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_area = sum(square_area)) %>% 
  arrange(desc(total_area))

fig1 <- plot_ly(type='pie', labels=tot_area$Continent, values=tot_area$total_area, 
                textinfo='label+percent',
                insidetextorientation='radial')
fig1 <- fig1 %>% layout(title = 'Total Area by Continent')
fig1

#Countries Ranking by Population Density by Year
#Performing Data Manipulation
#From the table, it is seen that the population density is for the year 2022.
#We would need to compare the densities for other years.
#Population density can be computed by dividing the population by the square area.
world_pop <- mutate(world_pop, pop_den_2020 = population_2020/square_area, pop_den_2015 = population_2015/square_area,
                    pop_den_2010 = population_2010/square_area, pop_den_2000 = population_2000/square_area,
                    pop_den_1990 = population_1990/square_area, pop_den_1980 = population_1980/square_area,
                    pop_den_1970 = population_1970/square_area)

# 2022
#top 10 countries with the highest population densities
top_pop_density_2022 <- world_pop %>% 
  group_by(Country, pop_density) %>% 
  arrange(desc(pop_density)) %>% 
  select("Country","pop_density") %>% 
  head(10)

# visualizing the countrie with the largest population density on a map
top_pop_density_2022_1 <- top_pop_density_2022
colnames(top_pop_density_2022_1) <- c('region', 'pop_density')
mapdata3 <- map_data("world")
mapdata3 <- left_join(mapdata3, top_pop_density_2022_1, by="region")
# View(mapdata3)

mapdata5 <- mapdata3 %>% 
  filter(!is.na(mapdata3$pop_density))
View(mapdata5)

dff2 <- mapdata5 %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat),
            group = group)
map4 <- ggplot(mapdata5, aes(long, lat, group = group))+
  geom_polygon(aes(fill = pop_density ), color = "white") +
  scale_fill_viridis_c(name='Population Density', option = "D") +
  coord_equal(ratio = 1) +
  theme_bw() +
  geom_text(data = dff2, 
            aes(long, lat, label = region, vjust =0.5, hjust=1,group = group), 
            size = 3, color = "red")
ggplotly(map4) #this plot is too small as the countries are small on the map.

#The map does not contain data for Hong Kong, Macau and Gibraltar

#visualizing the population density as a column plot
top_den_2022 <- ggplot(top_pop_density_2022, aes(x=Country, y=pop_density, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_density), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 2022')+
  theme(axis.text.x = element_text(angle = 45))
top_den_2022

#top 10 countries with the lowest population densities
tail_pop_density_2022 <- world_pop %>% 
  group_by(Country, pop_density) %>% 
  arrange(desc(pop_density)) %>% 
  select("Country","pop_density") %>% 
  tail(10)

bottom_den_2022 <- ggplot(tail_pop_density_2022, aes(x=Country, y=pop_density, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_density), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 2022')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_2022

#Greenland, Falkland Islands, and Mongolia are the countries with the smallest population densities.
#These countries have a trend of having the square area to be very large with population size close to the square area in magnitude.
#These population are usually from 0 to 4 times more than the square area.

# 2020
#top 10 countries with the highest population densities 2020
top_pop_density_2020 <- world_pop %>% 
  group_by(Country, pop_den_2020) %>% 
  arrange(desc(pop_den_2020)) %>% 
  select("Country","pop_den_2020") %>% 
  head(10)

top_den_2020 <- ggplot(top_pop_density_2020, aes(x=Country, y=pop_den_2020, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2020), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 2020')+
  theme(axis.text.x = element_text(angle = 45))
top_den_2020

# 2015
#top 10 countries with the highest population densities 2015
top_pop_density_2015 <- world_pop %>% 
  group_by(Country, pop_den_2015) %>% 
  arrange(desc(pop_den_2015)) %>% 
  select("Country","pop_den_2015") %>% 
  head(10)

top_den_2015 <- ggplot(top_pop_density_2015, aes(x=Country, y=pop_den_2015, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2015), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 2015')+
  theme(axis.text.x = element_text(angle = 45))
top_den_2015

#top 10 countries with the lowest population densities 2015
tail_pop_density_2015 <- world_pop %>% 
  group_by(Country, pop_den_2015) %>% 
  arrange(desc(pop_den_2015)) %>% 
  select("Country","pop_den_2015") %>% 
  tail(10)

bottom_den_2015 <- ggplot(tail_pop_density_2015, aes(x=Country, y=pop_den_2015, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2015), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 2015')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_2015

# 2010
#top 10 countries with the highest population densities 2010
top_pop_density_2010 <- world_pop %>% 
  group_by(Country, pop_den_2010) %>% 
  arrange(desc(pop_den_2010)) %>% 
  select("Country","pop_den_2010") %>% 
  head(10)

top_den_2010 <- ggplot(top_pop_density_2010, aes(x=Country, y=pop_den_2010, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2010), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 2010')+
  theme(axis.text.x = element_text(angle = 45))
top_den_2010

#There was an upward trend in Macau from 1970 which saw it become the country with the highest population density.
#This could be due to the population growth rate in Macau.

#top 10 countries with the lowest population densities 2010
tail_pop_density_2010 <- world_pop %>% 
  group_by(Country, pop_den_2010) %>% 
  arrange(desc(pop_den_2010)) %>% 
  select("Country","pop_den_2010") %>% 
  tail(10)

bottom_den_2010 <- ggplot(tail_pop_density_2010, aes(x=Country, y=pop_den_2010, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2010), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 2010')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_2010

# 2000
#top 10 countries with the highest population densities 2000
top_pop_density_2000 <- world_pop %>% 
  group_by(Country, pop_den_2000) %>% 
  arrange(desc(pop_den_2000)) %>% 
  select("Country","pop_den_2000") %>% 
  head(10)

top_den_2000 <- ggplot(top_pop_density_2000, aes(x=Country, y=pop_den_2000, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2000), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 2000')+
  theme(axis.text.x = element_text(angle = 45))
top_den_2000

#Monaco and Macau swapped positions, this is due to the population change. Monaco had a higher population growth rate compared to Macau.

#top 10 countries with the lowest population densities 2000
tail_pop_density_2000 <- world_pop %>% 
  group_by(Country, pop_den_2000) %>% 
  arrange(desc(pop_den_2000)) %>% 
  select("Country","pop_den_2000") %>% 
  tail(10)

bottom_den_2000 <- ggplot(tail_pop_density_2000, aes(x=Country, y=pop_den_2000, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_2000), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 2000')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_2000

# 1990
#top 10 countries with the highest population densities 1990
top_pop_density_1990 <- world_pop %>% 
  group_by(Country, pop_den_1990) %>% 
  arrange(desc(pop_den_1990)) %>% 
  select("Country","pop_den_1990") %>% 
  head(10)

top_den_1990 <- ggplot(top_pop_density_1990, aes(x=Country, y=pop_den_1990, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_1990), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 1990')+
  theme(axis.text.x = element_text(angle = 45))
top_den_1990

#top 10 countries with the lowest population densities 1990
tail_pop_density_1990 <- world_pop %>% 
  group_by(Country, pop_den_1990) %>% 
  arrange(desc(pop_den_1990)) %>% 
  select("Country","pop_den_1990") %>% 
  tail(10)

bottom_den_1990 <- ggplot(tail_pop_density_1990, aes(x=Country, y=pop_den_1990, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_1990), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 1990')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_1990

# 1980
# top 10 countries with the highest population densities 1980
top_pop_density_1980 <- world_pop %>% 
  group_by(Country, pop_den_1980) %>% 
  arrange(desc(pop_den_1980)) %>% 
  select("Country","pop_den_1980") %>% 
  head(10)

top_den_1980 <- ggplot(top_pop_density_1980, aes(x=Country, y=pop_den_1980, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_1980), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 1980')+
  theme(axis.text.x = element_text(angle = 45))
top_den_1980

#top 10 countries with the lowest population densities 1980
tail_pop_density_1980 <- world_pop %>% 
  group_by(Country, pop_den_1980) %>% 
  arrange(desc(pop_den_1980)) %>% 
  select("Country","pop_den_1980") %>% 
  tail(10)

bottom_den_1980 <- ggplot(tail_pop_density_1980, aes(x=Country, y=pop_den_1980, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_1980), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 1980')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_1980

# 1970
# top 10 countries with the highest population densities 1970
top_pop_density_1970 <- world_pop %>% 
  group_by(Country, pop_den_1970) %>% 
  arrange(desc(pop_den_1970)) %>% 
  select("Country","pop_den_1970") %>% 
  head(10)

top_den_1970 <- ggplot(top_pop_density_1970, aes(x=Country, y=pop_den_1970, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_1970), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Highest Population Density 1970')+
  theme(axis.text.x = element_text(angle = 45))
top_den_1970

#top 10 countries with the lowest population densities 1970
tail_pop_density_1970 <- world_pop %>% 
  group_by(Country, pop_den_1970) %>% 
  arrange(desc(pop_den_1970)) %>% 
  select("Country","pop_den_1970") %>% 
  tail(10)

bottom_den_1970 <- ggplot(tail_pop_density_1970, aes(x=Country, y=pop_den_1970, fill = Country)) + geom_col(position = "dodge") +
  geom_text(aes(label= pop_den_1970), vjust=0, size=3) +
  labs(x= 'Countries', y = 'Poultion Density', title = 'Countries with Lowest Population Density 1970')+
  theme(axis.text.x = element_text(angle = 45))
bottom_den_1970

#Total Population Density by Continent
#Preforming Data Manipulation
# reshaping the dataset to place continents on the columns and total population density as rows
pop_den_combined <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_den_2022 = sum(pop_density), total_den_2020 = sum(pop_den_2020),
            total_den_2015 = sum(pop_den_2015), total_den_2010 = sum(pop_den_2010),
            total_den_2000 = sum(pop_den_2000), total_den_1990 = sum(pop_den_1990),
            total_den_1980 = sum(pop_den_1970), total_den_1970 = sum(pop_den_1970)) 

pop_den_combined <- t(select(pop_den_combined, "total_den_2022", "total_den_2020", "total_den_2015", "total_den_2010",
                             "total_den_2000", "total_den_1990", "total_den_1980", "total_den_1970"))

colnames(pop_den_combined) <- c("Africa", "Asia", "Europe", "North_America", "Oceania", "South_America")

new_pop <- cbind(Years = rownames(pop_den_combined), pop_den_combined)
rownames(new_pop)<- 1:nrow(new_pop)
colnames(new_pop) <- c("Years","Africa", "Asia", "Europe", "North_America", "Oceania", "South_America")
new_pop <- as.data.frame(new_pop)

#Total Population Density Trends by Continents
# Africa
Africa1 <- ggplot(new_pop, aes(x=Years, y=Africa, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Africa), vjust=0, size=3) +
  labs(x= 'Years', y = 'Africa', title = 'Total Population Density Trend for Africa')+
  theme(axis.text.x = element_text(angle = 45))

# Asia
Asia1 <-ggplot(new_pop, aes(x=Years, y=Asia, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Asia), vjust=0, size=3) +
  labs(x= 'Years', y = 'Asia', title = 'Total Population Density Trend for Asia')+
  theme(axis.text.x = element_text(angle = 45))

Europe1 <-ggplot(new_pop, aes(x=Years, y=Europe, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Europe), vjust=0, size=3) +
  labs(x= 'Years', y = 'Europe', title = 'Total Population Density Trend for Europe')+
  theme(axis.text.x = element_text(angle = 45))

# North America
North_America1 <-ggplot(new_pop, aes(x=Years, y=North_America, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= North_America), vjust=0, size=3) +
  labs(x= 'Years', y = 'North America', title = 'Total Population Density Trend for North America')+
  theme(axis.text.x = element_text(angle = 45))

Oceania1 <- ggplot(new_pop, aes(x=Years, y=Oceania, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Oceania), vjust=0, size=3) +
  labs(x= 'Years', y = 'Oceania', title = 'Total Population Density Trend for Oceania')+
  theme(axis.text.x = element_text(angle = 45))

# South America
South_America1 <- ggplot(new_pop, aes(x=Years, y=South_America, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= South_America), vjust=0, size=3) +
  labs(x= 'Years', y = 'South America', title = 'Total Population Trend for South America')+
  theme(axis.text.x = element_text(angle = 45))

Africa1
Asia1
Europe1
North_America1
Oceania1
South_America1

# total population growth per continent.
tot_pop_growth <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop_growth = sum(growth_rate)) %>% 
  arrange(desc(total_pop_growth))

fig2 <- plot_ly(type='pie', labels=tot_pop_growth$Continent, values=tot_pop_growth$total_pop_growth, 
                textinfo='label+percent',
                insidetextorientation='radial')
fig2 <- fig2 %>% layout(title = 'Total Population Growth by Continent')
fig2

#The data set only had population growth rate for one year.
#the data does not contain years in sequence, so computing the growth rate for the previous year was not possible.
#according to the pie chart, Africa had the largest growth rate compared to the other continents.

#Performing Data Manipulation
#From the table, it is seen that the world population percentage is for the year 2022
#We would need to compare the percentages for other years
world_pop <- mutate(world_pop, pop_den_2020 = (world_pop$population_2022/sum(world_pop$population_2022))*100)
world_pop <- world_pop %>% 
  mutate(pop_percent_2020 = (population_2020/sum(population_2020))*100,
         pop_percent_2015 = (population_2015/sum(population_2015))*100,
         pop_percent_2010 = (population_2010/sum(population_2010))*100,
         pop_percent_2000 = (population_2000/sum(population_2000))*100,
         pop_percent_1990 = (population_1990/sum(population_1990))*100,
         pop_percent_1980 = (population_1980/sum(population_1980))*100,
         pop_percent_1970 = (population_1970/sum(population_1970))*100)

#Countries Ranking by World Population Percentage by Year
#Year 2022
#Top Countries with Highest Population Percentage

#2022
# top 10 countries with the highest population percent in the world 2022
top_pop_percent_2022 <- world_pop %>% 
  group_by(Country, world_pop_percent) %>% 
  arrange(desc(world_pop_percent)) %>% 
  select("Country","world_pop_percent") %>% 
  head(10)

top_pop_percent_2022_1 <- top_pop_percent_2022
colnames(top_pop_percent_2022_1) <- c('region', 'world_pop_percent')
mapdata3 <- left_join(mapdata3, top_pop_percent_2022_1, by="region")
# View(mapdata3)

mapdata6 <- mapdata3 %>% 
  filter(!is.na(mapdata3$world_pop_percent))
#View(mapdata6)

dff3 <- mapdata6 %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat),
            group = group)
map5 <- ggplot(mapdata6, aes(long, lat, group = group))+
  geom_polygon(aes(fill = world_pop_percent ), color = "white") +
  scale_fill_viridis_c(name='World Population Percentage', option = "D") +
  coord_equal(ratio = 1) +
  theme_bw() +
  geom_text(data = dff3, 
            aes(long, lat, label = region, vjust =0.5, hjust=1,group = group), 
            size = 3, color = "red")
map5

#China contributed the most to the world's population interms of percentage. Followed by India and the United States.
#Bangledesh, Russia and Mexico make up the top 10 countries for the year 2022.

#Countries with Lowest Population Percentage
# top 10 countries with the lowest population percent in the world 2022
tail_pop_percent_2022 <- world_pop %>% 
  group_by(Country, world_pop_percent) %>% 
  arrange(desc(world_pop_percent)) %>% 
  select("Country","world_pop_percent") %>% 
  tail(10)
tail_pop_percent_2022

tail_pop_percent_2022_1 <- tail_pop_percent_2022
colnames(tail_pop_percent_2022_1) <- c('region', 'world_pop_percent')
mapdata7 <- left_join(mapdata, tail_pop_percent_2022_1, by="region")
# View(mapdata3)

mapdata8 <- mapdata7 %>% 
  filter(!is.na(mapdata7$world_pop_percent))
#View(mapdata8)

dff4 <- mapdata8 %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat),
            group = group)
map6 <- ggplot(mapdata8, aes(long, lat, group = group))+
  geom_polygon(aes(fill = world_pop_percent ), color = "white") +
  scale_fill_viridis_c(name='World Population Percentage', option = "D") +
  coord_equal(ratio = 1) +
  theme_bw() +
  geom_text(data = dff4, 
            aes(long, lat, label = region, vjust =0.5, hjust=1,group = group), 
            size = 3, color = "red")
ggplotly(map6)

#The bottom 10 countries contributed approximately 0 percent to the world's population.
#Tables will be displayed for the bottom 10 countries.

tail_pop_percent_2022

#Year 2020
#Top Countries with Highest Population Percentage
# 2020
# top 10 countries with the highest population percent in the world 2020
top_pop_percent_2020 <- world_pop %>% 
  group_by(Country, pop_percent_2020) %>% 
  arrange(desc(pop_percent_2020)) %>% 
  select("Country","pop_percent_2020") %>% 
  head(10)

top_percent_2020 <- ggplot(top_pop_percent_2020, aes(x=Country, y=pop_percent_2020, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label= pop_percent_2020), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Percentage 2020')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_2020

#The same observations for the year 2022 applies to 2020.
#China, India and the United States make up the top 3 countries with the highest world percentage.

#Countries with Low Population Percentage
# top 10 countries with the lowest population percent in the world 2020
tail_pop_percent_2020 <- world_pop %>% 
  group_by(Country, pop_percent_2020) %>% 
  arrange(desc(pop_percent_2020)) %>% 
  select("Country","pop_percent_2020") %>% 
  tail(10)
tail_pop_percent_2020

#The bottom 10 countries contributed a little over 0 percent to the world's population in terms of percentage in the year 2020.

#Year 2015
#Top Countries with Highest Population Percentage
# 2015
# top 10 countries with the highest population percent in the world 2015
top_pop_percent_2015 <- world_pop %>% 
  group_by(Country, pop_percent_2015) %>% 
  arrange(desc(pop_percent_2015)) %>% 
  select("Country","pop_percent_2015") %>% 
  head(10)

top_percent_2015 <- ggplot(top_pop_percent_2015, aes(x=Country, y=pop_percent_2015, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label= pop_percent_2015), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Growth 2015')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_2015

#Countries with Low Population Percentage
# top 10 countries with the lowest population percent in the world 2015
tail_pop_percent_2015 <- world_pop %>% 
  group_by(Country, pop_percent_2015) %>% 
  arrange(desc(pop_percent_2015)) %>% 
  select("Country","pop_percent_2015") %>% 
  tail(10)
tail_pop_percent_2015

#Year 2010
#Top Countries with Highest Population Percentage
# 2010
# top 10 countries with the highest population percent in the world 2010
top_pop_percent_2010 <- world_pop %>% 
  group_by(Country, pop_percent_2010) %>% 
  arrange(desc(pop_percent_2010)) %>% 
  select("Country","pop_percent_2010") %>% 
  head(10)

top_percent_2010 <- ggplot(top_pop_percent_2010, aes(x=Country, y=pop_percent_2010, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label= pop_percent_2010), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Growth 2010')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_2010


#Countries with Low Population Percentage
# top 10 countries with the lowest population percent in the world 2010
tail_pop_percent_2010 <- world_pop %>% 
  group_by(Country, pop_percent_2010) %>% 
  arrange(desc(pop_percent_2010)) %>% 
  select("Country","pop_percent_2010") %>% 
  tail(10)
tail_pop_percent_2010

#Year 2000
#Top Countries with Highest Population Percentage
# 2000
# top 10 countries with the highest population percent in the world 2000
top_pop_percent_2000 <- world_pop %>% 
  group_by(Country, pop_percent_2000) %>% 
  arrange(desc(pop_percent_2000)) %>% 
  select("Country","pop_percent_2000") %>% 
  head(10)

top_percent_2000 <- ggplot(top_pop_percent_2000, aes(x=Country, y=pop_percent_2000, fill = Country)) +
  geom_col(position = "dodge") + geom_text(aes(label= pop_percent_2000), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Growth 2000')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_2000

#Countries with Low Population Percentage
# top 10 countries with the lowest population percent in the world 2000
tail_pop_percent_2000 <- world_pop %>% 
  group_by(Country, pop_percent_2000) %>% 
  arrange(desc(pop_percent_2000)) %>% 
  select("Country","pop_percent_2000") %>% 
  tail(10)
tail_pop_percent_2000

#Year 1990
#Top Countries with Highest Population Percentage
# 1990
# top 10 countries with the highest population percent in the world 1990
top_pop_percent_1990 <- world_pop %>% 
  group_by(Country, pop_percent_1990) %>% 
  arrange(desc(pop_percent_1990)) %>% 
  select("Country","pop_percent_1990") %>% 
  head(10)

top_percent_1990 <- ggplot(top_pop_percent_1990, aes(x=Country, y=pop_percent_1990, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label= pop_percent_1990), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Growth 1990')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_1990

#Countries with Low Population Percentage¶
# top 10 countries with the lowest population percent in the world 1990
tail_pop_percent_1990 <- world_pop %>% 
  group_by(Country, pop_percent_1990) %>% 
  arrange(desc(pop_percent_1990)) %>% 
  select("Country","pop_percent_1990") %>% 
  tail(10)
tail_pop_percent_1990

#Year 1980
#Top Countries with Highest Population Percentage
# 1980
# top 10 countries with the highest population percent in the world 1980
top_pop_percent_1980 <- world_pop %>% 
  group_by(Country, pop_percent_1980) %>% 
  arrange(desc(pop_percent_1980)) %>% 
  select("Country","pop_percent_1980") %>% 
  head(10)

top_percent_1980 <- ggplot(top_pop_percent_1980, aes(x=Country, y=pop_percent_1980, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label= pop_percent_1980), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Growth 1980')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_1980

#Countries with Low Population Percentage
# top 10 countries with the lowest population percent in the world 1980
tail_pop_percent_1980 <- world_pop %>% 
  group_by(Country, pop_percent_1980) %>% 
  arrange(desc(pop_percent_1980)) %>% 
  select("Country","pop_percent_1980") %>% 
  tail(10)
tail_pop_percent_1980

#Year 1970
#Top Countries with Highest Population Percentage
# 1970
# top 10 countries with the highest population percent in the world 1970
top_pop_percent_1970 <- world_pop %>% 
  group_by(Country, pop_percent_1970) %>% 
  arrange(desc(pop_percent_1970)) %>% 
  select("Country","pop_percent_1970") %>% 
  head(10)

top_percent_1970 <- ggplot(top_pop_percent_1970, aes(x=Country, y=pop_percent_1970, fill = Country)) +
  geom_col(position = "dodge") +
  geom_text(aes(label= pop_percent_1970), vjust=0, size=3) +
  labs(x= 'Countries', y = 'World Population Percent', title = 'Countries with High World Population Growth 1970')+
  theme(axis.text.x = element_text(angle = 45))
top_percent_1970

#Countries with Low Population Percentage
# top 10 countries with the lowest population percent in the world 1970
tail_pop_percent_1970 <- world_pop %>% 
  group_by(Country, pop_percent_1970) %>% 
  arrange(desc(pop_percent_1970)) %>% 
  select("Country","pop_percent_1970") %>% 
  tail(10)
tail_pop_percent_1970

# Total world population percentage trend by continent
# reshaping my data to place continents on the columns and total population growth as rows
pop_percent_combined <- world_pop %>% 
  group_by(Continent) %>% 
  summarise(total_pop_2022 = sum(world_pop_percent), total_pop_2020 = sum(pop_percent_2020),
            total_pop_2015 = sum(pop_percent_2015), total_pop_2010 = sum(pop_percent_2010),
            total_pop_2000 = sum(pop_percent_2000), total_pop_1990 = sum(pop_percent_1990),
            total_pop_1980 = sum(pop_percent_1980), total_pop_1970 = sum(pop_percent_1970)) 

pop_percent_combined <- t(select(pop_percent_combined, "total_pop_2022", "total_pop_2020", "total_pop_2015",
                                 "total_pop_2010", "total_pop_2000", "total_pop_1990", "total_pop_1980", "total_pop_1970"))

colnames(pop_percent_combined) <- c("Africa", "Asia", "Europe", "North_America", "Oceania", "South_America")

new_pop1 <- cbind(Years = rownames(pop_percent_combined), pop_percent_combined)
rownames(new_pop1)<- 1:nrow(new_pop1)
colnames(new_pop1) <- c("Years","Africa", "Asia", "Europe", "North_America", "Oceania", "South_America")
new_pop1 <- as.data.frame(new_pop1)

# Total world population percent trend by continent
# Africa
Africa2 <- ggplot(new_pop1, aes(x=Years, y=Africa, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Africa), vjust=0, size=3) +
  labs(x= 'Years', y = 'Africa', title = 'Total World Population Percent Trend for Africa')+
  theme(axis.text.x = element_text(angle = 45))

# Asia
Asia2 <-ggplot(new_pop1, aes(x=Years, y=Asia, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Asia), vjust=0, size=3) +
  labs(x= 'Years', y = 'Sum World Population percent (Asia)', title = 'Total World Population Percent Trend for Asia')+
  theme(axis.text.x = element_text(angle = 45))

# Europe
Europe2 <-ggplot(new_pop1, aes(x=Years, y=Europe, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Europe), vjust=0, size=3) +
  labs(x= 'Years', y = 'Sum World Population percent (Europe)', title = 'Total World Population Percent Trend for Europe')+
  theme(axis.text.x = element_text(angle = 45))

# North America
North_America2 <-ggplot(new_pop1, aes(x=Years, y=North_America, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= North_America), vjust=0, size=3) +
  labs(x= 'Years', y = 'Sum World Population percent (North America)', title = 'Total World Population Percent Trend for North America')+
  theme(axis.text.x = element_text(angle = 45))

# Oceania
Oceania2 <-ggplot(new_pop1, aes(x=Years, y=Oceania, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= Oceania), vjust=0, size=3) +
  labs(x= 'Years', y = 'Sum World Population percent (Oceania)', title = 'Total World Population Percent Trend for Oceania')+
  theme(axis.text.x = element_text(angle = 45))

# South America
South_America2 <-ggplot(new_pop1, aes(x=Years, y=South_America, fill = Years)) + geom_col(position = "dodge") +
  geom_text(aes(label= South_America), vjust=0, size=3) +
  labs(x= 'Years', y = 'Sum World Population percent (South America)', title = 'Total World Population Percent Trend for South America')+
  theme(axis.text.x = element_text(angle = 45))

Africa2
Asia2
Europe2
North_America2
Oceania2
South_America2
