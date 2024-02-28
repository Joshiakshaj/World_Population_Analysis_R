# World Population Visual Analysis using R

An Independent project to analyse World Population data from 1970 to 2022 and draw visual insights decadewise, continent-wise, etc.

Dataset Source:https://www.kaggle.com/datasets/iamsouravbanerjee/world-population-dataset

Requirements: MS Excel, RStudio (R programming  environement with R installed),
Packages and libraries: 
tidyverse, skimr, janitor, lubridate, hms, ggplot2, reshape2, ggpie, plotly, plotrix, gridExtra

Use MS Excel to clean data (change column names as will be required/ can be done with R but using Excel will allow for attention to detail and examining dataset)

RStudio to create code and Visualizations, storing them in image form through in-built option (Export).

Key Findings:
1. Africa has the most number of countries and South America has the least amount of countries (Countries Per Continent Viz)
   ![Countries_Per_Continent_Pie_Chart](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/296d2aec-cd5f-4695-9f80-1288ec64f95f)

3. In the year 2022, Asia has the highest total population of all the continents followed by Africa. (Total Population by Year by Continent)
   ![Total_Population_by_Continent _by_Year_2022](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/21dfff91-9529-46f2-9363-184a6ef85c66)

4. In 2020, 2015, 2010 the same trend was present as 2022 (Visualizations available in Visualizations folder in Repo)
5. Notably in the year 2000, the population for Africa exceeded that of Europe as it continues in its growth rate keeps rising compared to that of Europe, Asia still has the highest population.
   ![Total_Population_by_Continent _by_Year_2000](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/f55178db-1db2-4a6f-af74-0b4ea23b3cf0)

6. For the years 1990, 1980 and 1970, the growth in population of Africa has been increasing faster than than that of Europe. (Visualizations available in Visualizations folder in Repo)
7. All the continents over the years from 1970 through 2022 saw different levels of increases in their total population except Europe. (Total Population Trend Charts for each Continent/available in Visualizations folder in Repo))
   
8. Europe's population for the year 2022 saw a drop from the year 2020. (Specualted causes being COVID-19 Pandemic, lower birth rate than death rate)
   ![Total_Population_by_Continent _by_Year_2022](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/480e4225-eef1-4af9-af19-b6ba36572b7b) ![Total_Population_by_Continent_by_Year_2020](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/4da97444-96dd-4e5a-bb2d-be1faefbc4c8)


    To create maps, ggplot was used since plotly proved to slow down the execution
9. Russia, Canada and China are the largest countries by square area. 10th position is held by Algeria. (Top 10 Countries by Highest Square Area Charts)
    ![Countries_and_Square_Area](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/8a8ca701-117b-482c-afc8-551459230cc4)

10. Vatican City, Monaco and Gibraltar are the smallest countries by square area. (Top 10 Countries by Lowest Square Area Charts)
    ![Countries_With_Lowest_Sqaure_Area](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/ba395a35-8437-4a0f-b408-6c4c8701b109)

11. Asia has the largest sqaure area, followed by Africa. Oceania has the smallest square area. (Total Sqaure Area by Continent)
    ![Total_Square_Area_By_Continent](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/b4468fb6-8678-4faf-ba0c-0314a402a5d9)

    
    To rank countries by Population Density by Year, data manipulation had to perform in order to obtain more metrics
    Population Density = Sqaure Are/Population
12. Macau has the highest population density and this is because as at 2022, they have a population of 695168 and a square km area of 30. (Countries with Highest Population Density 2022)
13. Monaco has the second highest with a population of 36496 as at 2022 and a square area of 2.
    It is observed that countries with the highest population densities have low square areas.
    ![Countries_With_Highest_Population_Density_2022](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/9fce75ef-acd5-4d35-ae1c-45f90050a1bc)

14. Greenland, Falkland Islands, and Mongolia are the countries with the smallest population densities. (Countries with Lowest Population Density 2022)
    These countries have a trend of having the square area to be very large with population size close to the square area in magnitude (from 0 to 4 times more than the square area)
    ![Countries_With_lowest_Population_Density_2022](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/917213a0-3743-4efc-ab90-3613e340d3a7)

15. In 2010 (since 1970s) there was an upward trend in Macau and it became the most densely populated country. (Could be due to high population growth rate)
16. In 2000, Monaco had a higher population growth rate compared to Macau.
    ![Countries_With_Highest_Population_Densities_2000](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/b62ebbe6-5aaa-46c8-8ad8-2f65b0e5ddd2)


    Total Population Growth Per Continent could not be visualized becuase data restrictions. So, Africa had the largest growth rate (According to previously visualized pie chart)

    For World Population Percent by Continent, Data was manipulated as
    pop_percent_'year' = (population_'year'/sum(population_'year))*100 for all years (2020, 2015, 2010, 2000, 1990, 1980, 1970)

17. China contributed the most to the world's population in terms of percentage, followed by India and the US. Bangladesh and Mexico were also present in Top 10 contributers in population percentage for 2022.
    ![Countries_with_Highest_Population_Percentage_2020](https://github.com/Joshiakshaj/World_Population_Analysis_R/assets/129145776/5c371e93-55ec-407d-b682-c5cc7938202b)

18. Bottom 10 countries population density wise contributed to `0% of the world population:

 Groups:   Country, world_pop_percent [10]  ===========(to show bottom 10)
   
   Country                      world_pop_percent
   <chr>                                    <dbl>
   1 Seychelles                                   0
   2 Sint Maarten                                 0
   3 Tokelau                                      0
   4 Tonga                                        0
   5 Turks and Caicos Islands                     0
   6 Tuvalu                                       0
   7 United States Virgin Islands                 0
   8 Vanuatu                                      0
   9 Vatican City                                 0
   10 Wallis and Futuna                            0         

19. The same observations for the year 2022 apply to year 2020 (for both highest and lowest pop. percentage contribution)
20. World Population Percentage Trend by Continent(Visualizations available for each Contoinent for Each year) 
