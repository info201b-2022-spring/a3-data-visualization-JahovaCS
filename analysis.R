library("ggplot2")
library("tidyverse")
library("dplyr")
library("usmap")
library("patchwork")



incarcerations <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# New York Jail Black Average Population Rate in 2018
ny_black_jailRate <- incarcerations %>%
  filter(year == max(year) & state == 'NY') %>%
  summarize(mean = mean(black_jail_pop_rate, na.rm = TRUE))  %>%
  pull(mean)
  
# County in New York with the largest jail rate of Black in 2018 
ny_county_largestRate <- incarcerations %>%
  filter(year == max(year) & state == "NY") %>%
  filter(black_jail_pop_rate == max(black_jail_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)

# New York Black Population Average 
black_jail_population <- incarcerations %>%
  filter(year == max(year) & state == 'NY') %>%
  summarize(mean = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(mean)

# Total population in New York 
total_NY_pop <- incarcerations %>%
  filter(year == max(year) & state == "NY") %>%
  summarize(mean = mean(total_pop, na.rm = TRUE)) %>%
  pull(mean)


# Population of New York Age 15 - 64
ny_black_population15_64 <- incarcerations %>%
  filter(year == max(year) & state == "NY") %>%
  summarize(total = sum(black_pop_15to64)) %>%
  pull(total)
  
# NY Jail Black Population
ny_incarc_black <- incarcerations %>%
  filter(year == max(year) & state == 'NY') %>%
  summarize(mean = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(mean)               


# Dataset for Visual 1
# NY population data 
ny_population <- incarcerations %>%
  filter(state == "NY") %>%
  group_by(year) %>%
  summarise(total_pop = mean(total_pop))

# Plotting population in NY 
NY_population_graph <- ggplot(ny_population, aes(x = year, y = total_pop)) + 
  geom_line(aes(color = "NY Population"), size = 1) +
  labs(title = "New York Population", x = "Year", y = "Population")
  theme_minimal()

NY_population_graph


# dataset 2 
# NY Jail Black Population
ny_incarc_black_graph_df <- incarcerations %>%
  filter(state == "NY") %>%
  group_by(year) %>%
  mutate(black_jail_population = mean(black_jail_pop, na.rm = TRUE)) %>%
  mutate(total_jail_pop_NY = mean(total_jail_pop, na.rm = TRUE)) %>%
  arrange(year) %>%
  select(year,black_jail_population, total_jail_pop_NY)

# Visual for dataset 2
options(sciepen = 10000)
comparison_graph <- ggplot(ny_incarc_black_graph_df, aes(x=year)) + 
  geom_line(aes(y=black_jail_population, col="NY Black Jail Population")) + 
  geom_line(aes(y=total_jail_pop_NY, col="NY Jail Population")) + 
  labs(title="NY Population v. Black Population in Jail ", 
       caption="Population", y="Population") +  # title and caption
  #scale_x_date(labels = waiver(), breaks = waiver()) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("blue", "red")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid
comparison_graph
#map 
# data set 
# New York County 
ny_county_jail <- incarcerations %>%
  filter(year == max(year) & state == "NY")

ny_map_visualization <- plot_usmap(data = ny_county_jail,
                                   value = 'black_jail_pop',
                                   include = 'NY',
                                   color = 'black') + 
  scale_fill_continuous(name = 'Jail Population of Black', low = 'red', high = 'blue') +
  ggtitle("Black Jail Population in 2018")  + theme(legend.position = 'bottom')
ny_map_visualization











