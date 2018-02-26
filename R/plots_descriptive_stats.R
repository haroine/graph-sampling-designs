library(tidyverse)
library(igraph)
library(igraphdata)

# v <- data(package = "igraphdata") 
# test <- data(list = v$results[,3])

######### Snowball size / fwprobs #########

data_snowball_size <- readRDS("data/forestfire_parameters.rds")

data_snowball_size <- data_snowball_size %>% 
  filter(name_parameter == "fwprobs") %>%
  filter(stat_name == "clustering") %>%
  filter(parameter < 1.0)

plot_size_snowball_fwprobs <- ggplot(data=data_snowball_size, 
                             aes(x=parameter, y=size_snowball)) +
  geom_line() +
  labs(title = "Mean size of snowball sample",
       subtitle = "For a forest-fire networkof order 1000, initial sample drawn by Bernoulli of expected size 50",
       caption = "Ambassadors parameter for forest-fire = 1", 
       x = "fwprobs", y = "Snowball sample size") 

print(plot_size_snowball_fwprobs)

######### Snowball size / ambs #########

data_snowball_size <- readRDS("data/forestfire_parameters.rds")

data_snowball_size <- data_snowball_size %>% 
  filter(name_parameter == "ambs") %>%
  filter(stat_name == "clustering")

plot_size_snowball_ambs <- ggplot(data=data_snowball_size, 
                             aes(x=parameter, y=size_snowball)) +
  geom_line() +
  labs(title = "Mean size of snowball sample",
       subtitle = "For a forest-fire network of order 1000, initial sample drawn by Bernoulli of expected size 50",
       caption = "forward burning probability of 0.35", 
       x = "Ambassadors", y = "Snowball sample size") 

print(plot_size_snowball_ambs)

######### Stats #########

data_stats_ff <- readRDS("data/forestfire_snowball.rds")
data_stats_ff <- data_stats_ff %>%
  filter(name == "bernoulli") %>%
  arrange(stat_value)

## Order by stat value in the plot
levels_stat_name <- rev(unique(data_stats_ff$stat_name))
data_stats_ff$stat_name <- factor(data_stats_ff$stat_name, levels=levels_stat_name)

data_stats_ff_1 <- data_stats_ff %>%
  filter(stat_name != "betweenness")

plot_stats_ff <- ggplot(data=data_stats_ff, 
                        aes(x=parameter)) +
  geom_line(aes(y=stat_value, colour=stat_name)) +
  labs(title = "Value of stats",
       subtitle = "For a forest-fire network, initial sample drawn by Bernoulli of expected size 50",
       caption = "Ambassadors parameter for forest-fire = 1",
       x = "fwprobs", y = "Stat value") +
  scale_y_log10()

print(plot_stats_ff)
