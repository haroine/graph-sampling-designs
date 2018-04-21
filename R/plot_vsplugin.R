library(tidyverse)

result_forplugin <- readRDS("data/MSE_plugin_weighted.rds")

weight_vsplugin <- ggplot(data=result_forplugin, aes(x=snowball_sample_size)) +
  geom_line(aes(y=MSE_plugin, colour=graph_stat)) +
  geom_line(aes(y=MSE_weighted, colour=graph_stat))
  
print(weight_vsplugin)