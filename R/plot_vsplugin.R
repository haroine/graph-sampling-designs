library(tidyverse)

result_forplugin <- readRDS("data/MSE_plugin_weighted_2.rds")

result_forplugin_numeric <- result_forplugin %>%
  select(-graph_stat) %>%
  mutate_all(as.numeric)

result_forplugin_numeric$graph_stat <- result_forplugin$graph_stat

result_forplugin <- result_forplugin_numeric
rm(result_forplugin_numeric)

result_forplugin$prop_sample_size <- 100*result_forplugin$snowball_sample_size / 1e5

result_forplugin$rel_MSE_plugin <- result_forplugin$MSE_plugin/result_forplugin$stat_value
result_forplugin$rel_MSE_weighted <- result_forplugin$MSE_weighted/result_forplugin$stat_value

y_min <- 1e-5
y_max <- max(result_forplugin$rel_MSE_plugin) + 100

result_forplugin[result_forplugin$rel_MSE_plugin <= y_min,]$rel_MSE_plugin <- y_min
result_forplugin[result_forplugin$rel_MSE_weighted <= y_min,]$rel_MSE_weighted <- y_min

x_min <- 0.04
x_max <- 20

result_forplugin[result_forplugin$prop_sample_size <= x_min,]$prop_sample_size <- x_min
result_forplugin[result_forplugin$prop_sample_size >= x_max,]$prop_sample_size <- x_max


weight_vsplugin <- ggplot(data=result_forplugin, aes(x=prop_sample_size)) +
  geom_line(aes(y=rel_MSE_plugin, colour=graph_stat)) +
  scale_y_log10(breaks=c(0.001,0.01,0.1,1,10,100,1000), limits=c(y_min,y_max)) +
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10), limits=c(x_min,x_max)) +
  geom_line(aes(y=rel_MSE_weighted, colour=graph_stat), linetype="longdash") +
  labs(title = "Mean Squared errors for snowball sampling",
       subtitle = "Dashed: weighted estimates - Plain: plugin estimates",
       # caption = "Graph datasets from R package igraphdata",
       y = "Relative MSE", x = "Sampling fraction (in percentage)") 
  
print(weight_vsplugin)

### Biases

result_forplugin$rel_bias_plugin <- result_forplugin$bias_plugin/result_forplugin$stat_value
result_forplugin$rel_bias_weighted <- result_forplugin$bias_weighted/result_forplugin$stat_value

y_min <- 1e-2
y_max <- max(result_forplugin$rel_bias_plugin) + 10

result_forplugin[result_forplugin$rel_bias_plugin <= y_min,]$rel_bias_plugin <- y_min
result_forplugin[result_forplugin$rel_bias_weighted <= y_min,]$rel_bias_weighted <- y_min

x_min <- 0.04
x_max <- 20

result_forplugin[result_forplugin$prop_sample_size <= x_min,]$prop_sample_size <- x_min
result_forplugin[result_forplugin$prop_sample_size >= x_max,]$prop_sample_size <- x_max


weight_vsplugin2 <- ggplot(data=result_forplugin, aes(x=prop_sample_size)) +
  geom_line(aes(y=rel_bias_plugin, colour=graph_stat)) +
  scale_y_log10(breaks=c(0.001,0.01,0.1,1,10,100,1000), limits=c(y_min,y_max)) +
  scale_x_log10(breaks=c(0.05,0.1,0.5,1,5,10), limits=c(x_min,x_max)) +
  geom_line(aes(y=rel_bias_weighted, colour=graph_stat), linetype="longdash") +
  labs(title = "Biases for snowball sampling",
       subtitle = "Dashed: weighted estimates - Plain: plugin estimates",
       # caption = "Graph datasets from R package igraphdata",
       y = "Relative bias", x = "Sampling fraction (in percentage)") 
print(weight_vsplugin2)
