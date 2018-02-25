library(tidyverse)

ff_estimators <- readRDS("data/forestfire_simple.rds")
ff_estimators <- readRDS("data/forestfire_pps_centralities.rds")
estimators_stats <- ff_estimators

df_toplot <- estimators_stats %>% filter(name=="degree")
# df_toplot <- estimators_stats

plot_deff <- ggplot(data = df_toplot
                    , aes(x=parameter)) +
  geom_line(aes(y=deff_snowball, colour=stat_name)) +
  # geom_line(aes(y=deff_simple, colour=stat_name)) +
  geom_hline(yintercept = 1, linetype="longdash") +
  scale_y_log10()

print(plot_deff)

## TODO does pps sampling improve things for initial sample size?

## Size for Bernoulli initial sample (no differentiation between stats)
plot_size <- ggplot(data = estimators_stats, 
                    aes(x=parameter)) +
  geom_line(aes(y=size_snowball, colour=stat_name))

print(plot_size)