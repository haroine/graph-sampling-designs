library(tidyverse)

# ff_estimators <- readRDS("ff_ambs_100218.rds")
ff_estimators <- readRDS("ff_fwprob_100218_by_001.rds")
estimators_stats <- ff_estimators

plot_deff <- ggplot(data = estimators_stats, aes(x=parameter)) +
  geom_line(aes(y=deff_snowball, colour=stat_name)) +
  geom_hline(yintercept = 1, linetype="longdash") +
  scale_y_log10()

print(plot_deff)

## Size for Bernoulli initial sample (no differentiation between stats)
plot_size <- ggplot(data = estimators_stats, 
                    aes(x=parameter)) +
  geom_line(aes(y=size_snowball, colour=stat_name))

print(plot_size)