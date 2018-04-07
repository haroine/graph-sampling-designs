library(tidyverse)

ff_estimators <- readRDS("data/forestfire_snowball.rds")
estimators_stats <- ff_estimators

df_toplot <- estimators_stats %>% filter(name=="bernoulli")
# df_toplot$testvar <- df_toplot$deff_snowball / df_toplot$deff_simple
# df_toplot <- estimators_stats

df_toplot <- df_toplot  %>%
  arrange(deff_snowball)

## Order by stat value in the plot
levels_stat_name <- rev(unique(df_toplot$stat_name))
df_toplot$stat_name <- factor(df_toplot$stat_name, levels=levels_stat_name)

plot_deff <- ggplot(data = df_toplot
                    , aes(x=parameter)) +
  # geom_line(aes(y=deff_simple, colour=stat_name)) +
  geom_line(aes(y=deff_snowball, colour=stat_name)) +
  geom_hline(yintercept = 1, linetype="longdash") +
  geom_hline(yintercept = 2, linetype="longdash") +
  scale_y_log10(breaks=c(1,2,10,100)) +
  labs(title = "Design effect for snowball sampling",
       subtitle = "For a forest-fire networkof order 1000, initial sample drawn by Bernoulli of expected size 50",
       caption = "Ambassadors parameter for forest-fire = 1", 
       x = "fwprobs", y = "Deff") 

print(plot_deff)

## TODO does pps sampling improve things for initial sample size?
# 
# ## Size for Bernoulli initial sample (no differentiation between stats)
# plot_size <- ggplot(data = estimators_stats, 
#                     aes(x=parameter)) +
#   geom_line(aes(y=size_snowball, colour=stat_name))
# 
# print(plot_size)