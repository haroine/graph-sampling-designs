
#### Bernoulli
deff_BA <- readRDS("data/BA_estimators_bernoulli.rds")
deff_BA$parameter <- NULL
deff_BA$name <- "BA"

deff_yeast <- readRDS("data/yeast_snowball.rds")
deff_yeast <- deff_yeast %>% filter(name == "bernoulli")
deff_yeast$name <- NULL
deff_yeast$name <- "yeast"

deff_us <- readRDS("data/USairports_estimators_bernoulli.rds")
deff_us$parameter <- NULL
deff_us$name <- "USairports"

deff_immuno <- readRDS("data/immuno_estimators_bernoulli.rds")
deff_immuno$parameter <- NULL
deff_immuno$name <- "immuno"

deff_df <- rbind(deff_BA, deff_yeast, deff_us, deff_immuno)

## Order by stat value in the plot
levels_stat_name <- c("betweenness", "eigen", "degree", 
                      "clustering", "page_rank", "closeness")
deff_df$stat_name <- factor(deff_df$stat_name, levels=levels_stat_name)


plot_deff_real <- ggplot(data=deff_df, aes(x=name)) +
  geom_point(aes(y=deff_snowball, colour=stat_name)) +
  scale_y_log10() +
  geom_hline(yintercept = 1, linetype="longdash", colour="black") +
  labs(title = "",
       subtitle = "",
       caption = "",
       x = "Graph name", y = "Design effect") 

print(plot_deff_real)
