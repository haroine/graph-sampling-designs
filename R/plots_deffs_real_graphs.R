
#### Bernoulli
deff_yeast <- readRDS("data/yeast_snowball.rds")
deff_yeast <- deff_yeast %>% filter(name == "bernoulli")
deff_yeast$name <- "yeast"

deff_us <- readRDS("data/USairports_estimators_bernoulli.rds")
deff_us$name <- "USairports"

deff_immuno <- readRDS("data/immuno_estimators_bernoulli.rds")
deff_immuno$name <- "immuno"

deff_df <- rbind()