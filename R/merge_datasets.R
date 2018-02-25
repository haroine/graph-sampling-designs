library(tidyverse)

#### Yeast

yeast_bernoulli <- readRDS("data/yeast_estimators_bernoulli.rds")
yeast_other <- readRDS("data/yeast_centralities.rds")

yeast_bernoulli <- yeast_bernoulli %>%
  rename(name = parameter)

yeast_bernoulli$name <- "bernoulli"

yeast_other$parameter <- NULL

yeast_all <- rbind(yeast_bernoulli, yeast_other)

# saveRDS(yeast_all, "data/yeast_snowball.rds")

#### FF

ff_param1 <- readRDS("data/forestfire_ambs.rds")
ff_param1$name <- "ambs"
ff_param1 <- ff_param1 %>%
  rename(name_parameter = name)
ff_param2 <- readRDS("data/forestfire_fwprob_pps_all.rds")
ff_param2$name <- "fwprobs"
ff_param2 <- ff_param2 %>%
  rename(name_parameter = name)

ff_param <- rbind(ff_param1, ff_param2)

# saveRDS(ff_param, "data/forestfire_parameters.rds")

#### FF-pps

ff_pps1 <- readRDS("data/forestfire_pps_all.rds")
ff_pps2 <- readRDS("data/forestfire_pps_centralities.rds")

ff_pps <- rbind(ff_pps1, ff_pps2)

ff_pps <- ff_pps[order(ff_pps$parameter),]

ff_pps$key <- paste(ff_pps$stat_name, ff_pps$name
                    , ff_pps$parameter, sep="_")

ff_pps_cleaned <- ff_pps[!duplicated(ff_pps$key),]
ff_pps_cleaned$key <- NULL

ff_simple <- readRDS("data/forestfire_simple.rds")
ff_simple$name <- "bernoulli"

ff_all <- rbind(ff_simple, ff_pps_cleaned)

# saveRDS(ff_all, "data/forestfire_snowball.rds")