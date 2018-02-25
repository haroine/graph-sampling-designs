library(tidyverse)

table_estimators <- readRDS("backups/ff_fwprob_100218_by_001.rds")

table_estimators <- table_estimators %>% 
  select(stat_name, auxiliary_var=name, deff_snowball)

matrix_estimators <- matrix(NA, nrow = length(unique(table_estimators$stat_name))
                            , ncol = length(unique(table_estimators$auxiliary_var)))