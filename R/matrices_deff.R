library(tidyverse)
library(xtable)

table_estimators <- readRDS("data/forestfire_snowball.rds")
table_estimators <- table_estimators %>%
  filter(parameter == 0.03)

table_estimators <- table_estimators %>% 
  select(stat_name, auxiliary_var=name, deff_snowball, deff_simple)

ncol_matrix <- 2 * length(unique(table_estimators$auxiliary_var)) + 1
nrow_matrix <- length(unique(table_estimators$stat_name))

matrix_estimators <- matrix(NA, nrow = nrow_matrix
                            , ncol = ncol_matrix)

df_estimators <- as.data.frame(matrix_estimators)
names_df_estimators <- c(
  paste(unique(table_estimators$auxiliary_var),"simple",sep="_"),
  paste(unique(table_estimators$auxiliary_var),"snowball",sep="_"))
names_df_estimators <- names_df_estimators[order(names_df_estimators)]
names_df_estimators <- c("stat_name", names_df_estimators)
names(df_estimators) <- names_df_estimators

vec_stat_name <- unique(table_estimators$stat_name)
vec_auxiliary_vars <- unique(table_estimators$auxiliary_var)

for(k in 1:length(vec_stat_name)) {
  current_stat_name <- vec_stat_name[k]
  df_estimators[k,"stat_name"] <- current_stat_name
  for(current_aux in vec_auxiliary_vars) {
    for(current_design in c("simple", "snowball")) {
      current_col_name <- paste(current_aux, current_design, sep="_")
      df_estimators[k,current_col_name] <- as.numeric(table_estimators %>%
             filter(stat_name == current_stat_name) %>%
             filter(auxiliary_var == current_aux) %>%
             select(paste("deff",current_design, sep="_")))
    }

  }

}

# saveRDS(df_estimators, "data/matrix_deffs_ff_003.rds")

## Reorder

df_estimators <- readRDS("data/matrix_deffs_yeast.rds")
df_estimators <- df_estimators %>% arrange(stat_name)
df_estimators <- saveRDS(df_estimators, "data/matrix_deffs_yeast.rds")

print(xtable(df_estimators), include.rownames=FALSE)
## Formatage latex


