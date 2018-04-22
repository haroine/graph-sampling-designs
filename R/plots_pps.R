library(tidyverse)

pps_ff1 <- readRDS("data/matrix_deffs_ff_025.rds")
pps_ff2 <- readRDS("data/matrix_deffs_ff_003.rds")

##### Tidy 1

pps_ff1_tidy <- pps_ff1 %>%
  gather(name, deff, -stat_name) %>%
  separate(name,c("name","type"), "_") %>%
  filter(type=="snowball")

pps_ff1_tidy2 <- pps_ff1 %>%
  gather(name, deff, -stat_name) %>%
  separate(name,c("name","type"), "_") %>%
  filter(type=="simple")

pps_ff1_tidy_merged <- merge(pps_ff1_tidy, pps_ff1_tidy2, 
                      by=c("stat_name","name"))

names(pps_ff1_tidy_merged) <- c("stat_name","name","type1","deff_simple",
                                "type2","deff_snowball")

pps_ff1_tidy <- pps_ff1_tidy_merged %>%
  select(stat_name, name, deff_simple, deff_snowball)

pps_ff1_tidy <- na.omit(pps_ff1_tidy)
pps_ff1_tidy$parameter <- "FF 0.25"
pps_ff1_tidy$type <- NULL

##### Tidy 1

pps_ff2_tidy <- pps_ff2 %>%
  gather(name, deff, -stat_name) %>%
  separate(name,c("name","type"), "_") %>%
  filter(type=="snowball")

pps_ff2_tidy2 <- pps_ff2 %>%
  gather(name, deff, -stat_name) %>%
  separate(name,c("name","type"), "_") %>%
  filter(type=="simple")

pps_ff2_tidy_merged <- merge(pps_ff2_tidy, pps_ff2_tidy2, 
                             by=c("stat_name","name"))

names(pps_ff2_tidy_merged) <- c("stat_name","name","type1","deff_simple",
                                "type2","deff_snowball")

pps_ff2_tidy <- pps_ff2_tidy_merged %>%
  select(stat_name, name, deff_simple, deff_snowball)

pps_ff2_tidy <- na.omit(pps_ff2_tidy)
pps_ff2_tidy$parameter <- "FF 0.03"
pps_ff2_tidy$type <- NULL

#### Real networks

immuno_pps <- readRDS("data/immuno_estimators_pps.rds")

immuno_pps <- immuno_pps %>%
  select(stat_name, name, deff_simple, deff_snowball, parameter)

BA_pps <- readRDS("data/BA_estimators_pps.rds")

BA_pps <- BA_pps %>%
  select(stat_name, name, deff_simple, deff_snowball, parameter)

yeast_pps <- readRDS("data/yeast_snowball.rds")

yeast_pps <- yeast_pps %>%
  select(stat_name, name, deff_simple, deff_snowball)

yeast_pps$parameter <- "yeast"

## TODO Add ffs

pps_data <- rbind(yeast_pps, immuno_pps, BA_pps, pps_ff1_tidy, pps_ff2_tidy)

pps_data <- na.omit(pps_data)

pps_data$deff_gain <- pps_data$deff_snowball / pps_data$deff_simple

# pps_data_best <- pps_data %>%
#   group_by(stat_name) %>%
#   summarize(  )

all_aux_vars <- unique(pps_data$name)
all_aux_vars <- all_aux_vars[order(all_aux_vars)]
# current_aux_var <- all_aux_vars[1]

for(current_aux_var in all_aux_vars) {
  
  plot_pps <- ggplot(data=pps_data %>% filter(name==current_aux_var), aes(x=parameter)) +
    geom_point(aes(y=deff_gain, colour=stat_name)) +
    geom_hline(yintercept = 1, colour="black", linetype="longdash") +
    scale_y_log10() +
    labs(title=paste("Auxiliary variable:", current_aux_var),
         x="Network name", y="Deff ratio snowball / initial")
  
  print(plot_pps)
}
