
metrics_real <- readRDS("data/deffs_networks.rds")

deff_tidy <- metrics_real %>% 
  gather(deff1, deff2, 2:ncol(metrics_real)) %>%
  separate(deff1, into="var", sep="_", extra="drop", remove=F) %>%
  mutate(deff1 = gsub("deff[1:2]_", "", deff1))

names(deff_tidy) <- c("graph_name","var","metric","deff")

deff_tidy <- deff_tidy %>%
  mutate(deff=as.numeric(deff)) %>%
  arrange(graph_name, deff)

deff_tidy$deff <- deff_tidy$deff + 1

saveRDS(deff_tidy, "data/tidy_deffs_networks.rds")