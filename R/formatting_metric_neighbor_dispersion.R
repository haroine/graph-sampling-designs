
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

#### Correction (previously forgot to multiply by mean degree in formula)

deff_tidy <- readRDS("data/tidy_deffs_networks.rds")

corrected_deffs <- deff_tidy

corrected_deffs$dbar <- 1

set.seed(1005192119)

N <- 1000


corrected_deffs[corrected_deffs$graph_name=="0.05",]$dbar <- 
  mean(degree(forest.fire.game(N, 0.05, directed=F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="0.1",]$dbar <- 
  mean(degree(forest.fire.game(N, 0.10, directed=F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="0.15",]$dbar <- 
  mean(degree(forest.fire.game(N, 0.15, directed=F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="0.2",]$dbar <- 
  mean(degree(forest.fire.game(N, 0.20, directed=F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="0.25",]$dbar <- 
  mean(degree(forest.fire.game(N, 0.25, directed=F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="0.3",]$dbar <- 
  mean(degree(forest.fire.game(N, 0.30, directed=F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="BA",]$dbar <- 
  mean(degree(barabasi.game(N, directed = F)), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="immuno",]$dbar <- 
  mean(degree(immuno), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="USairports",]$dbar <- 
  mean(degree(USairports), na.rm=T)
corrected_deffs[corrected_deffs$graph_name=="yeast",]$dbar <- 
  mean(degree(yeast), na.rm=T)

corrected_deffs[corrected_deffs$metric == "deff2",]$deff <- 
(corrected_deffs[corrected_deffs$metric == "deff2",]$deff - 1) * corrected_deffs[corrected_deffs$metric == "deff2",]$dbar + 1

corrected_deffs$dbar <- NULL

saveRDS(corrected_deffs, "data/tidy_deffs_networks.rds")

