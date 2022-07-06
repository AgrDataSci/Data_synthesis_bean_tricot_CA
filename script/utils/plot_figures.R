library(readr)
library(sf)
library(ggplot2)
library(PlackettLuce)
library(network)
library(GGally)

library(grid)
library(RColorBrewer)

library(igraph)

#library(ggnet2)
#library(sna) #for function  %v% 

# library(ggsp)
# library(ggspatial)
# library(patchwork)
# library(cowplot)



#load trial data
tricot_data <- readr::read_csv("data/processed/red_beans_data.csv")

#load administrative boundaries
source("script/utils/load_adm_layers.R")

#--- Figure_1 ---#
sym_size <- 3.5

tricot_data_sf <- sf::st_as_sf(x = tricot_data, coords = c("lon", "lat"), crs = 4326)

#Honduras trials
hnd_trials <- tricot_data_sf[tricot_data_sf$trial_name == "HND_10_2016" |
                                     tricot_data_sf$trial_name == "HND_05_2017" |
                                     tricot_data_sf$trial_name == "HND_06_2017" |
                                     tricot_data_sf$trial_name == "HND_10_2017", ]

#Honduras-El Salvador trials
hnd_slv_trials <- tricot_data_sf[tricot_data_sf$trial_name == "HND_SLV_09_2015", ]

#Nicaragua trials
nic_trials <- tricot_data_sf[tricot_data_sf$trial_name == "NIC_09_2015"|
                                     tricot_data_sf$trial_name == "NIC_12_2015"|
                                     tricot_data_sf$trial_name == "NIC_12_2016"|
                                     tricot_data_sf$trial_name == "NIC_09_2016"|
                                     tricot_data_sf$trial_name == "NIC_06_2016", ]

#Costa Rica trials
cri_trials <- tricot_data_sf[tricot_data_sf$trial_name == "CRI_10_2017" |
                                     tricot_data_sf$trial_name == "CRI_12_2017" |
                                     tricot_data_sf$trial_name == "CRI_05_2018" |
                                     tricot_data_sf$trial_name == "CRI_12_2018", ] 


inta_trials <- c("CRI_10_2017", "CRI_05_2018", "CRI_12_2018", "CRI_12_2017")

catie_trials <- c("NIC_09_2015", "NIC_12_2015", "NIC_12_2016", "NIC_09_2016", "NIC_06_2016",
                  "HND_SLV_09_2015")

fipah_trials <- "HND_10_2017"

fipah_prr_trials <- c("HND_10_2016", "HND_06_2017", "HND_05_2017")


tricot_data_sf$org <- ifelse(tricot_data_sf$trial_name %in% catie_trials, "CATIE", 
                             ifelse(tricot_data_sf$trial_name %in% inta_trials, "INTA-UCR", 
                                    ifelse(tricot_data_sf$trial_name %in% fipah_trials, "FIPAH", "FIPAH-PRR")))

trials_pal <- viridisLite::turbo(n = 14)

ca_countries_spvt <- terra::vect(ca_adm_0_ext$geom)

ca_countries_ctrds <- as.data.frame(rbca_countries)

ca_countries_ctrds$x2 <- c(NULL,NULL, NULL, NULL)

ca_countries_ctrds$y2 <- c(NULL,NULL, NULL, NULL)

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "HND","x2"] <- sf::st_coordinates(sf::st_centroid(hnd_adm_0))[[1]] 
  
ca_countries_ctrds[ca_countries_ctrds$GID_0 == "HND","y2"] <- sf::st_coordinates(sf::st_centroid(hnd_adm_0))[[2]]  

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "SLV","x2"] <- sf::st_coordinates(sf::st_centroid(slv_adm_0))[[1]]

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "SLV","y2"] <- sf::st_coordinates(sf::st_centroid(slv_adm_0))[[2]]

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "NIC","x2"] <- sf::st_coordinates(sf::st_centroid(nic_adm_0))[[1]] 

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "NIC","y2"] <- sf::st_coordinates(sf::st_centroid(nic_adm_0))[[2]]

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "NIC","y2"] <- ca_countries_ctrds[ca_countries_ctrds$GID_0 == "NIC","y2"] - .5

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "CRI","x2"] <- sf::st_coordinates(sf::st_centroid(cri_adm_0))[[1]]

ca_countries_ctrds[ca_countries_ctrds$GID_0 == "CRI","y2"] <- sf::st_coordinates(sf::st_centroid(cri_adm_0))[[2]]


tricot_data_sf$trial_org <- paste0(tricot_data_sf$trial_name, ", ", tricot_data_sf$org)


ca_trials_map <- ggplot() +
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_sf(data = rbca_countries, fill = "white", col = "slategray4") +
  geom_sf(data = tricot_data_sf,
          aes(color = trial_name, shape = org),
          size = sym_size) +
  coord_sf(xlim = c(-91, -82), ylim = c(8, 16), expand = T) +
  ggtitle("") +
  labs(color = "Trial ID", shape = "Organization") +
  scale_color_manual(values = trials_pal) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  geom_text(data = ca_countries_ctrds, aes(label = GID_0,  x = x2, y = y2)) +
  theme(panel.background = element_rect(fill = "aliceblue",
                                        colour = "aliceblue",
                                        size = 0.5,
                                        linetype = "solid"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ca_trials_map

ggsave(filename = "output/figures/figure_1_004.png", width = 12, height = 8, dpi = 300)

#### end of Figure 1 ####

#### Figure 2 ####
#create PlackettLuce Rankings, group = F for assess connectivity
#load rbca_data after removing non connected genotypes
source("script/utils/load_rbca_data.R")

rbca_ranks <- gosset::rank_tricot(data = tricot_data,
                                  items = paste0("variety_", letters[1:3]),
                                  input = c("overall_best", "overall_worst"), group = F)

genotype_names <- unique(tricot_data_sf$variety_b)

genotype_names <- genotype_names[!is.na(genotype_names)]

genotypes <- vector(mode = "list", length = length(genotype_names))

genotypes <- tapply(genotype_names, 
                    INDEX = genotype_names,
                    function(X){
                      y <- tricot_data_sf[tricot_data_sf$variety_a == X, ]
                      y <- y[!is.na(y$variety_a), ]
                      }
                    )

rbca_adj <- adjacency(rbca_ranks)

net <- network::network(rbca_adj, names.eval = "weights")

igraph_net <- graph.adjacency(rbca_adj,weighted = T)



E(igraph_net)$weight

ggnet <- ggnetwork(igraph_net)

# ggnet$type <- ifelse(ggnet$name %in% exp_list, 
#                      "Experimental line", 
#                      "Released variety")
# 
# ggplot(ggnet, aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_edges(aes(color = weight/100), 
#              arrow = arrow(length = unit(6, "pt")),
#              color = "grey50", curvature = 0, size =.5) +
#   geom_nodes(aes(color = type), size = 8) +
#   theme_net()
  


landr_list <- c("Rosado", "Seda")

exp_list <- c("BCR 122-74", "BFS 47", "BRT 103-182", 
              "MHC 2-13-49", "MHR 311-17", "RRH 336-28",
              "RS 907-28", "RS 909-35", "SRS2-36-34")


genotypes <- network::network.vertex.names(net)

network::get.vertex.attribute(net, "vertex.names")

network::set.vertex.attribute(net, "Type", 
                              ifelse(genotypes %in% exp_list, 
                                     "Experimental line", 
                                     "Released variety"))

exp_list %in% genotypes

genotypes

# net %v% "Type" = ifelse(genotypes %in% landr_list, "Landrace", 
#                         ifelse(genotypes %in% exp_list, "Experimental line", "Released Variety"))

#net %v% "Type" = ifelse(genotypes %in% exp_list, "Experimental line", "Released variety")

custom_palette <- c("ALS 0532-6" = "#4476AC",
                    "EAP 9510-77" = "#4476AC",
                    "IBC 308-24" = "#4476AC",
                    "BCR 122-74" = "#49A75A",
                    "BFS 47" = "#49A75A",
                    "BRT 103-182" = "#49A75A",
                    "EAP 9508-93" = "#4476AC",
                    "703-SM 15216-11-5" = "#4476AC", 
                    "703-SM15216-11-4" = "#4476AC",
                    "MIB 397-72" = "#4476AC",
                    "IBC 302-29" = "#4476AC",
                    "IBC 301-204" = "#4476AC",
                    "429 DFSZ 15094-39-4" = "#4476AC",
                    "SRC 2-18-1" = "#4476AC",
                    "MPN 103-137" = "#4476AC",
                    "MHC 2-13-49" = "#4476AC",
                    "MHR 311-17" = "#4476AC", 
                    "BFS-24" = "#4476AC",
                    "Rosado" = "#FF7F00",
                    "RRH 336-28" = "#49A75A",
                    "RS 907-28" = "#49A75A",
                    "RS 909-35" = "#49A75A",
                    "Seda" = "#FF7F00",
                    "SJC 730-79" = "#4476AC", 
                    "SRS2-36-34" = "#4476AC",
                    "SX 14825-7-1" = "#4476AC",
                    "SRS 56-3" = "#4476AC")

#GGally::ggnet2(net, label = T,  node.color = "grey", arrow.size = 12, arrow.gap = 0.01) 

type_palette_1 <- c("Landrace" = "#FF7F00",
                    "Experimental line" = "#006699",
                    "Released Variety" = "#99CC33")

type_palette_2 <- c(#"Landrace" = "#f1c40f",
  "Experimental line" = "#d73027",
  "Released variety" = "#1a9850")

type_palette_3 <- c("Experimental line" = "#ca0020",
                    "Released variety" = "#104e8b")

GGally::ggnet2(net, 
               label = TRUE,
               arrow.size = 8, 
               edge.color = "gray40",
               mode = "fruchtermanreingold",
               arrow.gap = 0.03, 
               color = "Type",
               palette = type_palette_2,
               legend.size = 12,
               fontface = "plain"
                             ) 

ggsave("output/figures/gen_net_17_v03.png", width = 12, height = 6, dpi = 600, units = "in")

#### end of Figure 2 ####

genotype_names <- unique(tricot_data_sf$variety_b)

genotype_names <- genotype_names[!is.na(genotype_names)]

genotypes <- vector(mode = "list", length = length(genotype_names))

genotypes <- tapply(genotype_names, 
                    INDEX = genotype_names, 
                    function(X){
                      y <- tricot_data_sf[tricot_data_sf$variety_a == X, ]
                      y <- y[!is.na(y$variety_a), ]
                      }
                    )

plot_trial_maps <- function(X){
  gen_name <- unique(X$variety_a)
  ggplot() + 
    geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") + 
    geom_sf(data = rbca_countries, fill = "white", col = "slategray4") +
    geom_sf(data = X, 
            aes(color = trial_name), 
            size = sym_size, 
            #shape = 20, 
            alpha = 0.7) + 
    coord_sf(xlim = c(-91, -82), ylim = c(8, 16), expand = T) +
    ggtitle(gen_name) + 
    labs(color = "Trial ID") + 
    scale_color_manual(values = trials_pal) + 
    theme(panel.background = element_rect(fill = "aliceblue",
                                          colour = "aliceblue",
                                          size = 0.5, linetype = "solid"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.key.size = unit(.25, "in"),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.margin = margin(0, 0, 20, 0))
  
}

gen_trial_plots <- lapply(genotypes, plot_trial_maps)

#All genotypes in each trial

(gen_trial_plots$`ALS 0532-6` |
                gen_trial_plots$`BCR 122-74` |
                gen_trial_plots$`BFS 47` |
                gen_trial_plots$`BRT 103-182` |
                gen_trial_plots$`EAP 9508-93`) /
        (gen_trial_plots$`EAP 9510-77` | 
                 gen_trial_plots$`IBC 301-204` |
                 gen_trial_plots$`IBC 302-29` |
                 gen_trial_plots$`IBC 308-24` |
                 gen_trial_plots$`MHC 2-13-49`) /        
        (gen_trial_plots$`MIB 397-72` |
                 gen_trial_plots$`MPN 103-137`|
                 gen_trial_plots$`SJC 730-79` |
                 gen_trial_plots$`SRC 2-18-1` | 
                 gen_trial_plots$`SX 14825-7-1`) /
        (gen_trial_plots$`429 DFSZ 15094-39-4` |
                 gen_trial_plots$`703-SM 15216-11-5` )

ggsave("output/figures/genotype_x_trial_maps_03.png",
       dpi = 600,
       wid)

#only genotypes in top-3

(gen_trial_plots$`ALS 0532-6`  |
  gen_trial_plots$`BCR 122-74` |
   gen_trial_plots$`EAP 9508-93`|
    plot_spacer()) /
  (gen_trial_plots$`EAP 9510-77` |
     gen_trial_plots$`MHC 2-13-49`|
     gen_trial_plots$`SRC 2-18-1` | 
     gen_trial_plots$`SX 14825-7-1`)

# ggsave("output/figures/top_3_genotype_x_trial_maps_02_.png",
#        width = 18,
#        height = 9,
#        dpi = 600,
#        units = "in")

ggsave("output/figures/top_3_genotype_x_trial_maps_03_.png",
       width = 18,
       height = 9,
       dpi = 400,
       units = "in")

#####end Figure 9 ##########################














