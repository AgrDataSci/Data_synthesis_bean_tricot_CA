library(PlackettLuce)
library(RColorBrewer)
library(ggplot2)
library(patchwork)
library(colorspace)
library(partykit)

source("script/utils/plot_pltree_rbca.R")

source("script/utils/load_rbca_data.R")

load("data/processed/env_newdata/newdata_clim_all.rda")

source("script/utils/load_adm_layers.R")

years_list <- 2000:2019

rbca_countries_spvt <- terra::vect(rbca_countries)

#get a template for raster layers

template_file <- "data/processed/raster_template/Precipitation-Flux_C3S-glob-agric_AgERA5_20000101_final-v1.0.nc"

map_template <- terra::rast(x = template_file)

map_template <- terra::crop(x = map_template, rbca_countries_spvt)

map_template <- terra::mask(x = map_template, mask = rbca_countries_spvt, touches = FALSE)

map_template_pts <- terra::as.points(x = map_template)

plt_01 <- PlackettLuce::pltree(rbca_ranks ~ WSDI + R20mm + T10p + hts_mean_19_flo,
                               data = rbca_data,
                               npseudo = 0.5,
                               alpha = .9,
                               bonferroni = T,
                               verbose = FALSE,
                               dfsplit = F,
                               prune = "AIC"
                               )

plt_01

plot_tree_2(plt_01, log = TRUE, ref = NULL)

ggsave("output/figures/plt_01_v02.png",
       dpi = 600, 
       width = 10, 
       height = 8,
       units = "in")


#for AOA maps, run script 09_dis_index.R first

###########################################################
source("script/utils/get_entropy.R")
source("script/utils/get_ranking_probabilities.R")



prim_top_3_rank_probs <- get_ranking_probabilities(.plt_model = plt_01,
                                            .newdata = primera_newdata,
                                            .nsim = 1000000,
                                            .top_k = 3)



prim_top_3_rank_probs$rank_probs
prim_top_3_rank_probs$gen_names
prim_top_3_rank_probs$gen_names_list
prim_top_3_rank_probs$entropy


#------------------------------#
#raster of top_k genotypes from Simulations
top_k_prim_gens_rast <- map_template_pts

terra::values(top_k_prim_gens_rast) <- prim_top_3_rank_probs$gen_names

top_k_prim_gens_rast <-  terra::rasterize(top_k_prim_gens_rast, field = "value", map_template)

names(top_k_prim_gens_rast) <- "sim_top_k_gens"

top_k_prim_gens_rast

top_k_prim_gens_rast <- terra::crop(x = top_k_prim_gens_rast, y = rbca_countries_spvt)

top_k_prim_gens_rast <- terra::mask(x = top_k_prim_gens_rast, mask = rbca_countries_spvt, touches = FALSE)

top_k_prim_gens_rast_df <- terra::as.data.frame(top_k_prim_gens_rast, xy = T)

colpal_top_k <- RColorBrewer::brewer.pal(length(unique(prim_top_3_rank_probs$gen_names)), "Dark2")

top_k_rank_prob_prim_map <-  ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = top_k_prim_gens_rast_df, aes(x = x, y = y, fill = sim_top_k_gens)) +
  scale_fill_manual(values = colpal_top_k, guide = guide_legend(ncol = 2, title.position = "top")) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs(x = "long", y = "lat") + 
  labs(fill = paste("Top-3 Genotypes")) +
  ggtitle("a") +  
  theme(legend.title = element_text(size = 14),
        legend.position="bottom", 
        legend.text = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

#entropy map of top_k rank probabilities

top_k_prim_entrop_rast <- map_template_pts

terra::values(top_k_prim_entrop_rast) <- prim_top_3_rank_probs$entropy

top_k_prim_entrop_rast <-  terra::rasterize(top_k_prim_entrop_rast, field = "value", map_template)

names(top_k_prim_entrop_rast) <- "entropy"

top_k_prim_entrop_rast

top_k_prim_entrop_rast <- terra::crop(x = top_k_prim_entrop_rast, y = rbca_countries_spvt)

top_k_prim_entrop_rast <- terra::mask(x = top_k_prim_entrop_rast, mask = rbca_countries_spvt, touches = FALSE)

top_k_prim_entrop_rast_df <- terra::as.data.frame(top_k_prim_entrop_rast, xy = T)

#scale_fill_continuous_diverging(palette = "Red-Green", rev = T, mid = .5, limits = c(0,1)) +


top_k_entrop_prim_map <-  ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = top_k_prim_entrop_rast_df, aes(x = x, y = y, fill = entropy)) +
  scale_fill_continuous_sequential(palette = "Reds", rev = T) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "Normalized Entropy") +
  ggtitle("b") + 
  theme(legend.title = element_text(size = 14), 
        legend.position = "bottom",
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


top_k_rank_prob_prim_map |  (top_k_entrop_prim_map + aoa_prim_map) 


ggsave("output/figures/prim_top_3_rank_prob_entr_aoa.png",
       dpi = 300, width = 17, height = 10)


#-- Postrera --#
post_top_3_rank_probs <- get_ranking_probabilities(.plt_model = plt_01,
                                                   .newdata = postrera_newdata,
                                                   .nsim = 1000000,
                                                   .top_k = 3)



post_top_3_rank_probs$rank_probs
post_top_3_rank_probs$gen_names
post_top_3_rank_probs$gen_names_list
post_top_3_rank_probs$entropy


#------------------------------#
#raster of top_k genotypes from Simulations
top_k_post_gens_rast <- map_template_pts

terra::values(top_k_post_gens_rast) <- post_top_3_rank_probs$gen_names

top_k_post_gens_rast <-  terra::rasterize(top_k_post_gens_rast, field = "value", map_template)

names(top_k_post_gens_rast) <- "sim_top_k_gens"

top_k_post_gens_rast

top_k_post_gens_rast <- terra::crop(x = top_k_post_gens_rast, y = rbca_countries_spvt)

top_k_post_gens_rast <- terra::mask(x = top_k_post_gens_rast, mask = rbca_countries_spvt, touches = FALSE)

top_k_post_gens_rast_df <- terra::as.data.frame(top_k_post_gens_rast, xy = T)

colpal_top_k <- RColorBrewer::brewer.pal(length(unique(post_top_3_rank_probs$gen_names)), "Dark2")

top_k_rank_prob_post_map <-  ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = top_k_post_gens_rast_df, aes(x = x, y = y, fill = sim_top_k_gens )) +
  scale_fill_manual(values = colpal_top_k, guide = guide_legend(ncol = 2, title.position = "top")) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = paste("Top-3 Genotypes")) +
  ggtitle("a") + 
  theme(legend.title = element_text(size = 14),
        legend.position="bottom", 
        legend.text = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

#entropy map of top_k rank probabilities

top_k_post_entrop_rast <- map_template_pts

terra::values(top_k_post_entrop_rast) <- post_top_3_rank_probs$entropy

top_k_post_entrop_rast <-  terra::rasterize(top_k_post_entrop_rast, field = "value", map_template)

names(top_k_post_entrop_rast) <- "entropy"

top_k_post_entrop_rast

top_k_post_entrop_rast <- terra::crop(x = top_k_post_entrop_rast, y = rbca_countries_spvt)

top_k_post_entrop_rast <- terra::mask(x = top_k_post_entrop_rast, mask = rbca_countries_spvt, touches = FALSE)

top_k_post_entrop_rast_df <- terra::as.data.frame(top_k_post_entrop_rast, xy = T)

top_k_entrop_post_map <-  ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = top_k_post_entrop_rast_df, aes(x = x, y = y, fill = entropy)) +
  scale_fill_continuous_sequential(palette = "Reds", rev = T) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs(x = "long", y = "lat") + 
  labs(fill = "Normalized Entropy") +
  ggtitle("b") + 
  theme(legend.title = element_text(size = 14), 
        legend.position = "bottom",
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


top_k_rank_prob_post_map | (top_k_entrop_post_map + aoa_post_map)


ggsave("output/figures/post_top_3_rank_prob_entr_aoa.png",
       dpi = 300, width = 17, height = 10)


# -- Apante --#

apan_top_3_rank_probs <- get_ranking_probabilities(.plt_model = plt_01,
                                                   .newdata = apante_newdata,
                                                   .nsim = 1000000,
                                                   .top_k = 3)



apan_top_3_rank_probs$rank_probs
apan_top_3_rank_probs$gen_names
apan_top_3_rank_probs$gen_names_list
apan_top_3_rank_probs$entropy


#------------------------------#
#raster of top_k genotypes from Simulations
top_k_apan_gens_rast <- map_template_pts

terra::values(top_k_apan_gens_rast) <- apan_top_3_rank_probs$gen_names

top_k_apan_gens_rast <-  terra::rasterize(top_k_apan_gens_rast, field = "value", map_template)

names(top_k_apan_gens_rast) <- "sim_top_k_gens"

top_k_apan_gens_rast

top_k_apan_gens_rast <- terra::crop(x = top_k_apan_gens_rast, y = rbca_countries_spvt)

top_k_apan_gens_rast <- terra::mask(x = top_k_apan_gens_rast, mask = rbca_countries_spvt, touches = FALSE)

top_k_apan_gens_rast_df <- terra::as.data.frame(top_k_apan_gens_rast, xy = T)

colpal_top_k <- RColorBrewer::brewer.pal(length(unique(apan_top_3_rank_probs$gen_names)), "Dark2")

top_k_rank_prob_apan_map <-  ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = top_k_apan_gens_rast_df, aes(x = x, y = y, fill = sim_top_k_gens )) +
  scale_fill_manual(values = colpal_top_k, guide = guide_legend(ncol = 2, title.position = "top")) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = paste("Top-3 Genotypes")) +
  ggtitle("a") + 
  theme(legend.title = element_text(size = 14),
        legend.position="bottom", 
        legend.text = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

#entropy map of top_k rank probabilities

top_k_apan_entrop_rast <- map_template_pts

terra::values(top_k_apan_entrop_rast) <- apan_top_3_rank_probs$entropy

top_k_apan_entrop_rast <-  terra::rasterize(top_k_apan_entrop_rast, field = "value", map_template)

names(top_k_apan_entrop_rast) <- "entropy"

top_k_apan_entrop_rast

top_k_apan_entrop_rast <- terra::crop(x = top_k_apan_entrop_rast, y = rbca_countries_spvt)

top_k_apan_entrop_rast <- terra::mask(x = top_k_apan_entrop_rast, mask = rbca_countries_spvt, touches = FALSE)

top_k_apan_entrop_rast_df <- terra::as.data.frame(top_k_apan_entrop_rast, xy = T)

top_k_entrop_apan_map <-  ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = top_k_apan_entrop_rast_df, aes(x = x, y = y, fill = entropy)) +
  scale_fill_continuous_sequential(palette = "Reds", rev = T) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "Normalized Entropy") +
  ggtitle("b") + 
  theme(legend.title = element_text(size = 14), 
        legend.position = "bottom",
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


top_k_rank_prob_apan_map | (top_k_entrop_apan_map + aoa_apan_map)

ggsave("output/figures/apan_top_3_rank_prob_entr_aoa.png",
      dpi = 300, width = 17, height = 10)



