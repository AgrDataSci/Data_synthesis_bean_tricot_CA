#Disimilarity Index
library(CAST)

# data is assumed to be loaded in the previous scripts

model_vars <- c("WSDI", "R20mm", "T10p", "hts_mean_19_flo")


#--- Primera Season ----#
prim_aoa_di <- lapply(primera_newdata, 
                   function(X) aoa(newdata = X, train = rbca_data[-1],
                                   variables = model_vars,
                                   folds = rbca_folds)
                   )


prim_di <- lapply(prim_aoa_di, function(X) X$DI)

prim_di_m <- sapply(X = prim_di, FUN = cbind)

prim_di_mean <- apply(X = prim_di_m, MARGIN = 1, FUN = mean)

prim_di_map <- map_template_pts

terra::values(prim_di_map) <- prim_di_mean

prim_di_rast <-  terra::rasterize(prim_di_map, field = "value", map_template)

names(prim_di_rast) <- "di"

prim_di_rast_df <- terra::as.data.frame(x = prim_di_rast, xy = TRUE)

di_prim_map <- ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = prim_di_rast_df, aes(x = x, y = y, fill = di)) +
  scale_fill_viridis_c(option = "viridis") +
  #scale_fill_manual(palette = colpal_1) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "DI") +
  ggtitle("a") + 
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


#AOA Primera
prim_aoa <- lapply(prim_aoa_di, function(X) X$AOA)

prim_aoa_m <- sapply(X = prim_aoa, FUN = cbind)

prim_aoa_mean <- apply(X = prim_aoa_m, MARGIN = 1, FUN = function(X) floor(median(X)))

prim_aoa_map <- map_template_pts

terra::values(prim_aoa_map) <- prim_aoa_mean

prim_aoa_rast <-  terra::rasterize(prim_aoa_map, field = "value", map_template)

names(prim_aoa_rast) <- "aoa"

prim_aoa_rast_df <- terra::as.data.frame(x = prim_aoa_rast, xy = TRUE)

head(prim_aoa_rast_df)

palette.colors(palette = "Paired")

aoa_prim_map <- ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = prim_aoa_rast_df, aes(x = x, y = y, fill = as.factor(aoa))) +
  #scale_fill_viridis_d(option = "viridis") +
  scale_fill_manual(values = c("#E31A1C", "#B2DF8A")) +
  #scale_fill_discrete_diverging(palette = "Red-Green") +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "AOA") +
  ggtitle("c") + 
  theme(legend.title = element_text(size = 14), 
        legend.position = "bottom",
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


di_prim_map |aoa_prim_map

ggsave(filename = "output/figures/aoa_prim.png", width = 17, height = 8.5, dpi = 300)

#--- Postrera Season ----#


post_aoa_di <- lapply(postrera_newdata, 
                      function(X) aoa(newdata = X, train = rbca_data[-1],
                                      variables = model_vars,
                                      folds = rbca_folds)
                      )


post_di <- lapply(post_aoa_di, function(X) X$DI)

post_di_m <- sapply(X = post_di, FUN = cbind)

post_di_mean <- apply(X = post_di_m, MARGIN = 1, FUN = mean)

post_di_map <- map_template_pts

terra::values(post_di_map) <- post_di_mean

post_di_rast <-  terra::rasterize(post_di_map, field = "value", map_template)

names(post_di_rast) <- "di"

post_di_rast_df <- terra::as.data.frame(x = post_di_rast, xy = TRUE)

di_post_map <- ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = post_di_rast_df, aes(x = x, y = y, fill = di)) +
  scale_fill_viridis_c(option = "viridis") +
  #scale_fill_manual(palette = colpal_1) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "DI") +
  ggtitle("a") + 
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


post_aoa <- lapply(post_aoa_di, function(X) X$AOA)

post_aoa_m <- sapply(X = post_aoa, FUN = cbind)

post_aoa_mean <- apply(X = post_aoa_m, MARGIN = 1, FUN = function(X) floor(median(X)))

post_aoa_map <- map_template_pts

terra::values(post_aoa_map) <- post_aoa_mean

post_aoa_rast <-  terra::rasterize(post_aoa_map, field = "value", map_template)

names(post_aoa_rast) <- "aoa"

post_aoa_rast_df <- terra::as.data.frame(x = post_aoa_rast, xy = TRUE)

head(post_aoa_rast_df)

aoa_post_map <- ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = post_aoa_rast_df, aes(x = x, y = y, fill = as.factor(aoa))) +
  scale_fill_manual(values = c("#E31A1C", "#B2DF8A"), guide = guide_legend(title.position = "left")) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "AOA") +
  ggtitle("c") + 
  theme(legend.title = element_text(size = 14), 
        legend.position = "bottom",
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

di_post_map | aoa_post_map

ggsave(filename = "output/figures/aoa_post.png", width = 17, height = 8.5, dpi = 300)


#--- Apante Season ----#


apan_aoa_di <- lapply(apante_newdata, 
                      function(X) aoa(newdata = X, train = rbca_data[-1],
                                      variables = model_vars,
                                      folds = rbca_folds)
                      )


apan_di <- lapply(apan_aoa_di, function(X) X$DI)

apan_di_m <- sapply(X = apan_di, FUN = cbind)

apan_di_mean <- apply(X = apan_di_m, MARGIN = 1, FUN = mean)

apan_di_map <- map_template_pts

terra::values(apan_di_map) <- apan_di_mean

apan_di_rast <-  terra::rasterize(apan_di_map, field = "value", map_template)

names(apan_di_rast) <- "di"

apan_di_rast_df <- terra::as.data.frame(x = apan_di_rast, xy = TRUE)

di_apan_map <- ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = apan_di_rast_df, aes(x = x, y = y, fill = di)) +
  scale_fill_viridis_c(option = "viridis") +
  #scale_fill_manual(palette = colpal_1) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "DI") +
  ggtitle("a") + 
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


apan_aoa <- lapply(apan_aoa_di, function(X) X$AOA)

apan_aoa_m <- sapply(X = apan_aoa, FUN = cbind)

apan_aoa_mean <- apply(X = apan_aoa_m, MARGIN = 1, FUN = function(X) floor(median(X)))

apan_aoa_map <- map_template_pts

terra::values(apan_aoa_map) <- apan_aoa_mean

apan_aoa_rast <-  terra::rasterize(apan_aoa_map, field = "value", map_template)

names(apan_aoa_rast) <- "aoa"

apan_aoa_rast_df <- terra::as.data.frame(x = apan_aoa_rast, xy = TRUE)

head(apan_aoa_rast_df)

aoa_apan_map <- ggplot() + 
  geom_sf(data = ca_adm_0_ext, fill = "#F7F7F7", col = "gray") +
  geom_raster(data = apan_aoa_rast_df, aes(x = x, y = y, fill = as.factor(aoa))) +
  scale_fill_manual(values = c("#E31A1C", "#B2DF8A")) +
  geom_sf(data = rbca_countries, fill = NA, col = "gray40", size = 1) +
  geom_sf(data = rbca_inwater_sf, fill = "skyblue1", col = "steelblue4") +
  coord_sf(xlim = c(-89.9, -82.8), ylim = c(8.3, 15.8), expand = T) +
  labs( x = "long", y = "lat") + 
  labs(fill = "AOA") +
  ggtitle("c") + 
  theme(legend.title = element_text(size = 14), 
        legend.position = "bottom",
        title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

di_apan_map | aoa_apan_map

ggsave(filename = "output/figures/aoa_apan.png", width = 17, height = 8.5, dpi = 300)


#needs library patchwork
#size 13.2 × 6.98

#Primera
(prim_map | aoa_prim_map) + 
  patchwork::plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 20))

ggsave(filename = "output/figures/preds_aoa_prim_001.png",
       dpi = 300,
       device = "png")


#Postrera
(post_map | aoa_post_map) + 
  patchwork::plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 20))

ggsave(filename = "output/figures/preds_aoa_post_001.png",
       dpi = 300,
       device = "png")

#Apante
(apan_map | aoa_apan_map) + 
  patchwork::plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 20))



ggsave(filename = "output/figures/preds_aoa_apan_001.png",
       dpi = 300,
       device = "png")


