#first run 06.01_rank_probability_maps.R
library(reshape2)
library(ggplot2)
library(colorspace)

node_2_ranks <- unclass(plt_01[[2]]$node$info$object$rankings)

node_3_ranks <- unclass(plt_01[[3]]$node$info$object$rankings)

nrow(node_2_ranks) 

nrow(node_3_ranks)

node_2_presence <- data.frame(sort(colSums(node_2_ranks > 0), decreasing = T))

node_2_presence$genotype <- row.names(node_2_presence)

colnames(node_2_presence)[1] <- "node_2"

node_2_presence <- node_2_presence[, c("genotype", "node_2")]

row.names(node_2_presence) <- NULL

node_3_presence <- data.frame(sort(colSums(node_3_ranks > 0), decreasing = T))

node_3_presence$genotype <- row.names(node_3_presence)

colnames(node_3_presence)[1] <- "node_3"

node_3_presence <- node_3_presence[, c("genotype", "node_3")]

row.names(node_3_presence) <- NULL

presence_node <- merge(x = node_2_presence, y = node_3_presence, by = "genotype")

# write.table(presence_node, file = "data/processed/genotype_node.csv",
#             sep = ",")


pn <- presence_node[-1]

row.names(pn) <- presence_node[,1]

pn <- as.matrix(presence_node[,-1])

row.names(pn) <- presence_node[,1]

# png(filename = "output/figures/presence_node.png",
#     width = 6, height = 12, res = 600, units = "in")
# 
# pheatmap::pheatmap(mat = pn,
#                   cluster_rows = FALSE,
#                   cluster_cols = FALSE,
#                   angle_col = 0,
#                   display_numbers = TRUE,
#                   number_format = ,
#                   number_color = "gray15",
#                   fontsize = 14,
#                   fontsize_number = 14,
#                   cellheight = 12,
#                   cellwidth = 100,
#                   )
# dev.off()

presence_node_l <- melt(presence_node)

factor(presence_node_l$genotype)


ggplot() + 
  geom_tile(data = presence_node_l,
            aes(x = variable,
                y = genotype,
                fill = value)) +
  geom_text(data = presence_node_l, 
            aes(x = variable,
                y = genotype,
                label = value),
            size = 8,
            color = "Gray10") + 
  scale_y_discrete(limits = rev(unique(sort(presence_node_l$genotype)))) +
  scale_x_discrete(labels = c("Node 2", "Node 3")) + 
  scale_fill_continuous_sequential(palette = "TealGrn") +
  theme(axis.text.y = element_text(size = 16,
                                   color = "gray15"),
        axis.text.x = element_text(size = 16,
                                   color = "gray15"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  labs(fill = c("Evaluations"))


ggsave("output/figures/eval_x_node.png", dpi = 800, width = 10, height = 16, units = "in")



