source("script/utils/round_split_values_pltree_plot.R")


plot_tree_2 <- function(object, 
                        qve = TRUE,
                        log = TRUE, 
                        ref = NULL,
                        add.letters = FALSE,
                        ci.level = 0.95,
                        ...){
  
  dots <- list(...)
  
  font.size <- dots[["font.size"]]
  threshold <- dots[["threshold"]]
  terms     <- dots[["terms"]]
  adjust    <- dots[["adjust"]]
  nudge.x   <- dots[["nudge.x"]]
  nudge.y   <- dots[["nudge.y"]]
  letter.size <- dots[["letter.size"]]
  
  if(is.null(nudge.x)) nudge.x <- 0
  if(is.null(nudge.y)) nudge.y <- 0.35
  if(is.null(letter.size)) letter.size <- 18
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
  # get node information
  nodes <- list()
  
  for(i in seq_along(node_id)){
    nodes[[i]] <- object[[ node_id[i] ]]$node$info$object
  }
  
  # get number of observations in each inner node
  nobs <- list()
  
  for(i in seq_along(node_id)){
    
    nobs[[i]] <- as.integer(object[[ node_id[i] ]]$node$info$nobs) 
  }
  
  #get item names
  items <- dimnames(coef(object))[[2]]
  
  if(isTRUE(qve)){
    
    # get item parameters from model
    coeffs <- try(lapply(nodes, function(x){
      z <- psychotools::itempar(x, vcov = TRUE, log = log, ref = ref)
      # get estimates from item parameters using qvcalc
      z <- qvcalc::qvcalc(z)$qvframe
      }), silent = TRUE)
    
    if(isTRUE("try-error" %in% class(coeffs))){
      
      message("Unable to compute quasi-variance estimates with whole tree. Updating the model ",
              "using rankings from each node \n")
      
      coeffs <- try(lapply(nodes, function(x) {
        psychotools::itempar(x, vcov = FALSE, log = log, ref = ref)
      }), silent = TRUE)
      
      # update the model, this will check if ranking is complete in each node 
      # and refit the rankings from each node to get the qvSE 
      qvSE <- try(lapply(nodes, function(x){
        
        Z <- x$rankings
        
        Z <- Z[1:length(Z),, as.rankings = F]
        
        rmv <- which(colSums(Z) == 0)
        
        if (length(rmv) > 0) Z <- Z[, -rmv]
        
        Z <- update(x, rankings = Z, weights = freq(Z), start = NULL)
        
        Z <- psychotools::itempar(Z, vcov = TRUE, log = log, ref = ref)
        
        # get estimates from item parameters using qvcalc
        Z <- qvcalc::qvcalc(Z)
        
        # extract data frames with estimates
        Z <- Z$qvframe
        
        Z$items <- rownames(Z)
        
        Z
      }
      ), silent = TRUE)
      
      x <- list()
      for(i in seq_along(coeffs)){
        
        xi <- data.frame(estimate = as.vector(coeffs[[i]]),
                         items = items)
        
        xi <- merge(xi, qvSE[[i]][,c("items", "quasiSE")], by = "items", all.x = TRUE)
        
        x[[i]] <- xi
        
      }
      
      coeffs <- x
      
    }
    
    # if the error persists then return an error 
    if (isTRUE("try-error" %in% class(coeffs))) {
      stop("Unable to compute quasi-variance estimates. Check for errors/warnings in ",
           "your modelparty object. \n Also, you can try qve = FALSE \n")
    }
    
  }
  
  if (isFALSE(qve)) {
    
    add.letters <- FALSE
    
    coeffs <- itempar(object, vcov = FALSE, log = log, ref = ref)
    
    x <- list()
    for (i in seq_len(dim(coeffs)[[1]])) {
      xi <- data.frame(estimate = coeffs[i, ],
                       quasiSE = 0,
                       items = items)
      x[[i]] <- xi
    }
    
    coeffs <- x
    
  }
  
  # Add limits in error bars and item names
  coeffs <- lapply(coeffs, function(X){
    X <- within(X, {
      # bmax = X$estimate + stats::qnorm(1 - (1 - ci.level) / 2) * X$quasiSE
      # bmin = X$estimate - stats::qnorm(1 - (1 - ci.level) / 2) * X$quasiSE
      # bmax = X$estimate + (2 * X$quasiSE)
      # bmin = X$estimate - (2 * X$quasiSE)
      bmax = X$estimate +  X$quasiSE
      bmin = X$estimate -  X$quasiSE
      
      items <- items
    })
    return(X)
  })
  
  # Add node information and number of observations
  for (i in seq_along(node_id)) {
    
    coeffs[[i]] <- within(coeffs[[i]], {
      
      nobs <- nobs[[i]]
      
      node <- node_id[i]}
    )
    
  }
  
  coeffs <- do.call("rbind", coeffs)
  
  # if (isTRUE(qve)) {
  #   coeffs$bmin <- ifelse(coeffs$bmin < 0, 0, coeffs$bmin)
  #   coeffs$bmax <- ifelse(coeffs$bmax > 1, 1, coeffs$bmax)
  # }
  
  coeffs$id <- paste0(coeffs$node, "_", coeffs$items)
  
  if (isTRUE(add.letters)) {
    
    # try to compute the estimates and get the letters
    # sometimes it doesn't work, if it happens then return 
    # a message about the issue
    if (is.null(threshold)) {
      threshold <- 0.05
    }
    if (is.null(ref)) {
      ref <- 1
    }
    if (is.null(adjust)) {
      adjust <- "none"
    }
    
    groups <- tryCatch(
      {
        multcompPL(object, 
                   threshold = threshold,
                   terms = terms,
                   ref = ref,
                   adjust = adjust)
        
      }, error = function(cond){
        message("Unable to get letters for the plotting object.\n")
        return(NA)
      }
    )
    
    if (isTRUE(is.na(groups))) {
      coeffs <- cbind(coeffs, groups = "")
    }else{
      groups$id <- paste0(groups$node, "_", groups$term)
      coeffs <- merge(coeffs, groups[,c("id","group")], by = "id", all.x = TRUE)
      names(coeffs)[names(coeffs)=="group"] <- "groups"
    }
  }
  
  if (isFALSE(add.letters)) {
    
    coeffs$groups <- ""
    
  }
  
  node_lev <- unique(paste0("Node ", coeffs$node, " (n = ", coeffs$nobs, ")"))
  
  coeffs$id <- coeffs$node
  
  coeffs$node <- factor(paste0("Node ", coeffs$node, " (n = ", coeffs$nobs, ")"),
                        levels = node_lev)
  
  coeffs$items <- factor(coeffs$items, levels = rev(sort(items)))
  
  splitvar <- 0L
  p.value <- 0L
  id <- 0L
  estimate <- 0L
  bmin <- 0L
  bmax <- 0L
  
  # rls <- partykit:::.list.rules.party(plt_01,4)
  # ggparty:::add_splitvar_breaks_index()
  #From: https://stackoverflow.com/questions/60553985/number-of-decimal-places-on-edges-of-a-decision-tree-plot-with-ggparty
  rnd_labels <- add_splitvar_breaks_index_new(party_object = object,
                                                  plot_data = ggparty:::get_plot_data(object), 
                                                  round_digits = 3)
  
  #str(rls[1])
  
  # get the tree structure
  if (length(node_id) > 1) {
    tree <- 
      ggparty::ggparty(object, terminal_space = 0) +
      ggparty::geom_edge() +
      ggparty::geom_edge_label(mapping = aes(label = unlist(rnd_labels)), size = 6) +
      ggplot2::theme(legend.position = "none") +
      ggparty::geom_node_label(line_list = list(
        aes(label = splitvar),
        aes(label = paste("p =", round(p.value, 3))),
        # aes(label = paste("p =",
        #                   formatC(p.value,
        #                           format = "e",
        #                           digits = 1))),
        aes(label = ""),
        aes(label = paste("Node " , id))),
        line_gpar = list(list(size = 12,  fontface = "bold"),
                         list(size = 12),
                         list(size = 12),
                         list(size = 12,
                              col = "black",
                              #fontface = "bold",
                              alignment = "center")
        ),
        ids = "inner") +
      ggplot2::coord_cartesian(ylim = c(0.1, 1.1))
  }
  
  # Get max and min values for the x axis in the plot
  #xmax <- round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  xmax <- round(max(coeffs$bmax, na.rm = TRUE), digits = 4)
  xinter <- 0
  xmin <- min(coeffs$bmin, na.rm = TRUE)
  xbreaks <- round(c(mean(c(xmin, xmax)), xmax), 2)
  xbreaks <- c(xmin, xbreaks)
  
  message(paste("xmin", xmin))
  message(paste("xmax", xmax))
  message(paste("xbreaks", xbreaks))
  
  # if(isFALSE(log)) {
  #   xmin <- 0
  #   xinter <- 1/length(items)
  #   xbreaks <- round(c(mean(c(0, xmax)), xmax), 2)
  #   xbreaks <- c(0, xbreaks)
  #   
  # }else{
    # xinter <- 0
    # xmin <- min(coeffs$bmin, na.rm = TRUE)
    # xbreaks <- round(c(mean(c(xmin, xmax)), xmax), 2)
    # xbreaks <- c(xmin, xbreaks)
    # message(paste("xmin", xmin))
    # message(paste("xmax", xmax))
    # message(paste("xbreaks", xbreaks))
  # }
  
  xlabs <- as.character(round(xbreaks, 2))
  
  # Check font size for axis X and Y, and plot title
  s.axis <- 13
  
  #Add colors to genotypes names by type (released vs experimental)
  node_2_coef <- coeffs[coeffs$id == 2, ]
  
  exp_list <- c("BCR 122-74", "BFS 47", "BRT 103-182", 
                "MHC 2-13-49", "MHR 311-17", "RRH 336-28",
                "RS 907-28", "RS 909-35", "SRS2-36-34")
  
  #gen_pal_txt <- ifelse(items %in% exp_list, "#ca0020", "gray25")
  gen_pal_txt <- ifelse(items %in% exp_list, "#ca0020", "gray25")
  
  
  
  gen_pal_txt <- gen_pal_txt[order(node_2_coef$items)]
  
  ######----------------#####
  
  p <- 
    ggplot2::ggplot(coeffs, 
                    ggplot2::aes(x = estimate, 
                                 y = items)) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = xinter, 
                        color = "darkgray", size = 0.7) +
    # geom_text(aes(label = groups),
    #           position = position_nudge(y = nudge.y, x = nudge.x)) +
    ggplot2::geom_point(pch = 18, size = 2.5, 
                        fill = "grey20", color = "grey20") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin,
                                         xmax = bmax),
                            color= "grey20", height = 0.1) +
    # ggplot2::scale_x_continuous(limits = c(xmin, xmax),
    #                             breaks = xbreaks,
    #                             labels = xlabs) +
    ggplot2::facet_grid(. ~ node) +
    
    theme(axis.text.y = element_text(size = 40))+
    ggplot2::labs(x = "log(worth)", y = "") +
    ggplot2::theme(axis.text.x = element_text(size = s.axis, angle = 0,
                                                       hjust = 0.5, vjust = 1, 
                                                       face = "plain",
                                                       color = "black"),
                   axis.text.y = element_text(size = s.axis, angle = 0,
                                                       hjust = 1, vjust = 0.5, 
                                                       face = "bold",
                                                       color = gen_pal_txt),
                   text = element_text(size = letter.size),
                   strip.background = element_blank(),
                   #plot.background = ggplot2::element_blank(),
                   #panel.grid = element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.border = element_rect(color = "gray25", size = 1),
                   axis.ticks = element_line(color = "black", size = 0.5),
                   #axis.ticks.length = grid::unit(0.3, "cm")
                   )
  
  if(length(node_id) > 1){
    p <- (tree / p)
  }
  return(p)
}
