######################
### PCoA FUNCTIONS ###
######################

# Function to run PCoA and extract info
mypcoa_toplot <- function(species_matrix, env_matrix, traits_matrix){
  mypcoa <- capscale(species_matrix ~ 1, dist="bray", metaMDS = TRUE)
  ## add environmetal variables
  ef <- envfit(mypcoa, env_matrix, permu = 999, na.rm = T)
  ## add traits 
  tq <- envfit(mypcoa, traits_matrix, permu = 999)
  ## add species 
  spf <- envfit(mypcoa, species_matrix, permu = 999)
  
  ## data to plot
  ## significant effects
  sign_sp <- names(spf$vectors$pvals[spf$vectors$pvals < 0.05]) # get names of the significant sp
  species_scores <- spf$vectors$arrows %>%
    as.data.frame() %>%
    rownames_to_column("species") 
  species_scores <- filter(species_scores, species %in% sign_sp)
  
  ## get sites loads in axis
  site_scores <- as.data.frame(scores(mypcoa)$sites)
  
  ## get coordinates of environmental variables
  env_coord_cont <- as.data.frame(scores(ef, "vectors")) * ordiArrowMul(ef)
  env_variables <- colnames(env_matrix)
  rownames(env_coord_cont) <- env_variables
  
  ## remove ndvi because overlaps almost perfectly with glcm
  rowtoremove <- grep("ndvi", rownames(env_coord_cont))
  env_coord_cont <- env_coord_cont[-rowtoremove,] 
  
  ## get coordinates of traits 
  traits_coord_cat <- as.data.frame(scores(tq, "factors"))
  names_cat <- rownames(traits_coord_cat)
  names_cat <- gsub("diet_cat", "", names_cat)
  rownames(traits_coord_cat) <- names_cat
  # traits_coord_cat
  
  traits_coord_cont <- as.data.frame(scores(tq, "vectors")) * ordiArrowMul(tq)
  rownames(traits_coord_cont) <- c("Migrant", "BodyMass", "Longevity", "Clutch_size")
  # traits_coord_cont
  
  output <- list(site_scores, env_coord_cont, traits_coord_cat, traits_coord_cont)
  
  names(output) <- c("site_scores", "env_coord_cont", "traits_coord_cat", "traits_coord_cont")
  return(output)
}


# Function to plot pcoa
ggplotting_pcoa <- function(site_scores, env_coord_cont, traits_coord_cat, traits_coord_cont, 
                            mytitle, myxlim = c(-1.5, 1.5), myylim = c(-1.5, 1.5)){
  myplot <- ggplot(data = site_scores, aes(x = MDS1, y = MDS2)) +
    geom_point(colour = "orange",
               size = 5,
               alpha = 0.3) +
    geom_segment(
      data = env_coord_cont,
      aes(
        x = 0,
        y = 0,
        xend = MDS1,
        yend = MDS2
      ),
      linewidth = 1,
      alpha = 0.5,
      colour = "darkgreen",
      arrow = arrow(length = unit(0.3, "cm"))
    ) +
    geom_text(
      data = env_coord_cont,
      aes(x = MDS1 * 1.2, y = MDS2 * 1.2),
      size = 4,
      colour = "darkgreen",
      fontface = "bold",
      label = row.names(env_coord_cont)
    ) +
    geom_segment(
      data = traits_coord_cat,
      aes(
        x = 0,
        y = 0,
        xend = MDS1,
        yend = MDS2
      ),
      linewidth = 1,
      alpha = 0.5,
      colour = "black",
      arrow = arrow(length = unit(0.3, "cm"))
    ) +
    geom_text(
      data = traits_coord_cat,
      aes(x = MDS1 * 1.2, y = MDS2 * 1.2),
      size = 4,
      colour = "black",
      fontface = "bold",
      label = row.names(traits_coord_cat)
    ) +
    geom_segment(
      data = traits_coord_cont,
      aes(
        x = 0,
        y = 0,
        xend = MDS1,
        yend = MDS2
      ),
      linewidth = 1,
      alpha = 0.5,
      colour = "black",
      arrow = arrow(length = unit(0.3, "cm"))
    ) +
    geom_text(
      data = traits_coord_cont,
      aes(x = MDS1 * 1.2, y = MDS2 * 1.2),
      size = 4,
      colour = "black",
      fontface = "bold",
      label = row.names(traits_coord_cont)
    ) +
    geom_hline(yintercept = 0,
               col = "grey70",
               lty = 4) +
    geom_vline(xintercept = 0,
               col = "grey70",
               lty = 4) +
    lims(x = myxlim, y = myylim) +
    # xlim(c(-2.5, 2.5)) +
    # ylim(c(-2, 2)) +
    ggtitle(mytitle) +
    theme(
      axis.title = element_text(
        size = 14,
        face = "bold",
        colour = "black"
      ),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black"),
      axis.text = element_text(size = 14, colour = "black"))
  return(myplot)
}

# test <- mypcoa_toplot(species_matrix = species_matrix, 
#                       env_matrix = env_matrix, 
#                       traits_matrix = traits_matrix)
# 
# plot_test <- ggplotting_pcoa(site_scores = test$site_scores,
#                              env_coord_cont = test$env_coord_cont,
#                              traits_coord_cat = test$traits_coord_cat,
#                              traits_coord_cont = test$traits_coord_cont,
#                              mytitle = "test")
# plot_test
