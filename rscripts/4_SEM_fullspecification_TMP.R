
workdir <- getwd()
source("./rscripts/source_packages.R")

##load data grid scale
environment <- read.csv("./data/grids_environmental_variables.csv" )
birds_functional_measures<- read.csv("./output/functional_measures_transects.csv")

## standardize values
environment <- environment %>% 
  mutate(imperv_std = scale(imperv),
         popden_std = scale(pop_den),
         tcd_std = scale(tcd),
         light_std = scale(light),
         noise_std = scale(noise),
         vegpca1_std = scale(veg_pca1),
         vegpca2_std = scale(veg_pca2),
         ndvi_std = scale(ndvi),
         glcm_std = scale(glcm), 
         glcmhmg_std = scale(glcm_hmg)) %>% 
  dplyr::select(transect, imperv_std, popden_std, tcd_std, light_std, noise_std, 
                vegpca1_std, vegpca2_std, ndvi_std, glcm_std, glcmhmg_std) %>% 
  as.data.frame()


birds_functional_measures <- birds_functional_measures%>%
  mutate(birds_fr_std = scale(birds_fr),
         birds_fe_std = scale(birds_fe),
         birds_fd_std = scale(birds_fd))%>%
  dplyr::select(transect, birds_fr_std, birds_fe_std, birds_fd_std) %>% 
  as.data.frame()
head(birds_functional_measures)
summary(birds_functional_measures)
head(environment)

##put all data in one table
env_functional_data<- left_join(environment, birds_functional_measures, by = "transect")
summary(env_functional_data)
colnames(env_functional_data)
str(env_functional_data)

## Effect of pca1, pca2, glcm, noise and pop on bird fe
# effects of noise and pop will be both direct and indirect, through the veg structe

###########################################
#FUNCTIONAL EVENNESS

sem.fe.grid.model <-sem(
  ' # direct effects
  birds_fe_std ~ vegpca1_std + vegpca2_std + glcmhmg_std + popden_std + noise_std
  
  # mediators
  vegpca1_std ~ popden_std
  vegpca1_std ~ noise_std
  vegpca2_std ~ popden_std
  vegpca2_std ~ noise_std
  glcmhmg_std ~ popden_std
  glcmhmg_std ~ noise_std
  
  # variances of exogenous variables
  noise_std ~~ noise_std
  popden_std ~~ popden_std
  vegpca1_std ~~ vegpca1_std
  vegpca2_std ~~ vegpca2_std
  glcmhmg_std ~~ glcmhmg_std
  
  # covariance of exogenouse variables
  noise_std ~~ popden_std
  vegpca1_std ~~ glcmhmg_std
  vegpca2_std ~~ glcmhmg_std
  ')

fit_fe_grid <- lavaan::sem(sem.fe.grid.model, env_functional_data)
summary(fit_fe_grid,
        rsquare = T,
        standardized = T,
        fit.measures = T)
# saveRDS(fit_fe_grid, "./output/SEM_fe_gridscale_model.RDS")

parameterestimates(fit_fe_grid, standardized = T)
fitted(fit_fe_grid)
residuals(fit_fe_grid, "cor")
fitmeasures(fit_fe_grid)
modificationindices(fit_fe_grid, sort. = T)
semPlot:: semPaths(fit_fe_grid, "par", edge.label.cex = 1.2, fade = FALSE)
semPlot:: semPaths(fit_fe_grid, whatLabels = "std", layout = "tree")

semPlot::semPaths(fit_fe_grid,"std",'est', curveAdjacent = TRUE, style = "lisrel")

## fit of the model
cfi_model <- fitmeasures(fit_fe_grid)["cfi"]
cfi_model
# 0.9768607 


# extract parameter values
table_fe_grid <- parameterEstimates(fit_fe_grid,standardized=TRUE)  %>%  head(11)
# write.csv(table_fe_grid, "./output/SEM_fe_gridscale_parameterTable.csv", row.names = FALSE)

# get p values in text
b <- paste0(round(table_fe_grid$std.all, 3), "\np = ", round(table_fe_grid$pvalue, 3))
png("./output/SEM_fe_gridscale_plot.png")
semPaths(
  fit_fe_grid,
  edgeLabels = b,
  ask = FALSE, title = FALSE,
  what = 'std',
  whatLabels = 'std',
  style = 'ram',
  layout = 'tree',
  intercepts = FALSE,
  residuals = FALSE, 
  edge.label.cex = 1.2)
dev.off()


####################################
# FUNCTIONAL RICHNESS

sem.fr.grid.model <-sem(
  ' # direct effects
  birds_fr_std ~ vegpca1_std + vegpca2_std + glcmhmg_std + popden_std + noise_std
  
  # mediators
  vegpca1_std ~ popden_std
  vegpca1_std ~ noise_std
  vegpca2_std ~ popden_std
  vegpca2_std ~ noise_std
  glcmhmg_std ~ popden_std
  glcmhmg_std ~ noise_std
  
  # variances of exogenous variables
  noise_std ~~ noise_std
  popden_std ~~ popden_std
  vegpca1_std ~~ vegpca1_std
  vegpca2_std ~~ vegpca2_std
  glcmhmg_std ~~ glcmhmg_std
  
  # covariance of exogenouse variables
  noise_std ~~ popden_std
  vegpca1_std ~~ glcmhmg_std
  vegpca2_std ~~ glcmhmg_std
  ')

fit_fr_grid <- lavaan::sem(sem.fr.grid.model, env_functional_data)
summary(fit_fr_grid,
        rsquare = T,
        standardized = T,
        fit.measures = T)
# saveRDS(fit_fr_grid, "./output/SEM_fr_gridscale_model.RDS")


## fit of the model
cfi_model <- fitmeasures(fit_fr_grid)["cfi"]
cfi_model
# 0.9734063     

# extract parameter values
table_fr_grid <- parameterEstimates(fit_fr_grid, standardized=TRUE)  %>%  head(11)
# write.csv(table_fr_grid, "./output/SEM_fr_gridscale_parameterTable.csv", row.names = FALSE)

# get p values in text
b2 <- paste0(round(table_fr_grid$std.all, 3), "\np = ", round(table_fr_grid$pvalue, 3))
png("./output/SEM_fr_gridscale_plot.png")
semPaths(
  fit_fr_grid,
  edgeLabels = b2,
  ask = FALSE, title = FALSE,
  what = 'std',
  whatLabels = 'std',
  style = 'ram',
  layout = 'tree',
  intercepts = FALSE,
  residuals = FALSE, 
  edge.label.cex = 1.2)
dev.off()

####################################
# FUNCTIONAL DIVERGENCE
sem.fd.grid.model <- sem(
  ' # direct effects
  birds_fd_std ~ vegpca1_std + vegpca2_std + glcmhmg_std + popden_std + noise_std
  
  # mediators
  vegpca1_std ~ popden_std
  vegpca1_std ~ noise_std
  vegpca2_std ~ popden_std
  vegpca2_std ~ noise_std
  glcmhmg_std ~ popden_std
  glcmhmg_std ~ noise_std
  
  # variances of exogenous variables
  noise_std ~~ noise_std
  popden_std ~~ popden_std
  vegpca1_std ~~ vegpca1_std
  vegpca2_std ~~ vegpca2_std
  glcmhmg_std ~~ glcmhmg_std
  
  # covariance of exogenouse variables
  noise_std ~~ popden_std
  vegpca1_std ~~ glcmhmg_std
  vegpca2_std ~~ glcmhmg_std
  ')

fit_fd_grid <- lavaan::sem(sem.fd.grid.model, env_functional_data)
summary(fit_fd_grid,
        rsquare = T,
        standardized = T,
        fit.measures = T)
# saveRDS(fit_fd_grid, "./output/SEM_fd_gridscale_model.RDS")

## fit of the model
cfi_model <- fitmeasures(fit_fd_grid)["cfi"]
cfi_model
# 0.975681   


# extract parameter values
table_fd_grid <- parameterEstimates(fit_fd_grid, standardized=TRUE)  %>%  head(11)
# write.csv(table_fd_grid, "./output/SEM_fd_gridscale_parameterTable.csv", row.names = FALSE)

# get p values in text
b3 <- paste0(round(table_fd_grid$std.all, 3), "\np = ", round(table_fd_grid$pvalue, 3))
png("./output/SEM_fd_gridscale_plot.png")
semPaths(
  fit_fd_grid,
  edgeLabels = b3,
  ask = FALSE, title = FALSE,
  what = 'std',
  whatLabels = 'std',
  style = 'ram',
  layout = 'tree',
  intercepts = FALSE,
  residuals = FALSE, 
  edge.label.cex = 1.2)
dev.off()




#############################
### model for territories ###
#############################

terr_values <- read.csv(paste0(workdir, "/data/territories_points_environmental_variables.csv"))
head(terr_values)

terr_mean_values <- terr_values %>% 
  dplyr::select(-nvisits, -visit) %>% 
  group_by(transect) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
terr_mean_values <- terr_mean_values %>% 
  as.data.frame() %>% 
  mutate(imperv_std = scale(imperv),
         popden_std = scale(pop_den),
         tcd_std = scale(tcd),
         light_std = scale(light),
         noise_std = scale(noise),
         vegpca1_std = scale(veg_pca1),
         vegpca2_std = scale(veg_pca2),
         ndvi_std = scale(ndvi),
         glcm_std = scale(glcm), 
         glcmhmg_std = scale(glcm_hmg)) %>% 
  dplyr::select(transect, imperv_std, popden_std, tcd_std, light_std, noise_std, 
                vegpca1_std, vegpca2_std, ndvi_std, glcm_std, glcmhmg_std)
   

summary(terr_mean_values)
#join in one table with functional diversity measures
terr_data <- left_join(terr_mean_values, birds_functional_measures, by = "transect")
str(terr_data)

###############################
##fe
sem.fe.terrpts.model <-sem(
  ' # direct effects
  birds_fe_std ~ vegpca1_std + vegpca2_std + glcmhmg_std + popden_std + noise_std
  
  # mediators
  vegpca1_std ~ popden_std
  vegpca1_std ~ noise_std
  vegpca2_std ~ popden_std
  vegpca2_std ~ noise_std
  glcmhmg_std ~ popden_std
  glcmhmg_std ~ noise_std
  
  # variances of exogenous variables
  noise_std ~~ noise_std
  popden_std ~~ popden_std
  vegpca1_std ~~ vegpca1_std
  vegpca2_std ~~ vegpca2_std
  glcmhmg_std ~~ glcmhmg_std
  
  # covariance of exogenouse variables
  noise_std ~~ popden_std
  vegpca1_std ~~ glcmhmg_std
  vegpca2_std ~~ glcmhmg_std
  ')

fit_fe_terrpts <- lavaan::sem(sem.fe.terrpts.model, terr_data)
summary(fit_fe_terrpts,
        rsquare = T,
        standardized = T,
        fit.measures = T)
# saveRDS(fit_fe_terrpts, "./output/SEM_fe_terrptsscale_model.RDS")
cfi_model <- fitmeasures(fit_fe_terrpts)["cfi"]
cfi_model
# 0.9784498  

# extract parameter values
table_fe_terrpts <- parameterEstimates(fit_fe_terrpts, standardized=TRUE)  %>%  head(11)
# write.csv(table_fe_terrpts, "./output/SEM_fe_terrptsscale_parameterTable.csv", row.names = FALSE)

# get p values in text
bt1 <- paste0(round(table_fe_terrpts$std.all, 3), "\np = ", round(table_fe_terrpts$pvalue, 3))
png("./output/SEM_fe_terrptsscale_plot.png")
semPaths(
  fit_fe_terrpts,
  edgeLabels = bt1,
  ask = FALSE, title = FALSE,
  what = 'std',
  whatLabels = 'std',
  style = 'ram',
  layout = 'tree',
  intercepts = FALSE,
  residuals = FALSE, 
  edge.label.cex = 1.2)
dev.off()


###############################
##fr
sem.fr.terrpts.model <-sem(
  ' # direct effects
  birds_fr_std ~ vegpca1_std + vegpca2_std + glcmhmg_std + popden_std + noise_std
  
  # mediators
  vegpca1_std ~ popden_std
  vegpca1_std ~ noise_std
  vegpca2_std ~ popden_std
  vegpca2_std ~ noise_std
  glcmhmg_std ~ popden_std
  glcmhmg_std ~ noise_std
  
  # variances of exogenous variables
  noise_std ~~ noise_std
  popden_std ~~ popden_std
  vegpca1_std ~~ vegpca1_std
  vegpca2_std ~~ vegpca2_std
  glcmhmg_std ~~ glcmhmg_std
  
  # covariance of exogenouse variables
  noise_std ~~ popden_std
  vegpca1_std ~~ glcmhmg_std
  vegpca2_std ~~ glcmhmg_std
  ')

fit_fr_terrpts <- lavaan::sem(sem.fr.terrpts.model, terr_data)
summary(fit_fr_terrpts,
        rsquare = T,
        standardized = T,
        fit.measures = T)
# saveRDS(fit_fr_terrpts, "./output/SEM_fr_terrptsscale_model.RDS")

cfi_model <- fitmeasures(fit_fr_terrpts)["cfi"]
cfi_model
# 0.9747611  

# extract parameter values
table_fr_terrpts <- parameterEstimates(fit_fr_terrpts, standardized=TRUE)  %>%  head(11)
# write.csv(table_fr_terrpts, "./output/SEM_fr_terrptsscale_parameterTable.csv", row.names = FALSE)

# get p values in text
bt2 <- paste0(round(table_fr_terrpts$std.all, 3), "\np = ", round(table_fr_terrpts$pvalue, 3))
png("./output/SEM_fr_terrptsscale_plot.png")
semPaths(
  fit_fr_terrpts,
  edgeLabels = bt2,
  ask = FALSE, title = FALSE,
  what = 'std',
  whatLabels = 'std',
  style = 'ram',
  layout = 'tree',
  intercepts = FALSE,
  residuals = FALSE, 
  edge.label.cex = 1.2)
dev.off()


###############################
##fd
sem.fd.terrpts.model <-sem(
  ' # direct effects
  birds_fd_std ~ vegpca1_std + vegpca2_std + glcmhmg_std + popden_std + noise_std
  
  # mediators
  vegpca1_std ~ popden_std
  vegpca1_std ~ noise_std
  vegpca2_std ~ popden_std
  vegpca2_std ~ noise_std
  glcmhmg_std ~ popden_std
  glcmhmg_std ~ noise_std
  
  # variances of exogenous variables
  noise_std ~~ noise_std
  popden_std ~~ popden_std
  vegpca1_std ~~ vegpca1_std
  vegpca2_std ~~ vegpca2_std
  glcmhmg_std ~~ glcmhmg_std
  
  # covariance of exogenouse variables
  noise_std ~~ popden_std
  vegpca1_std ~~ glcmhmg_std
  vegpca2_std ~~ glcmhmg_std
  ')

fit_fd_terrpts <- lavaan::sem(sem.fd.terrpts.model, terr_data)
summary(fit_fd_terrpts,
        rsquare = T,
        standardized = T,
        fit.measures = T)
# saveRDS(fit_fd_terrpts, "./output/SEM_fd_terrptsscale_model.RDS")

cfi_model <- fitmeasures(fit_fd_terrpts)["cfi"]
cfi_model
# 0.9775899 

# extract parameter values
table_fd_terrpts <- parameterEstimates(fit_fd_terrpts, standardized=TRUE)  %>%  head(11)
# write.csv(table_fd_terrpts, "./output/SEM_fd_terrptsscale_parameterTable.csv", row.names = FALSE)

# get p values in text
bt3 <- paste0(round(table_fd_terrpts$std.all, 3), "\np = ", round(table_fd_terrpts$pvalue, 3))
png("./output/SEM_fd_terrptsscale_plot.png")
semPaths(
  fit_fd_terrpts,
  edgeLabels = bt3,
  ask = FALSE, title = FALSE,
  what = 'std',
  whatLabels = 'std',
  style = 'ram',
  layout = 'tree',
  intercepts = FALSE,
  residuals = FALSE, 
  edge.label.cex = 1.2)
dev.off()

