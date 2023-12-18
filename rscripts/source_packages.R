mylibraries <- c("dplyr", "vegan", "ggplot2", "readr", "ggcorrplot", "sf")

for (i in 1:length(mylibraries)) {
  if(mylibraries[i] %in% rownames(installed.packages()) == FALSE) {install.packages(mylibraries[i])}
}
lapply(mylibraries, require, character.only = TRUE)
