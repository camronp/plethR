#notes
#for package installation dependencies, use this: usethis::use_package("ggplot2")

packages <- c("devtools", "roxygen2", "ggrepel", "ggforce", "tidyverse", "readxl", "dplyr", "ggplot2", "zoo", "pheatmap", "extrafont", "RColorBrewer")

lapply(packages, library, character.only = TRUE)


#Next functions should be getting into the machine learning
#also make a function that exports the data into format easy for prism, possibly even a prism file

#updating the package
devtools::load_all()
devtools::document() #run to ensure everything updates
devtools::check() #define any pre-loaded issues
devtools::clean() #clean previous builds
devtools:: build() #rebuild package

#push everything that has been changed after this
