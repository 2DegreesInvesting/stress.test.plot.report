PROJECT_DIRECTORY <-  getwd()

for (r2dii_pkg in c("r2dii.colours", "r2dii.plot", "r2dii.utils", "r2dii.climate.stress.test")) {
  if (!(r2dii_pkg %in% installed.packages()[,"Package"])) {
    install.packages(file.path(PROJECT_DIRECTORY, "packages", r2dii_pkg),
                     repos = NULL,
                     type = "source",
                     character.only = TRUE)
  }
}

requirements <- c("ggplot2", "dplyr", "tidyverse", "scales", "cowplot", "shiny", "slickR", "svglite", "shinyWidgets"
)
for (pkg in requirements) {
  if (!(pkg %in% installed.packages()[,"Package"])) {
    install.packages(pkg, character.only = TRUE)
  }

}
