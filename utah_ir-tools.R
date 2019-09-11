# utah ir package
library(devtools)

devtools::install_github("ut-ir-tools/irTools")

# automatically installs wqTools but just in case here is code
#devtools::install_github("ut-ir-tools/wqTools")


library(irTools)

# lake profile dashboard
shiny::runApp()
