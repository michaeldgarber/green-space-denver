#The purpose of this is to set the working directory to the docs folder
#so when I execute code in the console, it occurs there.
#configure wd for web dev.capabilities
library(here)
setwd(here("docs"))

rmarkdown::render_site()

#Must knit the analyses file you care about separately from this render_site function