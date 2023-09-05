library(testthat, quietly = TRUE)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

source("solution.R") %>%
  suppressMessages() %>%
  suppressPackageStartupMessages()
test_dir("tests/testthat")

# In GitHub Classroom run this:
# sudo apt-get update; sudo apt-get install -y r-base-core r-cran-tidyverse r-cran-testthat; sudo Rscript -e 'install.packages("TSP")'
# Rscript tests/test.R
# 
# 
# Individual tests
# 
# sudo apt-get update; sudo apt-get install -y r-base-core r-cran-tidyverse r-cran-testthat; sudo Rscript -e 'install.packages("TSP")'
# Rscript -e 'library(testthat); suppressPackageStartupMessages(source("solution.R")); test_file("tests/testthat/test-1) Data preparation.R", reporter = ProgressReporter, stop_on_failure = TRUE)'