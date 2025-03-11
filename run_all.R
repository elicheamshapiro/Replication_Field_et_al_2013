# Clear the environment before running
rm(list = ls())

# Set working directory (Replace this directory with your own)
setwd("/Users/elishapiro/Documents/GitHub/Replication_Field_et_al_2013/")
#Joanna
#setwd("/Users/nowinski/Desktop/DEV/Replication/Publication-Data-and-Do-Files")

#Intall and load required packages
#install.packages("haven")
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("stargazer")
#install.packages("modelsummary")
#install.packages("broom")
# install.packages("readr")
# install.packages("xtable")
library(readr)
library(xtable)
library(haven)
library(tidyverse)
library(tidyr)
library(sandwich)
library(lmtest)
library(stargazer)
library(modelsummary)
library(broom)

# Read the Stata file
df <- read_dta("./data/Grace-Period-Data.dta")

# Print start message
cat("Running Replication...\n")

# List of R scripts to run
scripts <- c(
  "Figure_1.R",
  "Figure_2.R",
  "Figure_3.R",
  "Figure_4.R",
  "Table_1.R",
  "Table_3.R",
  "Tables_to_latex.R"
)

# Loop through and run each script
for (script in scripts) {
  cat("Running:", script, "\n")  # Print which script is running
  source(script, echo = TRUE)  # Run the script and print its output
}

# Print completion message
cat("Replication Complete\n")