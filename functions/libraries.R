    ## contains the libraries the workflow is dependent on ##

#contains the data:
library(palmerpenguins)

#for making plots:
library(ggplot2)

#for data cleaning:
suppressPackageStartupMessages(library(janitor))

#for data manipulation and piping:
suppressPackageStartupMessages(library(dplyr)) 

#for saving figures a png:
library(ragg)

#for saving figures as vectors:
library(svglite)

#for using a markdown file:
library(rmarkdown)

# for running the White test:
library(lmtest)
