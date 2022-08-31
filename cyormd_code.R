# code copied from the .rmd file

# packages
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", 
                                      repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", 
                                      repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", 
                                       repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", 
                                         repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", 
                                     repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", 
                                     repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", 
                                          repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", 
                                      repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(readxl)
library(viridis)
library(MLmetrics)
library(class)
library(rpart)
library(rpart.plot)
library(ranger)

# data_download
uci_url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip'

download.file(uci_url, 'DryBeanDataset.zip')

# data_unzip
unzip('DryBeanDataset.zip')

# data_read
# specify path
path <- paste0(getwd(), '/DryBeanDataset/Dry_Bean_Dataset.xlsx')
# read the first sheet (where the data is)
beans <- read_xlsx(path, sheet = 1)

# data_overview
# overview of the data's size and format
str(beans)

