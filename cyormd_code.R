# code copied from the .rmd file

# installing packages--------------------------------------------------
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

# Loading data--------------------------------------------------
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

# na_check
which(is.na(beans))

# num_classes
# checking the distribution of classes (bean types) in the dataset
beans %>% group_by(Class) %>%
  summarize(count = n())

# Boxplots--------------------------------------------------
# Area_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(Area > (quantile(Area, 0.75)) + 1.5 * IQR(Area) | 
           Area < (quantile(Area, 0.25)) - 1.5 * IQR(Area))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, Area), y = Area)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Areas')

# Perimeter_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(Perimeter > (quantile(Perimeter, 0.75)) + 1.5 * IQR(Perimeter) | 
           Perimeter < (quantile(Perimeter, 0.25)) - 1.5 * IQR(Perimeter))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, Perimeter), y = Perimeter)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Perimeters')

# MajorAxisLength_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(MajorAxisLength > (quantile(MajorAxisLength, 0.75)) + 1.5 * IQR(MajorAxisLength) | 
           MajorAxisLength < (quantile(MajorAxisLength, 0.25)) - 1.5 * IQR(MajorAxisLength))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, MajorAxisLength), y = MajorAxisLength)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Major Axis Lengths')

# MinorAxisLength_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(MinorAxisLength > (quantile(MinorAxisLength, 0.75)) + 1.5 * IQR(MinorAxisLength) | 
           MinorAxisLength < (quantile(MinorAxisLength, 0.25)) - 1.5 * IQR(MinorAxisLength))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, MinorAxisLength), y = MinorAxisLength)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Minor Axis Lengths')

# AspectRation_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(AspectRation > (quantile(AspectRation, 0.75)) + 1.5 * IQR(AspectRation) | 
           AspectRation < (quantile(AspectRation, 0.25)) - 1.5 * IQR(AspectRation))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, AspectRation), y =AspectRation)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Aspect Ratios')

# Eccentricity_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(Eccentricity > (quantile(Eccentricity, 0.75)) + 1.5 * IQR(Eccentricity) | 
           Eccentricity < (quantile(Eccentricity, 0.25)) - 1.5 * IQR(Eccentricity))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, Eccentricity), y = Eccentricity)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Eccentricities')

# ConvexArea_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(ConvexArea > (quantile(ConvexArea, 0.75)) + 1.5 * IQR(ConvexArea) | 
           ConvexArea < (quantile(ConvexArea, 0.25)) - 1.5 * IQR(ConvexArea))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, ConvexArea), y = ConvexArea)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Convex Areas')

# EquivDiameter_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(EquivDiameter > (quantile(EquivDiameter, 0.75)) + 1.5 * IQR(EquivDiameter) | 
           EquivDiameter < (quantile(EquivDiameter, 0.25)) - 1.5 * IQR(EquivDiameter))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, EquivDiameter), y = EquivDiameter)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Equivalent Diameters')

# Extent_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(Extent > (quantile(Extent, 0.75)) + 1.5 * IQR(Extent) | 
           Extent < (quantile(Extent, 0.25)) - 1.5 * IQR(Extent))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, Extent), y = Extent)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Extents')

# Solidity_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(Solidity > (quantile(Solidity, 0.75)) + 1.5 * IQR(Solidity) | 
           Solidity < (quantile(Solidity, 0.25)) - 1.5 * IQR(Solidity))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, Solidity), y = Solidity)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Solidities')

# roundness_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(roundness > (quantile(roundness, 0.75)) + 1.5 * IQR(roundness) | 
           roundness < (quantile(roundness, 0.25)) - 1.5 * IQR(roundness))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, roundness), y = roundness)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Roundnesses')

# Compactness_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(Compactness > (quantile(Compactness, 0.75)) + 1.5 * IQR(Compactness) | 
           Compactness < (quantile(Compactness, 0.25)) - 1.5 * IQR(Compactness))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, Compactness), y = Compactness)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Compactness Values')

# ShapeFactor1_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(ShapeFactor1 > (quantile(ShapeFactor1, 0.75)) + 1.5 * IQR(ShapeFactor1) | 
           ShapeFactor1 < (quantile(ShapeFactor1, 0.25)) - 1.5 * IQR(ShapeFactor1))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, ShapeFactor1), y = ShapeFactor1)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Shape Factor 1')

# ShapeFactor2_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(ShapeFactor2 > (quantile(ShapeFactor2, 0.75)) + 1.5 * IQR(ShapeFactor2) | 
           ShapeFactor2 < (quantile(ShapeFactor2, 0.25)) - 1.5 * IQR(ShapeFactor2))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, ShapeFactor2), y = ShapeFactor2)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Shape Factor 2')

# ShapeFactor3_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(ShapeFactor3 > (quantile(ShapeFactor3, 0.75)) + 1.5 * IQR(ShapeFactor3) | 
           ShapeFactor3 < (quantile(ShapeFactor3, 0.25)) - 1.5 * IQR(ShapeFactor3))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, ShapeFactor3), y = ShapeFactor3)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Shape Factor 3')

# ShapeFactor4_boxplot
# separately mark outliers
outliers <-
  beans %>%
  # do the following for each Class
  group_by(Class) %>%
  # filter for outliers; whiskers are 1.5 times IQR, while boxes go from Q1 to Q3
  filter(ShapeFactor4 > (quantile(ShapeFactor4, 0.75)) + 1.5 * IQR(ShapeFactor4) | 
           ShapeFactor4 < (quantile(ShapeFactor4, 0.25)) - 1.5 * IQR(ShapeFactor4))

beans %>%
  # plot with jittering outliers
  ggplot(aes(x = reorder(Class, ShapeFactor4), y = ShapeFactor4)) +
  # outliers plotted separately in geom_point()
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(data = outliers) + 
  xlab('Class') +
  ggtitle('Distribution of Shape Factor 4')

# Trimming--------------------------------------------------
# trim_group0
# trim off group 0 ("don't bother") variables, which will not be considered further
beans <-
  subset(beans, select = -c(Extent, Solidity, ShapeFactor4))
# checking for right deletion
head(beans, 3)

#Scatterplots--------------------------------------------------
# Perim_AR
beans %>%
  # normalize variables: get the difference of each value from the minimum, then divide by the range
  # this process will also be used to train and test the algorithms
  mutate(perim_tf = (Perimeter - min(Perimeter)) / (max(Perimeter) - min(Perimeter)),
         ar_tf = (AspectRation - min(AspectRation)) / (max(AspectRation) - min(AspectRation))) %>%
  # plot with coloring by bean variety (Class)
  ggplot(aes(x = perim_tf, y = ar_tf, color = Class)) +
  geom_point(alpha = 0.6) +
  # using the viridis color scale for color-based accessibility
  scale_color_viridis(discrete = T) +
  xlab('Perimeter (Normalized)') +
  ylab('Aspect Ratio (Normalized)') +
  ggtitle('Normalized Perimeter and Aspect Ratio Combinations')

# Perim_Comp
beans %>%
  # normalize variables: get the difference of each value from the minimum, then divide by the range
  # this process will also be used to train and test the algorithms
  mutate(perim_tf = (Perimeter - min(Perimeter)) / (max(Perimeter) - min(Perimeter)),
         comp_tf = (Compactness - min(Compactness)) / (max(Compactness) - min(Compactness))) %>%
  # plot with coloring by bean variety (Class)
  ggplot(aes(x = perim_tf, y = comp_tf, color = Class)) +
  geom_point(alpha = 0.6) +
  # using the viridis color scale for color-based accessibility
  scale_color_viridis(discrete = T) +
  xlab('Perimeter (Normalized)') +
  ylab('Compactness (Normalized)') +
  ggtitle('Normalized Perimeter and Compactness Combinations')

# Perim_SF2
beans %>%
  # normalize variables: get the difference of each value from the minimum, then divide by the range
  # this process will also be used to train and test the algorithms
  mutate(perim_tf = (Perimeter - min(Perimeter)) / (max(Perimeter) - min(Perimeter)),
         sf2_tf = (ShapeFactor2 - min(ShapeFactor2)) / (max(ShapeFactor2) - min(ShapeFactor2))) %>%
  # plot with coloring by bean variety (Class)
  ggplot(aes(x = perim_tf, y = sf2_tf, color = Class)) +
  geom_point(alpha = 0.6) +
  # using the viridis color scale for color-based accessibility
  scale_color_viridis(discrete = T) +
  xlab('Perimeter (Normalized)') +
  ylab('Shape Factor 2 (Normalized)') +
  ggtitle('Normalized Perimeter and Shape Factor 2 Combinations')

# Perim_SF3
beans %>%
  # normalize variables: get the difference of each value from the minimum, then divide by the range
  # this process will also be used to train and test the algorithms
  mutate(perim_tf = (Perimeter - min(Perimeter)) / (max(Perimeter) - min(Perimeter)),
         sf3_tf = (ShapeFactor3 - min(ShapeFactor3)) / (max(ShapeFactor3) - min(ShapeFactor3))) %>%
  # plot with coloring by bean variety (Class)
  ggplot(aes(x = perim_tf, y = sf3_tf, color = Class)) +
  geom_point(alpha = 0.6) +
  # using the viridis color scale for color-based accessibility
  scale_color_viridis(discrete = T) +
  xlab('Perimeter (Normalized)') +
  ylab('Shape Factor 3 (Normalized)') +
  ggtitle('Normalized Perimeter and Shape Factor 3 Combinations')

# ED_AR
beans %>%
  # normalize variables: get the difference of each value from the minimum, then divide by the range
  # this process will also be used to train and test the algorithms
  mutate(ed_tf = (EquivDiameter - min(EquivDiameter)) / (max(EquivDiameter) - min(EquivDiameter)),
         ar_tf = (AspectRation - min(AspectRation)) / (max(AspectRation) - min(AspectRation))) %>%
  # plot with coloring by bean variety (Class)
  ggplot(aes(x = ed_tf, y = ar_tf, color = Class)) +
  geom_point(alpha = 0.6) +
  # using the viridis color scale for color-based accessibility
  scale_color_viridis(discrete = T) +
  xlab('Equivalent Diameter (Normalized)') +
  ylab('Aspect Ratio (Normalized)') +
  ggtitle('Normalized Equivalent Diameter and Aspect Ratio Combinations')

# Data format prep--------------------------------------------------
# x_y_split
beans_x <- subset(beans, select = -c(Class))
beans_y <- subset(beans, select = c(Class))

# x_as_matrix
beans_x <- as.matrix(beans_x)

# x_normalization
# for every column, apply the range formula onto each cell value:
# new value = (value - column min) / (column max - column min)
max_predictors <- apply(beans_x, 2, max) # column maxima
min_predictors <- apply(beans_x, 2, min) # column minima
colranges <- max_predictors - min_predictors # column ranges
beans_x <- sweep(beans_x, 2, STAT = min_predictors, FUN = '-') # subtract minima
beans_x <- sweep(beans_x, 2, STAT = colranges, FUN = '/') # divide by col ranges

# testing_matrix_norm_ED_AR
# reconstitute the data table
as.data.frame(beans_x) %>%
  mutate(Class = beans_y$Class) %>%
  # plot with coloring by bean variety (Class)
  ggplot(aes(x = EquivDiameter, y = AspectRation, color = Class)) +
  geom_point(alpha = 0.6) +
  # using the viridis color scale for color-based accessibility
  scale_color_viridis(discrete = T) +
  xlab('Equivalent Diameter (Normalized)') +
  ylab('Aspect Ratio (Normalized)') +
  ggtitle('Normalized Equivalent Diameter and Aspect Ratio Combinations')

# data_split
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# creating the validation indeces
val_index <-
  createDataPartition(y = beans$Class,
                      times = 1,
                      p = 0.2,
                      list = F)
# split the data
# as the x-y split had been made earlier, the train-val split must be made for both x and y
beans_x_train <- beans_x[-val_index,]
beans_y_train <- beans_y[-val_index,]
beans_x_val <- beans_x[val_index,]
beans_y_val <- beans_y[val_index,]

# val_set_dim
dim(beans_x_val)

# val_set_classes
as.data.frame(beans_y_val) %>%
  group_by(Class) %>%
  summarize(counts = n())

# train_set_rows
dim(beans_x_train)

# train_set_classes
as.data.frame(beans_y_train) %>%
  group_by(Class) %>%
  summarize(counts = n())

# custom_multiclass_f1
# this func should take same args as a function called defaultSummary()
# data = a dataframe with columns 'obs' and 'pred' (observed/actual and predicted)
macro_f1 <- function(data, lev = NULL, model = NULL) {
  mF1 <- mean(f1(data$pred, data$obs)) # arithmetic mean for macro F1
  names(mF1) <- 'mF1'# names the function output
  return(mF1)
}

f1 <- function(predicted, actual) {
  mat <- as.matrix(table(predicted, actual)) # should make a table where rows are pred, cols are actual/obs
  precision <- diag(mat) / rowSums(mat) # true pos / all predicted pos
  recall <- diag(mat) / colSums(mat) # true pos / all actual pos
  f1 <- ifelse(precision + recall == 0,
               0,
               2 * (precision * recall) / (precision + recall))
  return(f1)
}

# test_matrix
test_table <- data.table(obs = c(1, 1, 2, 2, 2, 3, 4),
                         pred = c(1, 1, 2, 2, 1, 3, 4))

test_table

# table_test
as.matrix(table(test_table$pred, test_table$obs))

# f1_test
f1(test_table$pred, test_table$obs) # okay so the f1 function returns something... and the right answers to boot!

# macro_f1_test
macro_f1(test_table) # the macro_f1 func as a whole works

# xval_method
# 10-fold cross-validation
tenfold_xval <-
  trainControl(method = 'cv',
               number = 10,
               p = 0.9,
               summaryFunction = macro_f1) # custom macro f1 func in progress

# Logistic Regression--------------------------------------------------
# model1_Perimeter_AspectRation
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

# using the y ~ x formula seems to keep throwing errors
model_1a <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,5)], # col 2 for perimeter, 5 for aspect ratio
        method = 'multinom', # multinomial log reg
        trControl = tenfold_xval, # use your 10-fold X-val 
        tuneGrid = data.frame(decay = 0), # no need for regularized variable weights
        metric = 'mF1') # my custom metric

# model_1a_results
model_1a$results

# Model1_Perimeter_Compactness
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_1b <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,10)], # col 2 for perimeter, 10 for compactness
        method = 'multinom', # multinomial log reg
        trControl = tenfold_xval, # use your 10-fold X-val 
        tuneGrid = data.frame(decay = 0), # no need for regularized variable weights
        metric = 'mF1') # my custom metric

# model_1b_results
model_1b$results

# model1_Perimeter_ShapeFactor2
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_1c <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12)], # col 2 for perimeter, 12 for SF2
        method = 'multinom', # multinomial log reg
        trControl = tenfold_xval, # use your 10-fold X-val 
        tuneGrid = data.frame(decay = 0), # no need for regularized variable weights
        metric = 'mF1') # my custom metric

# model_1c_results
model_1c$results

# model1_Perimeter_ShapeFactor3
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_1d <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,13)], # col 2 for perimeter, 13 for SF3
        method = 'multinom', # multinomial log reg
        trControl = tenfold_xval, # use your 10-fold X-val 
        tuneGrid = data.frame(decay = 0), # no need for regularized variable weights
        metric = 'mF1') # my custom metric

# model_1d_results
model_1d$results

# model1_EquivDiameter_AspectRation
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_1e <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(8,5)], # col 8 for equiv. diam., 5 for aspect ratio
        method = 'multinom', # multinomial log reg
        trControl = tenfold_xval, # use your 10-fold X-val 
        tuneGrid = data.frame(decay = 0), # no need for regularized variable weights
        metric = 'mF1') # my custom metric

# model_1e_results
model_1e$results

# KNN--------------------------------------------------
# model2_Perimeter_AspectRation
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_2a <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,5)], # col 2 for perimeter, 5 for aspect ratio
        method = 'knn',
        tuneGrid = data.frame(k = nearest_neighbors),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

ggplot(model_2a, highlight = T)

#model_2a_besttune
# model_2a$bestTune # k = 90 is best for this one
model_2a$results[90,]

# model2_Perimeter_Compactness
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_2b <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,10)], # col 2 for perimeter, 10 for compactness
        method = 'knn',
        tuneGrid = data.frame(k = nearest_neighbors),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

ggplot(model_2b, highlight = T)

# model_2b_besttune
# model_2b$bestTune # k=86
model_2b$results[86,]

# model2_Perimeter_ShapeFactor2
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_2c <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12)], # col 2 for perimeter, 12 for SF2
        method = 'knn',
        tuneGrid = data.frame(k = nearest_neighbors),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

ggplot(model_2c, highlight = T)

# model_2c_besttune
# model_2c$bestTune # k = 49
model_2c$results[49,]

# model2_Perimeter_ShapeFactor3
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_2d <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,13)], # col 2 for perimeter, 13 for SF3
        method = 'knn',
        tuneGrid = data.frame(k = nearest_neighbors),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

ggplot(model_2d, highlight = T)

# model_2d_besttune
# model_2d$bestTune # k=79
model_2d$results[79,]

# model2_EquivDiameter_AspectRation
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# trying various k values (# nearest neighbors)
nearest_neighbors <- seq(1, 100, 1)

model_2e <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(8,5)], # col 8 for equiv. diam., 5 for aspect ratio
        method = 'knn',
        tuneGrid = data.frame(k = nearest_neighbors),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

ggplot(model_2e, highlight = T)

# model_2e_besttune
# model_2e$bestTune # k=74
model_2e$results[74,]

# Two-Variable Model Results----------------------------------------
models_1_2 <-
  data.table(variables = c('Perimeter, Aspect Ratio',
                           'Perimeter, Compactness',
                           'Perimeter, Shape Factor 2',
                           'Perimeter, Shape Factor 3',
                           'Equivalent Diameter, Aspect Ratio'),
             logreg_mF1_unregularized = c(model_1a$results[,2],
                                          model_1b$results[,2],
                                          model_1c$results[,2],
                                          model_1d$results[,2],
                                          model_1e$results[,2]),
             knn_mF1 = c(model_2a$results[90,2],
                         model_2b$results[86,2],
                         model_2c$results[49,2],
                         model_2d$results[79,2],
                         model_2e$results[74,2]),
             k = c(90, 86, 49, 79, 74))
models_1_2

# Decision Trees--------------------------------------------------
# model3_Perimeter_ShapeFactor2
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# Trying various Complexity Parameters
cps <- seq(0.005, 0.050, 0.001)

model_3a <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12)], # col 2 for perimeter, 12 for SF2
        method = 'rpart',
        tuneGrid = data.frame(cp = cps),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

head(model_3a$results, 10)

# model3_Perimeter_ShapeFactor2_MajorAxisLength
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# Trying various Complexity Parameters
cps <- seq(0.005, 0.050, 0.001)

model_3b <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12,3)], # col 2 for perimeter, 12 for SF2, 3 for maj ax
        method = 'rpart',
        tuneGrid = data.frame(cp = cps),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

head(model_3b$results, 10)

# model3_Perimeter_ShapeFactor2_Eccentricity
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# Trying various Complexity Parameters
cps <- seq(0.005, 0.050, 0.001)

model_3c <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12,6)], # col 2 for perimeter, 12 for SF2, 6 for Eccentricity
        method = 'rpart',
        tuneGrid = data.frame(cp = cps),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

head(model_3c$results, 10)

# model3_Perimeter_ShapeFactor2_MajorAxisLength_Eccentricity
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# Trying various Complexity Parameters
cps <- seq(0.005, 0.050, 0.001)

model_3d <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12,3,6)],
        method = 'rpart',
        tuneGrid = data.frame(cp = cps),
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

head(model_3d$results, 10)

# Perimeter_ShapeFactor2_Eccentricity
rpart.plot(model_3c$finalModel)

# Random Forest-----------------------------------------
# model_4
# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')

# RF tuning grid
forest_grid <-
  expand.grid(
    mtry = 1:3, # how many variables to use for splitting at each tree?
    splitrule = 'gini', # for classification. Not changing this
    min.node.size = 1:40 
  )

model_4 <-
  train(y = beans_y_train$Class,
        x =  beans_x_train[,c(2,12,6)],
        method = 'ranger',
        tuneGrid = forest_grid,
        num.trees = 250, # rough runtime of 2 hrs
        trControl = tenfold_xval, # use your 10-fold X-val 
        metric = 'mF1') # my custom metric

model_4$results[85:95,]

# model4_final_model
model_4$bestTune

# model_4_plot
plot(model_4) 

# Validations------------------------------------
# validation_setup
# convert beans_y_val$Class to class: Factor
beans_y_val$Class <- as.factor(beans_y_val$Class) # convert to factor class
levels(beans_y_val$Class) # checking that format is right

# validation_LR
validation_preds_LR <- predict(model_1c$finalModel, beans_x_val)

confusionMatrix(validation_preds_LR, beans_y_val$Class)

# LR F1s
# manual calculation of precision and recall values from confusion matrix
LR_precision_BAR <- 216 / (216 + 18 + 1 + 2)
LR_recall_BAR <- 216 / (216 + 41 + 3 + 5)
LR_precision_BOM <- 105 / 105
LR_recall_BOM <- 105 / 105
LR_precision_CAL <- 300 / (300 + 41 + 20 + 1)
LR_recall_CAL <- 300 / (300 + 18 + 5 + 3)
LR_precision_DER <- 648 / (648 + 7 + 12 + 43)
LR_recall_DER <- 648 / (648 + 1 + 12 + 49)
LR_precision_HOR <- 352 / (352 + 5 + 1 + 17)
LR_recall_HOR <- 352 / (352 + 1 + 20 + 7 + 6)
LR_precision_SEK <- 377 / (377 + 3 + 12 + 11)
LR_recall_SEK <- 377 / (377 + 12 + 17)
LR_precision_SIR <- 454 / (454 + 5 + 3 + 49 + 6 + 17)
LR_recall_SIR <- 454 / (454 + 2 + 1 + 43 + 17 + 11)

# F1 func from matrix
f1_from_matrix <- function(precision, recall) {
  score <- 2 * (precision * recall) / (precision + recall)
  return(score)
}

# F1 values
LR_F1_BAR <- f1_from_matrix(LR_precision_BAR, LR_recall_BAR)
LR_F1_BOM <- f1_from_matrix(LR_precision_BOM, LR_recall_BOM)
LR_F1_CAL <- f1_from_matrix(LR_precision_CAL, LR_recall_CAL)
LR_F1_DER <- f1_from_matrix(LR_precision_DER, LR_recall_DER)
LR_F1_HOR <- f1_from_matrix(LR_precision_HOR, LR_recall_HOR)
LR_F1_SEK <- f1_from_matrix(LR_precision_SEK, LR_recall_SEK)
LR_F1_SIR <- f1_from_matrix(LR_precision_SIR, LR_recall_SIR)
LR_F1_MACRO <- mean(c(LR_F1_BAR,
                      LR_F1_BOM,
                      LR_F1_CAL,
                      LR_F1_DER,
                      LR_F1_HOR,
                      LR_F1_SEK,
                      LR_F1_SIR))

# validation_KNN
# KNN in R is not like the others, in that it fits and evaluates in one function
# Thus, a second training is done with the pre-tuned k value

# arbitrary seed for reproducibility
set.seed(1, sample.kind = 'Rounding')


validation_preds_KNN <- knn(train = beans_x_train[,c(2,12)],
                            cl = beans_y_train$Class,
                            test = beans_x_val[,c(2,12)],
                            k = 49) # KNN run on optimized k value tuned from caret::train

confusionMatrix(validation_preds_KNN, beans_y_val$Class)

# KNN F1s
# manual calculation of precision and recall values from confusion matrix
KNN_precision_BAR <- 219 / (219 + 22 + 2)
KNN_recall_BAR <- 219 / (219 + 39 + 3 + 4)
KNN_precision_BOM <- 105 / 105
KNN_recall_BOM <- 105 / 105
KNN_precision_CAL <- 299 / (299 + 39 + 16 + 1)
KNN_recall_CAL <- 299 / (299 + 22 + 3 + 2)
KNN_precision_DER <- 648 / (648 + 7 + 7 + 15)
KNN_recall_DER <- 648 / (648 + 2 + 19 + 11)
KNN_precision_HOR <- 350 / (350 + 3 + 2 + 7)
KNN_recall_HOR <- 350 / (350 + 2 + 16 + 7 + 11)
KNN_precision_SEK <- 381 / (381 + 3 + 19 + 8)
KNN_recall_SEK <- 381 / (381 + 7 + 18)
KNN_precision_SIR <- 457 / (457 + 4 + 2 + 41 + 11 + 18)
KNN_recall_SIR <- 457 / (457 + 1 + 55 + 7 + 8)

# F1 values
KNN_F1_BAR <- f1_from_matrix(KNN_precision_BAR, KNN_recall_BAR)
KNN_F1_BOM <- f1_from_matrix(KNN_precision_BOM, KNN_recall_BOM)
KNN_F1_CAL <- f1_from_matrix(KNN_precision_CAL, KNN_recall_CAL)
KNN_F1_DER <- f1_from_matrix(KNN_precision_DER, KNN_recall_DER)
KNN_F1_HOR <- f1_from_matrix(KNN_precision_HOR, KNN_recall_HOR)
KNN_F1_SEK <- f1_from_matrix(KNN_precision_SEK, KNN_recall_SEK)
KNN_F1_SIR <- f1_from_matrix(KNN_precision_SIR, KNN_recall_SIR)
KNN_F1_MACRO <- mean(c(KNN_F1_BAR,
                       KNN_F1_BOM,
                       KNN_F1_CAL,
                       KNN_F1_DER,
                       KNN_F1_HOR,
                       KNN_F1_SEK,
                       KNN_F1_SIR))

# validation_2factor_DT
# DT preds take dataframes...
validation_preds_2DT <- predict(model_3a$finalModel, data.frame(beans_x_val), type = 'class')

confusionMatrix(validation_preds_2DT, beans_y_val$Class)

# DT2 F1s
# manual calculation of precision and recall values from confusion matrix
DT2_precision_BAR <- 202 / (202 + 46 + 3 + 1 + 2)
DT2_recall_BAR <- 202 / (202 + 48 + 6 + 1 + 8)
DT2_precision_BOM <- 105 / 105
DT2_recall_BOM <- 105 / 105
DT2_precision_CAL <- 250 / (250 + 48 + 16)
DT2_recall_CAL <- 250 / (250 + 46 + 27 + 3)
DT2_precision_DER <- 661 / (661 + 7 + 23 + 58)
DT2_recall_DER <- 661 / (661 + 17 + 32)
DT2_precision_HOR <- 347 / (347 + 6 + 27 + 9)
DT2_recall_HOR <- 347 / (347 + 3 + 16 + 7 + 13)
DT2_precision_SEK <- 363 / (363 + 1 + 17 + 7)
DT2_recall_SEK <- 363 / (363 + 1 + 23 + 19)
DT2_precision_SIR <- 452 / (452 + 8 + 3 + 32 + 13 + 19)
DT2_recall_SIR <- 452 / (452 + 2 + 58 + 9 + 7)

# F1 values
DT2_F1_BAR <- f1_from_matrix(DT2_precision_BAR, DT2_recall_BAR)
DT2_F1_BOM <- f1_from_matrix(DT2_precision_BOM, DT2_recall_BOM)
DT2_F1_CAL <- f1_from_matrix(DT2_precision_CAL, DT2_recall_CAL)
DT2_F1_DER <- f1_from_matrix(DT2_precision_DER, DT2_recall_DER)
DT2_F1_HOR <- f1_from_matrix(DT2_precision_HOR, DT2_recall_HOR)
DT2_F1_SEK <- f1_from_matrix(DT2_precision_SEK, DT2_recall_SEK)
DT2_F1_SIR <- f1_from_matrix(DT2_precision_SIR, DT2_recall_SIR)
DT2_F1_MACRO <- mean(c(DT2_F1_BAR,
                       DT2_F1_BOM,
                       DT2_F1_CAL,
                       DT2_F1_DER,
                       DT2_F1_HOR,
                       DT2_F1_SEK,
                       DT2_F1_SIR))

# validation_3factor_DT
validation_preds_3DT <- predict(model_3c$finalModel, data.frame(beans_x_val), type = 'class')

confusionMatrix(validation_preds_3DT, beans_y_val$Class)

# DT3 F1s
# manual calculation of precision and recall values from confusion matrix
DT3_precision_BAR <- 184 / (184 + 23 + 1 + 1)
DT3_recall_BAR <- 184 / (184 + 76 + 3 + 2)
DT3_precision_BOM <- 105 / 105
DT3_recall_BOM <- 105 / 105
DT3_precision_CAL <- 294 / (294 + 76 + 34 + 4)
DT3_recall_CAL <- 294 / (294 + 23 + 8 + 1)
DT3_precision_DER <- 645 / (645 + 7 + 19 + 61)
DT3_recall_DER <- 645 / (645 + 33 + 32)
DT3_precision_HOR <- 329 / (329 + 8 + 1)
DT3_recall_HOR <- 329 / (367 + 1 + 34 + 7 + 15)
DT3_precision_SEK <- 367 / (367 + 3 + 33 + 5)
DT3_recall_SEK <- 367 / (367 + 1 + 19 + 19)
DT3_precision_SIR <- 457 / (457 + 2 + 1 + 32 + 15 + 19)
DT3_recall_SIR <- 457 / (457 + 4 + 61 + 1 + 5)

# F1 values
DT3_F1_BAR <- f1_from_matrix(DT3_precision_BAR, DT3_recall_BAR)
DT3_F1_BOM <- f1_from_matrix(DT3_precision_BOM, DT3_recall_BOM)
DT3_F1_CAL <- f1_from_matrix(DT3_precision_CAL, DT3_recall_CAL)
DT3_F1_DER <- f1_from_matrix(DT3_precision_DER, DT3_recall_DER)
DT3_F1_HOR <- f1_from_matrix(DT3_precision_HOR, DT3_recall_HOR)
DT3_F1_SEK <- f1_from_matrix(DT3_precision_SEK, DT3_recall_SEK)
DT3_F1_SIR <- f1_from_matrix(DT3_precision_SIR, DT3_recall_SIR)
DT3_F1_MACRO <- mean(c(DT3_F1_BAR,
                       DT3_F1_BOM,
                       DT3_F1_CAL,
                       DT3_F1_DER,
                       DT3_F1_HOR,
                       DT3_F1_SEK,
                       DT3_F1_SIR))

# validation_RF
validation_preds_RF <- predict(model_4$finalModel, beans_x_val)

confusionMatrix(validation_preds_RF$predictions, beans_y_val$Class)

# RF F1s
# manual calculation of precision and recall values from confusion matrix
RF_precision_BAR <- 236 / (236 + 1 + 15 + 2 + 2)
RF_recall_BAR <- 236 / (236 + 23 + 3 + 3)
RF_precision_BOM <- 104 / 104
RF_recall_BOM <- 104 / 105
RF_precision_CAL <- 301 / (301 + 23 + 11)
RF_recall_CAL <- 301 / (301 + 15 + 7 + 3)
RF_precision_DER <- 656 / (656 + 6 + 9 + 55)
RF_recall_DER <- 656 / (656 + 3 + 16 + 35)
RF_precision_HOR <- 355 / (355 + 7 + 3 + 7)
RF_recall_HOR <- 355 / (355 + 2 + 11 + 6 + 12)
RF_precision_SEK <- 383 / (383 + 3 + 16 + 11)
RF_recall_SEK <- 383 / (383 + 9 + 14)
RF_precision_SIR <- 453 / (453 + 3 + 3 + 35 + 12 + 14)
RF_recall_SIR <- 453 / (453 + 2 + 55 + 7 + 11)

# F1 values
RF_F1_BAR <- f1_from_matrix(RF_precision_BAR, RF_recall_BAR)
RF_F1_BOM <- f1_from_matrix(RF_precision_BOM, RF_recall_BOM)
RF_F1_CAL <- f1_from_matrix(RF_precision_CAL, RF_recall_CAL)
RF_F1_DER <- f1_from_matrix(RF_precision_DER, RF_recall_DER)
RF_F1_HOR <- f1_from_matrix(RF_precision_HOR, RF_recall_HOR)
RF_F1_SEK <- f1_from_matrix(RF_precision_SEK, RF_recall_SEK)
RF_F1_SIR <- f1_from_matrix(RF_precision_SIR, RF_recall_SIR)
RF_F1_MACRO <- mean(c(RF_F1_BAR,
                      RF_F1_BOM,
                      RF_F1_CAL,
                      RF_F1_DER,
                      RF_F1_HOR,
                      RF_F1_SEK,
                      RF_F1_SIR))

# F1_tabulation
F1_table <-
  data.table(model = c('Logistic Regression',
                       'KNN',
                       'Decision Tree (2 Variables)',
                       'Decision Tree (3 variables)',
                       'Random Forest (2 variables)'),
             Barbunya = c(LR_F1_BAR,
                          KNN_F1_BAR,
                          DT2_F1_BAR,
                          DT3_F1_BAR,
                          RF_F1_BAR),
             Bombay = c(LR_F1_BOM,
                        KNN_F1_BOM,
                        DT2_F1_BOM,
                        DT3_F1_BOM,
                        RF_F1_BOM),
             Cali = c(LR_F1_CAL,
                      KNN_F1_CAL,
                      DT2_F1_CAL,
                      DT3_F1_CAL,
                      RF_F1_CAL),
             Dermason = c(LR_F1_DER,
                          KNN_F1_DER,
                          DT2_F1_DER,
                          DT3_F1_DER,
                          RF_F1_DER),
             Horoz = c(LR_F1_HOR,
                       KNN_F1_HOR,
                       DT2_F1_HOR,
                       DT3_F1_HOR,
                       RF_F1_HOR),
             Seker = c(LR_F1_SEK,
                       KNN_F1_SEK,
                       DT2_F1_SEK,
                       DT3_F1_SEK,
                       RF_F1_SEK),
             Sira = c(LR_F1_SIR,
                      KNN_F1_SIR,
                      DT2_F1_SIR,
                      DT3_F1_SIR,
                      RF_F1_SIR),
             Macro = c(LR_F1_MACRO,
                       KNN_F1_MACRO,
                       DT2_F1_MACRO,
                       DT3_F1_MACRO,
                       RF_F1_MACRO)
  )
F1_table

