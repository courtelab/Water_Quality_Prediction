#####################################################################################################################################
########################################################### DATA PREPARATION ########################################################
#####################################################################################################################################

###############################################################
# Create edx set, validation set (final hold-out test set) ####
###############################################################
# Note: this process could take a couple of minutes
# Install packages if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(R.matlab)) install.packages("R.matlab", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
# Load required libraries
library(tidyverse)
library(caret)
library(data.table)
library(R.matlab)
library(plyr)
library(lubridate)
library(pls)
library(matrixStats)
library(kableExtra)
library(knitr)


##########################
#### RMSE CALCULATION ####
##########################
# Building RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


##########################
#### DOWNLOAD DATA  ######
##########################
# Water Quality Prediction
# Informations: https://archive-beta.ics.uci.edu/ml/datasets/water+quality+prediction-1
# Data: https://archive-beta.ics.uci.edu/api/static/ml/datasets/733

dl <- tempfile()
download.file("https://archive-beta.ics.uci.edu/api/static/ml/datasets/733/data.zip", dl)

my_data <- readMat(unzip(dl, "water_dataset.mat"))


##########################
#### PREDICTORS NAMES ####
##########################
i <- seq(1, 11)
names.pred <- sapply(i, function(j){ # What are the predictors' names
my_data$features[[j]][[1]][[1]]
})

# Changing the predictors' names to make them easier to read
names.pred <- c("P1.max",
                "P2.max",
                "P2.min",
                "P1.min",
                "P1.mean",
                "P3.max",
                "P3.mean",
                "P3.min",
                "P4.mean",
                "P4.min",
                "P4.max")


###################################
#### TRAINING SET CONSTRUCTION ####
###################################
# extraction of train_set
train_set <- ldply(my_data$X.tr, data.frame) # extraction of the train set from list to data.frame

# giving predictors' name
names(train_set) <- names.pred # adding predictors names to the set

# Addition of stations' numbers
stations <- rep(seq(1, 37), times = 423) # creating the stations' list
train_set <- train_set %>%
  cbind(stations) %>% # adding station number to the train set
  mutate(stations = as.character(stations))

# Addition of dates
date <- rep(ymd(20160128):ymd(20170325), each = 37) # creating the dates' list
train_set <- train_set %>%
  cbind(date) %>% # adding the dates to the train set
  mutate(date = as_date(date)) # converting date to lubridate format

# Addition of input J0
Y.J0.tr <- ldply(my_data$Y.tr, data.frame) # extraction of the input from list to data frame
train_set <- train_set %>%
  cbind(Y.J0.tr) # adding Y.J0 to the data frame of the train set
names(train_set)[14] <- "Y.J0"

# adding output J+1, to predict
train_set <- train_set[-((nrow(train_set)-36):nrow(train_set)),]  # removing the 37 last rows (no prediction possible for next day)
temp <- Y.J0.tr[-(1:37),]
train_set <- train_set %>%
  cbind(temp) # adding the J+1 output to the train set for prediction
names(train_set)[15] <- "Y.J1"
rm(temp)

# The stations are divided into 3 water systems. In each system, the stations are connected.
G1 <- my_data$location.group[[1]][[1]]
G2 <- my_data$location.group[[2]][[1]]
G3 <- my_data$location.group[[3]][[1]]

# Adding water system groups to train set
train_set <- train_set %>%
  mutate(System = ifelse(stations %in% G1, "G1", ifelse(stations %in% G2, "G2", "G3")))


##################################
#### TESTING SET CONSTRUCTION ####
##################################
# extraction of test_set
test_set <- ldply(my_data$X.te, data.frame) # extraction of the test set from list to data.frame

# giving predictors' name
names(test_set) <- names.pred  # adding predictors names to the set

# Addition of stations' numbers
stations <- rep(seq(1, 37), times = 282)  # creating the stations' list
test_set <- test_set %>%
  cbind(stations) %>% # adding station number to the test set
  mutate(stations = as.character(stations))

# Addition of dates
date <- rep(ymd(20170326):ymd(20180101), each = 37) # creating the dates' list
test_set <- test_set %>%
  cbind(date) %>%  # adding the dates to the test set
  mutate(date = as_date(date)) # converting date to lubridate format

# Addition of input J0
Y.J0.te <- ldply(my_data$Y.te, data.frame) # extraction of the input from list to data frame
test_set <- test_set %>%
  cbind(Y.J0.te) # adding Y.J0 to the data frame of the train set
names(test_set)[14] <- "Y.J0"

# adding output J+1, to predict
test_set <- test_set[-((nrow(test_set)-36):nrow(test_set)),]  # removing the 37 last rows (no prediction possible for next day)
temp <- Y.J0.te[-(1:37),]
test_set <- test_set %>%
  cbind(temp)    # adding the J+1 output to the test set for prediction
names(test_set)[15] <- "Y.J1"
rm(temp)

# Adding water system groups to test set
test_set <- test_set %>%
  mutate(System = ifelse(stations %in% G1, "G1", ifelse(stations %in% G2, "G2", "G3")))


#####################################################################################################################################
########################################################### DATA EXPLORATION ########################################################
#####################################################################################################################################


#################################################################
#### ADJUSTMENT OF THE OUTPUT TO MAKE IT MORE UNDERSTANDABLE ####
#################################################################
# the distance between 2 outputs is a constant with decimals, then we can modify the output to avoid decimals to have round numbers.

dist_out <- dist(sort(unique(train_set$Y.J1)))[1]

# Modification of the outputs of train and test sets (standardization)
train_set <- train_set %>%
  mutate(Y.J0 = Y.J0/dist_out) %>%
  mutate(Y.J1 = Y.J1/dist_out)


################################
#### VIZUALIZING THE OUTPUT ####
################################
min(train_set$Y.J1) # minimum output to figure on the drawing for comparison
max(train_set$Y.J1) # maximum output to figure on the drawing for comparison
n_distinct(train_set$Y.J1) # number of levels

# Density curves of all outputs in train set
train_set %>%
  ggplot(aes(x = Y.J1)) +
  geom_density(bw = 0.5) +
  scale_x_continuous(limits = c(min(train_set$Y.J1), max(train_set$Y.J1)))


################################
#### VIZUALIZING PREDICTORS ####
################################
pred_set <- select(train_set, c(-12, -13, -14, -15)) %>%
  pivot_longer(cols = seq(1, 11), names_to = "predictor")
pred_set %>%
  ggplot(aes(y = value, fill = predictor)) +
  geom_boxplot()

colMeans(select(train_set, c(-12, -13, -14, -15, -16))) # means of columns
colSds(as.matrix(select(train_set, c(-12, -13, -14, -15, -16)))) # SD of columns


################################################
#### RELATION BETWEEN PREDICTORS AND OUTPUT ####
################################################
## Scaling the matrix
set_scale <- train_set %>%
  select(1:11, 14, 15) # keeping only predictors and outputs
x_centered <- sweep(set_scale, 2, colMeans(set_scale))  # centering the data frame
x_scaled <- sweep(x_centered, 2, colSds(as.matrix(set_scale)), FUN = "/")  # normalizing the data frame

## Distance
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features))

## Clustering
h <- hclust(d_features)
groups <- cutree(h, k = 4) # groups
split(names(groups), groups)

# Image function
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
  axis(side = 2, 1:ncol(x), rev(colnames(x)), las = 2)
}

# Imaging the correlation
my_image(cor(x_scaled), zlim = c(-1,1))

# Value of correlation
cor(x_scaled)

# principal components calculation
pca <- prcomp(x_scaled)
summary(pca)

# PC1 and PC2 plot
data.frame(pca$x[,1:2], type = set_scale$Y.J1) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point(size = 0.5) +
  scale_color_gradient(low="blue", high="red") +
  geom_smooth()

# calculation of correlation with P2.max, P3.max and Y.J0 for every stations
i <- seq(1, 37)
cor_detail <- sapply(i, function(j){
  set_scale <- train_set %>%
    filter(stations == j) %>%
    select(1:11, 14, 15) # keeping only predictors and outputs
  x_centered <- sweep(set_scale, 2, colMeans(set_scale))  # centering the data frame
  x_scaled <- sweep(x_centered, 2, colSds(as.matrix(set_scale)), FUN = "/")  # normalizing the data frame
  cor(x_scaled)[,"Y.J1"][c(2,6,12)]
})

# Plotting all the correlation of P2.max, P3.max and Y.J1 with Y.J1 of the stations
data.frame(t(cor_detail)) %>%
  mutate(stations = 1:37) %>%
  pivot_longer(cols = 1:3, names_to = "comp", values_to = "correlation_with_Y.J1") %>%
  ggplot(aes(x = stations, y = correlation_with_Y.J1)) +
  geom_point() +
  geom_label(aes(label = stations)) +
  facet_grid(. ~ comp)

##################################################################
#### SPLITTING THE TRAINING SET INTO TRAIN AND VALIDATION SET ####
##################################################################
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = train_set$stations, times = 1, p = 0.2, list = FALSE)
train <- train_set[-test_index,]
validation <- train_set[test_index,]


#################################################################################
#### CREATING SETS OF TRAINING AND TESTING SETS WITH GROUPS OF WATER SYSTEMS ####
#################################################################################
train_G1 <- train %>%
  filter(System == "G1")
train_G2 <- train %>%
  filter(System == "G2")
train_G3 <- train %>%
  filter(System == "G3")

validation_G1 <- validation %>%
  filter(System == "G1")
validation_G2 <- validation %>%
  filter(System == "G2")
validation_G3 <- validation %>%
  filter(System == "G3")


##############################
#### MODEL 1: Y.J1 = Y.J0 ####
##############################
# calculating RMSE for model 1
# G1
rmse_g1_m1 <- RMSE(validation_G1$Y.J0, validation_G1$Y.J1)
# G2
rmse_g2_m1 <- RMSE(validation_G2$Y.J0, validation_G2$Y.J1)
# G3
rmse_g3_m1 <- RMSE(validation_G3$Y.J0, validation_G3$Y.J1)


# Tabulate group RMSE
RMSE_Table_global.tr <- tibble(Water_System = c("G1","G2","G3"),
                     RMSE_Model_1 = c(rmse_g1_m1, rmse_g2_m1, rmse_g3_m1))
RMSE_Table_global.tr %>%
  knitr::kable(caption = "RMSE for Model 1 - Training Set", booktabs = T, linesep = "") %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))



###############################
#### MODEL 2: LINEAR MODEL ####
###############################
# Building "lm" model of caret package
# G1
fit_g1_m2.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0,
                   method = "lm",
                   data = train_G1)
set.seed(1, sample.kind = "Rounding")
Y.hat_g1_m2 <- predict(fit_g1_m2.tr, validation_G1)
rmse_g1_m2 <- RMSE(validation_G1$Y.J1, Y.hat_g1_m2)
# G2
fit_g2_m2.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                   method = "lm",
                   data = train_G2)
set.seed(1, sample.kind = "Rounding")
Y.hat_g2_m2 <- predict(fit_g2_m2.tr, validation_G2)
rmse_g2_m2 <- RMSE(validation_G2$Y.J1, Y.hat_g2_m2)
#G3
fit_g3_m2.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                   method = "lm",
                   data = train_G3)
set.seed(1, sample.kind = "Rounding")
Y.hat_g3_m2 <- predict(fit_g3_m2.tr, validation_G3)
rmse_g3_m2 <- RMSE(validation_G3$Y.J1, Y.hat_g3_m2)

# Tabulate group RMSE
RMSE_Table_global.tr <- cbind(RMSE_Table_global.tr, tibble(RMSE_Model_2 = c(rmse_g1_m2, rmse_g2_m2, rmse_g3_m2)))
RMSE_Table_global.tr %>%
  knitr::kable(caption = "RMSE for Models 1/2 - Training Set", booktabs = T, linesep = "") %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))


#######################
#### MODEL 3: BRNN ####
#######################
# Building "brnn" model of caret package
# tuning parameter neurons (1 or 2 to avoid delay) with the 3 groups
# G1
i <- seq(1, 2, 1)
rmse_g1_m3.tr <- sapply(i, function(j){
  fit_g1_m3 <- train(Y.J1 ~ P2.max + P3.max + Y.J0,
                     method = "brnn",
                     tuneGrid = expand.grid(neurons = j),
                     data = train_G1)
  set.seed(1, sample.kind = "Rounding")
  Y.hat_g1_m3 <- predict(fit_g1_m3, validation_G1)
  RMSE(validation_G1$Y.J1, Y.hat_g1_m3)
})
# G2
i <- seq(1, 2, 1)
rmse_g2_m3.tr <- sapply(i, function(j){
  fit_g2_m3 <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                     method = "brnn",
                     tuneGrid = expand.grid(neurons = j),
                     data = train_G2)
  set.seed(1, sample.kind = "Rounding")
  Y.hat_g2_m3 <- predict(fit_g2_m3, validation_G2)
  RMSE(validation_G2$Y.J1, Y.hat_g2_m3)
})
# G3
i <- seq(1, 2, 1)
rmse_g3_m3.tr <- sapply(i, function(j){
  fit_g3_m3 <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                     method = "brnn",
                     tuneGrid = expand.grid(neurons = j),
                     data = train_G3)
  set.seed(1, sample.kind = "Rounding")
  Y.hat_g3_m3 <- predict(fit_g3_m3, validation_G3)
  RMSE(validation_G3$Y.J1, Y.hat_g3_m3)
})

# Selection of the best tuning parameter ncomp
k_g1_m3 <- i[which.min(rmse_g1_m3.tr)]
k_g2_m3 <- i[which.min(rmse_g2_m3.tr)]
k_g3_m3 <- i[which.min(rmse_g3_m3.tr)]

# Best rmse with the best tuning parameter
rmse_g1_m3 <- min(rmse_g1_m3.tr)
rmse_g2_m3 <- min(rmse_g2_m3.tr)
rmse_g3_m3 <- min(rmse_g3_m3.tr)


# Tabulate group RMSE
RMSE_Table_global.tr <- cbind(RMSE_Table_global.tr, tibble(RMSE_Model_3 = c(rmse_g1_m3, rmse_g2_m3, rmse_g3_m3)))
RMSE_Table_global.tr %>%
  knitr::kable(caption = "RMSE for Models 1/2/3 - Training Set", booktabs = T, linesep = "") %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))

# Fitting with best tuning parameter
fit_g1_m3.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0,
                      method = "brnn",
                      tuneGrid = expand.grid(neurons = k_g1_m3),
                      data = train_G1)

fit_g2_m3.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                      method = "brnn",
                      tuneGrid = expand.grid(neurons = k_g2_m3),
                      data = train_G2)

fit_g3_m3.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                      method = "brnn",
                      tuneGrid = expand.grid(neurons = k_g3_m3),
                      data = train_G3)


######################
#### MODEL 4: PCR ####
######################
# Building "pcr" model of caret package
# tuning parameter ncomp with the 3 groups
# G1
i <- seq(1, 3, 1)
rmse_g1_m4.tr <- sapply(i, function(j){
  fit_g1_m4 <- train(Y.J1 ~ P2.max + P3.max + Y.J0,
                     method = "pcr",
                     tuneGrid = expand.grid(ncomp = j),
                     data = train_G1)
  set.seed(1, sample.kind = "Rounding")
  Y.hat_g1_m4 <- predict(fit_g1_m4, validation_G1)
  RMSE(validation_G1$Y.J1, Y.hat_g1_m4)
})
# G2
i <- seq(1, 4, 1)
rmse_g2_m4.tr <- sapply(i, function(j){
  fit_g2_m4 <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                     method = "pcr",
                     tuneGrid = expand.grid(ncomp = j),
                     data = train_G2)
  set.seed(1, sample.kind = "Rounding")
  Y.hat_g2_m4 <- predict(fit_g2_m4, validation_G2)
  RMSE(validation_G2$Y.J1, Y.hat_g2_m4)
})
# G3
i <- seq(1, 4, 1)
rmse_g3_m4.tr <- sapply(i, function(j){
  fit_g3_m4 <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                     method = "pcr",
                     tuneGrid = expand.grid(ncomp = j),
                     data = train_G3)
  set.seed(1, sample.kind = "Rounding")
  Y.hat_g3_m4 <- predict(fit_g3_m4, validation_G3)
  RMSE(validation_G3$Y.J1, Y.hat_g3_m4)
})

# Selection of the best tuning parameter ncomp
k_g1_m4 <- i[which.min(rmse_g1_m4.tr)]
k_g2_m4 <- i[which.min(rmse_g2_m4.tr)]
k_g3_m4 <- i[which.min(rmse_g3_m4.tr)]

# Best RMSE with the best tuning parameter
rmse_g1_m4 <- min(rmse_g1_m4.tr)
rmse_g2_m4 <- min(rmse_g2_m4.tr)
rmse_g3_m4 <- min(rmse_g3_m4.tr)

# Tabulate group RMSE
RMSE_Table_global.tr <- cbind(RMSE_Table_global.tr, tibble(RMSE_Model_4 = c(rmse_g1_m4, rmse_g2_m4, rmse_g3_m4)))
RMSE_Table_global.tr %>%
  knitr::kable(caption = "RMSE for Models 1/2/3/4 - Training Set", booktabs = T, linesep = "") %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))

# Fitting with best tuning parameter
fit_g1_m4.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0,
                   method = "pcr",
                   tuneGrid = expand.grid(ncomp = k_g1_m4),
                   data = train_G1)

fit_g2_m4.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                   method = "pcr",
                   tuneGrid = expand.grid(ncomp = k_g2_m4),
                   data = train_G2)

fit_g3_m4.tr <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                   method = "pcr",
                   tuneGrid = expand.grid(ncomp = k_g3_m4),
                   data = train_G3)


#########################
#### STATION DETAILS ####
#########################

######### Model 1
# creating the table for stations inside G1 group for model 1
RMSE_Table_G1.tr <- tibble(Stations = as.character(G1),
                          Model_Observations = RMSE(validation_G1$Y.J0, validation_G1$Y.J1))

# Looking at RMSE inside G2 group for model 1
i <- as.character(G2)
rmse_g2_m1_detail <- sapply(i, function(j){
  set <- validation_G2 %>%
    filter(stations == j)
  RMSE(set$Y.J0, set$Y.J1)
})

# creating the table for stations inside G2 group for model 1
RMSE_Table_G2.tr <- tibble(Stations = as.character(G2),
                           Model_Observations = rmse_g2_m1_detail)

# Looking at RMSE inside G3 group for model 1
i <- as.character(G3)
rmse_g3_m1_detail <- sapply(i, function(j){
  set <- validation_G3 %>%
    filter(stations == j)
  RMSE(set$Y.J0, set$Y.J1)
})

# creating the table for stations inside G3 group for model 1
RMSE_Table_G3.tr <- tibble(Stations = as.character(G3),
                           Model_Observations = rmse_g3_m1_detail)


########### Model 2
# creating the table for stations inside G1 group for model 2
RMSE_Table_G1.tr <- cbind(RMSE_Table_G1.tr,
                       tibble(Model_Linear_Regression = RMSE(predict(fit_g1_m2.tr, validation_G1), validation_G1$Y.J1)))

# Looking at RMSE inside G2 group for model 2
i <- as.character(G2)
rmse_g2_m2_detail <- sapply(i, function(j){
  set <- validation_G2 %>%
    filter(stations == j)
  Y.hat <- predict(fit_g2_m2.tr, set)
  RMSE(set$Y.J1, Y.hat)
})

# creating the table for stations inside G2 group for model 2
RMSE_Table_G2.tr <- cbind(RMSE_Table_G2.tr,
                       tibble(Model_Linear_Regression = rmse_g2_m2_detail))

# Looking at RMSE inside G3 group for model 2
i <- as.character(G3)
rmse_g3_m2_detail <- sapply(i, function(j){
  set <- validation_G3 %>%
    filter(stations == j)
  Y.hat <- predict(fit_g3_m2.tr, set)
  RMSE(set$Y.J1, Y.hat)
})

# creating the table for stations inside G3 group for model 2
RMSE_Table_G3.tr <- cbind(RMSE_Table_G3.tr,
                       tibble(Model_Linear_Regression = rmse_g3_m2_detail))


############# Model 3
# creating the table for stations inside G1 group for model 3
RMSE_Table_G1.tr <- cbind(RMSE_Table_G1.tr, tibble(Model_BRNN = RMSE(predict(fit_g1_m3.tr, validation_G1), validation_G1$Y.J1)))

# Looking at RMSE inside G2 group for model 3
i <- as.character(G2)
rmse_g2_m3_detail <- sapply(i, function(j){
  set <- validation_G2 %>%
    filter(stations == j)
  Y.hat <- predict(fit_g2_m3.tr, set)
  RMSE(set$Y.J1, Y.hat)
})

# creating the table for stations inside G2 group for model 3
RMSE_Table_G2.tr <- cbind(RMSE_Table_G2.tr, tibble(Model_BRNN = rmse_g2_m3_detail))

# Looking at RMSE inside G3 group for model 3
i <- as.character(G3)
rmse_g3_m3_detail <- sapply(i, function(j){
  set <- validation_G3 %>%
    filter(stations == j)
  Y.hat <- predict(fit_g3_m3.tr, set)
  RMSE(set$Y.J1, Y.hat)
})

# creating the table for stations inside G3 group for model 3
RMSE_Table_G3.tr <- cbind(RMSE_Table_G3.tr, tibble(Model_BRNN = rmse_g3_m3_detail))


############# Model 4
# creating the table for stations inside G1 group for model 4
RMSE_Table_G1.tr <- cbind(RMSE_Table_G1.tr, tibble(Model_PCR = RMSE(predict(fit_g1_m4.tr, validation_G1), validation_G1$Y.J1)))

# Looking at RMSE inside G2 group for model 4
i <- as.character(G2)
rmse_g2_m4_detail <- sapply(i, function(j){
  set <- validation_G2 %>%
    filter(stations == j)
  Y.hat <- predict(fit_g2_m4.tr, set)
  RMSE(set$Y.J1, Y.hat)
})

# creating the table for stations inside G2 group for model 4
RMSE_Table_G2.tr <- cbind(RMSE_Table_G2.tr, tibble(Model_PCR = rmse_g2_m4_detail))

# Looking at RMSE inside G3 group for model 4
i <- as.character(G3)
rmse_g3_m4_detail <- sapply(i, function(j){
  set <- validation_G3 %>%
    filter(stations == j)
  Y.hat <- predict(fit_g3_m4.tr, set)
  RMSE(set$Y.J1, Y.hat)
})

# creating the table for stations inside G3 group for model 4
RMSE_Table_G3.tr <- cbind(RMSE_Table_G3.tr, tibble(Model_PCR = rmse_g3_m4_detail))


######## Compilation of stations data
# Combining all previous tables to build an overall table of RMSE of all stations with all methods
RMSE_Table_G1.tr <- RMSE_Table_G1.tr %>%
  mutate(Stations = as.numeric(Stations)) %>%
  mutate(Water_System = "G1") %>%
  select(6, 1:5)

RMSE_Table_G2.tr <- RMSE_Table_G2.tr %>%
  mutate(Stations = as.numeric(Stations)) %>%
  mutate(Water_System = "G2") %>%
  select(6, 1:5)

RMSE_Table_G3.tr <- RMSE_Table_G3.tr %>%
  mutate(Stations = as.numeric(Stations)) %>%
  mutate(Water_System = "G3") %>%
  select(6, 1:5)

RMSE_Table.tr <- rbind(RMSE_Table_G1.tr, RMSE_Table_G2.tr, RMSE_Table_G3.tr)

# Boxplot of models applied on all stations
RMSE_Table_2 <- RMSE_Table.tr %>%
  pivot_longer(cols = 3:6,
               names_to = "Model",
               values_to = "RMSE")

RMSE_Table_2 %>%
  ggplot(aes(y = RMSE)) +
  geom_boxplot() +
  facet_wrap(~ Model)

# Visualizing the best model for each station.
Best_RMSE <- RMSE_Table_2 %>%
  group_by(Stations) %>%
  dplyr::summarize(RMSE = min(RMSE)) %>%
  select(`RMSE`) %>%
  left_join(RMSE_Table_2, by = "RMSE")

Best_RMSE %>%
  ggplot(aes(x = Model)) +
  geom_bar(width = 0.3) +
  scale_y_continuous(breaks = seq(0,50,1))



##############################################################################################################################
############################################## FINAL HOLDOUT TEST ############################################################
##############################################################################################################################

# Standardizing test set output (like it was done for train set)
test_set <- test_set %>%
  mutate(Y.J0 = Y.J0/dist_out) %>%
  mutate(Y.J1 = Y.J1/dist_out)

# Dividing the data of train and test set by water system groups (because they use different models)
train_f_G1 <- train_set %>%
  filter(System == "G1")
train_f_G2 <- train_set %>%
  filter(System == "G2")
train_f_G3 <- train_set %>%
  filter(System == "G3")

test_f_G1 <- test_set %>%
  filter(System == "G1")
test_f_G2 <- test_set %>%
  filter(System == "G2")
test_f_G3 <- test_set %>%
  filter(System == "G3")


############################
#### FINAL HOLDOUT TEST ####
############################

# The water system G1 has a minimum RMSE with model 4 (pcr).

# The water system G2 has a minimum RMSE with model 3 (brnn).

# The water system G3 has a minimum RMSE with model 3 (brnn).

# We will use those models for the water systems with the final holdout test on the test_set.

################# Model 4
# Calculating the model with train set
fit_g1_m4.te <- train(Y.J1 ~ P2.max + P3.max + Y.J0,
                      method = "pcr",
                      tuneGrid = expand.grid(ncomp = k_g1_m4),
                      data = train_f_G1)
set.seed(1, sample.kind = "Rounding")
Y.hat_g1_m4 <- predict(fit_g1_m4.te, test_f_G1)
rmse_g1_m4 <-   RMSE(test_f_G1$Y.J1, Y.hat_g1_m4)


# Tabulate group RMSE
RMSE_Table_global.te <- tibble(RMSE_G1_Test_Set = rmse_g1_m4)

################### Model 3
# Calculating the model with train set
fit_g2_m3.te <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                      method = "brnn",
                      tuneGrid = expand.grid(neurons = k_g2_m3),
                      data = train_f_G2)
set.seed(1, sample.kind = "Rounding")
Y.hat_g2_m3 <- predict(fit_g2_m3.te, test_f_G2)
rmse_g2_m3 <-  RMSE(test_f_G2$Y.J1, Y.hat_g2_m3)

fit_g3_m3.te <- train(Y.J1 ~ P2.max + P3.max + Y.J0 + stations,
                      method = "brnn",
                      tuneGrid = expand.grid(neurons = k_g3_m3),
                      data = train_f_G3)
set.seed(1, sample.kind = "Rounding")
Y.hat_g3_m3 <- predict(fit_g3_m3.te, test_f_G3)
rmse_g3_m3 <- RMSE(test_f_G3$Y.J1, Y.hat_g3_m3)


# Tabulate group RMSE
RMSE_Table_global.te <- cbind(RMSE_Table_global.te, tibble(RMSE_G2_Test_Set = rmse_g2_m3, RMSE_G3_Test_Set = rmse_g3_m3))
RMSE_Table_global.te %>%
  knitr::kable(caption = "Final Holdout Test", booktabs = T, linesep = "") %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))




