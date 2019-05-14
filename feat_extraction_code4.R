install.packages("ggplot2")
install.packages("dplyr")
nstall.packages("caret")
install.packages("randomForest")
#### Library load ####
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)

setwd("D:/Data Analytics 2/Market Dial")

#### Data load ####
store_att <- read.csv("SUU_store_attributes_mav.csv", stringsAsFactors = F)
store_att <- store_att[,-1]
store_att <- store_att[1:336,]
lift <- read.csv("SUU_test_data_and_MarketDial_lift.csv")
lift <- lift[,-1]
lift$date_pre_start <- as.Date(lift$date_pre_start, format = "%Y-%m-%d")
lift$date_implementation_start <- as.Date(lift$date_implementation_start, format = "%Y-%m-%d")
lift$date_test_start <- as.Date(lift$date_test_start, format = "%Y-%m-%d")
lift$date_test_end <- as.Date(lift$date_test_end, format = "%Y-%m-%d")

test12 <- read.csv("test_12_revenue_data")
test12 <- test12[,-1]
test12$date_week <- as.Date(test12$date_week, format = "%Y-%m-%d")
test2 <- read.csv("test_2_revenue_data")
test2 <- test2[,-1]
test2$date_week <- as.Date(test2$date_week, format = "%Y-%m-%d")
test21 <- read.csv("test_21_revenue_data")
test21 <- test21[,-1]
test21$date_week <- as.Date(test21$date_week, format = "%Y-%m-%d")
test34 <- read.csv("test_34_revenue_data")
test34 <- test34[,-1]
test34$date_week <- as.Date(test34$date_week, format = "%Y-%m-%d")
test5 <- read.csv("test_5_revenue_data")
test5 <- test5[,-1]
test5$date_week <- as.Date(test5$date_week, format = "%Y-%m-%d")
test7 <- read.csv("test_7_revenue_data")
test7 <- test7[,-1]
test7$date_week <- as.Date(test7$date_week, format = "%Y-%m-%d")
test8 <- read.csv("test_8_revenue_data")
test8 <- test8[,-1]
test8$date_week <- as.Date(test8$date_week, format = "%Y-%m-%d")

#### Data clean ####
str(store_att)
low_state <- names(table(store_att$state)[(table(store_att$state) < 5)])
store_att[store_att$state %in% low_state,"state"] <- "Other"

for (i in which(sapply(store_att, is.numeric))) {
  if (sum(is.na(store_att[,i])) < 5) {
    store_att[is.na(store_att[,i]),i] <- mean(store_att[,i], na.rm = T)
  }
}

# Manually fix NAs in factors

store_att[,which(sapply(store_att, is.character))] <- 
  lapply(store_att[,which(sapply(store_att, is.character))], factor)

store_att[is.na(store_att$sales_liquor_percent_retail_sales),
          "sales_liquor_percent_retail_sales"] <- 0

df <- store_att
ap <- unique(apply(df,FUN = is.na, MARGIN = 2))
rowSums(ap)
sapply(store_att, function(y) sum(length(which(is.na(y)))))
ap <- ap[rowSums(ap) != 0,]

store_att$NA1 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[1,]))) == ncol(df)
store_att$NA2 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[2,]))) == ncol(df)
store_att$NA3 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[3,]))) == ncol(df)
store_att$NA4 <- rowSums(t(apply(is.na(df), 1, function(x) x == ap[4,]))) == ncol(df)


for (i in which(sapply(store_att, is.numeric))) {
  if (sum(is.na(store_att[,i])) > 0) {
    store_att[is.na(store_att[,i]),i] <- mean(store_att[,i], na.rm = T)
  }
}
# Store attributes is ready to go!

#### Tie together lift and store attributes for each test ####
min2 <- min(lift[lift$test_id == 2, "date_implementation_start"])
min5 <- min(lift[lift$test_id == 5, "date_implementation_start"])
min7 <- min(lift[lift$test_id == 7, "date_implementation_start"])
min8 <- min(lift[lift$test_id == 8, "date_implementation_start"])
min12 <- min(lift[lift$test_id == 12, "date_implementation_start"])
min21 <- min(lift[lift$test_id == 21, "date_implementation_start"])
min34 <- min(lift[lift$test_id == 34, "date_implementation_start"])

library(dplyr)
df2 <- test2 %>%
  filter(date_week < min2) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_2_direct_category_revenue),
            Store_Avg = mean(test_2_whole_store_revenue),
            Cat_Var = var(test_2_direct_category_revenue),
            Store_Var = var(test_2_whole_store_revenue),
            Pct_Cat = mean(test_2_direct_category_revenue) / 
              mean(test_2_whole_store_revenue))
df5 <- test5 %>%
  filter(date_week < min5) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_5_direct_category_revenue),
            Store_Avg = mean(test_5_whole_store_revenue),
            Cat_Var = var(test_5_direct_category_revenue),
            Store_Var = var(test_5_whole_store_revenue),
            Pct_Cat = mean(test_5_direct_category_revenue) / 
              mean(test_5_whole_store_revenue))
df7 <- test7 %>%
  filter(date_week < min7) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_7_direct_category_revenue),
            Store_Avg = mean(test_7_whole_store_revenue),
            Cat_Var = var(test_7_direct_category_revenue),
            Store_Var = var(test_7_whole_store_revenue),
            Pct_Cat = mean(test_7_direct_category_revenue) / 
              mean(test_7_whole_store_revenue))
df8 <- test8 %>%
  filter(date_week < min8) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_8_direct_category_revenue),
            Store_Avg = mean(test_8_whole_store_revenue),
            Cat_Var = var(test_8_direct_category_revenue),
            Store_Var = var(test_8_whole_store_revenue),
            Pct_Cat = mean(test_8_direct_category_revenue) / 
              mean(test_8_whole_store_revenue))
df12 <- test12 %>%
  filter(date_week < min12) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_12_direct_category_revenue),
            Store_Avg = mean(test_12_whole_store_revenue),
            Cat_Var = var(test_12_direct_category_revenue),
            Store_Var = var(test_12_whole_store_revenue),
            Pct_Cat = mean(test_12_direct_category_revenue) / 
              mean(test_12_whole_store_revenue))
df21 <- test21 %>%
  filter(date_week < min21) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_21_direct_category_revenue),
            Store_Avg = mean(test_21_whole_store_revenue),
            Cat_Var = var(test_21_direct_category_revenue),
            Store_Var = var(test_21_whole_store_revenue),
            Pct_Cat = mean(test_21_direct_category_revenue) / 
              mean(test_21_whole_store_revenue))
df34 <- test34 %>%
  filter(date_week < min34) %>%
  group_by(store_id) %>%
  summarise(Cat_Avg = mean(test_34_direct_category_revenue),
            Store_Avg = mean(test_34_whole_store_revenue),
            Cat_Var = var(test_34_direct_category_revenue),
            Store_Var = var(test_34_whole_store_revenue),
            Pct_Cat = mean(test_34_direct_category_revenue) / 
              mean(test_34_whole_store_revenue))

## Other dataframes can now be added

store_att2 <- merge(store_att, df2, by = "store_id")
store_att2 <- merge(store_att2, lift[lift$test_id == 2,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att5 <- merge(store_att, df5, by = "store_id")
store_att5 <- merge(store_att5, lift[lift$test_id == 5,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att7 <- merge(store_att, df7, by = "store_id")
store_att7 <- merge(store_att7, lift[lift$test_id == 7,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att8 <- merge(store_att, df8, by = "store_id")
store_att8 <- merge(store_att8, lift[lift$test_id == 8,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att12 <- merge(store_att, df12, by = "store_id")
store_att12 <- merge(store_att12, lift[lift$test_id == 12,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att21 <- merge(store_att, df21, by = "store_id")
store_att21 <- merge(store_att21, lift[lift$test_id == 21,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att34 <- merge(store_att, df34, by = "store_id")
store_att34 <- merge(store_att34, lift[lift$test_id == 34,c("treatment_store_id","lift")], by.x = "store_id", by.y = "treatment_store_id")
store_att2$testId <- 2
store_att5$testId <- 5
store_att7$testId <- 7
store_att8$testId <- 8
store_att12$testId <- 12
store_att21$testId <- 21
store_att34$testId <- 34

store_lifts <- rbind(store_att2, store_att5, store_att7, store_att8,
                     store_att12, store_att21, store_att34)

#### Evaluating lift ####


lift_mod <- store_lifts[,-1]
lift_mod <- lift_mod[lift_mod$lift != Inf,]
lift_mod$testId <- as.factor(lift_mod$testId)


# Explore tuning parameters.
# Results: Need at least 75 trees and mtry does not vary.
#control <- trainControl(method = "cv", number = 10, search = "random")
#rf1 <- train(lift ~., data = train, trControl = control,
#             method = "rf")

## R-Square hovers between .06 and .1 with optimal tuning parameter
# It's better than nothing, but it's not highly predictive.

set.seed(54321)
VarResults <- data.frame(var = character(), imp = numeric())
for (i in 1:20) {
  trainIndex = createDataPartition(lift_mod$lift,
                                   p = 0.8,
                                   list = FALSE)
  train <- lift_mod[trainIndex,]
  test <- lift_mod[-trainIndex,]
  rf2 <- randomForest(lift ~., data = train, mtry = 10,
                      ntree = 500, importance = TRUE)
  temp <- data.frame(var = row.names(varImp(rf2)), imp = varImp(rf2)$Overall)
  VarResults <- rbind(VarResults,temp)
}

SummaryVarResults <- VarResults %>%
  group_by(var) %>%
  summarise(imp = mean(imp)) %>%
  arrange(desc(imp))

# Important variables. The most important variable is the
# test that is being run. This is specific to the experiment
# and is not generalizable. Beyond that, within specific tests
# the category is what is most important.
ImpVars <- as.character(SummaryVarResults$var[2:6])
# Percent of revenue in category, average revenue in category,
# category variance, carbonated beverage sales, and store
# sales are the most important.

# When running models on predicting category revenue, and store
# revenue, a few more variables were added that were not included
# previously. While this is not directly predictive of lift,
# it is predictive of those items that feed into the variables
# important to lift (category and store revenue and variance) 
# and they may be important in different tests in the future 
# 
# 
ImpVars <- c(ImpVars, "store_size_square_footage", "population_total",
             "consumer_expenditure_alcohol_average",
             "sales_noveltyentertnmntgifts_percent_retail_sales")
ImpWeights <- c(.2,.2,.15,.15,.15,.05,.05,.05,.05)
# Weights are approximately the model variable importance
# plus 0.2 in weights for remaining category that are likely
# to be important because of how they predict store revenue
# and category revenue. The 0.2 weight is arbitrary, while
# the 0.8 breakdown of weights approximatley follows the
# variable weight in increasing predictive power.

Imps <- data.frame(vars = ImpVars, weights = ImpWeights)


#### Determine sample size based on margin of error input ####



#### Implement K-Means Clustering Using Weights ####
# Use Imps data frame to subset lift_mod data frame
#https://stackoverflow.com/questions/48901178/weighted-kmeans-r

install.packages("flexclust")
library(flexclust)
cclust(Imps, 15, dist = "euclidean",method = "kmeans",
       weights = Imps$weights)


#### Implement PCA Clustering Method (Perfect Ignorance) ####
# Remove label
unsupervised <- subset(lift_mod, select = -c(lift,testId,
                                             NA1, NA2, NA3,
                                             NA4))

# K-Modes would be the appropriate response here for categorical
# variables but initial evaluation showed somewhat similar results
# as simply adding dummies. For scaling purposes K-Modes or a 
# similar approach should almost certainly be used.
# See FactoMineR package in R.
unsupervised <- fastDummies::dummy_cols(unsupervised)
# Zero origin can be omitted category for each.
unsupervised <- subset(unsupervised, select = -c(region_Canyon,
                                             state_WY,
                                             store_style_COUNTRY,
                                             state, store_style,
                                             region))
unsup_scaled <- as.data.frame(scale(unsupervised[,1:48], center = TRUE))
unsup_scaled <- cbind(unsup_scaled, unsupervised[,50:62])
pca1 <- prcomp(unsup_scaled)
plot(pca1)
unsup_points <- pca1$x[,1:4]

