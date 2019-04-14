#***Predicting the no. of feedbacks a blog document is going to receive****#

# STEP 1 : Importing the files (with no headers)

# TRAIN DATASET
base_train_set <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_train.csv", header = FALSE)
dim(base_train_set) # Validating the dimensions to be same as given data
head(base_train_set[,51]) # Random manual check for some column value

# TEST DATASETS
test_set_feb <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_test-2012.02.21.00_00.csv", header = FALSE)
dim(test_set_feb)
test_set_march <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_test-2012.03.21.00_00.csv", header = FALSE)
dim(test_set_march)

test_set_feb_2 <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_test-2012.02.03.00_00.csv", header = FALSE)
dim(test_set_feb_2)
test_set_march_2 <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_test-2012.03.03.00_00.csv", header = FALSE)
dim(test_set_march_2)

#**************EXPERIMENT 1 STARTS*********

# STEP 2 : Extracting only the relavant columns for Experiment 1
train_set_basic_feature <- base_train_set[,c(51:60,281)]
train_set_basic_feature <- as.data.frame(train_set_basic_feature)
dim(train_set_basic_feature)
head(train_set_basic_feature)

test_set_feb_basic <- test_set_feb[,c(51:60,281)] 
dim(test_set_feb_basic)
test_set_march_basic <- test_set_march[,c(51:60,281)]
dim(test_set_march_basic)

test_set_feb_basic_2 <- test_set_feb_2[,c(51:60,281)] 
dim(test_set_feb_basic_2)
test_set_march_basic_2 <- test_set_march_2[,c(51:60,281)]
dim(test_set_march_basic_2)

# Checking for any "NA" entries
a <- train_set_basic_feature[is.na(train_set_basic_feature)]
dim(a)

# STEP 3 : Build the model on whole of the train set
basic_feature_model <- lm(V281 ~ ., data = train_set_basic_feature)
summary(basic_feature_model)
residuals <- residuals(basic_feature_model)
(mse_train_basic <- round(mean(residuals^2),2)) # Mean Squared Error
(sd_train_basic <- round(sd(residuals),2)) # Std. Deviation

# STEP 4 : Test the model on the 1st set of dates from Feb & March i.e, 21st day
pred <- predict(basic_feature_model,test_set_feb_basic)
(mse_test_basic_feb <- round(mean((test_set_feb_basic$V281 - pred)^2),2))
(sd_test_basic_feb <- round(sd(test_set_feb_basic$V281 - pred),2))

pred <- predict(basic_feature_model,test_set_march_basic)
(mse_test_basic_march <- round(mean((test_set_march_basic$V281 - pred)^2),2))
(sd_test_basic_march <- round(sd(test_set_march_basic$V281 - pred),2))

# Testing with 2nd dates in each of the Feb and March i.e, 3rd day 

pred <- predict(basic_feature_model,test_set_feb_basic_2)
(mse_test_basic_feb_2 <- round(mean((test_set_feb_basic_2$V281 - pred)^2),2))
(sd_test_basic_feb_2 <- round(sd(test_set_feb_basic_2$V281 - pred),2))

pred <- predict(basic_feature_model,test_set_march_basic_2)
(mse_test_basic_march_2 <- round(mean((test_set_march_basic_2$V281 - pred)^2),2))
(sd_test_basic_march_2 <- round(sd(test_set_march_basic_2$V281 - pred),2))

#***************EXPERIMENT 1 ENDS*********

#**************EXPERIMENT 2 STARTS*********

train_set_textual_feature <- base_train_set[,c(63:262,281)] # Picking only the relavent features
train_set_textual_feature <- as.data.frame(train_set_textual_feature)
dim(train_set_textual_feature)
head(train_set_textual_feature)

test_set_feb_textual <- test_set_feb[,c(63:262,281)]
head(test_set_feb_textual)
test_set_march_textual <- test_set_march[,c(63:262,281)]
head(test_set_march_textual)

test_set_feb_textual_2 <- test_set_feb_2[,c(63:262,281)]
head(test_set_feb_textual_2)
test_set_march_textual_2 <- test_set_march_2[,c(63:262,281)]
head(test_set_march_textual_2)

textual_feature_model <- lm(V281 ~ ., data = train_set_textual_feature)
summary(textual_feature_model_1)
residuals <- residuals(textual_feature_model)
(mse_train_textual <- round(mean(residuals^2),2))
(sd_train_textual <- round(sd(residuals),2))

pred <- predict(textual_feature_model,test_set_feb_textual)
(mse_test_textual_feb <- round(mean((test_set_feb_textual$V281 - pred)^2),2))
(sd_test_textual_feb <- round(sd(test_set_feb_textual$V281 - pred),2))

pred <- predict(textual_feature_model,test_set_march_textual)
(mse_test_textual_march <- round(mean((test_set_march_textual$V281 - pred)^2),2))
(sd_test_textual_march <- round(sd(test_set_march_textual$V281 - pred),2))

pred <- predict(textual_feature_model,test_set_feb_textual_2)
(mse_test_textual_feb_2 <- round(mean((test_set_feb_textual_2$V281 - pred)^2),2))
(sd_test_textual_feb_2 <- round(sd(test_set_feb_textual_2$V281 - pred),2))

pred <- predict(textual_feature_model,test_set_march_textual_2)
(mse_test_textual_march_2 <- round(mean((test_set_march_textual_2$V281 - pred)^2),2))
(sd_test_textual_march_2 <- round(sd(test_set_march_textual_2$V281 - pred),2))

#**************EXPERIMENT 2 ENDS*********

# SUMMARY OF LM FOR EXPERIMENT 1 & EXPERIMENT 2


(result_lm <- data.frame("Metric" = c("SD_TRAIN","MSE_TRAIN",
                                      "SD_TEST_FEB","MSE_TEST_FEB",
                                      "SD_TEST_MARCH","MSE_TEST_MARCH"),
                          "BASIC" = c(sd_train_basic,mse_train_basic,sd_test_basic_feb,
                                      mse_test_basic_feb,sd_test_basic_march,
                                      mse_test_basic_march),
                          "TEXTUAL"= c(sd_train_textual,mse_train_textual,sd_test_textual_feb,
                                        mse_test_textual_feb,sd_test_textual_march,
                                        mse_test_textual_march)))

(result_lm_sd <- data.frame("Metric" = c("SD_TRAIN","SD_TEST_FEB_1",
                                        "SD_TEST_FEB_2","SD_TEST_MARCH_1","SD_TEST_MARCH_2"),
                         "BASIC" = c(sd_train_basic,sd_test_basic_feb,sd_test_basic_feb_2,
                                     sd_test_basic_march,sd_test_basic_march_2),
                         "TEXTUAL"= c(sd_train_textual,sd_test_textual_feb,sd_test_textual_feb_2,
                                      sd_test_textual_march,sd_test_textual_march_2)))

(result_lm_mse <- data.frame("Metric" = c("MSE_TRAIN","MSE_TEST_FEB_1",
                                        "MSE_TEST_FEB_2","MSE_TEST_MARCH_1","MSE_TEST_MARCH_2"),
                           "BASIC" = c(mse_train_basic,mse_test_basic_feb,mse_test_basic_feb_2,
                                       mse_test_basic_march,mse_test_basic_march_2),
                           "TEXTUAL"= c(mse_train_textual,mse_test_textual_feb,mse_test_textual_feb_2,
                                        mse_test_textual_march,mse_test_textual_march_2)))                                    


# Exploring the GLMNET model aiming better fit & efficiency

# Experiment 1

library(glmnet)
x <- as.matrix(base_train_set[,51:60])
head(x)
y <-  as.matrix(base_train_set[281])
head(y)

basic_train_glmnet <- glmnet(x, y, family = "gaussian", alpha = 0)
basic_train_glmnet
summary(basic_train_glmnet)

# Testing on Feb & March Sets
pred <- predict(basic_train_glmnet,as.matrix(test_set_feb_basic[,1:10]), type = "link", s = 1.69)
pred
(glmnet_mse_basic_feb <- round(mean((test_set_feb_basic$V281 - pred)^2),2))

pred <- predict(basic_train_glmnet,as.matrix(test_set_march_basic[,1:10]), type = "link", s = 1.69)
(glmnet_mse_basic_march <- round(mean((test_set_march_basic$V281 - pred)^2),2))

pred <- predict(basic_train_glmnet,as.matrix(test_set_feb_basic_2[,1:10]), type = "link", s = 1.69)
pred
(glmnet_mse_basic_feb_2 <- round(mean((test_set_feb_basic_2$V281 - pred)^2),2))

pred <- predict(basic_train_glmnet,as.matrix(test_set_march_basic_2[,1:10]), type = "link", s = 1.69)
(glmnet_mse_basic_march_2 <- round(mean((test_set_march_basic_2$V281 - pred)^2),2))

# EXPERIMENT 2

x <- as.matrix(base_train_set[,63:262])
head(x)
y <-  as.matrix(base_train_set[281])
head(y)

textual_train_glmnet <- glmnet(x, y, family = "gaussian", alpha = 0)
textual_train_glmnet
summary(textual_train_glmnet)

# Testing 

pred <- predict(textual_train_glmnet,as.matrix(test_set_feb_textual[,1:200]), type = "link", s = 1.69)
(glmnet_mse_textual_feb <- round(mean((test_set_feb_textual$V281 - pred)^2),2))

pred <- predict(textual_train_glmnet,as.matrix(test_set_march_textual[,1:200]), type = "link", s = 1.69)
(glmnet_mse_textual_march <- round(mean((test_set_march_textual$V281 - pred)^2),2))

pred <- predict(textual_train_glmnet,as.matrix(test_set_feb_textual_2[,1:200]), type = "link", s = 1.69)
(glmnet_mse_textual_feb_2 <- round(mean((test_set_feb_textual_2$V281 - pred)^2),2))

pred <- predict(textual_train_glmnet,as.matrix(test_set_march_textual_2[,1:200]), type = "link", s = 1.69)
(glmnet_mse_textual_march_2 <- round(mean((test_set_march_textual_2$V281 - pred)^2),2))

(result_glmnet <- data.frame("Metric" = c("MSE_TEST_FEB_1","MSE_TEST_FEB_2"),
                         "BASIC" = c(glmnet_mse_basic_feb,glmnet_mse_basic_feb_2),
                         "TEXTUAL"= c(glmnet_mse_textual_feb,glmnet_mse_textual_feb_2)))

(result_glmnet_2 <- data.frame("Metric" = c("MSE_TEST_MARCH_1","MSE_TEST_MARCH_2"),
                             "BASIC" = c(glmnet_mse_basic_march,glmnet_mse_basic_march_2),
                             "TEXTUAL"= c(glmnet_mse_textual_march,glmnet_mse_textual_march_2)))
