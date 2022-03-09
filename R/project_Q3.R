# Name: Lisha Jiang
# RUID: 106008675
# Project | Question #3
# Due Date: December 12, 2021

####################################################
#########           Question #3            #########
####################################################

####################################### Housekeeping

set.seed(123456)

library(DescTools)
library(car)

############################## Import and Clean Data

raw.data <- read.csv("frozen_entree.csv")
# head(raw.data)

### Extracted by Type

raw.data_T1 <- raw.data[1:24, -13]
# head(raw.data_T1)
raw.data_T2 <- raw.data[25:67, -13]
# head(raw.data_T2)
raw.data_T3 <- raw.data[68:141, -13]
# head(raw.data_T3)
raw.data_T4 <- raw.data[142:191, -13]
# head(raw.data_T3)
raw.data_T5 <- raw.data[192:204, -13]
# head(raw.data_T5)

############################# 70/30 Train Test Split

### Type 1
set.seed(123456)
sample.size_T1 <- round(nrow(raw.data_T1)*.70)
# print(sample.size_T1)
index <- sample(seq_len(nrow(raw.data_T1)), size = sample.size_T1)
# print(index)
train.data_T1 <- raw.data_T1[index, ]
# head(train.data_T1)
test.data_T1 <- raw.data_T1[-index, ]
# head(test.data_T1)

### Type 2
set.seed(123456)
sample.size_T2 <- round(nrow(raw.data_T2)*.70)
# print(sample.size_T2)
index <- sample(seq_len(nrow(raw.data_T2)), size = sample.size_T2)
# print(index)
train.data_T2 <- raw.data_T2[index, ]
# head(train.data_T2)
test.data_T2 <- raw.data_T2[-index, ]
# head(test.data_T2)

### Type 3
set.seed(123456)
sample.size_T3 <- round(nrow(raw.data_T3)*.70)
# print(sample.size_T3)
index <- sample(seq_len(nrow(raw.data_T3)), size = sample.size_T3)
# print(index)
train.data_T3 <- raw.data_T3[index, ]
# head(train.data_T3)
test.data_T3 <- raw.data_T3[-index, ]
# head(test.data_T3)

### Type 4
set.seed(123456)
sample.size_T4 <- round(nrow(raw.data_T4)*.70)
# print(sample.size_T4)
index <- sample(seq_len(nrow(raw.data_T4)), size = sample.size_T4)
# print(index)
train.data_T4 <- raw.data_T4[index, ]
# head(train.data_T4)
test.data_T4 <- raw.data_T4[-index, ]
# head(test.data_T4)

### Type 5
set.seed(123456)
sample.size_T5 <- round(nrow(raw.data_T5)*.70)
# print(sample.size_T5)
index <- sample(seq_len(nrow(raw.data_T5)), size = sample.size_T5)
# print(index)
train.data_T5 <- raw.data_T5[index, ]
# head(train.data_T5)
test.data_T5 <- raw.data_T5[-index, ]
# head(test.data_T5)

### All Types
train.data <- rbind(train.data_T1, 
                    train.data_T2, 
                    train.data_T3, 
                    train.data_T4, 
                    train.data_T5)
# head(train.data)
test.data <- rbind(test.data_T1, 
                   test.data_T2, 
                   test.data_T3, 
                   test.data_T4, 
                   test.data_T5)
# head(test.data)

##################### Logistic Regression: Data Prep

### Real Meat (Type 2, 3) vs. Fake Meat (Type 5) ###

################### Training Data ##################

##### Training | Real Meat == TRUE (1)
temp <- rbind(train.data_T2, train.data_T3)
# head(temp)
temp <- temp[, -14:-15]
# head(temp)
type <- rep(1, nrow(temp))
# print(type)
real.meat.1 <- cbind(temp, type)
# head(real.meat.1)

##### Training | Fake Meat == FALSE (0)
temp <- train.data_T5[, -14:-15]
# head(temp)
type <- rep(0, nrow(temp))
# print(type)
real.meat.0 <- cbind(temp, type)
# head(real.meat.0)

##### Training | T/F Combined
data.train <- rbind(real.meat.0, real.meat.1) 
# head(data.train)

################### Testing Data ###################

##### Testing | Real Meat == TRUE (1)
temp <- rbind(test.data_T2, test.data_T3)
# head(temp)
temp <- temp[, -14:-15]
# head(temp)
type <- rep(1, nrow(temp))
# print(type)
real.meat.1 <- cbind(temp, type)
# head(real.meat.1)

##### Testing | Fake Meat == FALSE (0)
temp <- test.data_T5[, -14:-15]
# head(temp)
type <- rep(0, nrow(temp))
# print(type)
real.meat.0 <- cbind(temp, type)
# head(real.meat.0)

##### Testing | T/F Combined
data.test <- rbind(real.meat.0, real.meat.1) 
# head(data.test)

###################### Logistic Regression: Training

#################### Full Model ####################

logit.full <- glm(type ~ ., 
                  data = data.train, 
                  family = "binomial")
# summary(logit.full)

############ Null (Intercept-Only) Model ###########

logit.null <- glm(type ~ 1, 
                  data = data.train, 
                  family = "binomial")
# summary(logit.null)

########### Model Building: Forward Step ###########

### Forward Step
stpfrwd.AIC <- step(logit.null,
                    scope = list(lower = logit.null,
                                 upper = logit.full),
                    direction = "forward",
                    k = 2,
                    trace = FALSE)
formula(stpfrwd.AIC)

### Resulting Forward Step Model
stpfrwd.model <- glm(type ~ cholesterol + sat.fat, 
                     family = "binomial", 
                     data = data.train)
summary(stpfrwd.model)

### Pseudo R-Squared (McFadden)
PseudoR2(stpfrwd.model, which = "McFadden")

### Variance Inflation Factor (VIF)
vif(stpfrwd.model)

############# Logistic Regression: Testing (Forward)

############ Model Accuracy: Probability ###########

test.prob <- predict(stpfrwd.model, newdata = data.test, type = "response")
# round(test.prob, 5)

predicted.type <- ifelse(test.prob > 0.5, 1, 0)
# print(predicted.type)
observed.type <- data.test$type
# print(observed.type)
predicted.type == observed.type
mean(predicted.type == observed.type)

###### Logistic Regression: Interpretation (Forward)

#################### Odds Ratio ####################

odds.ratio <- exp(coef(stpfrwd.model))
odds.ratio <- as.data.frame(odds.ratio)
print(odds.ratio)
