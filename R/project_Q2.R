####################################################
#########           Question #2            #########
####################################################

####################################### Housekeeping

set.seed(123456)

library(olsrr)

############################## Import and Clean Data

raw.data <- read.csv("frozen_entree.csv")
head(raw.data)

### Extracted by Type

raw.data_T1 <- raw.data[1:24, -13]
# head(raw.data_T1)
raw.data_T2 <- raw.data[25:67, -13]
# print(raw.data_T2)
raw.data_T3 <- raw.data[68:141, -13]
# print(raw.data_T3)
raw.data_T4 <- raw.data[142:191, -13]
# print(raw.data_T3)
raw.data_T5 <- raw.data[192:204, -13]
# print(raw.data_T5)

######################### Initial Visual Examination

pairs(raw.data[, -13:-16])

################### Regression Training: Price (OLS)

full.mod <- lm(price ~ ., data = raw.data[, -13:-16])
# summary(full.mod)
# formula(full.mod)
ols_step_forward_p(full.mod)
ols_step_backward_p(full.mod)
ols_step_best_subset(full.mod)

full.mod_T1 <- lm(price ~ ., data = raw.data_T1[, -13:-15])
# summary(full.mod_T1)
formula(full.mod_T1)
ols_step_forward_p(full.mod_T1)
ols_step_backward_p(full.mod_T1)
ols_step_best_subset(full.mod_T1)

full.mod_T2 <- lm(price ~ ., data = raw.data_T2[, -13:-15])
# summary(full.mod_T2)
# formula(full.mod_T2)
ols_step_forward_p(full.mod_T2)
ols_step_backward_p(full.mod_T2)
ols_step_best_subset(full.mod_T2)

full.mod_T3 <- lm(price ~ ., data = raw.data_T3[, -13:-15])
# summary(full.mod_T3)
# formula(full.mod_T3)
ols_step_forward_p(full.mod_T3)
ols_step_backward_p(full.mod_T3)
ols_step_best_subset(full.mod_T3)

full.mod_T4 <- lm(price ~ ., data = raw.data_T4[, -13:-15])
# summary(full.mod_T4)
# formula(full.mod_T4)
ols_step_forward_p(full.mod_T4)
ols_step_backward_p(full.mod_T4)
ols_step_best_subset(full.mod_T4)

full.mod_T5 <- lm(price ~ ., data = raw.data_T5[, -13:-15])
# summary(full.mod_T5)
# formula(full.mod_T5)
ols_step_forward_p(full.mod_T5)
ols_step_backward_p(full.mod_T5)
ols_step_best_subset(full.mod_T5)



