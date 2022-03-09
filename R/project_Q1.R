####################################################
#########           Question #1            #########
####################################################

####################################### Housekeeping

set.seed(123456)

library(ggplot2)

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

#################################### Price vs Rating

################### Modeling (OLS) #################

### All Types (1-5)

lmod <- lm(price ~ rating, data = raw.data[, -15:-16])
T.all <- summary(lmod)
formula(lmod)
R.sq_T.all <- T.all$r.squared

### Individual Types (1, 2, 3, 4, 5)

lmod_T1 <- lm(price ~ rating, data = raw.data_T1)
T1 <- summary(lmod_T1)
R.sq_T1 <- T1$r.squared

lmod_T2 <- lm(price ~ rating, data = raw.data_T2)
T2 <- summary(lmod_T2)
R.sq_T2 <- T2$r.squared

lmod_T3 <- lm(price ~ rating, data = raw.data_T3)
T3 <- summary(lmod_T3)
R.sq_T3 <- T3$r.squared

lmod_T4 <- lm(price ~ rating, data = raw.data_T4)
T4 <- summary(lmod_T4)
R.sq_T4 <- T4$r.squared

lmod_T5 <- lm(price ~ rating, data = raw.data_T5)
T5 <- summary(lmod_T5)
R.sq_T5 <- T5$r.squared

### R-Squared Values (Chart)

R.sq.val <- rbind(R.sq_T.all,
                  R.sq_T1,
                  R.sq_T2,
                  R.sq_T3,
                  R.sq_T4,
                  R.sq_T5)
R.sq.val <- as.data.frame(R.sq.val)
colnames(R.sq.val) <- c("Value")
round(R.sq.val, 5)

################# Visual Examination ###############

ggplot(raw.data, 
       aes(x = rating, 
           y = price, 
           color = as.factor(type))) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Retail Price vs Customer Rating",
       x = "rating (# of stars)", 
       y = "price ($)",
       color = "type") +
  theme(legend.position = "bottom")

