# Polynomial Regression ---------------------------------------------------
library(tidyverse)
library(caTools)


# import the datasrt
dataset <- read.csv("Position_Salaries.csv")
dataset

# check the data for missing values

dataset %>% 
  summarise_all(~ sum(is.na(.)))

# Position is going to be dropped and we are left with 
# Salary which is the dependent value and Level as independent value

dataset1 <- dataset %>% 
  select(
    Level, Salary
  )
dataset1

# Splitting the dataset into training and test set
# not this time because the dataset is small and bidimensional

# set.seed(123)
# split <- sample.split(dataset1$Level, SplitRatio = 0.8)
# training <- subset(dataset1, split == TRUE)
# test <- subset(dataset1, split == FALSE)

# Feature scaling
# training[,1:2] <- scale(training[,1:2])
# test[,1:2] <- scale(test[,1:2])

# polynomial regression is a multiple simple linear regression with polynomial terms

# Fitting Linear Regression to the dataset -----------------------------
lin_reg <- lm(formula = Salary ~ ., data = dataset1)
summary(lin_reg)

# Fitting Polynomial Regression to the dataset ----------------------------
# create the polynomial terms by rising Level at power of 2 ,3 or more

dataset2 <- dataset1 %>% 
  mutate(
    Level2 = Level^2,
    Level3 = Level^3,
    Level4 = Level^4
  )
dataset2

 # Constructing the Polynomial Regression

poly_reg <- lm(formula = Salary ~ ., 
               data = dataset2)
summary(poly_reg)

# Visualising the Linear Regression results -------------------------------

ggplot()+
  geom_point(aes(dataset2$Level,dataset2$Salary), 
             color = '#890000',
             shape = 21,
             size = 3)+
  geom_line(aes(dataset2$Level, y = predict(lin_reg, newdata = dataset2)),
            color = '#002949',
            size = 0.8)+
  labs(
    title = "Linear Regression",
    subtitle = "by Paul Juverdeanu",
    x = "Level",
    y = "Salary"
  )+
  theme_light()+

# Visualising the Polynomial Regression results ---------------------------

ggplot()+
  geom_point(aes(dataset2$Level,dataset2$Salary), 
             color = '#890000',
             shape = 21,
             size = 3)+
  geom_line(aes(dataset2$Level, y = predict(poly_reg, newdata = dataset2)),
            color = '#002949',
            size = 0.8)+
  labs(
    title = "Polynomial Regression",
    subtitle = "by Paul Juverdeanu",
    x = "Level",
    y = "Salary"
  )+
  theme_light()+
  theme(plot.title = element_text(face = "bold", size = 14, color = '#890000'),
        plot.subtitle = element_text(face = "italic", size = 8, color = '#002949'),
        axis.title.x = element_text(face = "bold", size = 10, color = '#890000'),
        axis.title.y = element_text(face = "bold", size = 10, color = '#890000'))

# Predicting a new result with Linear Regression --------------------------
# we have to create a data frame with a single value that we want to check

y_pred <- predict(lin_reg, data.frame(Level = 6.5))
y_pred


# Predicting a new result with Polynomial Regression ----------------------

y_pred_poly <- predict(poly_reg, data.frame(Level = 6.5,
                                            Level2 = 6.5^2,
                                            Level3 = 6.5^3,
                                            Level4 = 6.5^4))
y_pred_poly







































































































