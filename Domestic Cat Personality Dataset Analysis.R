# Install the necessary libraries
library(dplyr)
library(ggplot2)

# Loading data
cat_data <-read.csv("C:/Users/ASUS/Downloads/Cat_personality_data.csv", header=TRUE, sep=";",encoding="UTF-8", check.names = FALSE, stringsAsFactors = FALSE) # Read .csv file

# Display the first few lines
head(cat_data)

# Display the last few lines
tail(cat_data)

# Missing value check
missing_values <- sum(is.na(cat_data))
missing_values

# Visualize the number of cats by gender
ggplot(cat_data, aes(x = Cat_sex)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Number of Cats by Gender", x = "Gender", y = "Frequency")

# Number of NA values
na_count <- sum((cat_data$Cat_sex != "Female" & cat_data$Cat_sex != "Male") & cat_data$Cat_sex != "Unsure" )
cat("Number of NA values: ", na_count, "\n")

# number of "Unsure" values
unsure_count <- sum(cat_data$Cat_sex == "Unsure", na.rm = TRUE)
cat("Unsure number of values: ", unsure_count, "\n")

# Remove empty and "unsure" values
cleaned_data <- cat_data %>%
  filter(cat_data$Cat_sex == "Female" | cat_data$Cat_sex == "Male")

# Checking the remaining data
cat("Number of rows in the cleaned dataset: ", nrow(cleaned_data), "\n")

# View the Cat_sex column
cat_data$Cat_sex
cleaned_data$Cat_sex

cats_data <- cleaned_data

# The number of cats by gender
table(cat_data$"Cat_sex")
table(cleaned_data$"Cat_sex")

summary(cats_data)


# Visualize the number of cats by gender
ggplot(cats_data, aes(x = Cat_sex)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Number of Cats by Gender", x = "Gender", y = "Frequency")

# Histogram of the vigilant personality score
ggplot(cats_data, aes(x = Personality1_Vigilant)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Vigilant Personality Trait Distribution", x = "Score", y = "Frequency")

# 2-Model Selection:

# 1. GLM (Generalized Linear Model)
cats_data$Cat_sex <- ifelse(cats_data$Cat_sex == "Female", 1, 0)

# GLM: Logistic Regression - modeling cat_sex with the whole Personality
glm_logit <- glm(Cat_sex ~ ., 
                 family = binomial(link = "logit"), 
                 data = cats_data)
summary(glm_logit)

# GLM: Probit Regression - modeling cat_sex with the whole Personality
glm_probit <- glm(Cat_sex ~ ., 
                  family = binomial(link = "probit"), 
                  data = cats_data)
summary(glm_probit)

# GLM: Logistic Regression - modeling cat_sex with Personality51_Shy, Personality39_Calm, Personality31_BullyingPersonality19_Fearful_other_cats, Personality22_Friendly_other_cats, Personality21_Greedy, Personality24_Dominant and Country

log_model1 <- glm(Cat_sex ~ Personality51_Shy + Personality17_Smart + Personality31_Bullying, 
                  data = cats_data, 
                  family = "binomial")
summary(log_model1)

log_model2 <- glm(Cat_sex ~ Personality19_Fearful_other_cats + Personality22_Friendly_other_cats,
                  data = cats_data, 
                  family = "binomial")
summary(log_model2)


log_model3 <- glm(Cat_sex ~ Personality19_Fearful_other_cats + Personality22_Friendly_other_cats + Personality21_Greedy + Personality24_Dominant + Country, 
                 family = binomial(link = "logit"), 
                 data = cats_data)
summary(log_model3)

# GLM: Probit Regression - modeling cat_sex with Personality19_Fearful_other_cats, Personality22_Friendly_other_cats, Personality21_Greedy, Personality24_Dominant and Country
probit_model3 <- glm(Cat_sex ~ Personality19_Fearful_other_cats + Personality22_Friendly_other_cats + Personality21_Greedy + Personality24_Dominant + Country, 
                  family = binomial(link = "probit"), 
                  data = cats_data)
summary(probit_model3)

# AIC Performance Comparison
AIC(glm_logit) # Logit Model
AIC(glm_probit) # Probit Model
AIC(log_model1)
AIC(log_model2)
AIC(log_model3)
AIC(probit_model3)

