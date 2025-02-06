library(ggplot2)
library(GGally)
library(reshape2)
library(gridExtra)


df <- read.csv("medical_costs.csv")

# average medical cost by age
age <- df$Age
avg_medical_age <- aggregate(cost ~ Age, data = df, FUN = mean)
cost <- df$Medical.Cost

ggplot(avg_medical_age, aes(x=Age, y=cost)) +
  geom_line(color="darkgreen") +
  geom_point(color = "darkgreen") +
  labs(title = "Average Medical Cost by Age", x = "Age", y = "Average Medical Cost") + 
  theme_minimal() + 
  theme(panel.grid.major = element_line(color = "gray"))

ggplot(avg_medical_age, aes(x=Age, y=cost)) +
  geom_histogram(color="black", fill = "lightblue") +
  labs(title = "Average Medical Cost by Age", x = "Age", y = "Average Medical Cost") + 
  theme_minimal() + 
  theme(panel.grid.major = element_line(color = "gray"))




bmi <- df$BMI
avg_medical_bmi <- aggregate(cost ~ bmi, data = df, FUN= mode)
