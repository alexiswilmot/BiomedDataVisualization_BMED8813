# install ggplot 2
install.packages("ggplot2")
# loading the libraries:
library(ggplot2)
library(scales)
# loading the data to a dataframe:
medical_data <- read.csv("medical_costs.csv")

# Q1 - visualizations in R:

# pi chart with smoker vs non-smokers:
smoker_counts <- table(medical_data$Smoker)
print(smoker_counts)
smoker_percentages <- prop.table(smoker_counts) * 100
print(smoker_percentages)
print(table)
smoker_df <- as.data.frame(smoker_counts)
names(smoker_df) <- c("Smoker", "count")
smoker_df_percentage <- (smoker_df$count / sum(smoker_df$count) ) * 100


{ 
  p <- ggplot(smoker_df, aes(x = "", y = count, fill = Smoker)) + 
    geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) +
    theme_void() + 
    labs(title = "Proportion of Smokers vs. Non-Smokers") +
    geom_text(aes(label = paste0(round(smoker_df_percentage, 1), "%")),
              position = position_stack(vjust = 0.5), 
              color = "black", size = 5) + 
    # Add yes/no labels outside
    #geom_text(aes(x = 1.6, y = cumsum(count) - count / 2, label = Smoker), 
              #size = 5, color = "black") +
  theme(legend.position = "right")
  print(p)
}

# proportion of smoker vs non-smoker by region:

prop_data <- table(medical_data$Region, medical_data$Smoker)
prop_data <- prop.table(prop_data, margin = 1)
prop_data <- as.data.frame(prop_data)
colnames(prop_data) <- c("region", "smoker", "Freq")
{
  p <- ggplot(prop_data, aes(x = region, y = Freq, fill = smoker)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("no" = "lightblue", "yes" = "salmon")) +
    labs(title = "Proportion of Smokers vs. Non-Smokers by Region",
         y = "Proportion",
         x = "Region") +
    theme_minimal() 
  print(p)
}
# average medical cost by age:
avg_cost_by_age <- aggregate(Medical.Cost ~ Age, data = medical_data, FUN = mean)
{
p <- ggplot(avg_cost_by_age, aes(x = Age, y = Medical.Cost)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_point(color = "forestgreen", size = 2) +
  labs(title = "Average Medical Cost by Age",
       x = "Age",
       y = "Average Medical Cost") +
  scale_y_continuous(limits = c(10000, 13500),
                     breaks = seq(10000, 13500, by = 500)) +
  theme_minimal()
  print(p)
}
# Q2 - visualizations with medical costs

# histogram with medical costs across ages:
{
  p <- ggplot(medical_data, aes(x=Age)) + 
    geom_histogram(fill = "lightblue", color = "black") + 
    labs(title = paste("Distribution of Medical Costs Across Ages"), 
         x = "Age (years)", y = "Total Medical Costs ($)") +
      theme_minimal()
  print(p)
}

# attempt 2: 
library(dplyr)
library(scales)
total_costs_by_age <- medical_data %>%
  group_by(Age) %>%
  summarise(total_medical_costs = sum(Medical.Cost, na.rm = TRUE))
# Create histogram of medical costs
{ p <- ggplot(total_costs_by_age, aes(x = Age, y = total_medical_costs)) +
    geom_col(fill = "lightblue", color = "black") +
    labs(title = "Distribution of Medical Costs Across Ages",
         x = "Age (years)",
         y = "Total Medical Costs ($)") +
    scale_y_continuous(labels = scales::dollar_format(),
                       limits = c(0, 6000000),
                       breaks = seq(0, 6000000, by = 2000000)) +
    theme_minimal()

  print(p)
  }

# attempt 2 to match original image from assignment
total_costs_by_age <- medical_data %>%
  mutate(Age = floor(Age/2)*2) %>%  # Group ages into 2-year bins
  group_by(Age) %>%
  summarise(total_medical_costs = sum(Medical.Cost, na.rm = TRUE))

{p <- ggplot(total_costs_by_age, aes(x = Age, y = total_medical_costs)) +
  geom_col(fill = "lightblue", color = "black", width = 1.8) +  # Increased width to match target
  labs(title = "Distribution of Medical Costs Across Ages",
       x = "Age (years)",
       y = "Total Medical Costs ($)") +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0, 6000000),
                     breaks = seq(0, 6000000, by = 2000000)) +
  scale_x_continuous(breaks = seq(20, 60, by = 10)) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

print(p)

}
# scatter plot with medical cost by BMI:
{
  p <- ggplot(medical_data, aes_string(x = "BMI", y = "Medical.Cost")) + 
    geom_point(color = "salmon") + 
    labs(title = paste("Medical Costs by BMI"), x = "BMI", y = "Medical Costs ($)") +
    theme_minimal()
  print(p)
}

# box plot comparing medical costs across regions:
{
  p <- ggplot(medical_data, aes_string(x = "Region", y = "Medical.Cost")) + 
    geom_boxplot(fill = "lightblue", color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = paste("Distribution of Medical Costs by Region"), x = "Region", y = "Medical Costs ($)") +
    theme_minimal()
  print(p)
}

