# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

# Load the dataset from the provided sample data
merged_data <- read.csv("C:/Users/Kashi/Desktop/Data Analysis/Merged_Data.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert Arrest.Date to date format
merged_data$Arrest.Date <- as.Date(merged_data$Arrest.Date, format = "%Y-%m-%d")

# Create a new column for crime type
merged_data$Crime.Type <- ifelse(merged_data$Drug.Related == TRUE, "Drug Crime",
                                 ifelse(merged_data$Arrest.Weapon != "None", "Weapon Crime", "Non-Weapon Crime"))

# Filter out rows with missing temperature data
merged_data <- merged_data %>% filter(!is.na(TMAX))

# Count the number of crimes for each crime type and temperature
crime_counts <- merged_data %>%
  group_by(Crime.Type, TMAX) %>%
  summarise(Count = n())

# Count the number of crimes for all crime types and temperature
all_crime_counts <- merged_data %>%
  group_by(TMAX) %>%
  summarise(Count = n())

# Add a new column for crime type "All Crime"
all_crime_counts$Crime.Type <- "All Crime"

# Combine the crime counts for all crime types and individual crime types
combined_crime_counts <- bind_rows(crime_counts, all_crime_counts)

# Count the number of days for each temperature value
temp_counts <- merged_data %>%
  group_by(TMAX) %>%
  summarise(Days = n_distinct(Arrest.Date))

# Join temp_counts with combined_crime_counts
combined_crime_counts <- combined_crime_counts %>%
  left_join(temp_counts, by = "TMAX") %>%
  mutate(Arrest_Rate = Count / Days)

# Calculate Pearson's correlation coefficients, p-values, and sample sizes for each crime type
correlations <- combined_crime_counts %>%
  group_by(Crime.Type) %>%
  summarise(Correlation = cor(TMAX, Arrest_Rate, method = "pearson"),
            P_Value = cor.test(TMAX, Arrest_Rate, method = "pearson")$p.value,
            Sample_Size = n())

# Print the correlation coefficients, p-values, and sample sizes
print(correlations)

# Customize the plot theme and aesthetics
plot_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray90"),
        panel.border = element_rect(color = "gray30", fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.margin = margin(10, 20, 10, 20))

# Define color palette using RColorBrewer
crime_colors <- brewer.pal(4, "Set1")

# Calculate the min and max temperature values rounded to the nearest 10
temp_min <- floor(min(combined_crime_counts$TMAX) / 10) * 10
temp_max <- ceiling(max(combined_crime_counts$TMAX) / 10) * 10

# Scatter plot of Drug Crime vs. Temperature with trend line
drug_crime_plot <- ggplot(combined_crime_counts %>% filter(Crime.Type == "Drug Crime"), aes(x = TMAX, y = Arrest_Rate)) +
  geom_point(color = "black", alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1.5, color = crime_colors[1]) +
  labs(title = "Drug Crime vs. Temperature",
       x = "Maximum Temperature (°F)",
       y = "Drug Arrest Rate") +
  scale_x_continuous(breaks = seq(temp_min, temp_max, by = 10)) +
  plot_theme

# Scatter plot of Weapon Crime vs. Temperature with trend line
weapon_crime_plot <- ggplot(combined_crime_counts %>% filter(Crime.Type == "Weapon Crime"), aes(x = TMAX, y = Arrest_Rate)) +
  geom_point(color = "black", alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1.5, color = crime_colors[2]) +
  labs(title = "Weapon Crime vs. Temperature",
       x = "Maximum Temperature (°F)",
       y = "Weapon Arrest Rate") +
  scale_x_continuous(breaks = seq(temp_min, temp_max, by = 10)) +
  plot_theme

# Scatter plot of Non-Weapon Crime vs. Temperature with trend line
non_weapon_crime_plot <- ggplot(combined_crime_counts %>% filter(Crime.Type == "Non-Weapon Crime"), aes(x = TMAX, y = Arrest_Rate)) +
  geom_point(color = "black", alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1.5, color = crime_colors[3]) +
  labs(title = "Non-Weapon Crime vs. Temperature",
       x = "Maximum Temperature (°F)",
       y = "Non-Weapon Arrest Rate") +
  scale_x_continuous(breaks = seq(temp_min, temp_max, by = 10)) +
  plot_theme

# Scatter plot of All Crime vs. Temperature with trend line
all_crime_plot <- ggplot(combined_crime_counts %>% filter(Crime.Type == "All Crime"), aes(x = TMAX, y = Arrest_Rate)) +
  geom_point(color = "black", alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1.5, color = crime_colors[4]) +
  labs(title = "All Crime vs. Temperature",
       x = "Maximum Temperature (°F)",
       y = "Arrest Rate") +
  scale_x_continuous(breaks = seq(temp_min, temp_max, by = 10)) +
  plot_theme

# Display the plots separately
print(drug_crime_plot)
print(weapon_crime_plot)
print(non_weapon_crime_plot)
print(all_crime_plot)