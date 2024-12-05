# Load necessary libraries
library(ggplot2)
library(dplyr)
library(extrafont) # For custom fonts
library(RColorBrewer) # For better color palettes

# Read the data
data <- read.csv("Bar Diagrams/data.csv", header = TRUE)

# Filter data for high density and the specified types
filtered_data <- data %>%
  filter(density == "high" & type %in% c("without", "rickshaw", "car", "cng", "std_ped", "across_ped", "along_ped"))

# Calculate mean and standard deviation for speed and time
stats <- filtered_data %>%
  group_by(type) %>%
  summarise(
    speed_avg = mean(speed),
    speed_sd = sd(speed),
    time_avg = mean(time),
    time_sd = sd(time)
  )

# Color palette from RColorBrewer
colors <- brewer.pal(7, "Set1")  # Set3 provides a good set of 7 distinct colors

# Speed bar plot with error bars and average values displayed
speed_plot <- ggplot(stats, aes(x = type, y = speed_avg, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = speed_avg - speed_sd, ymax = speed_avg + speed_sd),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = round(speed_avg, 1), y = speed_avg / 2), # Display average in the middle of bars
    position = position_dodge(width = 0.8),
    size = 4,
    family = "Times New Roman",
    color = "black"
  ) +
  labs(
    title = "",
    x = "Type",
    y = "Speed",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    
    axis.text.x = element_text(size = 14, family = "Times New Roman", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, family = "Times New Roman"),
    axis.title.x = element_text(size = 16, family = "Times New Roman", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, family = "Times New Roman", margin = margin(r = 10)),
    plot.title = element_text(size = 18, family = "Times New Roman", face = "bold"),
    legend.position = "none",
    legend.direction = "vertical",
    legend.justification = c(0.5, 0.5),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 14, family = "Times New Roman"),
    plot.margin = margin(10, 10, 10, 10)
    
  ) +
  scale_fill_manual(values = setNames(colors, c("without", "rickshaw", "car", "cng", "std_ped", "across_ped", "along_ped")))

# Display the speed plot
print(speed_plot)

# Time bar plot with error bars and average values displayed
time_plot <- ggplot(stats, aes(x = type, y = time_avg, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = time_avg - time_sd, ymax = time_avg + time_sd),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = round(time_avg, 1), y = time_avg / 2), # Display average in the middle of bars
    position = position_dodge(width = 0.8),
    size = 4,
    family = "Times New Roman",
    color = "black"
  ) +
  labs(
    title = "",
    x = "Type",
    y = "Time",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, family = "Times New Roman", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, family = "Times New Roman"),
    axis.title.x = element_text(size = 16, family = "Times New Roman", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, family = "Times New Roman", margin = margin(r = 10)),
    plot.title = element_text(size = 18, family = "Times New Roman", face = "bold"),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.justification = c(0.5, 0.5),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 14, family = "Times New Roman"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_fill_manual(values = setNames(colors, c("without", "rickshaw", "car", "cng", "std_ped", "across_ped", "along_ped")))

# Display the time plot
print(time_plot)
