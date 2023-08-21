fASGHit <- read.csv("~/Downloads/FrontierAllStarHitters.csv")
fASGHit <- fASGHit[!(is.na(fASGHit$ExitSpeed) | is.na(fASGHit$launch_angle) | is.na(fASGHit$Distance)), ]
mASGHit <- read.csv("~/Downloads/MLBev.csv")
# Adding "League" column to fASGHit with value "Frontier"
fASGHit$League <- "Frontier"
 
# Adding "League" column to mASGHit with value "MLB"
mASGHit$League <- "MLB"
# Create a new data frame "fASGSummary" with unique player names
fASGSummary <- data.frame(last_name = unique(fASGHit$Batter), stringsAsFactors = FALSE)

# Calculate average ExitSpeed for each player
fASGSummary$AvgExitVelo <- aggregate(ExitSpeed ~ Batter, data = fASGHit, mean)$ExitSpeed

# Calculate average Distance for each player
fASGSummary$avg_distance <- aggregate(Distance ~ Batter, data = fASGHit, mean)$Distance

# Calculate average Launch Angle for each player
fASGSummary$avg_launch <- aggregate(launch_angle ~ Batter, data = fASGHit, mean)$launch_angle

# Define the list of desired batter names
desired_names <- c("Rusber Estrada", "Sicnarf Loopstok", "Zak Whalin", "Keon Barnum", "Juremi Profar", "Tucker Nathans", "Cito Culver", "James Nelson", "J.R. DiSarcina", "Matt McDermott", "Ti'Quan Forbes", "Josh Rehwaldt", "Pat Kivlehan", "Edwin Mateo", "Rodrigo Orozco", "Dakota Phillips", "Blake Grant-Parks", "Melvin Novoa", "Brennan Price", "Peter Zimmermannn", "Chase Dawson", "Gabe Holt", "D.J. Stewart", "Jomar Reyes", "Abdiel Diaz", "Jarrod Watking", "Andrew Penner", "Jeffrey Baez", "Jairus Richards", "Gaige Howard")

# Filter the fASGSummary data frame to include only desired batters
filtered_fASGSummary <- subset(fASGSummary, last_name %in% desired_names)

library(ggplot2)
library(dplyr)

# Merge the data frames based on the "last_name" column
merged_df <- merge(filtered_fASGSummary, mASGHit, by = "last_name", all = TRUE)

# Combine the "AvgExitVelo.x" and "AvgExitVelo.y" columns into a single column "AvgExitVelo"
merged_df <- merged_df %>%
  mutate(AvgExitVelo = coalesce(AvgExitVelo.x, AvgExitVelo.y)) %>%
  select(-AvgExitVelo.x, -AvgExitVelo.y)

merged_df <- merged_df %>%
  mutate(AvgLaunchAngle = coalesce(avg_launch.x, avg_launch.y)) %>%
  select(-avg_launch.x, -avg_launch.y)

merged_df <- merged_df %>%
  mutate(AvgDistance = coalesce(avg_distance.x, avg_distance.y)) %>%
  select(-avg_distance.x, -avg_distance.y)

# Update the league column to replace null values with "Frontier"
merged_df$League <- ifelse(is.na(merged_df$League), "Frontier", merged_df$League)


# Manually calculated averages. The code I created for averages was producing
# incorrect values, even when ignoring yakkertech outliers. 
merged_df[13, "AvgExitVelo"] <- 84.33758402
merged_df[43, "AvgExitVelo"] <- 85.93
merged_df[47, "AvgExitVelo"] <- 86.64
merged_df[37, "AvgExitVelo"] <- 85.64232833
merged_df[54, "AvgExitVelo"] <-87.25597974
merged_df[58, "AvgExitVelo"] <-84.33758402
merged_df[14, "AvgExitVelo"] <-92.00562401
merged_df[42, "AvgExitVelo"] <- 86.89649346
merged_df[71, "AvgExitVelo"] <-94.64632491
merged_df[61, "AvgExitVelo"] <-86.00839109
merged_df[52, "AvgExitVelo"] <- 89.17661389
merged_df[39, "AvgExitVelo"] <-90.2902823
merged_df[64, "AvgExitVelo"] <-88.66572939
merged_df[20, "AvgExitVelo"] <-87.85680229
merged_df[58, "AvgLaunchAngle"] <-21.45714286
merged_df[1, "AvgDistance"] <-149.2051318
merged_df[52, "AvgDistance"] <-213.1600188
merged_df[23, "AvgDistance"] <-180.0861049
merged_df[58, "AvgDistance"] <-168.7934392
merged_df[18, "AvgDistance"] <-187.8029309
merged_df[33, "AvgDistance"] <-176.7507137
merged_df[39, "AvgDistance"] <-204.0869619
merged_df[26, "AvgDistance"] <-163.2197116
merged_df[23, "AvgLaunchAngle"] <-19.71653543
merged_df[20, "AvgLaunchAngle"] <-20.78846154
merged_df[39, "AvgLaunchAngle"] <-22.41818182
merged_df[46, "AvgLaunchAngle"] <-25.17283951
merged_df[19, "AvgLaunchAngle"] <-21.32666667
merged_df[14, "AvgLaunchAngle"] <-20.41509434
merged_df[33, "AvgLaunchAngle"] <-19.36190476
merged_df[18, "AvgLaunchAngle"] <-23.28472222
merged_df[27, "AvgLaunchAngle"] <-22.17834395
merged_df[13, "AvgLaunchAngle"] <-17.05405405
merged_df[38, "AvgLaunchAngle"] <-20.34090909
merged_df[64, "AvgLaunchAngle"] <-21.13492063
merged_df[17, "AvgLaunchAngle"] <-22.44751381
merged_df[70, "AvgLaunchAngle"] <-25.375
merged_df[42, "AvgLaunchAngle"] <-25.36
# Create a line graph for average launch angle
library(ggplot2)

# Numbers still seem incorrect. Could be inconsistency of yakkertech
# ggplot(merged_df, aes(x = League, y = AvgLaunchAngle, fill = League)) +
#   geom_boxplot() +
#   labs(title = "Average Launch Angle for All-Stars", x = "League", y = "Average Launch Angle") +
#   scale_fill_manual(values = c("Frontier" = "blue", "MLB" = "red")) +
#   theme(axis.text.x = element_blank())
# 



# Create a line graph for average distance


# Add a new column for labeling
merged_df <- merged_df %>%
  mutate(label = ifelse(row_number() %% 2 == 0, TRUE, FALSE))

# Subset the data points to be labeled
label_points <- c("Acuna Jr.", "Ohtani", "Judge", "Merrifield", "Arraez", "Brennan Price", "Tucker Nathans", "Juremi Profar", "Gaige Howard")

# Plot with average exit velocites.
ggplot(merged_df, aes(x = League, y = AvgExitVelo, fill = League)) +
  geom_boxplot() +
  labs(title = "Average Exit Velocity Box Plot for All-Stars", x = "League", y = "Average Exit Velo", caption = "Sources: MLB Statcast and Yakkertech") +
  scale_fill_manual(values = c("Frontier" = "blue", "MLB" = "red"))

# Save the plot with increased width
#ggsave("output.png", plot, width = 15, height = 6)


# Create a line graph for average distance. Do not trust these values either
# ggplot(merged_df, aes(x = League, y = AvgDistance, fill = League)) +
#   geom_boxplot() +
#   labs(title = "Average Projected Batted Ball Distance for All-Star Hitters", x = "League", y = "Average Batted Ball Distance") +
#   scale_fill_manual(values = c("Frontier" = "blue", "MLB" = "red")) +
#   theme(axis.text.x = element_blank())

