pASGHit <- read.csv("~/Downloads/yakker23 (1).csv")
# Main yakkertech file with events from each game

# Define the names to filter for just these All Star pitchers
desired_names <- c("Griffin Baker", "Grant Larson", "Reymin Guduan", "Lance Lusk", "Frank Moscatiello", 
                   "Robbie Hitt", "Tucker Smith", "Matt Vogel", "Mark Moclair",
                   "Cole Cook", "Leoni De La Cruz", "Kobey Schlotman", "Collin Sullivan",
                   "Henry Omana", "Nate Florence", "Tim Holdgrafer", "Edgar Martinez", "Garrett Christman")

# Filter the data based on the "Pitcher" column and the desired names
filtered_data <- subset(pASGHit, Pitcher %in% desired_names)
library(dplyr)
# Filter the data based on the "Pitcher" column with fastball velocity 
# and select the specific columns
filtered_data <- filtered_data %>%
  filter(PitchClass == "Fastballs") %>%
  select(Pitcher, TaggedPitchType, PitchClass, RelSpeed, SpinRate)

library(dplyr)

# Calculate the average RelSpeed for each pitcher
summary_data <- filtered_data %>%
  filter(TaggedPitchType %in% c("Fastball", "Sinker")) %>%
  group_by(Pitcher) %>%
  summarise(FastballAvg = mean(RelSpeed, na.rm = TRUE))

# Create a new data frame with each unique pitcher
pitcher_data <- data.frame(Pitcher = unique(summary_data$Pitcher), stringsAsFactors = FALSE)

# Merge the data frames to add the "PitcherAvg" and "FastballAvg" columns
merged_data <- merge(pitcher_data, summary_data, by = "Pitcher", all.x = TRUE)

# Fill missing values in "FastballAvg" with NA
merged_data$FastballAvg[is.nan(merged_data$FastballAvg)] <- NA

# Add a new column "League" and fill it with "Frontier" to define when merging 
# data
merged_data$League <- "Frontier"
merged_data[1, "FastballAvg"] <- 86.7
# Had to manually fix on player whose average was filled with misreads

# Read csv from MLB Statcast
pASGMLB <- read.csv("~/Downloads/pitch_arsenals.csv")

# Select All Stars
MLB_names <- c("Ohtani", "Cole", "Castillo", "Gray", "Eovaldi", 
                   "Gausman", "McClanahan", "Valdez", "Lorenzen",
                   "Kirby", "López", "Romano", "Jansen",
                   "Clase", "Bautista", "Cano", "Estévez", "Gallen",
               "Strider", "Elder", "Steele", "Keller", "Gray", "Kershaw",
               "Stroman", "Senga", "Cobb", "Burnes", "Díaz", "Hader", "Williams",
               "Doval", "Bednar", "Kimbrel")
filtered_mlb <- subset(pASGMLB, last_name %in% MLB_names)

# Delete rows where last names are duplicated
rows_to_delete <- c(40, 17, 21, 26, 39)
# Delete the specified rows from the data frame
filtered_mlb <- filtered_mlb %>%
  slice(-rows_to_delete)

# Filter by fastball velocity
filtered_mlb <- filtered_mlb %>%
  select(first_name, last_name, ff_avg_speed, si_avg_speed)
filtered_mlb <- filtered_mlb %>%
  rename(Pitcher = last_name)
# Replace NA values in fastball average with values from sinker average
filtered_mlb$ff_avg_speed[is.na(filtered_mlb$ff_avg_speed)] <- filtered_mlb$si_avg_speed[is.na(filtered_mlb$ff_avg_speed)]
filtered_mlb <- filtered_mlb %>%
  rename(FastballAvg = ff_avg_speed)

# Add the value "MLB" to define the two leagues when merging the data sets
filtered_mlb$League <- "MLB"
filtered_mlb <- filtered_mlb %>%
  select(-first_name, -si_avg_speed)

# Merge the data frames
merged_pitchers <- merge(filtered_mlb, merged_data, by = "Pitcher", all = TRUE)

# Rename the columns to remove  suffixes
merged_pitchers <- merged_pitchers %>%
  rename(FastballAvg = FastballAvg.x, League = League.x)

#merged_pitchers$FastballAvg[is.na(merged_pitchers$FastballAvg)] <- merged_pitchers$FastballAvg.y[is.na(merged_pitchers$FastballAvg)]

# Remove additional suffixes and place repeated data in the proper name
merged_pitchers$League[is.na(merged_pitchers$League)] <- merged_pitchers$League.y[is.na(merged_pitchers$League)]
merged_pitchers$FastballAvg <- ifelse(is.na(merged_pitchers$FastballAvg), merged_pitchers$FastballAvg.y, merged_pitchers$FastballAvg)


library(ggplot2)
ggplot(merged_pitchers, aes(x = League, y = FastballAvg, fill = League)) +
  geom_boxplot() +
  labs(title = "Average Fastball Velocity for All-Star Pitchers", 
       x = "League", y = "Average Fastball Velocity", caption = "Sources: MLB Statcast and Yakkertech") +
  scale_fill_manual(values = c("Frontier" = "Purple", "MLB" = "Green")) + 
  theme(axis.text.x = element_blank()) + theme_bw() + 
  scale_y_continuous(breaks = seq(0, ceiling(max(merged_pitchers$FastballAvg)), 1))
