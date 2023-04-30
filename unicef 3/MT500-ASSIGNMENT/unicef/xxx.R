#. check working directory
# getwd
# import .csv file to Global Enviroment
birthsunder2.5 <- read.csv("unicef_indicator_1 (1).csv", header = TRUE, sep = "," )
antenatalcare <- read.csv("unicef_indicator_2 (1).csv", header = TRUE, sep = ",")
metadata <- read.csv("unicef_metadata.csv", header = TRUE, sep = ",")
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(dplyr)
library(readr)
birthsunder2.5 <- read.csv("unicef_indicator_1 (1).csv")
antenatalcare <- read.csv("unicef_indicator_2 (1).csv")
metadata <-  read.csv("unicef_metadata.csv")
library(readr)
# Births under 2.5kg by country
births_by_country <- birthsunder2.5 %>%
  group_by(country) %>%
  summarize(total_births = sum(obs_value))
# Get world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")
# Join the birth data from uniced to the world map data and plot
merged_data <- world_map %>%
  left_join(births_by_country, by = c("sovereignt" = "country"))
ggplot() +
  geom_sf(data = merged_data, aes(fill = total_births), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Births under 2500g by country") +
  theme_void()
births_by_country <- birthsunder2.5%>%
  group_by(country) %>%
  summarize(total_births = sum(obs_value)) %>%
  arrange(desc(total_births)) 
top_10 <- head(births_by_country, 10) 
print(top_10)
# Antenatal care attended by women worlwide during pregnancy
library(ggplot2)
library(dplyr)
# Grouping country antenatal
avg_care_by_country <- antenatalcare %>%
  group_by(country) %>%
  summarize(avg_care_percent = mean(obs_value)) %>%
  arrange(avg_care_percent) 
bottom_10 <- avg_care_by_country %>%
  slice(1:10)
ggplot(bottom_10, aes(x = country, y = avg_care_percent)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "Country", y = "Avg. Antenatal Care Attendance (%)",
       title = "Average Antenatal Care Attendance by Country (Bottom 10)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Linear relationship of Military expenfiture and Life expectacy at birth
library(ggplot2)
ggplot(metadata, aes(x = Life.expectancy.at.birth..total..years., y = Military.expenditure....of.GDP.)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Life expectancy at birth, total (years)", y = "Military expenditure (% of GDP)",
       title = "Military Expenditure vs Life Expectancy") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Countries that fund military expenditure the most worlwide from 1960-2020
library(ggplot2)
military_metadata <- metadata[, c("country", "year", "Military.expenditure....of.GDP.")]
names(military_metadata) <- c("country", "year", "military_expenditure")

# Subset the military expenditure data for the top 10 countries in 2021 (excluding Kuwait)
top_10_countries_2021 <- head(subset(military_metadata, year == 2021 & country != "Kuwait")[order(subset(military_metadata, year == 2021 & country != "Kuwait")$military_expenditure, decreasing = TRUE),], 10)

# Split the military expenditure data into the top 10 countries over time that spend the most on military (excluding Kuwait)
# Plot time series chart of our results
military_top_10 <- subset(military_metadata, country %in% top_10_countries_2021$country & country != "Kuwait")
ggplot(military_top_10, aes(x = year, y = military_expenditure, color = country)) +
  geom_line() +
  labs(title = "Top 10 Countries by Military Expenditure Over Time (1960-2020, excluding Kuwait)",
       x = "Year", y = "Military Expenditure % of GDP")