# VIDEO GAME SALES ANALYSIS 
# Load Libraries
library(ggplot2); 
library(dplyr); 
library(corrplot); 
library(gridExtra); 
library(caret)

cat("\n=== VIDEO GAME SALES ANALYSIS ===\n\n")

# 1. DATA IMPORT
games <- read.csv("vgsales.csv", stringsAsFactors = FALSE)
cat("Dataset loaded:", nrow(games), "rows,", ncol(games), "columns\n")
print(head(games))

# 2. DATA CLEANING
cat("\n=== DATA CLEANING ===\n")
cat("Missing values:\n"); print(colSums(is.na(games)))

games_clean <- games %>%
  filter(!is.na(Year), !is.na(Publisher), Year != "N/A", Publisher != "") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 1980, Year <= 2020) %>%
  distinct(Name, Platform, .keep_all = TRUE)

cat("Cleaned:", nrow(games_clean), "rows (removed", nrow(games) - nrow(games_clean), ")\n")

# 3. EXPLORATORY DATA ANALYSIS
cat("\n=== EDA SUMMARY ===\n")
cat("Total Games:", nrow(games_clean), "\n")
cat("Total Sales:", round(sum(games_clean$Global_Sales), 2), "M units\n")
cat("Avg Sales:", round(mean(games_clean$Global_Sales), 2), "M units\n\n")

# Genre Analysis
genre_stats <- games_clean %>%
  group_by(Genre) %>%
  summarise(Count = n(), Sales = sum(Global_Sales), Avg = mean(Global_Sales)) %>%
  arrange(desc(Sales))
cat("=== TOP GENRES ===\n"); print(genre_stats)

# Platform Analysis
platform_stats <- games_clean %>%
  group_by(Platform) %>%
  summarise(Count = n(), Sales = sum(Global_Sales)) %>%
  arrange(desc(Sales)) %>% head(10)
cat("\n=== TOP PLATFORMS ===\n"); print(platform_stats)

# Regional Sales
regional <- data.frame(
  Region = c("North America", "Europe", "Japan", "Other"),
  Sales = c(sum(games_clean$NA_Sales), sum(games_clean$EU_Sales), 
            sum(games_clean$JP_Sales), sum(games_clean$Other_Sales)))
regional$Percent <- round(regional$Sales/sum(regional$Sales)*100, 1)
cat("\n=== REGIONAL SALES ===\n"); print(regional)

# Top Publishers & Games
top_pubs <- games_clean %>% group_by(Publisher) %>% 
  summarise(Games = n(), Sales = sum(Global_Sales)) %>% 
  arrange(desc(Sales)) %>% head(10)
cat("\n=== TOP PUBLISHERS ===\n"); print(top_pubs)

top_games <- games_clean %>% select(Name, Platform, Year, Genre, Global_Sales) %>% 
  arrange(desc(Global_Sales)) %>% head(10)
cat("\n=== TOP GAMES ===\n"); print(top_games)

# 4. VISUALIZATION
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Genre Sales
p1 <- ggplot(genre_stats, aes(x = reorder(Genre, Sales), y = Sales, fill = Genre)) +
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() +
  labs(title = "Sales by Genre", x = "Genre", y = "Sales (M)") + theme_minimal()

# Regional Distribution
p2 <- ggplot(regional, aes(x = reorder(Region, -Sales), y = Sales, fill = Region)) +
  geom_bar(stat = "identity") + geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5) +
  labs(title = "Regional Sales", x = "Region", y = "Sales (M)") + 
  theme_minimal() + theme(legend.position = "none")

# Top Platforms
p3 <- ggplot(platform_stats, aes(x = reorder(Platform, Sales), y = Sales, fill = Platform)) +
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() +
  labs(title = "Top 10 Platforms", x = "Platform", y = "Sales (M)") + theme_minimal()

# Yearly Trend
yearly <- games_clean %>% group_by(Year) %>% summarise(Sales = sum(Global_Sales))
p4 <- ggplot(yearly, aes(x = Year, y = Sales)) +
  geom_line(color = "blue", size = 1.2) + geom_point(color = "red", size = 2) +
  labs(title = "Sales Trend Over Years", x = "Year", y = "Sales (M)") + theme_minimal()

# Top Publishers
p5 <- ggplot(head(top_pubs, 6), aes(x = reorder(Publisher, Sales), y = Sales, fill = Publisher)) +
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() +
  labs(title = "Top 6 Publishers", x = "Publisher", y = "Sales (M)") + theme_minimal()

# Sales Distribution
p6 <- ggplot(games_clean, aes(x = Global_Sales)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Sales Distribution", x = "Sales (M)", y = "Count") + theme_minimal()

# Display plots
print(p1); print(p2); print(p3); print(p4); print(p5); print(p6)
grid.arrange(p1, p2, p3, p4, ncol = 2)

# 5. REGRESSION ANALYSIS
cat("\n=== REGRESSION ANALYSIS ===\n")

# Prepare data
games_model <- games_clean %>%
  filter(Year >= 1995) %>%
  select(Global_Sales, NA_Sales, EU_Sales, JP_Sales, Year) %>%
  na.omit()

# Train-test split
set.seed(123)
idx <- createDataPartition(games_model$Global_Sales, p = 0.8, list = FALSE)
train <- games_model[idx, ]
test <- games_model[-idx, ]

cat("Training:", nrow(train), "| Testing:", nrow(test), "\n")

# Build model
model <- lm(Global_Sales ~ NA_Sales + EU_Sales + JP_Sales + Year, data = train)
cat("\n"); print(summary(model))

# Predictions
pred <- predict(model, test)
rmse <- sqrt(mean((test$Global_Sales - pred)^2))
mae <- mean(abs(test$Global_Sales - pred))
r2 <- cor(test$Global_Sales, pred)^2

cat("\n=== MODEL PERFORMANCE ===\n")
cat("RMSE:", round(rmse, 4), "M | MAE:", round(mae, 4), "M | R²:", round(r2, 4), "\n")

# Prediction Plot
p7 <- ggplot(data.frame(Actual = test$Global_Sales, Predicted = pred), 
             aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Sales", x = "Actual (M)", y = "Predicted (M)") +
  theme_minimal()
print(p7)

# 6. CLUSTERING & CORRELATION
cat("\n=== CORRELATION ANALYSIS ===\n")

# Correlation matrix
corr_data <- games_clean %>% 
  select(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) %>% 
  na.omit()
corr_matrix <- cor(corr_data)
print(round(corr_matrix, 3))

# Visualize correlation
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, title = "Regional Sales Correlation",
         mar = c(0, 0, 2, 0))

# Genre by region (Fixed NA column name)
genre_region <- games_clean %>%
  group_by(Genre) %>%
  summarise(North_America = sum(NA_Sales), Europe = sum(EU_Sales), 
            Japan = sum(JP_Sales), Other = sum(Other_Sales))
cat("\n=== GENRE SALES BY REGION ===\n"); print(genre_region)

# 7. KEY INSIGHTS
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Most Popular Genre:", genre_stats$Genre[1], "(", round(genre_stats$Sales[1], 2), "M)\n")
cat("2. Top Platform:", platform_stats$Platform[1], "(", round(platform_stats$Sales[1], 2), "M)\n")
cat("3. Largest Market:", regional$Region[which.max(regional$Sales)], "(", 
    regional$Percent[which.max(regional$Sales)], "%)\n")
cat("4. Model R²:", round(r2, 3), "- Regional sales predict global sales well\n")
cat("5. Best Year:", yearly$Year[which.max(yearly$Sales)], 
    "(", round(max(yearly$Sales), 2), "M sales)\n\n")

