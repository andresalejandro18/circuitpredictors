library(ggplot2)
library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(sf)
library(tigris)
library(dplyr)
library(glmnet)
library(pROC)
library(rnaturalearth)

# Descriptive Stats



# Map circuit countries to regions
# Note: country in circuits.csv is text (e.g., "UK", "Italy")
circuit_regions <- circuits %>%
  mutate(circuit_region = case_when(
    country %in% c("UK", "Germany", "Italy", "Spain", "France", "Belgium", 
                   "Austria", "Netherlands", "Portugal", "Monaco", "Hungary",
                   "Russia", "Turkey", "Azerbaijan", "Sweden", "Switzerland") ~ "Europe",
    country %in% c("USA", "Canada", "Mexico", "Brazil", "Argentina") ~ "Americas",
    country %in% c("Japan", "China", "Singapore", "Malaysia", "South Korea",
                   "India", "Bahrain", "UAE", "Saudi Arabia", "Qatar",
                   "South Africa", "Morocco") ~ "Asia",
    country %in% c("South Africa", "Morocco") ~ "Africa",
    country %in% c("Australia") ~ "Oceania",
    TRUE ~ "Other"
  ))

# Merging Data

f1_integrated <- results %>%
  # Join with races
  left_join(races %>% select(raceId, year, round, circuitId, name, date), 
            by = "raceId") %>%
  rename(raceName = name) %>%
  # Join with circuits
  left_join(circuit_regions %>% select(circuitId, circuitRef, name, 
                                       location, country, circuit_region, 
                                       lat, lng), 
            by = "circuitId") %>%
  rename(circuitName = name) %>%
  # Join with drivers
  left_join(driver_regions %>% select(driverId, driverRef, number, code,
                                      forename, surname, dob, nationality, 
                                      driver_region), 
            by = "driverId")

# Mapping current races
world <- ne_countries(scale = "medium", returnclass = "sf")
plot(st_geometry(world), main = "World Map")

f1_races <- data.frame(
  race = c("Australian GP", "Chinese GP", "Japanese GP", "Bahrain GP", 
           "Saudi Arabian GP", "Miami GP", "Emilia Romagna GP", "Monaco GP",
           "Spanish GP", "Canadian GP", "Austrian GP", "British GP",
           "Belgian GP", "Hungarian GP", "Dutch GP", "Italian GP",
           "Azerbaijan GP", "Singapore GP", "United States GP", "Mexico GP",
           "Brazil GP", "Las Vegas GP", "Qatar GP", "Abu Dhabi GP"),
  city = c("Melbourne", "Shanghai", "Suzuka", "Sakhir", 
           "Jeddah", "Miami", "Imola", "Monte Carlo",
           "Barcelona", "Montreal", "Spielberg", "Silverstone",
           "Spa", "Budapest", "Zandvoort", "Monza",
           "Baku", "Singapore", "Austin", "Mexico City",
           "São Paulo", "Las Vegas", "Lusail", "Abu Dhabi"),
  lon = c(144.968, 121.220, 136.535, 50.510,
          39.104, -80.239, 11.713, 7.420,
          2.261, -73.523, 14.765, -1.017,
          5.971, 19.249, 4.540, 9.281,
          49.843, 103.860, -97.641, -99.090,
          -46.697, -115.167, 51.454, 54.603),
  lat = c(-37.850, 31.339, 34.843, 26.033,
          21.632, 25.958, 44.344, 43.735,
          41.570, 45.500, 47.220, 52.071,
          50.437, 47.579, 52.389, 45.620,
          40.372, 1.291, 30.133, 19.404,
          -23.702, 36.161, 25.490, 24.467)
)

# Convert to sf object
f1_sf <- st_as_sf(f1_races, coords = c("lon", "lat"), crs = 4326)

# Create the plot
par(mar = c(2, 2, 3, 2))
plot(st_geometry(world), col = "lightgray", border = "white",
     main = "Formula 1 2025 Season")

# Add race points
plot(st_geometry(f1_sf), add = TRUE, col = "red", pch = 19, cex = 1.2)

# Add labels for selected races (to avoid overcrowding)
# You can adjust which races to label
selected_races <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24)
text(f1_races$lon[selected_races], 
     f1_races$lat[selected_races], 
     labels = f1_races$city[selected_races], 
     pos = 3, cex = 0.6, col = "darkred", font = 1)

# Add a legend
legend("bottomleft", 
       legend = c("Race Location"), 
       col = c("red"), 
       pch = 19, 
       cex = 0.8,
       bg = "white")

# Correlation Analysis and Prediction Analysis

cat("=== STARTING MODELING ANALYSIS ===\n")
cat("Total observations:", nrow(f1_integrated), "\n\n")

# ============================================================
# STEP 1: FEATURE ENGINEERING FOR MODELING
# ============================================================

cat("=== CREATING FEATURES FOR MODELING ===\n")

f1_features <- f1_integrated %>%
  mutate(
    # Target variables
    is_win = ifelse(positionOrder == 1, 1, 0),
    is_podium = ifelse(positionOrder %in% 1:3, 1, 0),
    is_points = ifelse(points > 0, 1, 0),
    is_top5 = ifelse(positionOrder %in% 1:5, 1, 0),
    
    # Geographic matching features
    is_home_country = ifelse(
      tolower(trimws(nationality)) == tolower(trimws(country)), 1, 0
    ),
    is_home_region = ifelse(
      driver_region == circuit_region & !is.na(driver_region) & !is.na(circuit_region), 
      1, 0
    ),
    
    # Flexible nationality-country matching
    is_home_match = case_when(
      nationality == "British" & country == "UK" ~ 1,
      nationality == "American" & country == "USA" ~ 1,
      nationality == "Dutch" & country == "Netherlands" ~ 1,
      tolower(nationality) == tolower(country) ~ 1,
      TRUE ~ 0
    ),
    
    # Performance metrics
    finished_race = ifelse(statusId == 1, 1, 0),
    positions_gained = ifelse(!is.na(grid) & grid > 0 & !is.na(positionOrder), 
                              grid - positionOrder, NA),
    
    # Qualifying performance
    started_from_pole = ifelse(grid == 1, 1, 0),
    started_top3 = ifelse(grid %in% 1:3, 1, 0),
    started_top10 = ifelse(grid %in% 1:10, 1, 0),
    
    # Driver age
    driver_age = as.numeric(difftime(as.Date(date), as.Date(dob), units = "days")) / 365.25
  ) %>%
  # Create cumulative career statistics
  arrange(driverId, date) %>%
  group_by(driverId) %>%
  mutate(
    career_race_number = row_number(),
    career_wins_before = lag(cumsum(is_win), default = 0),
    career_podiums_before = lag(cumsum(is_podium), default = 0),
    career_points_before = lag(cumsum(points), default = 0)
  ) %>%
  ungroup()

# Calculate driver statistics
driver_stats <- f1_features %>%
  group_by(driverId) %>%
  summarise(
    total_races = n(),
    win_rate = mean(is_win, na.rm = TRUE),
    podium_rate = mean(is_podium, na.rm = TRUE),
    avg_position = mean(positionOrder, na.rm = TRUE),
    .groups = "drop"
  )

# Merge driver stats back
f1_features <- f1_features %>%
  left_join(driver_stats, by = "driverId")

cat("Features created successfully!\n")
cat("Total rows with features:", nrow(f1_features), "\n\n")

# ============================================================
# STEP 2: DATA PREPARATION FOR MODELING
# ============================================================

cat("=== PREPARING DATA FOR MODELING ===\n")

modeling_data <- f1_features %>%
  filter(
    !is.na(is_podium), 
    !is.na(grid), 
    grid > 0,
    !is.na(driver_age),
    !is.na(career_race_number)
  ) %>%
  mutate(
    career_wins_before = ifelse(is.na(career_wins_before), 0, career_wins_before),
    career_podiums_before = ifelse(is.na(career_podiums_before), 0, career_podiums_before),
    career_points_before = ifelse(is.na(career_points_before), 0, career_points_before)
  )

cat("Clean observations:", nrow(modeling_data), "\n")
cat("Podium finishes:", sum(modeling_data$is_podium), "\n")
cat("Podium rate:", round(100 * mean(modeling_data$is_podium), 2), "%\n\n")

# ============================================================
# STEP 3: CORRELATION MATRIX ANALYSIS
# ============================================================

cat("=== CORRELATION MATRIX ANALYSIS ===\n\n")

# Select numeric features
numeric_features <- modeling_data %>%
  select(
    is_win, is_podium, is_points, positionOrder,
    is_home_match, is_home_region,
    grid, started_from_pole, started_top3, started_top10,
    positions_gained, finished_race,
    career_race_number, career_wins_before, career_podiums_before,
    career_points_before, win_rate, podium_rate,
    driver_age, year
  ) %>%
  select_if(is.numeric) %>%
  na.omit()

# Calculate correlation matrix
cor_matrix <- cor(numeric_features, use = "pairwise.complete.obs")

# Find strongest correlations with podium finish
podium_correlations <- cor_matrix[, "is_podium"] %>%
  sort(decreasing = TRUE)

cat("TOP 15 CORRELATIONS WITH PODIUM FINISH:\n")
print(round(head(podium_correlations, 15), 4))

cat("\nBOTTOM 5 CORRELATIONS (Negative):\n")
print(round(tail(podium_correlations, 5), 4))

# Highlight nationality-location correlations
cat("\n=== NATIONALITY-LOCATION CORRELATIONS ===\n")
cat("is_home_match correlation with is_podium:", 
    round(cor_matrix["is_home_match", "is_podium"], 4), "\n")
cat("is_home_region correlation with is_podium:", 
    round(cor_matrix["is_home_region", "is_podium"], 4), "\n")
cat("is_home_match correlation with is_win:", 
    round(cor_matrix["is_home_match", "is_win"], 4), "\n\n")

# Save correlation matrix plot
png("correlation_matrix.png", width = 1400, height = 1400, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         title = "F1 Feature Correlation Matrix",
         mar = c(0,0,2,0),
         addCoef.col = "black", number.cex = 0.6)
cat("✓ Correlation matrix saved as 'correlation_matrix.png'\n\n")

# ============================================================
# STEP 4: DETAILED NATIONALITY-LOCATION ANALYSIS
# ============================================================

cat("=== NATIONALITY-LOCATION RELATIONSHIP ===\n\n")

# Home vs Away performance
home_away_analysis <- modeling_data %>%
  group_by(is_home_match) %>%
  summarise(
    n = n(),
    wins = sum(is_win),
    podiums = sum(is_podium),
    points_finishes = sum(is_points),
    win_rate = 100 * mean(is_win),
    podium_rate = 100 * mean(is_podium),
    avg_position = mean(positionOrder, na.rm = TRUE),
    avg_grid = mean(grid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(race_type = ifelse(is_home_match == 1, "Home Race", "Away Race"))

print(home_away_analysis)

# Chi-square test
cat("\n=== CHI-SQUARE TEST: Home Match vs Podium ===\n")
home_podium_table <- table(modeling_data$is_home_match, modeling_data$is_podium)
print(home_podium_table)
chi_test <- chisq.test(home_podium_table)
print(chi_test)

if(chi_test$p.value < 0.05) {
  cat("\n✓✓✓ SIGNIFICANT correlation detected! (p < 0.05)\n")
} else {
  cat("\nNo significant correlation (p >= 0.05)\n")
}

# Odds ratio
odds_ratio <- (home_podium_table[2,2] / home_podium_table[2,1]) / 
  (home_podium_table[1,2] / home_podium_table[1,1])
cat("\nOdds Ratio (Home vs Away):", round(odds_ratio, 3))
cat("\nInterpretation: Home drivers are", round(odds_ratio, 2), 
    "times more/less likely to podium\n\n")

# Region analysis
cat("=== REGION MATCH ANALYSIS ===\n")
region_analysis <- modeling_data %>%
  group_by(is_home_region) %>%
  summarise(
    n = n(),
    podiums = sum(is_podium),
    podium_rate = 100 * mean(is_podium),
    avg_position = mean(positionOrder, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(race_type = ifelse(is_home_region == 1, "Home Region", "Away Region"))

print(region_analysis)

region_podium_table <- table(modeling_data$is_home_region, modeling_data$is_podium)
print(region_podium_table)
chi_test_region <- chisq.test(region_podium_table)
cat("\nChi-square test p-value:", format.pval(chi_test_region$p.value, digits = 3), "\n\n")

# ============================================================
# STEP 5: TRAIN-TEST SPLIT
# ============================================================

cat("=== TRAIN-TEST SPLIT ===\n")

split_year <- quantile(modeling_data$year, 0.8)
train_data <- modeling_data %>% filter(year <= split_year)
test_data <- modeling_data %>% filter(year > split_year)

cat("Training:", nrow(train_data), "obs (years <=", split_year, ")\n")
cat("Testing:", nrow(test_data), "obs (years >", split_year, ")\n\n")

# ============================================================
# STEP 6: LOGISTIC REGRESSION MODELS
# ============================================================

cat("=== BUILDING LOGISTIC REGRESSION MODELS ===\n\n")

# Model 1: Baseline
cat("Model 1: Baseline (grid only)\n")
model1 <- glm(is_podium ~ grid, 
              data = train_data, family = binomial())
print(model1)

# Model 2: Career stats
cat("Model 2: Grid + Career Stats\n")
model2 <- glm(is_podium ~ grid + career_wins_before + career_podiums_before + 
                career_race_number + win_rate,
              data = train_data, family = binomial())

# Model 3: Full with nationality-location
cat("Model 3: Full Model with Nationality-Location Features\n")
model3 <- glm(is_podium ~ grid + career_wins_before + career_podiums_before +
                career_race_number + win_rate + is_home_match + is_home_region +
                driver_age + started_from_pole + year,
              data = train_data, family = binomial())

cat("\n=== MODEL 3 COEFFICIENTS ===\n")
summary_model3 <- summary(model3)
print(summary_model3$coefficients)

# Check significance of nationality-location variables
if("is_home_match" %in% rownames(summary_model3$coefficients)) {
  home_coef <- summary_model3$coefficients["is_home_match", ]
  cat("\n=== HOME MATCH EFFECT ===\n")
  cat("Coefficient:", round(home_coef[1], 4), "\n")
  cat("Std. Error:", round(home_coef[2], 4), "\n")
  cat("P-value:", format.pval(home_coef[4], digits = 4), "\n")
  cat("Significant:", ifelse(home_coef[4] < 0.05, "YES ✓✓✓", "NO"), "\n")
  
  # Convert to odds ratio
  odds <- exp(home_coef[1])
  cat("Odds Ratio:", round(odds, 3), "\n")
  cat("Interpretation: Home match", 
      ifelse(odds > 1, "increases", "decreases"), 
      "podium odds by", round(abs(odds - 1) * 100, 1), "%\n")
}

if("is_home_region" %in% rownames(summary_model3$coefficients)) {
  region_coef <- summary_model3$coefficients["is_home_region", ]
  cat("\n=== HOME REGION EFFECT ===\n")
  cat("Coefficient:", round(region_coef[1], 4), "\n")
  cat("P-value:", format.pval(region_coef[4], digits = 4), "\n")
  cat("Significant:", ifelse(region_coef[4] < 0.05, "YES ✓✓✓", "NO"), "\n\n")
}

# ============================================================
# STEP 7: MODEL EVALUATION
# ============================================================

cat("=== MODEL EVALUATION ON TEST SET ===\n\n")

# Predictions
pred1 <- predict(model1, newdata = test_data, type = "response")
pred2 <- predict(model2, newdata = test_data, type = "response")
pred3 <- predict(model3, newdata = test_data, type = "response")

# ROC and AUC
roc1 <- roc(test_data$is_podium, pred1, quiet = TRUE)
roc2 <- roc(test_data$is_podium, pred2, quiet = TRUE)
roc3 <- roc(test_data$is_podium, pred3, quiet = TRUE)

cat("MODEL PERFORMANCE (AUC-ROC):\n")
cat("Model 1 (Baseline):               ", round(auc(roc1), 4), "\n")
cat("Model 2 (+ Career Stats):         ", round(auc(roc2), 4), "\n")
cat("Model 3 (+ Nationality-Location): ", round(auc(roc3), 4), "\n")
cat("\nImprovement from adding nationality:", 
    round(auc(roc3) - auc(roc2), 4), "\n\n")

# Confusion matrix for Model 3
pred3_class <- ifelse(pred3 > 0.5, 1, 0)
conf_matrix <- table(Predicted = pred3_class, Actual = test_data$is_podium)

cat("CONFUSION MATRIX (Model 3):\n")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nPERFORMACE METRICS:\n")
cat("Accuracy: ", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall:   ", round(recall, 4), "\n")
cat("F1-Score: ", round(f1_score, 4), "\n\n")

# Plot ROC curves
png("roc_curves.png", width = 900, height = 700, res = 110)
plot(roc1, col = "blue", lwd = 2, main = "ROC Curves - Podium Prediction")
lines(roc2, col = "green", lwd = 2)
lines(roc3, col = "red", lwd = 2)
legend("bottomright", 
       legend = c(paste("Model 1 (Baseline): AUC =", round(auc(roc1), 3)),
                  paste("Model 2 (+ Career): AUC =", round(auc(roc2), 3)),
                  paste("Model 3 (+ Nationality): AUC =", round(auc(roc3), 3))),
       col = c("blue", "green", "red"), lty = 1, lwd = 2, cex = 0.9)
cat("✓ ROC curves saved as 'roc_curves.png'\n\n")

# ============================================================
# STEP 8: RANDOM FOREST
# ============================================================

cat("=== RANDOM FOREST CLASSIFICATION ===\n")

rf_train <- train_data %>%
  select(is_podium, grid, is_home_match, is_home_region,
         career_wins_before, career_podiums_before, career_race_number,
         win_rate, driver_age, started_from_pole, year) %>%
  na.omit() %>%
  mutate(is_podium = as.factor(is_podium))

rf_test <- test_data %>%
  select(is_podium, grid, is_home_match, is_home_region,
         career_wins_before, career_podiums_before, career_race_number,
         win_rate, driver_age, started_from_pole, year) %>%
  na.omit() %>%
  mutate(is_podium = as.factor(is_podium))

set.seed(123)
rf_model <- randomForest(is_podium ~ ., data = rf_train, 
                         ntree = 200, importance = TRUE)

# Evaluate
rf_pred <- predict(rf_model, newdata = rf_test, type = "prob")[,2]
roc_rf <- roc(rf_test$is_podium, rf_pred, quiet = TRUE)

cat("Random Forest AUC:", round(auc(roc_rf), 4), "\n")
cat("OOB Error Rate:", round(rf_model$err.rate[nrow(rf_model$err.rate), 1], 4), "\n\n")

# Feature importance
cat("TOP 10 FEATURE IMPORTANCE:\n")
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, "MeanDecreaseGini"]
) %>% arrange(desc(Importance))
print(head(importance_df, 10))

# Save feature importance plot
png("feature_importance.png", width = 900, height = 700, res = 110)
varImpPlot(rf_model, main = "Feature Importance for Podium Prediction")
cat("\n✓ Feature importance saved as 'feature_importance.png'\n\n")

# ============================================================
# STEP 9: SAVE RESULTS
# ============================================================

# Model comparison
results_summary <- data.frame(
  Model = c("Logistic Baseline", "Logistic + Career", 
            "Logistic + Nationality", "Random Forest"),
  AUC = c(auc(roc1), auc(roc2), auc(roc3), auc(roc_rf)),
  Includes_Nationality = c(FALSE, FALSE, TRUE, TRUE)
)

write.csv(results_summary, "model_results.csv", row.names = FALSE)

# Correlation summary
correlation_summary <- data.frame(
  Feature = names(podium_correlations),
  Correlation = as.numeric(podium_correlations)
)
write.csv(correlation_summary, "correlations.csv", row.names = FALSE)

cat("=== ANALYSIS COMPLETE ===\n")
cat("✓ Correlation matrix: correlation_matrix.png\n")
cat("✓ ROC curves: roc_curves.png\n")
cat("✓ Feature importance: feature_importance.png\n")
cat("✓ Results: model_results.csv\n")
cat("✓ Correlations: correlations.csv\n\n")

cat("KEY FINDINGS:\n")
cat("1. Strongest predictor:", names(podium_correlations)[1], 
    "(r =", round(podium_correlations[1], 3), ")\n")
cat("2. Home match correlation:", 
    round(cor_matrix["is_home_match", "is_podium"], 4), "\n")
cat("3. Best performing model:", 
    results_summary$Model[which.max(results_summary$AUC)], "\n")
cat("4. Nationality-location effect:", 
    ifelse(chi_test$p.value < 0.05, "SIGNIFICANT ✓", "Not significant"), "\n")
