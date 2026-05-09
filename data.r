# =========================
# 1. LOAD LIBRARIES
# =========================

library(tidyverse)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(sandwich)  


# =========================
# 2. IMPORT DATA
# =========================

data <- read.csv("psephology.csv")

# Inspect data
str(data)
head(data)


# =========================
# 3. DATA CLEANING
# =========================

# Ensure numeric variables
data$turnout_24  <- as.numeric(data$turnout_24)
data$turnout_19  <- as.numeric(data$turnout_19)
data$income_pc   <- as.numeric(data$income_pc)
data$un_rate     <- as.numeric(data$un_rate)
data$mpi         <- as.numeric(data$mpi)
data$regional_24 <- as.numeric(data$regional_24)


# =========================
# 4. MAIN REGRESSION MODEL
# =========================

model <- lm(turnout_24 ~ turnout_19 + income_pc + un_rate + mpi + regional_24, data = data)

cat("MAIN MODEL RESULTS:\n")
summary(model)

stargazer(model, 
          type = "text", 
          out = "ols_regression_table.txt")


# =========================
# 4A. HAC (NEWEY-WEST) 
# =========================

hac_se <- NeweyWest(model)
hac_results <- coeftest(model, vcov = hac_se)

cat("\nHAC (NEWEY-WEST) STANDARD ERRORS:\n")
print(hac_results)


# =========================
# 5. SAVE MAIN MODEL OUTPUT
# =========================

sink("regression_output.txt")

cat("MAIN MODEL RESULTS:\n")
print(summary(model))

cat("\nHAC (NEWEY-WEST) STANDARD ERRORS:\n")
print(hac_results)

sink()

stargazer(model, 
          type = "text", 
          se = list(sqrt(diag(hac_se))),   # ✅ MODIFIED
          out = "regression_table.txt")


# =========================
# 6. DIAGNOSTIC TESTS
# =========================

# Run tests
jarque.bera.test(residuals(model))
qqnorm(residuals(model))
qqline(residuals(model))
shapiro_result <- shapiro.test(residuals(model))
bp_result      <- bptest(model)
vif_result     <- vif(model)


# Print nicely in console
cat("\nDIAGNOSTIC TESTS:\n")
print(shapiro_result)
print(bp_result)
print(vif_result)
print(jarque.bera.test)
print(qqnorm)
print(qqline)

# Save diagnostics
sink("diagnostics_output.txt")

cat("DIAGNOSTIC TESTS:\n\n")

cat("Shapiro-Wilk Test:\n")
print(shapiro_result)

cat("\nBreusch-Pagan Test:\n")
print(bp_result)

cat("\nVariance Inflation Factors (VIF):\n")
print(vif_result)

sink()


# =========================
# 7. GRAPHS
# =========================

# Actual vs Fitted
p1 <- ggplot(data, aes(x = turnout_24, y = fitted(model))) +
  geom_point(color = "#2E86AB", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "#E74C3C", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs Fitted Turnout",
    x = "Actual Turnout",
    y = "Fitted Turnout"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("actual_vs_fitted.png", plot = p1, width = 6, height = 4)


# Residual Plot
p2 <- ggplot(data, aes(x = fitted(model), y = residuals(model))) +
  geom_point(color = "#8E44AD", size = 3) +
  geom_hline(yintercept = 0, color = "#E67E22", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Residual Plot",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("residual_plot.png", plot = p2, width = 6, height = 4)


# Histogram of Residuals
p3 <- ggplot(data, aes(x = residuals(model))) +
  geom_histogram(fill = "#1ABC9C", color = "black", bins = 10) +
  labs(
    title = "Distribution of Residuals",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("histogram.png", plot = p3, width = 6, height = 4)


# Q-Q Plot
p4 <- ggplot(data, aes(sample = residuals(model))) +
  stat_qq(color = "#3498DB", size = 2) +
  stat_qq_line(color = "#E74C3C", linewidth = 1) +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("qq_plot.png", plot = p4, width = 6, height = 4)


# =========================
# 8. CHANGE IN TURNOUT MODEL
# =========================

data$turnout_change <- data$turnout_24 - data$turnout_19

model_change <- lm(turnout_change ~ income_pc + un_rate + mpi + regional_24, data = data)

cat("\nCHANGE MODEL RESULTS:\n")
summary(model_change)


# =========================
# 8A. HAC FOR CHANGE MODEL
# =========================

hac_se_change <- NeweyWest(model_change)
hac_change_results <- coeftest(model_change, vcov = hac_se_change)

cat("\nCHANGE MODEL (HAC SE):\n")
print(hac_change_results)


# Save output
sink("turnout_change_output.txt")

cat("CHANGE MODEL RESULTS:\n")
print(summary(model_change))

cat("\nCHANGE MODEL (HAC SE):\n")
print(hac_change_results)

sink()