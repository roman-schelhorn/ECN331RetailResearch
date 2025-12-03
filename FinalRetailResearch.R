# =============================================================================
# 1. SETUP AND DATA IMPORT
# =============================================================================
install.packages("readxl")
install.packages("ggplot2")

library(readxl)  
library(ggplot2)

# Load the Data
MainData <- read_excel("C:/Users/schel/OneDrive - Lebanon Valley College/Documents/ECN331/MainRetailData.xlsx")
View(MainData)

MonthlyData <- read_excel("C:/Users/schel/OneDrive - Lebanon Valley College/Documents/ECN331/MonthlyRetailData.xlsx")
View(MonthlyData)

QuarterlyData <- read_excel("C:/Users/schel/OneDrive - Lebanon Valley College/Documents/ECN331/QuarterlyRetailData.xlsx")
View(QuarterlyData)

# =============================================================================
# PART 1: CORRELATIONS (1-6) - Using Main (Annual) Data
# =============================================================================

r1 <- cor(MainData$AnnualVIX, MainData$RetailPercent, use = "complete.obs")
r2 <- cor(MainData$AnnualVIX, MainData$RetailHoldings, use = "complete.obs")
r3 <- cor(MainData$AnnualVIX, MainData$RobinhoodAccounts, use = "complete.obs")
r4 <- cor(MainData$AnnualWilshireSD, MainData$RetailPercent, use = "complete.obs")
r5 <- cor(MainData$AnnualWilshireSD, MainData$RetailHoldings, use = "complete.obs")
r6 <- cor(MainData$AnnualWilshireSD, MainData$RobinhoodAccounts, use = "complete.obs")

cat("\n--- Correlation Coefficients ---\n")
cat("r1 (VIX vs %Retail):          ", round(r1, 4), "\n")
cat("r2 (VIX vs Holdings):         ", round(r2, 4), "\n")
cat("r3 (VIX vs Robinhood Ann.):   ", round(r3, 4), "\n")
cat("r4 (WilshireSD vs %Retail):   ", round(r4, 4), "\n")
cat("r5 (WilshireSD vs Holdings):  ", round(r5, 4), "\n")
cat("r6 (WilshireSD vs Robinhood Ann.):", round(r6, 4), "\n")


# =============================================================================
# PART 2: REGRESSION MODELS & PLOTS (1-10)
# =============================================================================
# We will create the model (mX) and the plot (pX) for each.

# --- MODEL 1: Annual VIX vs %Retail ---
m1 <- lm(AnnualVIX ~ RetailPercent, data = MainData)
p1 <- ggplot(MainData, aes(x = RetailPercent, y = AnnualVIX)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Eq 1: Annual VIX vs. %Retail", x = "Retail Share (%)", y = "Annual VIX") +
  theme_minimal()

# --- MODEL 2: Annual VIX vs Retail Holdings (+ Inst. Holdings) ---
# Note: Plotting VIX vs Retail Holdings only for visualization
m2 <- lm(AnnualVIX ~ RetailHoldings + InstitutionalHoldings, data = MainData)
p2 <- ggplot(MainData, aes(x = RetailHoldings, y = AnnualVIX)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Eq 2: Annual VIX vs. Retail Holdings", x = "Retail Holdings ($B)", y = "Annual VIX") +
  theme_minimal()

# --- MODEL 3: Annual VIX vs Robinhood Accounts ---
m3 <- lm(AnnualVIX ~ RobinhoodAccounts, data = MainData)
p3 <- ggplot(subset(MainData, !is.na(RobinhoodAccounts)), aes(x = RobinhoodAccounts, y = AnnualVIX)) +
  geom_point(color = "forestgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Eq 3: Annual VIX vs. Robinhood Accounts", x = "Accounts (Millions)", y = "Annual VIX") +
  theme_minimal()

# --- MODEL 4: Annual WilshireSD vs %Retail ---
m4 <- lm(AnnualWilshireSD ~ RetailPercent, data = MainData)
p4 <- ggplot(MainData, aes(x = RetailPercent, y = AnnualWilshireSD)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Eq 4: Wilshire SD vs. %Retail", x = "Retail Share (%)", y = "Wilshire SD (Points)") +
  theme_minimal()

# --- MODEL 5: Annual WilshireSD vs Retail Holdings (+ Inst. Holdings) ---
m5 <- lm(AnnualWilshireSD ~ RetailHoldings + InstitutionalHoldings, data = MainData)
p5 <- ggplot(MainData, aes(x = RetailHoldings, y = AnnualWilshireSD)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Eq 5: Wilshire SD vs. Retail Holdings", x = "Retail Holdings ($B)", y = "Wilshire SD (Points)") +
  theme_minimal()

# --- MODEL 6: Annual WilshireSD vs Robinhood Accounts ---
m6 <- lm(AnnualWilshireSD ~ RobinhoodAccounts, data = MainData)
p6 <- ggplot(subset(MainData, !is.na(RobinhoodAccounts)), aes(x = RobinhoodAccounts, y = AnnualWilshireSD)) +
  geom_point(color = "forestgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Eq 6: Wilshire SD vs. Robinhood Accounts", x = "Accounts (Millions)", y = "Wilshire SD (Points)") +
  theme_minimal()

# --- MODEL 7 (Quarterly): VIX vs Robinhood MAU ---
m7 <- lm(QuarterlyVIX ~ RobinhoodMAU, data = QuarterlyData)
p7 <- ggplot(QuarterlyData, aes(x = RobinhoodMAU, y = QuarterlyVIX)) +
  geom_point(color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Eq 7: Quarterly VIX vs. Robinhood MAU", x = "Monthly Active Users (Millions)", y = "Quarterly VIX") +
  theme_minimal()

# --- MODEL 8 (Monthly): VIX vs Robinhood Accounts ---
m8 <- lm(MonthlyVIX ~ RobinhoodAccounts, data = MonthlyData)
p8 <- ggplot(MonthlyData, aes(x = RobinhoodAccounts, y = MonthlyVIX)) +
  geom_point(color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Eq 8: Monthly VIX vs. Robinhood Accounts", x = "Funded Accounts (Millions)", y = "Monthly VIX") +
  theme_minimal()

# --- MODEL 9 (Quarterly): WilshireSD vs Robinhood MAU ---
m9 <- lm(QuarterlyWilshireSD ~ RobinhoodMAU, data = QuarterlyData)
p9 <- ggplot(QuarterlyData, aes(x = RobinhoodMAU, y = QuarterlyWilshireSD)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Eq 9: Quarterly Wilshire SD vs. Robinhood MAU", x = "Monthly Active Users (Millions)", y = "Wilshire SD (Points)") +
  theme_minimal()

# --- MODEL 10 (Monthly): WilshireSD vs Robinhood Accounts ---
m10 <- lm(MonthlyWilshireSD ~ RobinhoodAccounts, data = MonthlyData)
p10 <- ggplot(MonthlyData, aes(x = RobinhoodAccounts, y = MonthlyWilshireSD)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Eq 10: Monthly Wilshire SD vs. Robinhood Accounts", x = "Funded Accounts (Millions)", y = "Wilshire SD (Points)") +
  theme_minimal()

# =============================================================================
# PART 3: OUTPUT ALL RESULTS
# =============================================================================

# Print Summaries
print(summary(m1))
print(summary(m2))
print(summary(m3))
print(summary(m4))
print(summary(m5))
print(summary(m6))
print(summary(m7))
print(summary(m8))
print(summary(m9))
print(summary(m10))

# Print Plots (Display one by one or click through them in RStudio)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
print(p10)

# =============================================================================
# PART 4: MUTLICOLINEARITY CHECK
# =============================================================================

# Check correlation between Retail and Institutional Holdings
holdings_corr <- cor(MainData$RetailHoldings, MainData$InstitutionalHoldings, use = "complete.obs")

cat("\n--- Multicollinearity Check ---\n")
cat("Correlation between Retail & Inst. Holdings:", round(holdings_corr, 4), "\n")

# =============================================================================
# PART 5: PREPARE MATERIALS FOR USE IN PAPERS/PRESENTATIONS
# =============================================================================

install.packages("stargazer")
library(stargazer)

# ---------------------------------------------------------
# A. EXPORT REGRESSION TABLES
# ---------------------------------------------------------
# This creates a Word file named "Reg_Results_Annual.doc"
# It groups your Annual Models (1-6) into two distinct tables in one file
# or you can separate them. Here, I have separated them by Dependent Variable.

# Table 1: Annual VIX Models (m1, m2, m3)
stargazer(m1, m2, m3,
          type = "html", # Renders correctly when opened in Word
          out = "Table1_AnnualVIX.doc",
          title = "Regression Results: Annual VIX",
          dep.var.labels = c("Annual VIX"),
          covariate.labels = c("Retail Share (%)", 
                               "Retail Holdings", 
                               "Inst. Holdings", 
                               "Robinhood Accts"),
          digits = 3)

# Table 2: Annual Wilshire SD Models (m4, m5, m6)
stargazer(m4, m5, m6,
          type = "html",
          out = "Table2_WilshireSD.doc",
          title = "Regression Results: Wilshire Standard Deviation",
          dep.var.labels = c("Wilshire SD"),
          covariate.labels = c("Retail Share (%)", 
                               "Retail Holdings", 
                               "Inst. Holdings", 
                               "Robinhood Accts"),
          digits = 3)

# Table 3: Robinhood Specific Models (m7, m8, m9, m10)
# Note: Since these use different datasets (Monthly/Quarterly), N will vary significantly.
stargazer(m7, m8, m9, m10,
          type = "html",
          out = "Table3_Robinhood_HighFreq.doc",
          title = "Regression Results: Robinhood Activity (Monthly/Quarterly)",
          dep.var.labels = c("Q-VIX", "M-VIX", "Q-Wilshire SD", "M-Wilshire SD"),
          covariate.labels = c("Robinhood MAU", "Robinhood Accts"),
          digits = 3)

# ---------------------------------------------------------
# B. EXPORT PLOTS (High Res for Paper)
# ---------------------------------------------------------
# This saves your ggplot objects (p1, p2, etc.) as PNG files to your folder.

# Create a list of your plots
plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

# Loop through and save them (width/height in inches)
for (i in 1:length(plot_list)) {
  file_name <- paste0("Figure_", i, ".png")
  ggsave(filename = file_name, plot = plot_list[[i]], 
         width = 6.5, height = 4.5, dpi = 300)
}