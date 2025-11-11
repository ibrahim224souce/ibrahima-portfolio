
# ===============================
# Turtle Games — DA301 (R script)
# Clean EDA, plots, stats, model
# ===============================

## 0) Setup -------------------------------------------------------------

# Choose your CSV in a file picker (easiest) OR set a path manually.
# METHOD A (recommended): click-to-choose
csv_path <- file.choose()     # choose: turtle_reviews.csv

# METHOD B (manual): comment the line above and set your path:
# csv_path <- "/Users/ibrahima/my assessment/turtle_reviews.csv"

stopifnot(file.exists(csv_path))
df <- read.csv(csv_path, stringsAsFactors = FALSE)

# Create an outputs folder next to the CSV (so everything saves together)
out_dir <- file.path(dirname(csv_path), "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir)
message("Saving to: ", out_dir)

## 1) Basic inspection --------------------------------------------------

cat("\n== HEAD ==\n"); print(head(df))
cat("\n== STR ==\n");  print(str(df))
cat("\n== NA counts per column ==\n"); print(colSums(is.na(df)))

# Drop exact duplicate rows (just in case)
n_before <- nrow(df)
df <- df[!duplicated(df), ]
cat("\nDropped duplicates:", n_before - nrow(df), "\n")

## 2) Descriptive statistics (numeric vars) -----------------------------

# NOTE: R turns special characters into dots in names.
# Your numeric columns are these (check str(df) if different):
num_cols <- c("age", "remuneration..k..", "spending_score..1.100.", "loyalty_points")

desc <- data.frame(
  variable = num_cols,
  n    = sapply(df[num_cols], function(x) sum(!is.na(x))),
  mean = sapply(df[num_cols], function(x) mean(x, na.rm=TRUE)),
  median = sapply(df[num_cols], function(x) median(x, na.rm=TRUE)),
  sd   = sapply(df[num_cols], function(x) sd(x, na.rm=TRUE)),
  min  = sapply(df[num_cols], function(x) min(x, na.rm=TRUE)),
  max  = sapply(df[num_cols], function(x) max(x, na.rm=TRUE))
)
cat("\n== Descriptive statistics ==\n"); print(desc)

write.csv(desc, file.path(out_dir, "descriptive_stats.csv"), row.names = FALSE)

## 3) Quick visualisations (base R, no packages) -----------------------

# 3a) Histogram of Loyalty (Male vs Female)
png(file.path(out_dir, "plot_loyalty_hist.png"), width=1200, height=800)
par(mfrow=c(1,2))
hist(df$loyalty_points[df$gender=="Male"],   main="Loyalty Points (Male)",   xlab="Loyalty Points", col="gray85", border="white")
hist(df$loyalty_points[df$gender=="Female"], main="Loyalty Points (Female)", xlab="Loyalty Points", col="gray85", border="white")
par(mfrow=c(1,1))
dev.off()

# 3b) Boxplot: Loyalty by Gender
png(file.path(out_dir, "plot_loyalty_by_gender.png"), width=1200, height=800)
boxplot(loyalty_points ~ gender, data=df,
        main="Loyalty Points by Gender", ylab="Loyalty Points",
        col=c("lightskyblue","lightpink"), border="gray40")
dev.off()

# 3c) Scatter: Remuneration vs Spending Score
png(file.path(out_dir, "plot_remuneration_vs_spending.png"), width=1200, height=800)
plot(df$remuneration..k.., df$spending_score..1.100.,
     xlab="Remuneration (k£)", ylab="Spending Score (1–100)",
     main="Remuneration vs Spending Score",
     pch=19, col=rgb(0.2,0.4,0.7,0.5))
grid()
dev.off()

## 4) Simple linear model (like your Python regression) -----------------

# Make sure 'gender' is a factor with 2+ levels
df$gender <- factor(df$gender)

# Fit model
lm_fit <- lm(loyalty_points ~ age + remuneration..k.. + spending_score..1.100. + gender, data=df)
sum_lm <- summary(lm_fit)

cat("\n== Linear Model Summary ==\n"); print(sum_lm)

# Save the summary to a text file for your appendix
sink(file.path(out_dir, "r_lm_summary.txt"))
print(sum_lm)
sink()

## 5) Save a cleaned copy of the data used ------------------------------

write.csv(df, file.path(out_dir, "turtle_games_clean.csv"), row.names = FALSE)

cat("\nDone. Files saved in:\n", out_dir, "\n",
    "- descriptive_stats.csv\n",
    "- plot_loyalty_hist.png\n",
    "- plot_loyalty_by_gender.png\n",
    "- plot_remuneration_vs_spending.png\n",
    "- r_lm_summary.txt\n",
    "- turtle_games_clean.csv\n", sep="")
    