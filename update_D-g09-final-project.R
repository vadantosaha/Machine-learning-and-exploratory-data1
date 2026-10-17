# Group Members:
#  21-45953-3   HASAN, MD. FAHAD
#  22-47152-1   SAHA, BEDANTA
#  22-47153-1   SAHA, BIDHAN
# ================================================================================
# INSTALL & LOAD PACKAGES
# ================================================================================
cat("\n========================================================================\n")
cat("    COMPREHENSIVE MSD CLASSIFICATION PROJECT - COMPLETE PIPELINE\n")
cat("========================================================================\n\n")
packages <- c(
  "ggplot2",        # Visualization
  "dplyr",          # Data manipulation
  "corrplot",       # Correlation matrix visualization
  "caret",          # ML framework & train/test split
  "rpart",          # Decision trees (CART)
  "rpart.plot",     # Decision tree visualization
  "class",          # KNN algorithm
  "gridExtra",      # Multiple plots
  "RColorBrewer",   # Color palettes
  "tidyr",          # Data reshaping
  "e1071",          # SVM & skewness calculation
  "infotheo",        # Mutual Information (optional)
  "gplots"
  
)
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.r-project.org" , dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
#--------------------------------------
# Create folder for plots
#--------------------------------------
if(!dir.exists("plots")){
  dir.create("plots")
}
cat("\n All packages loaded successfully\n")
# ================================================================================
# A. DATA COLLECTION
# ================================================================================
cat("\n========================================================================\n")
cat("            SECTION A: DATA COLLECTION\n")
cat("========================================================================\n\n")
cat("Loading dataset from Google Drive (public link - no local paths)...\n")
url <- "https://drive.google.com/uc?export=download&id=1j0EaA8a0pw53ZC2_q47F24tq9y9wQ4sh"
data <- read.csv(url)

cat(" Dataset loaded successfully!\n\n")
cat("Dataset Dimensions:\n")
cat("  - Number of Rows:", nrow(data), "\n")
cat("  - Number of Columns:", ncol(data), "\n")
cat("  - Total Cells:", nrow(data) * ncol(data), "\n\n")

# Display first few rows
cat("--- First 10 Rows of Dataset ---\n")
print(head(data, 10))

cat("\n--- Column Names and Types ---\n")
print(str(data))

# ================================================================================
# B. DATA EXPLORATION & EDA
# ================================================================================

cat("\n========================================================================\n")
cat("            SECTION B: DATA EXPLORATION & EDA\n")
cat("========================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# B.1: DESCRIPTIVE STATISTICS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- PART 1: DESCRIPTIVE STATISTICS ---\n\n")

cat("Summary Statistics:\n")
print(summary(data))

# Identify numeric and categorical columns
numeric_cols <- names(data)[sapply(data, is.numeric)]
categorical_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

cat("\n\nFeature Type Identification:\n")
cat("  Numeric Features (", length(numeric_cols), "):", 
    paste(numeric_cols, collapse = ", "), "\n")
cat("  Categorical Features (", length(categorical_cols), "):", 
    paste(categorical_cols, collapse = ", "), "\n")

# Detailed statistics for numeric columns
cat("\n\nDetailed Statistics for Numeric Features:\n\n")

# Helper function to calculate mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate comprehensive statistics
detailed_stats <- data.frame(
  Feature = numeric_cols,
  Mean = sapply(numeric_cols, function(col) mean(data[[col]], na.rm = TRUE)),
  Median = sapply(numeric_cols, function(col) median(data[[col]], na.rm = TRUE)),
  Std_Dev = sapply(numeric_cols, function(col) sd(data[[col]], na.rm = TRUE)),
  Min = sapply(numeric_cols, function(col) min(data[[col]], na.rm = TRUE)),
  Max = sapply(numeric_cols, function(col) max(data[[col]], na.rm = TRUE)),
  Q1 = sapply(numeric_cols, function(col) quantile(data[[col]], 0.25, na.rm = TRUE)),
  Q3 = sapply(numeric_cols, function(col) quantile(data[[col]], 0.75, na.rm = TRUE)),
  Skewness = sapply(numeric_cols, function(col) e1071::skewness(data[[col]], na.rm = TRUE)),
  Kurtosis = sapply(numeric_cols, function(col) e1071::kurtosis(data[[col]], na.rm = TRUE)),
  Count = sapply(numeric_cols, function(col) sum(!is.na(data[[col]])))
)

print(detailed_stats)

# Statistics for categorical features
cat("\n\nCategorical Features Distribution:\n\n")

for (col in categorical_cols) {
  cat("Feature:", col, "\n")
  freq_table <- table(data[[col]])
  pct_table <- round(prop.table(freq_table) * 100, 2)
  result_df <- data.frame(
    Category = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = as.numeric(pct_table)
  )
  print(result_df)
  cat("\n")
}

#-----------------------------------------------------------
#   Univariate Analysis
#-----------------------------------------------------------
#-----------------------------------------------------------
# 1. Histograms for all numeric columns
#-----------------------------------------------------------
graphics.off()

numeric_cols <- names(data)[sapply(data, is.numeric)]
numeric_cols <- numeric_cols[numeric_cols != "id"]

for (col in numeric_cols) {
  
  png(paste0("plots/Histogram_", col, ".png"),
      width = 1200, height = 800)
  
  x <- data[[col]]
  
  hist(x,
       main = paste("Distribution of", col),
       xlab = col,
       col = "steelblue",
       border = "white",
       breaks = 30)
  
  mean_val <- mean(x, na.rm = TRUE)
  
  abline(v = mean_val, col = "red", lwd = 2, lty = 2)
  
  legend("topright",
         legend = paste("Mean =", round(mean_val, 2)),
         col = "red",
         lty = 2,
         lwd = 2,
         cex = 0.8)
  
  dev.off()
}

#-----------------------------------------------------------
# 2. Boxplots for all numeric columns
#-----------------------------------------------------------
graphics.off()

for (col in numeric_cols) {
  
  png(paste0("plots/Boxplot_", col, ".png"),
      width = 1200, height = 800)
  
  x <- data[[col]]
  
  boxplot(x,
          main = paste("Boxplot of", col),
          ylab = col,
          col = "tomato",
          outline = TRUE)
  
  dev.off()
}

#-----------------------------------------------------------
# 3. Bar Charts for Categorical Variables
#-----------------------------------------------------------
graphics.off()

cat_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

for (col in cat_cols) {
  
  png(paste0("plots/Bar_", col, ".png"),
      width = 1200, height = 800)
  
  freq_table <- sort(table(data[[col]]), decreasing = TRUE)
  
  n_colors <- max(3, min(length(freq_table), 8))
  
  barplot(freq_table,
          main = paste("Frequency:", col),
          col = RColorBrewer::brewer.pal(n_colors, "Set2"),
          las = 2,
          cex.names = 0.8,
          ylab = "Count")
  
  dev.off()
}

#-----------------------------------------------------------
# 4. Bubble Charts for Numeric Pairs
#-----------------------------------------------------------
graphics.off()

for (i in 1:(length(numeric_cols)-1)) {
  for (j in (i+1):length(numeric_cols)) {
    
    x_col <- numeric_cols[i]
    y_col <- numeric_cols[j]
    
    png(paste0("plots/Bubble_", x_col, "_vs_", y_col, ".png"),
        width = 1200, height = 800)
    
    size_col <- numeric_cols[j]
    
    plot(data[[x_col]], data[[y_col]],
         main = paste("Bubble Chart:", x_col, "vs", y_col),
         xlab = x_col,
         ylab = y_col,
         pch = 21,
         bg = rgb(0.2,0.4,0.6,0.5),
         cex = ifelse(is.na(data[[size_col]]), 1,
                      data[[size_col]] / max(data[[size_col]], na.rm=TRUE) * 5))
    
    abline(lm(data[[y_col]] ~ data[[x_col]]),
           col = "red",
           lwd = 2)
    
    dev.off()
  }
}

#-----------------------------------------------------------
# 5. Pie Charts for Categorical Variables
#-----------------------------------------------------------
graphics.off()

for (col in cat_cols) {
  
  png(paste0("plots/Pie_", col, ".png"),
      width = 1200, height = 800)
  
  freq_table <- table(data[[col]])
  
  n_colors <- max(3, min(length(freq_table), 8))
  
  pie(freq_table,
      main = paste("Pie Chart of", col),
      col = RColorBrewer::brewer.pal(n_colors, "Set2"))
  
  dev.off()
}

#-----------------------------------------------------------
# 6. Treemaps for Categorical Variables
#-----------------------------------------------------------
graphics.off()

for (col in cat_cols) {
  
  png(paste0("plots/Treemap_", col, ".png"),
      width = 1200, height = 800)
  
  treemap_data <- data |>
    dplyr::count(.data[[col]])
  
  treemap::treemap(treemap_data,
                   index = col,
                   vSize = "n",
                   title = paste("Treemap of", col),
                   palette = "Set2",
                   border.col = "white")
  
  dev.off()
}

#---------------------------------------------------------
# 1.2 Frequency Table for Categorical Variables
#---------------------------------------------------------
# Identify categorical columns
cat_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
for (col in cat_cols) {
  freq_table <- table(data[[col]])
  cat("Frequency Table for:", col, "\n")
  print(freq_table)
}


# ─────────────────────────────────────────────────────────────────────────────
# B  MISSING VALUES ANALYSIS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- PART 2: MISSING VALUES DETECTION ---\n\n")

missing_summary <- data.frame(
  Column = names(data),
  Missing_Count = colSums(is.na(data)),
  Missing_Percentage = round(colSums(is.na(data)) / nrow(data) * 100, 2)
)

print(missing_summary)

if (sum(is.na(data)) == 0) {
  cat("\n No missing values detected in the dataset!\n")
} else {
  cat("\n⚠ Missing values detected. Will be handled in preprocessing section.\n")
}

# ================================================================================
# CORRECTED: ARTIFICIAL MISSING VALUES CREATION
# ================================================================================

cat("\n========================================================================\n")
cat("         CREATING ARTIFICIAL MISSING VALUES FOR DEMONSTRATION\n")
cat("========================================================================\n\n")

cat("Requirement: Create 50 missing values as demonstration\n")
cat("Reason: Dataset originally has no missing values\n\n")

# Display original state
cat("BEFORE creating artificial missing values:\n")
cat("  - Total missing values:", sum(is.na(data)), "\n")
cat("  - Columns with missing: ", sum(colSums(is.na(data)) > 0), "\n\n")

# Set seed for reproducibility
set.seed(789)

# Parameters
n_missing_to_create <- 50
n_rows <- nrow(data)
n_numeric_cols <- length(numeric_cols)

cat("Creating", n_missing_to_create, "artificial missing values...\n\n")

# Select random rows
missing_row_indices <- sample(1:n_rows, size = min(n_missing_to_create, n_rows), 
                              replace = FALSE)

cat("Selected", length(missing_row_indices), "random rows\n\n")

# Distribute missing values across numeric columns
missing_cols_list <- c()
for (i in 1:n_missing_to_create) {
  col_idx <- ((i - 1) %% n_numeric_cols) + 1
  col_name <- numeric_cols[col_idx]
  row_idx <- missing_row_indices[i]
  
  data[[col_name]][row_idx] <- NA
  missing_cols_list <- c(missing_cols_list, col_name)
}

cat("Distribution of artificial missing values:\n")
missing_dist <- table(missing_cols_list)
print(missing_dist)

# Verify missing values were created
cat("\n\nAFTER creating artificial missing values:\n")
missing_per_col <- colSums(is.na(data))
missing_per_col_nonzero <- missing_per_col[missing_per_col > 0]

cat("  - Total missing values:", sum(is.na(data)), "\n")
cat("  - Columns with missing values:", length(missing_per_col_nonzero), "\n\n")

cat("Missing values per column:\n")
print(missing_per_col_nonzero)

# Visualize missing values
cat("\n\nGenerating missing values visualization...\n")
png("plots/00_Missing_Values_Before_Handling.png", width = 1000, height = 700)

barplot(missing_per_col_nonzero,
        main = "Artificial Missing Values Created (Before Handling)",
        xlab = "Columns",
        ylab = "Count of Missing Values",
        col = "#e74c3c",
        las = 2,
        border = NA,
        ylim = c(0, max(missing_per_col_nonzero) * 1.2))

text(seq_along(missing_per_col_nonzero), missing_per_col_nonzero, 
     labels = missing_per_col_nonzero, 
     pos = 3, 
     cex = 0.8,
     col = "#c0392b")

dev.off()
cat("✓ Saved: 00_Missing_Values_Before_Handling.png\n")

cat("\n Artificial missing values successfully created!\n")
cat("  Total created:", sum(is.na(data)), "\n")
# ================================================================================
# END OF ARTIFICIAL MISSING VALUES CREATION
# ================================================================================
#--------------------------------------------
#  Replace or remove 
#--------------------------------------------
# Helper function to get mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
# Choose method for numeric columns: "mean" or "median"
numeric_method <- "median"  # change to "mean" if you want mean
# Loop through all columns
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    if (numeric_method == "mean") {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    } else if (numeric_method == "median") {
      data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }
  } else if (is.character(data[[col]]) || is.factor(data[[col]])) {
    # Replace categorical NAs with mode
    mode_val <- get_mode(data[[col]])
    data[[col]][is.na(data[[col]])] <- mode_val
  }
}
# Check missing values
colSums(is.na(data))

# ─────────────────────────────────────────────────────────────────────────────
# B.3: OUTLIER DETECTION
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- PART 3: OUTLIER DETECTION (IQR Method) ---\n\n")

outlier_summary <- list()

for (col in numeric_cols) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- sum(data[[col]] < lower_bound | data[[col]] > upper_bound, na.rm = TRUE)
  outlier_pct <- round(outliers / sum(!is.na(data[[col]])) * 100, 2)
  
  outlier_summary[[col]] <- data.frame(
    Feature = col,
    Lower_Bound = round(lower_bound, 2),
    Upper_Bound = round(upper_bound, 2),
    Outlier_Count = outliers,
    Outlier_Percentage = outlier_pct
  )
}

outlier_df <- do.call(rbind, outlier_summary)
rownames(outlier_df) <- NULL
print(outlier_df)

# ─────────────────────────────────────────────────────────────────────────────
# B.4: DATA VISUALIZATIONS - DISTRIBUTIONS & RELATIONSHIPS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- PART 4: GENERATING EXPLORATORY VISUALIZATIONS ---\n\n")

# A. Histograms for numeric features
cat("Generating histograms for numeric features...\n")
for (col in numeric_cols) {
  png(paste0("plots/01_Histogram_", col, ".png"), 
      width = 1200, height = 800)
  
  hist(data[[col]],
       main = paste("Distribution of", col),
       xlab = col,
       ylab = "Frequency",
       col = "#3498db",
       border = "white",
       breaks = 30)
  
  mean_val <- mean(data[[col]], na.rm = TRUE)
  median_val <- median(data[[col]], na.rm = TRUE)
  
  abline(v = mean_val, col = "red", lwd = 2.5, lty = 2)
  abline(v = median_val, col = "green", lwd = 2.5, lty = 3)
  
  legend("topright",
         legend = c(paste("Mean =", round(mean_val, 2)),
                    paste("Median =", round(median_val, 2))),
         col = c("red", "green"),
         lty = c(2, 3),
         lwd = 2.5)
  
  dev.off()
  cat("   Saved: 01_Histogram_", col, ".png\n", sep = "")
}

# B. Boxplots for numeric features (outlier detection)
cat("\nGenerating boxplots for outlier detection...\n")
for (col in numeric_cols) {
  png(paste0("plots/02_Boxplot_", col, ".png"), 
      width = 1200, height = 800)
  
  boxplot(data[[col]],
          main = paste("Boxplot of", col, "- Outlier Detection"),
          ylab = col,
          col = "#e74c3c",
          border = "#c0392b",
          outline = TRUE)
  
  dev.off()
  cat("   Saved: 02_Boxplot_", col, ".png\n", sep = "")
}

# C. Bar charts for categorical features
cat("\nGenerating bar charts for categorical features...\n")
for (col in categorical_cols) {
  png(paste0("plots/03_BarChart_", col, ".png"), 
      width = 1200, height = 800)
  
  freq_table <- sort(table(data[[col]]), decreasing = TRUE)
  colors <- RColorBrewer::brewer.pal(min(length(freq_table), 8), "Set2")
  
  barplot(freq_table,
          main = paste("Frequency Distribution of", col),
          ylab = "Count",
          col = colors,
          las = 2,
          border = NA)
  
  dev.off()
  cat("  Saved: 03_BarChart_", col, ".png\n", sep = "")
}

# D. Correlation matrix (numeric features only)
cat("\nGenerating correlation matrix...\n")

numeric_data <- data[, numeric_cols]
cor_matrix <- cor(numeric_data, use = "complete.obs")

png("plots/04_CorrelationMatrix.png", width = 1000, height = 1000)
corrplot::corrplot(cor_matrix,
                   method = "color",
                   type = "upper",
                   order = "hclust",
                   addCoef.col = "black",
                   tl.col = "black",
                   tl.srt = 45,
                   main = "Correlation Matrix - Numeric Features")
dev.off()
cat("   Saved: 04_CorrelationMatrix.png\n")

# E. Pie charts for target variable and categorical features
cat("\nGenerating pie charts for categorical features...\n")
for (col in categorical_cols) {
  png(paste0("plots/05_PieChart_", col, ".png"), 
      width = 1000, height = 1000)
  
  freq_table <- table(data[[col]])
  colors <- RColorBrewer::brewer.pal(min(length(freq_table), 8), "Set2")
  
  pie(freq_table,
      main = paste("Proportion of", col),
      labels = paste(names(freq_table), 
                     "\n(", round(prop.table(freq_table)*100, 1), "%)", sep = ""),
      col = colors,
      cex = 0.8)
  
  dev.off()
  cat("  Saved: 05_PieChart_", col, ".png\n", sep = "")
}

cat("\n EDA visualizations completed!\n")

# ================================================================================
# C. DATA PREPROCESSING
# ================================================================================

cat("\n========================================================================\n")
cat("            SECTION C: DATA PREPROCESSING\n")
cat("========================================================================\n\n")

data_processed <- data

# ─────────────────────────────────────────────────────────────────────────────
# C.1: HANDLE MISSING VALUES
# ─────────────────────────────────────────────────────────────────────────────

cat("--- PART 1: HANDLING MISSING VALUES ---\n\n")

# For numeric columns: impute with median
for (col in numeric_cols) {
  if (sum(is.na(data_processed[[col]])) > 0) {
    median_val <- median(data_processed[[col]], na.rm = TRUE)
    data_processed[[col]][is.na(data_processed[[col]])] <- median_val
    cat("   Imputed missing values in '", col, "' with median (", 
        round(median_val, 2), ")\n", sep = "")
  }
}

# For categorical columns: impute with mode
for (col in categorical_cols) {
  if (sum(is.na(data_processed[[col]])) > 0) {
    mode_val <- get_mode(data_processed[[col]])
    data_processed[[col]][is.na(data_processed[[col]])] <- mode_val
    cat("  ✓ Imputed missing values in '", col, "' with mode ('", 
        mode_val, "')\n", sep = "")
  }
}

cat("\n Missing value handling complete!\n")

# ─────────────────────────────────────────────────────────────────────────────
# C.2: CATEGORICAL ENCODING
# ─────────────────────────────────────────────────────────────────────────────

cat("\n CATEGORICAL ENCODING \n\n")

cat("Encoding categorical variables to numeric format...\n")

for (col in categorical_cols) {
  # Store original values for reference
  unique_vals <- unique(data_processed[[col]])
  n_unique <- length(unique_vals)
  
  # Encode as numeric factor
  data_processed[[col]] <- as.numeric(factor(data_processed[[col]]))
  
  cat(" Encoded '", col, "' (", n_unique, " unique values)\n", sep = "")
}

cat("\n Categorical encoding complete!\n")

# ─────────────────────────────────────────────────────────────────────────────
# C.3: SKEWNESS ANALYSIS & TRANSFORMATION
# ─────────────────────────────────────────────────────────────────────────────

cat("\n  SKEWNESS ANALYSIS & TRANSFORMATION \n\n")

cat("Skewness Before Transformation:\n")
skewness_before <- sapply(numeric_cols, function(col) {
  e1071::skewness(data_processed[[col]], na.rm = TRUE)
})

skew_df_before <- data.frame(
  Feature = numeric_cols,
  Skewness = round(skewness_before, 4),
  Interpretation = ifelse(abs(skewness_before) < 0.5, "Approximately Symmetric",
                          ifelse(skewness_before < 0, "Left Skewed", "Right Skewed"))
)
print(skew_df_before)

# Apply transformations to reduce skewness
cat("\n\nApplying log and sqrt transformations to reduce skewness...\n")

for (col in numeric_cols) {
  # Try log transformation for positive skewed data
  if (skewness_before[col] > 0.5 && min(data_processed[[col]], na.rm = TRUE) > 0) {
    data_processed[[col]] <- log1p(data_processed[[col]])
    cat("   Applied log transformation to '", col, "'\n", sep = "")
  }
  # Try sqrt transformation for moderate skew
  else if (skewness_before[col] > 0.3 && min(data_processed[[col]], na.rm = TRUE) > 0) {
    data_processed[[col]] <- sqrt(data_processed[[col]])
    cat("   Applied sqrt transformation to '", col, "'\n", sep = "")
  }
}

cat("\n\nSkewness After Transformation:\n")
skewness_after <- sapply(numeric_cols, function(col) {
  e1071::skewness(data_processed[[col]], na.rm = TRUE)
})

skew_df_after <- data.frame(
  Feature = numeric_cols,
  Before = round(skewness_before, 4),
  After = round(skewness_after, 4),
  Improvement = round(abs(skewness_before) - abs(skewness_after), 4)
)
print(skew_df_after)

# ─────────────────────────────────────────────────────────────────────────────
# C.4: NORMALIZATION & STANDARDIZATION
# ─────────────────────────────────────────────────────────────────────────────

cat("\n NORMALIZATION (Min-Max Scaling) \n\n")

# Min-Max Scaling function: (x - min) / (max - min)
min_max_scale <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

cat("Applying Min-Max scaling (range 0-1) to numeric features...\n")

for (col in numeric_cols) {
  data_processed[[col]] <- min_max_scale(data_processed[[col]])
  cat("   Scaled '", col, "' to range [0, 1]\n", sep = "")
}

cat("\n\nVerifying Min-Max Scaling (should be between 0 and 1):\n")
scaling_check <- data.frame(
  Feature = numeric_cols,
  Min = sapply(numeric_cols, function(col) min(data_processed[[col]], na.rm = TRUE)),
  Max = sapply(numeric_cols, function(col) max(data_processed[[col]], na.rm = TRUE)),
  Mean = sapply(numeric_cols, function(col) mean(data_processed[[col]], na.rm = TRUE))
)
print(scaling_check)

cat("\n Normalization complete!\n")

# ─────────────────────────────────────────────────────────────────────────────
# C.5: FEATURE ENGINEERING & SELECTION
# ─────────────────────────────────────────────────────────────────────────────

cat("\n PART 5: FEATURE SELECTION (Correlation Analysis) \n\n")

# Calculate correlation with target variable
cat("Identifying target variable...\n")
if ("mental_health_risk" %in% names(data_processed)) {
  target_col <- "mental_health_risk"
  cat("   Target variable: mental_health_risk\n")
} else {
  # Find the most likely target (last column or contains 'risk', 'target')
  target_col <- names(data_processed)[ncol(data_processed)]
  cat("   Target variable: ", target_col, "\n", sep = "")
}

# Calculate feature importance via correlation
numeric_data_processed <- data_processed[, numeric_cols]

if (target_col %in% numeric_cols) {
  correlations <- cor(numeric_data_processed[, numeric_cols], 
                      data_processed[[target_col]], 
                      use = "complete.obs")
  
  cor_df <- data.frame(
    Feature = rownames(correlations),
    Correlation = round(as.numeric(correlations), 4),
    Abs_Correlation = round(abs(as.numeric(correlations)), 4)
  )
  
  cor_df <- cor_df[order(-cor_df$Abs_Correlation), ]
  
  cat("\nFeature Correlation with Target Variable:\n")
  print(cor_df)
  
  # Visualize feature correlations
  png("plots/06_Feature_Correlations.png", width = 1000, height = 700)
  barplot(cor_df$Abs_Correlation,
          names.arg = cor_df$Feature,
          main = paste("Feature Correlation with", target_col),
          ylab = "Absolute Correlation",
          col = "#2ecc71",
          las = 2,
          border = NA)
  dev.off()
  cat("\n Saved: 06_Feature_Correlations.png\n")
}

cat("\n Preprocessing complete!\n")

cat("\n Final Preprocessed Dataset Summary \n")
cat("Dataset shape:", nrow(data_processed), "×", ncol(data_processed), "\n")
cat("Missing values: ", sum(is.na(data_processed)), "\n", sep = "")
print(head(data_processed, 5))

# ================================================================================
# D. MODELING
# ================================================================================

cat("\n========================================================================\n")
cat("            SECTION D: MODELING - CLASSIFICATION\n")
cat("========================================================================\n\n")

# Prepare data for modeling
data_model <- data_processed

# Ensure target is factor
if (target_col %in% names(data_model)) {
  # Convert to factor if numeric
  if (is.numeric(data_model[[target_col]])) {
    data_model[[target_col]] <- factor(data_model[[target_col]])
  } else {
    data_model[[target_col]] <- factor(data_model[[target_col]])
  }
}

cat("Target variable levels:\n")
print(table(data_model[[target_col]]))

# ─────────────────────────────────────────────────────────────────────────────
# D.1: TRAIN-TEST SPLIT (80-20)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n TRAIN-TEST SPLIT (80-20) \n\n")

set.seed(123)
train_index <- caret::createDataPartition(data_model[[target_col]], 
                                          p = 0.8, list = FALSE)

train_data <- data_model[train_index, ]
test_data <- data_model[-train_index, ]

X_train <- train_data[, -which(names(train_data) == target_col)]
y_train <- train_data[[target_col]]

X_test <- test_data[, -which(names(test_data) == target_col)]
y_test <- test_data[[target_col]]

cat("Training set size:", nrow(train_data), "rows\n")
cat("Test set size:", nrow(test_data), "rows\n")
cat("Number of features:", ncol(X_train), "\n\n")

cat("Training set class distribution:\n")
print(table(y_train))
cat("\nTest set class distribution:\n")
print(table(y_test))

# ─────────────────────────────────────────────────────────────────────────────
# D.2: MODEL 1 - DECISION TREE (UNRESTRICTED)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- MODEL 1: DECISION TREE (UNRESTRICTED) ---\n\n")
cat("Configuration: Minimal constraints for complex tree\n")
cat("  - minsplit: 5\n")
cat("  - minbucket: 2\n")
cat("  - cp: 0.001\n")
cat("  - maxdepth: 30\n\n")

dt1 <- rpart::rpart(
  formula = formula(paste(target_col, "~ .")),
  data = train_data,
  method = "class",
  control = rpart::rpart.control(
    minsplit = 5,
    minbucket = 2,
    cp = 0.001,
    maxdepth = 30
  )
)

cat("Decision Tree 1 trained successfully!\n\n")

# Predictions
dt1_pred_train <- predict(dt1, train_data, type = "class")
dt1_pred_test <- predict(dt1, test_data, type = "class")

# Confusion Matrix - Test Set
dt1_cm_test <- caret::confusionMatrix(dt1_pred_test, y_test)

cat("CONFUSION MATRIX (Test Set):\n")
print(dt1_cm_test$table)

# Performance Metrics
dt1_accuracy <- dt1_cm_test$overall['Accuracy']
dt1_precision <- mean(dt1_cm_test$byClass[, 'Precision'], na.rm = TRUE)
dt1_recall <- mean(dt1_cm_test$byClass[, 'Recall'], na.rm = TRUE)
dt1_f1 <- 2 * (dt1_precision * dt1_recall) / (dt1_precision + dt1_recall)

cat("\nDECISION TREE 1 - PERFORMANCE METRICS (Test Set):\n")
cat("  Accuracy: ", round(dt1_accuracy, 4), "\n", sep = "")
cat("  Precision:", round(dt1_precision, 4), "\n", sep = "")
cat("  Recall:   ", round(dt1_recall, 4), "\n", sep = "")
cat("  F1-Score: ", round(dt1_f1, 4), "\n\n", sep = "")

# Visualize tree
png("plots/07_DecisionTree_1_Unrestricted.png", width = 1400, height = 900)
rpart.plot::rpart.plot(dt1,
                       main = "Decision Tree 1 - Unrestricted",
                       type = 4,
                       extra = 102,
                       cex = 0.8)
dev.off()
cat("Saved: 07_DecisionTree_1_Unrestricted.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# D.3: MODEL 2 - DECISION TREE (RESTRICTED)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- MODEL 2: DECISION TREE (RESTRICTED) ---\n\n")
cat("Configuration: Constraints for simpler, generalizable tree\n")
cat("  - minsplit: 20\n")
cat("  - minbucket: 10\n")
cat("  - cp: 0.01\n")
cat("  - maxdepth: 10\n\n")

dt2 <- rpart::rpart(
  formula = formula(paste(target_col, "~ .")),
  data = train_data,
  method = "class",
  control = rpart::rpart.control(
    minsplit = 20,
    minbucket = 10,
    cp = 0.01,
    maxdepth = 10
  )
)

cat("Decision Tree 2 trained successfully!\n\n")

# Predictions
dt2_pred_train <- predict(dt2, train_data, type = "class")
dt2_pred_test <- predict(dt2, test_data, type = "class")

# Confusion Matrix
dt2_cm_test <- caret::confusionMatrix(dt2_pred_test, y_test)

cat("CONFUSION MATRIX (Test Set):\n")
print(dt2_cm_test$table)

# Performance Metrics
dt2_accuracy <- dt2_cm_test$overall['Accuracy']
dt2_precision <- mean(dt2_cm_test$byClass[, 'Precision'], na.rm = TRUE)
dt2_recall <- mean(dt2_cm_test$byClass[, 'Recall'], na.rm = TRUE)
dt2_f1 <- 2 * (dt2_precision * dt2_recall) / (dt2_precision + dt2_recall)

cat("\nDECISION TREE 2 - PERFORMANCE METRICS (Test Set):\n")
cat("  Accuracy: ", round(dt2_accuracy, 4), "\n", sep = "")
cat("  Precision:", round(dt2_precision, 4), "\n", sep = "")
cat("  Recall:   ", round(dt2_recall, 4), "\n", sep = "")
cat("  F1-Score: ", round(dt2_f1, 4), "\n\n", sep = "")

# Visualize tree
png("plots/08_DecisionTree_2_Restricted.png", width = 1400, height = 900)
rpart.plot::rpart.plot(dt2,
                       main = "Decision Tree 2 - Restricted",
                       type = 4,
                       extra = 102,
                       cex = 0.8)
dev.off()
cat(" Saved: 08_DecisionTree_2_Restricted.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# D.4: MODEL 3 - KNN (k=3)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- MODEL 3: K-NEAREST NEIGHBORS (k=3) ---\n\n")

knn1_pred_test <- class::knn(
  train = X_train,
  test = X_test,
  cl = y_train,
  k = 3
)

knn1_cm_test <- caret::confusionMatrix(knn1_pred_test, y_test)

cat("CONFUSION MATRIX (Test Set):\n")
print(knn1_cm_test$table)

# Performance Metrics
knn1_accuracy <- knn1_cm_test$overall['Accuracy']
knn1_precision <- mean(knn1_cm_test$byClass[, 'Precision'], na.rm = TRUE)
knn1_recall <- mean(knn1_cm_test$byClass[, 'Recall'], na.rm = TRUE)
knn1_f1 <- 2 * (knn1_precision * knn1_recall) / (knn1_precision + knn1_recall)

cat("\nKNN (k=3) - PERFORMANCE METRICS (Test Set):\n")
cat("  Accuracy: ", round(knn1_accuracy, 4), "\n", sep = "")
cat("  Precision:", round(knn1_precision, 4), "\n", sep = "")
cat("  Recall:   ", round(knn1_recall, 4), "\n", sep = "")
cat("  F1-Score: ", round(knn1_f1, 4), "\n\n", sep = "")

# ─────────────────────────────────────────────────────────────────────────────
# D.5: MODEL 4 - KNN (k=7)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n\n--- MODEL 4: K-NEAREST NEIGHBORS (k=7) ---\n\n")

knn2_pred_test <- class::knn(
  train = X_train,
  test = X_test,
  cl = y_train,
  k = 7
)

knn2_cm_test <- caret::confusionMatrix(knn2_pred_test, y_test)

cat("CONFUSION MATRIX (Test Set):\n")
print(knn2_cm_test$table)

# Performance Metrics
knn2_accuracy <- knn2_cm_test$overall['Accuracy']
knn2_precision <- mean(knn2_cm_test$byClass[, 'Precision'], na.rm = TRUE)
knn2_recall <- mean(knn2_cm_test$byClass[, 'Recall'], na.rm = TRUE)
knn2_f1 <- 2 * (knn2_precision * knn2_recall) / (knn2_precision + knn2_recall)

cat("\nKNN (k=7) - PERFORMANCE METRICS (Test Set):\n")
cat("  Accuracy: ", round(knn2_accuracy, 4), "\n", sep = "")
cat("  Precision:", round(knn2_precision, 4), "\n", sep = "")
cat("  Recall:   ", round(knn2_recall, 4), "\n", sep = "")
cat("  F1-Score: ", round(knn2_f1, 4), "\n\n", sep = "")

# ================================================================================
# E. EVALUATION & INTERPRETATION
# ================================================================================

cat("\n========================================================================\n")
cat("            SECTION E: EVALUATION & INTERPRETATION\n")
cat("========================================================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# E.1: MODEL COMPARISON TABLE
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- COMPREHENSIVE MODEL COMPARISON ---\n\n")

comparison_df <- data.frame(
  Model = c("Decision Tree 1 (Unrestricted)",
            "Decision Tree 2 (Restricted)",
            "KNN (k=3)",
            "KNN (k=7)"),
  Accuracy = c(dt1_accuracy, dt2_accuracy, knn1_accuracy, knn2_accuracy),
  Precision = c(dt1_precision, dt2_precision, knn1_precision, knn2_precision),
  Recall = c(dt1_recall, dt2_recall, knn1_recall, knn2_recall),
  F1_Score = c(dt1_f1, dt2_f1, knn1_f1, knn2_f1),
  Training_Samples = c(nrow(train_data), nrow(train_data), nrow(train_data), nrow(train_data)),
  Test_Samples = c(nrow(test_data), nrow(test_data), nrow(test_data), nrow(test_data))
)

comparison_df <- comparison_df[order(-comparison_df$Accuracy), ]
rownames(comparison_df) <- NULL

print(comparison_df)

# Find best models
best_accuracy_model <- comparison_df$Model[which.max(comparison_df$Accuracy)]
best_f1_model <- comparison_df$Model[which.max(comparison_df$F1_Score)]
best_precision_model <- comparison_df$Model[which.max(comparison_df$Precision)]
best_recall_model <- comparison_df$Model[which.max(comparison_df$Recall)]

cat("\n\n--- BEST MODELS BY METRIC ---\n")
cat("Best Accuracy:  ", best_accuracy_model, " (", 
    round(max(comparison_df$Accuracy), 4), ")\n", sep = "")
cat("Best Precision: ", best_precision_model, " (", 
    round(max(comparison_df$Precision), 4), ")\n", sep = "")
cat("Best Recall:    ", best_recall_model, " (", 
    round(max(comparison_df$Recall), 4), ")\n", sep = "")
cat("Best F1-Score:  ", best_f1_model, " (", 
    round(max(comparison_df$F1_Score), 4), ")\n\n", sep = "")

# ─────────────────────────────────────────────────────────────────────────────
# E.2: VISUALIZATION - PERFORMANCE COMPARISON
# ─────────────────────────────────────────────────────────────────────────────

cat("Creating metrics heatmap...\n")

# Plot 1: Metrics Heatmap
png("plots/09_Metrics_Heatmap.png", width = 1000, height = 700)

metrics_matrix <- as.matrix(comparison_df[, c("Accuracy", "Precision", "Recall", "F1_Score")])
rownames(metrics_matrix) <- c("DT1", "DT2", "KNN3", "KNN7")

heatmap.2(metrics_matrix,
          main = "Model Performance Metrics Comparison",
          xlab = "Metrics",
          ylab = "Models",
          col = colorRampPalette(c("tomato", "yellow", "seagreen"))(50),
          scale = "column",
          margins = c(10, 15),
          cellnote = round(metrics_matrix, 3),
          notecex = 0.9,
          dendrogram = "none",
          Rowv = FALSE,
          Colv = FALSE,
          trace = "none")

dev.off()
cat("  ✓ Saved: 09_Metrics_Heatmap.png\n")


# Plot 2: Barplot Comparison
png("plots/10_Accuracy_Comparison.png", width = 1000, height = 700)

barplot(comparison_df$Accuracy,
        names.arg = c("DT1", "DT2", "KNN3", "KNN7"),
        main = "Model Accuracy Comparison",
        ylab = "Accuracy",
        ylim = c(0, 1),
        col = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"),
        border = NA)

grid(NA, NULL, lwd = 2, lty = 2)

legend("topright",
       legend = round(comparison_df$Accuracy, 4),
       col = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"),
       pch = 15,
       cex = 0.9)

dev.off()
cat("   Saved: 10_Accuracy_Comparison.png\n")

# Plot 3: Multi-metric Radar/Line Plot
png("plots/11_Multi_Metric_Comparison.png", width = 1200, height = 700)

matplot(1:4,
        t(comparison_df[, c("Accuracy", "Precision", "Recall", "F1_Score")]),
        type = "b",
        col = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"),
        pch = 16:19,
        lwd = 2.5,
        main = "Model Performance Across All Metrics",
        xlab = "Metrics",
        ylab = "Score",
        ylim = c(0, 1),
        xaxt = "n")

axis(1, at = 1:4, labels = c("Accuracy", "Precision", "Recall", "F1-Score"))

legend("bottomleft",
       legend = c("DT1", "DT2", "KNN3", "KNN7"),
       col = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"),
       pch = 16:19,
       lwd = 2.5,
       cex = 0.9)

grid(NA, NULL, lwd = 1, lty = 3)

dev.off()
cat("  ✓ Saved: 11_Multi_Metric_Comparison.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# E.3: CONFUSION MATRICES VISUALIZATION
# ─────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# E.3: CONFUSION MATRIX VISUALIZATION (WITH NUMBERS)
# ─────────────────────────────────────────────────────────────────────────────

cat("\nGenerating confusion matrix visualizations...\n")

library(gplots)

# Function to plot confusion matrix with numbers
plot_cm <- function(cm_table, title, filename, color) {
  
  cm <- as.matrix(cm_table)
  
  png(filename, width = 1000, height = 800)
  
  heatmap.2(cm,
            main = title,
            col = colorRampPalette(c("white", color))(50),
            trace = "none",
            dendrogram = "none",
            Rowv = FALSE,
            Colv = FALSE,
            
            # ✅ SHOW NUMBERS (FIXED)
            cellnote = matrix(cm, nrow(cm)),
            notecol = "black",
            notecex = 1.5,
            
            margins = c(10, 10))
  
  dev.off()
}

# ─────────────────────────────────────────────────────────
# DT1
# ─────────────────────────────────────────────────────────
plot_cm(dt1_cm_test$table,
        "Confusion Matrix - DT1 (Unrestricted)",
        "plots/12_CM_DT1.png",
        "#3498db")

cat("✓ Saved: 12_CM_DT1.png\n")

# ─────────────────────────────────────────────────────────
# DT2
# ─────────────────────────────────────────────────────────
plot_cm(dt2_cm_test$table,
        "Confusion Matrix - DT2 (Restricted)",
        "plots/13_CM_DT2.png",
        "#e74c3c")

cat("✓ Saved: 13_CM_DT2.png\n")

# ─────────────────────────────────────────────────────────
# KNN (k=3)
# ─────────────────────────────────────────────────────────
plot_cm(knn1_cm_test$table,
        "Confusion Matrix - KNN (k=3)",
        "plots/14_CM_KNN3.png",
        "#2ecc71")

cat("✓ Saved: 14_CM_KNN3.png\n")

# ─────────────────────────────────────────────────────────
# KNN (k=7)
# ─────────────────────────────────────────────────────────
plot_cm(knn2_cm_test$table,
        "Confusion Matrix - KNN (k=7)",
        "plots/15_CM_KNN7.png",
        "#f39c12")

cat("✓ Saved: 15_CM_KNN7.png\n")

cat("\n✓ Confusion matrix visualizations completed!\n")


# ─────────────────────────────────────────────────────────────────────────────
# E.4: FEATURE IMPORTANCE
# ─────────────────────────────────────────────────────────────────────────────

cat("\nExtracting feature importance from decision trees...\n")

# DT1 Feature Importance
if (!is.null(dt1$variable.importance)) {
  dt1_importance_df <- data.frame(
    Feature = names(dt1$variable.importance),
    Importance = as.numeric(dt1$variable.importance)
  )
  dt1_importance_df <- dt1_importance_df[order(-dt1_importance_df$Importance), ]
  
  cat("\n  DT1 Feature Importance (Top 10):\n")
  print(head(dt1_importance_df, 10))
  
  png("plots/16_Feature_Importance_DT1.png", width = 1000, height = 700)
  barplot(head(dt1_importance_df, 10)$Importance,
          names.arg = head(dt1_importance_df, 10)$Feature,
          main = "Feature Importance - DT1 (Unrestricted)",
          ylab = "Importance Score",
          col = "#3498db",
          las = 2,
          border = NA)
  dev.off()
  cat("  ✓ Saved: 16_Feature_Importance_DT1.png\n")
}

# DT2 Feature Importance
if (!is.null(dt2$variable.importance)) {
  dt2_importance_df <- data.frame(
    Feature = names(dt2$variable.importance),
    Importance = as.numeric(dt2$variable.importance)
  )
  dt2_importance_df <- dt2_importance_df[order(-dt2_importance_df$Importance), ]
  
  cat("\n  DT2 Feature Importance (Top 10):\n")
  print(head(dt2_importance_df, 10))
  
  png("plots/17_Feature_Importance_DT2.png", width = 1000, height = 700)
  barplot(head(dt2_importance_df, 10)$Importance,
          names.arg = head(dt2_importance_df, 10)$Feature,
          main = "Feature Importance - DT2 (Restricted)",
          ylab = "Importance Score",
          col = "#e74c3c",
          las = 2,
          border = NA)
  dev.off()
  cat("  ✓ Saved: 17_Feature_Importance_DT2.png\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# E.6: SUMMARY STATISTICS TABLE
# ─────────────────────────────────────────────────────────────────────────────

dir.create("plots", showWarnings = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# E.1: MODEL COMPARISON TABLE (CLASSIFICATION)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- MODEL COMPARISON (CLASSIFICATION) ---\n\n")

comparison_df <- data.frame(
  Model = c("Decision Tree 1", "Decision Tree 2", "KNN (k=3)", "KNN (k=7)"),
  Accuracy = c(dt1_accuracy, dt2_accuracy, knn1_accuracy, knn2_accuracy),
  Precision = c(dt1_precision, dt2_precision, knn1_precision, knn2_precision),
  Recall = c(dt1_recall, dt2_recall, knn1_recall, knn2_recall),
  F1_Score = c(dt1_f1, dt2_f1, knn1_f1, knn2_f1)
)

comparison_df <- comparison_df[order(-comparison_df$F1_Score), ]
print(comparison_df)

best_model <- comparison_df$Model[1]

# ─────────────────────────────────────────────────────────────────────────────
# E.2: VISUALIZATION
# ─────────────────────────────────────────────────────────────────────────────

png("plots/01_accuracy.png", 800, 600)
barplot(comparison_df$Accuracy,
        names.arg = comparison_df$Model,
        col = "skyblue",
        las = 2,
        main = "Model Accuracy Comparison")
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# E.3: CONFUSION MATRIX (DT1)
# ─────────────────────────────────────────────────────────────────────────────

png("plots/02_confusion_dt1.png", 800, 600)
heatmap(dt1_cm_test$table,
        Colv = NA, Rowv = NA,
        main = "Confusion Matrix - Decision Tree 1")
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# E.4: FEATURE IMPORTANCE
# ─────────────────────────────────────────────────────────────────────────────

if (!is.null(dt1$variable.importance)) {
  
  imp <- sort(dt1$variable.importance, decreasing = TRUE)
  
  png("plots/03_feature_importance.png", 800, 600)
  barplot(head(imp, 10),
          las = 2,
          col = "orange",
          main = "Top 10 Important Features")
  dev.off()
}

# ─────────────────────────────────────────────────────────────────────────────
# E.5: REGRESSION (DEPRESSION SCORE)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- REGRESSION (Depression Score Prediction) ---\n")

# Keep only numeric columns
reg_train <- train_data[, sapply(train_data, is.numeric)]
reg_test  <- test_data[, sapply(test_data, is.numeric)]

# Train Linear Regression
lm_model <- lm(depression_score ~ ., data = reg_train)

# Prediction
pred <- predict(lm_model, reg_test)
actual <- reg_test$depression_score

# Metrics
rmse <- sqrt(mean((actual - pred)^2))
mae  <- mean(abs(actual - pred))
r2   <- cor(actual, pred)^2

cat("RMSE:", round(rmse, 4), "\n")
cat("MAE :", round(mae, 4), "\n")
cat("R²  :", round(r2, 4), "\n")

# Plot
png("plots/04_regression_plot.png", 800, 600)
plot(actual, pred,
     main = "Actual vs Predicted Depression Score",
     xlab = "Actual",
     ylab = "Predicted",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# E.6: CLUSTERING (K-MEANS)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- CLUSTERING EVALUATION ---\n")

library(cluster)

cluster_data <- scale(data[, sapply(data, is.numeric)])

set.seed(123)
kmeans_model <- kmeans(cluster_data, centers = 3)

sil <- silhouette(kmeans_model$cluster, dist(cluster_data))
sil_score <- mean(sil[, 3])

cat("Silhouette Score:", round(sil_score, 4), "\n")

# Plot clustering
png("plots/05_clustering.png", 800, 600)
plot(cluster_data[,1:2],
     col = kmeans_model$cluster,
     pch = 19,
     main = "K-Means Clustering")
dev.off()

# ─────────────────────────────────────────────────────────────────────────────
# E.7: INTERPRETATION
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- INTERPRETATION ---\n")

cat("Best Classification Model:", best_model, "\n")
cat("Evaluation based on F1-score (balanced metric)\n")

cat("\nInsights:\n")
cat("- Decision Tree is interpretable\n")
cat("- KNN depends on k value\n")
cat("- Higher F1-score indicates better balance\n")
cat("- Regression predicts depression score effectively\n")
cat("- Clustering shows hidden mental health patterns\n")

# ─────────────────────────────────────────────────────────────────────────────
# E.8: SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- SUMMARY ---\n")

cat("Dataset Size:", nrow(data), "\n")
cat("Train Size:", nrow(train_data), "\n")
cat("Test Size:", nrow(test_data), "\n")

cat("\nModels Used:\n")
cat("- Decision Tree\n")
cat("- KNN\n")
cat("- Linear Regression\n")
cat("- K-Means Clustering\n")

cat("\nAll plots saved in 'plots/' folder\n")

