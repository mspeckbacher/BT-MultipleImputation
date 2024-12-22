# List of required packages
required_packages <- c("ggplot2", "ggcorrplot", "naniar")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
}

# Install any missing packages
install_missing_packages(required_packages)

# Load the packages
lapply(required_packages, require, character.only = TRUE)

if (!dir.exists("plots")) {
  dir.create("plots")
}

# Seed
set.seed(13)

# Create output directories
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Define parameters for the plots
n <- 1000000
p_values <- c(5, 10)


# Function for histograms
create_histogram <- function(data, column, filename, title) {
  plot <- ggplot(data, aes_string(x = column)) +
    geom_histogram(bins = 100, fill = "grey", color = "black") +
    labs(x = bquote(x[.(gsub("x", "", column))]), y = "Frequency") +
    theme_minimal()+
    theme(
      axis.text = element_text(size = 18),  
      axis.title = element_text(size = 20))
  ggsave(filename = filename, plot = plot, width = 8, height = 6)
}

# Function to standardize
standardize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

  
x1 <- rnorm(n, mean = 0, sd = 5)
create_histogram(data.frame(x1), "x1", "plots/hist_x1_original.png", 
                 "Histogram of x1 (Original)")
x1 <- standardize(x1)
create_histogram(data.frame(x1), "x1", "plots/hist_x1_standardized.png", 
                 "Histogram of x1 (Standardized)")

x2 <- x1 + rt(n, df = 10) * 0.15
create_histogram(data.frame(x2), "x2", "plots/hist_x2_original.png", 
                 "Histogram of x2 (Original)")
x2 <- standardize(x2)
create_histogram(data.frame(x2), "x2", "plots/hist_x2_standardized.png", 
                 "Histogram of x2 (Standardized)")

x3 <- x2 + rexp(n, rate = 1) * 0.15
create_histogram(data.frame(x3), "x3", "plots/hist_x3_original.png", 
                 "Histogram of x3 (Original)")
x3 <- standardize(x3)
create_histogram(data.frame(x3), "x3", "plots/hist_x3_standardized.png", 
                 "Histogram of x3 (Standardized)")

x4 <- x3 + rpois(n, lambda = 1) * 0.1
create_histogram(data.frame(x4), "x4", "plots/hist_x4_original.png", 
                 "Histogram of x4 (Original)")
x4 <- standardize(x4)
create_histogram(data.frame(x4), "x4", "plots/hist_x4_standardized.png", 
                 "Histogram of x4 (Standardized)")

x5 <- x4 + rchisq(n, df = 2) * 0.1
create_histogram(data.frame(x5), "x5", "plots/hist_x5_original.png", 
                 "Histogram of x5 (Original)")
x5 <- standardize(x5)
create_histogram(data.frame(x5), "x5", "plots/hist_x5_standardized.png", 
                 "Histogram of x5 (Standardized)")

x6 <- x5 + rgamma(n, shape = 2, rate = 5) * 0.3
create_histogram(data.frame(x6), "x6", "plots/hist_x6_original.png", 
                 "Histogram of x6 (Original)")
x6 <- standardize(x6)
create_histogram(data.frame(x6), "x6", "plots/hist_x6_standardized.png", 
                 "Histogram of x6 (Standardized)")

x7 <- x6 + runif(n, min = -0.2, max = 0.2)
create_histogram(data.frame(x7), "x7", "plots/hist_x7_original.png", 
                 "Histogram of x7 (Original)")
x7 <- standardize(x7)
create_histogram(data.frame(x7), "x7", "plots/hist_x7_standardized.png", 
                 "Histogram of x7 (Standardized)")

x8 <- x7 + rbinom(n, size = 1, prob = 0.5) * 0.3
create_histogram(data.frame(x8), "x8", "plots/hist_x8_original.png", 
                 "Histogram of x8 (Original)")
x8 <- standardize(x8)
create_histogram(data.frame(x8), "x8", "plots/hist_x8_standardized.png", 
                 "Histogram of x8 (Standardized)")

x9 <- x8 + sqrt(rexp(n, rate = 0.5)) * 0.3
create_histogram(data.frame(x9), "x9", "plots/hist_x9_original.png", 
                 "Histogram of x9 (Original)")
x9 <- standardize(x9)
create_histogram(data.frame(x9), "x9", "plots/hist_x9_standardized.png", 
                 "Histogram of x9 (Standardized)")

x10 <- x9 + log1p(rchisq(n, df = 1)) * 0.3
create_histogram(data.frame(x10), "x10", "plots/hist_x10_original.png", 
                 "Histogram of x10 (Original)")
x10 <- standardize(x10)
create_histogram(data.frame(x10), "x10", "plots/hist_x10_standardized.png", 
                 "Histogram of x10 (Standardized)")

  
for (p in p_values) {
  # Select the first p paramters
  x_vars <- list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)[1:p]
  data <- as.data.frame(x_vars)
  colnames(data) <- paste0("x", 1:p)
  
  # Calculate y
  eta <- rowSums(data * 1)
  lambda <- exp(eta)
  data$y <- rpois(n, lambda = lambda)
  
  combination_name <- paste("combination_n", n, "_p", p, sep = "")
  
  # Histogram for y
  hist_plot <- ggplot(data, aes(x = y)) +
    geom_histogram(binwidth = 5, fill = "grey", color = "black") +
    labs(x = "y", y = "Frequency") +
    scale_x_continuous(limits = c(0, 200)) +  
    theme_minimal()+
    theme(
      axis.text = element_text(size = 18),  
      axis.title = element_text(size = 20))
  
  ggsave(filename = paste0("plots/histogram_", combination_name, ".png"),
         plot = hist_plot, width = 8, height = 6)
  
  # Spearman-corr
  spearman_corr <- cor(data, method = 'spearman')
  num_variables <- ncol(spearman_corr)  
  variable_names <- c(
    sapply(1:(num_variables - 1), function(i) as.expression(bquote(x[.(i)]))),
    "y"  
  )
  
  rownames(spearman_corr) <- variable_names
  colnames(spearman_corr) <- variable_names
  
  corr_plot <- ggcorrplot(spearman_corr,
                          method = "square",
                          type = "lower",
                          lab = TRUE,
                          lab_size = 3,
                          colors = c("blue", "white", "red"),
                          legend.title = "Spearman\nCorrelation") +
    theme_minimal()+
    theme(
      axis.text = element_text(size = 18),  
      axis.title = element_blank())+
    scale_x_discrete(labels = parse(text = variable_names[-1])) + 
    scale_y_discrete(labels = parse(text = variable_names))
  
  print(corr_plot)
  
  ggsave(filename = paste0("plots/spearman_corr_", combination_name, ".png"),
         plot = corr_plot, width = 8, height = 6)
}


# Load datasets with missing values generated in missing.R
load("datasets_missing.RData")

display_MCAR <- datasets_missing$MCAR_0.4$combination_n1000_p10[[1]] %>% 
  arrange(desc(y))
display_MNAR <- datasets_missing$MNAR_0.4$combination_n1000_p10[[1]] %>% 
  arrange(desc(y))
display_MAR <- datasets_missing$MAR_0.4$combination_n1000_p10[[1]] %>% 
  arrange(desc(y))

# MCAR Plot
plot_MCAR <- vis_miss(display_MCAR, show_perc = FALSE, show_perc_col = FALSE) + 
  theme(
    axis.text.x = element_text(size = 18, angle = 0),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20)  ) + 
    scale_x_discrete(labels = function(labels) {
      variable_names <- c(
        sapply(1:(length(labels) - 1), 
               function(i) as.expression(bquote(x[.(i)]))), "y"  
      )
      parse(text = variable_names)
    })

# MNAR Plot
plot_MNAR <- vis_miss(display_MNAR, show_perc = FALSE, show_perc_col = FALSE) + 
  theme(
    axis.text.x = element_text(size = 18, angle = 0),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20)  ) + 
    scale_x_discrete(labels = function(labels) {
      variable_names <- c(
        sapply(1:(length(labels) - 1), 
               function(i) as.expression(bquote(x[.(i)]))), "y"  
      )
      parse(text = variable_names)
    })

# MAR Plot
plot_MAR <- vis_miss(display_MAR, show_perc = FALSE, show_perc_col = FALSE) + 
  theme(
    axis.text.x = element_text(size = 18, angle = 0),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 26)
    ) + 
    scale_x_discrete(labels = function(labels) {
      variable_names <- c(
        sapply(1:(length(labels) - 1), 
               function(i) as.expression(bquote(x[.(i)]))), "y"  
      )
      parse(text = variable_names)
    })

print(plot_MCAR)
print(plot_MNAR)
print(plot_MAR)

ggsave("plots/plot_MCAR.png", plot = plot_MCAR, width = 10, height = 8, 
       dpi = 300)
ggsave("plots/plot_MNAR.png", plot = plot_MNAR, width = 10, height = 8, 
       dpi = 300)
ggsave("plots/plot_MAR.png", plot = plot_MAR, width = 10, height = 8, 
       dpi = 300)
