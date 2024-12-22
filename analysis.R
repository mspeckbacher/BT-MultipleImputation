# List of required packages
required_packages <- c("dplyr", "tidyr", "ggplot2")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
}

# Install any missing packages
install_missing_packages(required_packages)

# Load the packages
lapply(required_packages, require, character.only = TRUE)

# Load results created by imputation.R
load("results.RData")

# Create output directories
output_dir <- "tables"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

# Function to escape special characters
escape_latex <- function(x) {
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

# Reformat results data frame
results <- results %>%
  mutate(
    Parameter = factor(recode(Parameter, !!!setNames(paste0("x", 0:10), 
                                                     as.character(1:11))),
                       levels = paste0("x", 0:10),
                       ordered = TRUE),
    Method = factor(Method, levels = c("rf", "cart")),
    Missing_mechanism = factor(Missing_mechanism),
    Missing_perc = gsub("[^0-9.]", "", Missing_mechanism),
    Missing_type = factor(
      gsub("[0-9._]", "", Missing_mechanism),
      levels = c("MCAR", "MAR", "MNAR")
    ),
    n_value = ifelse(grepl("n1000", Combination), 1000, 500),
    p_value = ifelse(grepl("p10", Combination), 10, 5),
    Method = toupper(Method)
  )

# Extract different settings
unique_n <- unique(results$n_value)
unique_p <- unique(results$p_value)
unique_Missing_Type <- unique(results$Missing_type)
unique_Missing_Perc <- unique(results$Missing_perc)


# Bias --------------------------------------------------------------------

# Calculate Bias
bias_results <- results %>%
  mutate(
    True_Value = ifelse(Parameter == "x0", 0, 1),
    Bias = Estimate - True_Value
  ) %>%
  group_by(n_value, p_value, Missing_type, Missing_perc, Parameter, Method) %>%
  summarise(
    Mean_Bias = mean(Bias),
    .groups = "drop"
  )

output_file <- file.path(output_dir, "Bias_Tables.txt")
sink(output_file)

# Generate LaTeX tables for bias 
for (p in unique_p) {
  for (type in unique_Missing_Type) {
    for (n in unique_n) {
      for (perc in unique_Missing_Perc) {
        
        # Filter and reshape the data
        table_data <- bias_results %>%
          filter(n_value == n, p_value == p, 
                 Missing_type == type, Missing_perc == perc) %>%
          pivot_wider(names_from = Parameter, values_from = Mean_Bias) %>%
          arrange(Method)
        
        # Rename columns
        colnames(table_data) <- gsub("^x0$", "$\\\\beta_0$", 
                                     colnames(table_data))
        colnames(table_data) <- gsub("^x(\\d+)$", "$\\\\beta_{\\1}$", 
                                     colnames(table_data))
        
        all_columns <- c("Method", "$\\beta_0$", paste0("$\\beta_{", 1:10, "}$"))
        present_columns <- intersect(all_columns, colnames(table_data))
        table_data <- table_data %>%
          select(all_of(present_columns))
        
        # Table caption
        table_caption <- paste0(
          "Bias for $n = ", n, "$ with $p = ", p,
          "$ for ", escape_latex(as.character(type)), " and ", 
          as.numeric(perc) * 100, "\\% missing"
        )
        
        # Write table
        cat("\\begin{table}[ht!]\n")
        cat("\\centering\n")
        cat(paste0("\\caption{", table_caption, "}\n"))
        cat("\\begin{tabular}{l|" , paste(rep(">{\\centering\\arraybackslash}p{0.875cm}", ncol(table_data) - 1), collapse = ""), "}\n", sep = "")
        cat("\\hline\n")
        cat("Method & ", paste(colnames(table_data)[-1], collapse = " & "), " \\\\\n")
        cat("\\hline\n")
        for (row in 1:nrow(table_data)) {
          cat(toupper(table_data$Method[row]), " & ",
              paste(sprintf("%.2f", table_data[row, -1]), collapse = " & "), " \\\\\n", sep = "")
        }
        cat("\\hline\n")
        cat("\\end{tabular}\n")
        cat(paste0("\\label{tab:bias_", n, "_", p, "_", type, "_", perc, "}\n"))
        cat("\\end{table}\n\n")
      }
    }
  }
}

sink()

cat("LaTeX tables for bias have been saved in tables/Bias_Tables.txt.")


# Coverage rate -----------------------------------------------------------

# Calculate coverage rate 
coverage_results <- results %>%
  mutate(
    True_Value = ifelse(Parameter == "x0", 0, 1),
    Covered = ifelse(CI_Lower <= True_Value & CI_Upper >= True_Value, 1, 0)
  ) %>%
  group_by(n_value, p_value, Missing_type, Missing_perc, Parameter, Method) %>%
  summarise(
    Coverage_Rate = mean(Covered) * 100,
    .groups = "drop"
  )

output_file <- file.path(output_dir, "Coverage_Tables.txt")
sink(output_file)

# Generate LaTeX tables for coverage rate
for (p in unique_p) {
  for (type in unique_Missing_Type) {
    for (n in unique_n) {
      for (perc in unique_Missing_Perc) {
        
        # Filter and reshape the data
        table_data <- coverage_results %>%
          filter(n_value == n, p_value == p, 
                 Missing_type == type, Missing_perc == perc) %>%
          pivot_wider(names_from = Parameter, values_from = Coverage_Rate) %>%
          arrange(Method)
        
        # Rename columns
        colnames(table_data) <- gsub("^x0$", "$\\\\beta_0$", 
                                     colnames(table_data))
        colnames(table_data) <- gsub("^x(\\d+)$", "$\\\\beta_{\\1}$", 
                                     colnames(table_data))
        
        all_columns <- c("Method", "$\\beta_0$", paste0("$\\beta_{", 1:10, "}$"))
        present_columns <- intersect(all_columns, colnames(table_data))
        table_data <- table_data %>%
          select(all_of(present_columns))
        
        # Table caption
        table_caption <- paste0(
          "Coverage Rate (\\%) for $n = ", n, "$ with $p = ", p,
          "$ for ", escape_latex(as.character(type)), " and ", 
          as.numeric(perc) * 100, "\\% missing"
        )
        
        # Write table
        cat("\\begin{table}[ht!]\n")
        cat("\\centering\n")
        cat(paste0("\\caption{", table_caption, "}\n"))
        cat("\\begin{tabular}{l|" , paste(rep(">{\\centering\\arraybackslash}p{0.875cm}", ncol(table_data) - 1), collapse = ""), "}\n", sep = "")
        cat("\\hline\n")
        cat("Method & ", paste(colnames(table_data)[-1], collapse = " & "), " \\\\\n")
        cat("\\hline\n")
        for (row in 1:nrow(table_data)) {
          cat(toupper(table_data$Method[row]), " & ",
              paste(sprintf("%.1f", table_data[row, -1]), collapse = " & "), " \\\\\n", sep = "")
        }
        cat("\\hline\n")
        cat("\\end{tabular}\n")
        cat(paste0("\\label{tab:coverage_", n, "_", p, "_", type, "_", perc, "}\n"))
        cat("\\end{table}\n\n")
      }
    }
  }
}
sink()

cat("LaTeX tables for coverage rate have been saved in 
    tables/Coverage_Tables.txt.")


# Confidence interval width -----------------------------------------------

# Tables for confidence interval width

output_file <- file.path(output_dir, "CI_Width_Tables.txt")
sink(output_file)

for (p in unique_p) {
  for (type in unique_Missing_Type) {
    for (n in unique_n) {
      for (perc in unique_Missing_Perc) {
        
        # Filter and reshape the data
        table_data <- results %>%
          filter(p_value == p, Missing_type == type,
                 n_value == n, Missing_perc == perc) %>%
          group_by(Method, Parameter) %>%
          summarise(Median_CI_Width = median(CI_Width), .groups = "drop") %>%
          pivot_wider(names_from = Parameter, values_from = Median_CI_Width) %>%
          arrange(Method)
        
        # Rename columns
        colnames(table_data) <- gsub("^x(\\d+)$", "$\\\\beta_{\\1}$", 
                                     colnames(table_data))
        colnames(table_data) <- gsub("\\(Intercept\\)", "$\\\\beta_0$", 
                                     colnames(table_data))
        
        # Table caption
        table_caption <- paste0(
          "Median CI Widths for $n = ", n, "$ with $p = ", p,
          "$ for ", type, " and ", as.numeric(perc) * 100, "\\% missing"
        )
        
        # Write table
        cat("\\begin{table}[ht!]\n")
        cat("\\centering\n")
        cat(paste0("\\caption{", table_caption, "}\n"))
        cat("\\begin{tabular}{l|" , paste(rep(">{\\centering\\arraybackslash}p{0.8cm}", ncol(table_data) - 1), collapse = ""), "}\n", sep = "")
        cat("\\hline\n")
        cat("Method & ", paste(colnames(table_data)[-1], collapse = " & "), " \\\\\n")
        cat("\\hline\n")
        for (row in 1:nrow(table_data)) {
          cat(toupper(table_data$Method[row]), " & ",
              paste(sprintf("%.2f", table_data[row, -1]), collapse = " & "), " \\\\\n", sep = "")
        }
        cat("\\hline\n")
        cat("\\end{tabular}\n")
        cat(paste0("\\label{tab:median_ci_widths_", n, "_", type, "_", perc, "}\n"))
        cat("\\end{table}\n\n")
      }
    }
  }
}

sink()

cat("LaTeX tables for median confidence interval width have been saved in 
    tables/CI_Width_Tables.txt")

# Plots for confidence interval width

for (p in unique_p) {
  for (type in unique_Missing_Type) {
    for (n in unique_n) {
      for (perc in unique_Missing_Perc) {

      plot_data <- results %>%
          filter(p_value == p, Missing_type == type,
                 n_value == n, Missing_perc == perc)
      
      # Create the boxplots
      viz <- ggplot(plot_data, aes(x = Parameter, y = CI_Width, 
                                   fill = Method)) +
        geom_boxplot(outliers = F) +  # Remove outliers
        labs(x = "Parameter", y = "CI Width") +
        theme_minimal() +
        theme(
          legend.position = "right",
          axis.text = element_text(size = 18),  
          axis.title = element_text(size = 20),  
          legend.text = element_text(size = 18), 
          legend.title = element_text(size = 20)) +
        scale_fill_manual(values = c("RF" = "#19c5f4", "CART" = "#ff9933"))+
        coord_cartesian(ylim = c(0, NA))+
        scale_x_discrete(labels = function(labels) {
          variable_names <- sapply(0:(length(labels) - 1), function(i) as.expression(bquote(x[.(i)])))
          parse(text = variable_names)
        })
      

      # Print and save plot 
      print(viz)
      ggsave(paste0("plots/WidthCI_p", p, "_n", n, "_", type,"_", perc, ".png"), 
             viz, width = 12, height = 8, dpi = 300)  
      }
    }
  }
}

cat("Boxplots for confidence interval width have been saved in plots")
