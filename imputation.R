# List of required packages
required_packages <- c("mice", "parallel")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
}

# Install any missing packages
install_missing_packages(required_packages)

# Load the packages
lapply(required_packages, require, character.only = TRUE)

# Seed
set.seed(13)

# !!Very Long Run Time!!

# Start timing
start_time <- Sys.time()

# Load datasets with missing values generated in missing.R
load("datasets_missing.RData")

# Define maximal number of datasets, number of imputations, the methods 
# used for imputation, and the different missing mechanisms
max_datasets <- 1000 # Set lower number to reduce runtime
num_imputations <- 10
imputation_methods <- c("rf", "cart")
missing_mechanisms <- c("MCAR", "MAR", "MNAR")

# Initialize a clean results dataframe
results <- data.frame(
  Missing_mechanism = character(),
  Combination = character(),
  Dataset_Number = integer(),
  Method = character(),
  Parameter = character(),
  Estimate = numeric(),
  StdError = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  CI_Width = numeric(),
  stringsAsFactors = FALSE
)


# Function to process a single dataset
process_single_dataset <- function(dataset, missing_mechanism, combination, 
                                   dataset_num) {
  
  # Dynamically specify the formula based on dataset columns
  formula <- paste("y ~", paste(names(dataset)[-ncol(dataset)], 
                                collapse = " + "))
  
  result_rows <- list()
  
  for (method in imputation_methods) {
    
    # Multiple imputation
    imp <- mice(dataset, m = num_imputations, method = method, seed = 13, 
                printFlag = FALSE, maxit = 10)
    
    # Fit Poisson model and pool results
    model <- with(imp, glm(as.formula(formula), family = poisson))
    pooled <- pool(model)
    
    # Extract parameter estimates and confidence intervals
    ci <- summary(pooled, conf.int = TRUE)
    for (param in rownames(ci)) {
      ci_width <- abs(ci[param, "2.5 %"] - ci[param, "97.5 %"])
      
      # Store parameter results in results data frame
      result_rows[[length(result_rows) + 1]] <- data.frame(
        Missing_mechanism = missing_mechanism,
        Combination = combination,
        Dataset_Number = dataset_num,
        Method = method,
        Parameter = param,
        Estimate = ci[param, "estimate"],
        StdError = ci[param, "std.error"],
        CI_Lower = ci[param, "2.5 %"],
        CI_Upper = ci[param, "97.5 %"],
        CI_Width = ci_width
      )
    }
  }
  
  # Bind result rows and return
  do.call(rbind, result_rows)
}

# Loop through each missing mechanism 
for (missing_mechanism in missing_mechanisms) {
  cat("Processing missing mechanism:", missing_mechanism, "\n")
  
  # Subset datasets based on the missing mechanism
  current_datasets <- datasets_missing[grep(paste0("^", missing_mechanism), 
                                            names(datasets_missing))]
  
  # Reduce datasets to max_datasets per combination
  current_datasets <- lapply(current_datasets, function(missing_mechanism) {
    lapply(missing_mechanism, function(combination) {
      head(combination, max_datasets)
    })
  })
  
  # Create task list 
  task_list <- unique(do.call(rbind, lapply(names(current_datasets), 
                                            function(missing_mechanism) {
    combinations <- names(current_datasets[[missing_mechanism]])
    data.frame(missing_mechanism = missing_mechanism,
               combination = combinations, stringsAsFactors = FALSE)
  })))
  
  # Parallelization
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, varlist = c("current_datasets", "process_single_dataset", 
                                "imputation_methods", "max_datasets", 
                                "task_list", "num_imputations"))
  clusterEvalQ(cl, library(mice))
  
  # Process datasets in parallel
  results_nested <- parLapply(cl, seq_len(nrow(task_list)), function(i) {
    task <- task_list[i, ]
    missing_mechanism <- task$missing_mechanism
    combination <- task$combination
    
    # Restrict to max_datasets datasets
    datasets_to_process <- 
      head(current_datasets[[missing_mechanism]][[combination]], max_datasets)
    
    # Process each dataset 
    lapply(seq_along(datasets_to_process), function(dataset_num) {
      dataset <- datasets_to_process[[dataset_num]]
      process_single_dataset(dataset, missing_mechanism, combination, 
                             dataset_num)
    })
  })
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  # Append results directly to the main results data frame
  for (i in seq_len(nrow(task_list))) {
    task <- task_list[i, ]
    missing_mechanism <- task$missing_mechanism
    combination <- task$combination
    
    # Restrict to max_datasets datasets
    datasets_to_process <- 
      head(current_datasets[[missing_mechanism]][[combination]], max_datasets)
    
    # Copy results from results_nested to the main results data frame
    for (dataset_num in seq_along(datasets_to_process)) {
      dataset_results <- results_nested[[i]][[dataset_num]]
      results <- rbind(results, dataset_results)  
    }
  }
}

# Save the results 
save(results, file = "results.RData")

# End timing
end_time <- Sys.time()
time_diff <- difftime(end_time, start_time, units = "secs")
hours <- as.integer(time_diff) %/% 3600
minutes <- (as.integer(time_diff) %% 3600) %/% 60
seconds <- as.integer(time_diff) %% 60

cat("MICE-algorithm finished. Total execution time:", hours, "hours,", 
    minutes, "minutes,", seconds, "seconds\n")
