# List of required packages
required_packages <- c("devtools", "reshape2", "mice", "httr", "gdata", 
                       "mltools")

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

# !!Long Run Time!!

# Load function to induce missing values from the R-miss-tastic platform (Mayer et al., 2024)
source_url('https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/generate/amputation.R')

# Load datasets created in datagen.R
load("datasets.RData")

# Initialize list to store processed datasets
datasets_missing <- list()

# Define missing percentage and mechanism
missing_percentage <- c(0.2, 0.4, 0.6)
mechanisms <- c("MCAR", "MNAR", "MAR")

for (mechanism in mechanisms) {
  
  print(paste('mechanism:', mechanism))
  
  for (perc_missing in missing_percentage) {
    
    mechanism_percent_key <- paste0(mechanism, "_", perc_missing)
    datasets_missing[[mechanism_percent_key]] <- list()
    
    print(paste('Missing Percentage:', perc_missing))
    
    for (combination_name in names(datasets)) {
      
      print(combination_name)
      
      # Initialize a sub-list for the current combination
      datasets_missing[[mechanism_percent_key]][[combination_name]] <- list()
      
      for (i in seq_along(datasets[[combination_name]])) {
        
        # Extract the current dataset
        current_data <- datasets[[combination_name]][[i]]
        
        # Set mechanism-specific arguments
        self_mask <- if (mechanism %in% c("MNAR")) "upper" else NULL
        idx_covariates <- if (mechanism == "MAR") {
          matrix(c(rep(0, ncol(current_data) - 1), 1), nrow = 1)
        } else {
          NULL
        }
        
        # Apply the specified mechanism to create missing values with the current percentage
        data_missing <- produce_NA(
          data = current_data, 
          mechanism = mechanism,       
          perc.missing = perc_missing, 
          by.patterns = FALSE, 
          self.mask = self_mask,       
          idx.incomplete = rep(1, ncol(current_data) - 1),
          idx.covariates = idx_covariates,
          seed = 13
        )$data.incomp
        
        datasets_missing[[mechanism_percent_key]][[combination_name]][[i]] <- data_missing
      }
    }
  }
}

# Save all datasets with missing values
save(datasets_missing, file = "datasets_missing.RData")

print("missing values induced and new datsets saved")
