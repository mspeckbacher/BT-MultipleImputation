# Seed
set.seed(13)

# Define parameters
n_values <- c(500, 1000)  
p_values <- c(5, 10)    
num_datasets <- 1000    

# Initialize list to store datasets
datasets <- list() 

# Loop over each combination of n and p to generate datasets
for (n in n_values) {
  
  print(paste('n-value:', n))
  
  for (p in p_values) {

    combination_name <- paste0("combination_n", n, "_p", p)
    datasets[[combination_name]] <- list()
    
    print(paste('p-value:', p))
    
    for (i in 1:num_datasets) {
      
      # Generate 10 variables and standardize
      x1 <- rnorm(n, mean = 0, sd = 5)
      x1 <- (x1 - min(x1)) / (max(x1) - min(x1))
      
      x2 <- x1 + rt(n, df = 10) * 0.15
      x2 <- (x2 - min(x2)) / (max(x2) - min(x2))
      
      x3 <- x2 + rexp(n, rate = 1) * 0.15
      x3 <- (x3 - min(x3)) / (max(x3) - min(x3))
      
      x4 <- x3 + rpois(n, lambda = 1) * 0.1
      x4 <- (x4 - min(x4)) / (max(x4) - min(x4))
      
      x5 <- x4 + rchisq(n, df = 2) * 0.1
      x5 <- (x5 - min(x5)) / (max(x5) - min(x5))
      
      x6 <- x5 + rgamma(n, shape = 2, rate = 5) * 0.3
      x6 <- (x6 - min(x6)) / (max(x6) - min(x6))
      
      x7 <- x6 + runif(n, min = -0.2, max = 0.2)
      x7 <- (x7 - min(x7)) / (max(x7) - min(x7))
      
      x8 <- x7 + rbinom(n, size = 1, prob = 0.5) * 0.3
      x8 <- (x8 - min(x8)) / (max(x8) - min(x8))
      
      x9 <- x8 + sqrt(rexp(n, rate = 0.5)) * 0.3
      x9 <- (x9 - min(x9)) / (max(x9) - min(x9))
      
      x10 <- x9 + log1p(rchisq(n, df = 1)) * 0.3
      x10 <- (x10 - min(x10)) / (max(x10) - min(x10))
      
      # Select the first p variables and create a data frame
      x_vars <- list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)[1:p]
      data <- as.data.frame(x_vars)
      colnames(data) <- paste0("x", 1:p)
      
      # Generate the target variable
      eta <- rowSums(data * 1)
      lambda <- exp(eta)
      data$y <- rpois(n, lambda = lambda)
      
      # Store the dataset in the list
      datasets[[combination_name]][[as.character(i)]] <- data
    }
  }
}

# Save all datasets 
save(datasets, file = "datasets.RData")

print("datasets created and saved")
