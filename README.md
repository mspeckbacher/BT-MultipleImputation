# A Comparison of Tree-Based Methods for Multiple Imputation

This repository contains the code used for the thesis **"A Comparison of Tree-Based Methods for Multiple Imputation"** to reproduce and verify the results. The scripts enable data generation, missing value induction, imputation, and analysis of results, focusing on bias, coverage rate, confidence interval width, and visualizations.

## Repository Structure

- **`datagen.R`**: Script to generate datasets. 
- **`missing.R`**: Script to induce missing values in the generated datasets. Requires datasets created from `datagen.R`.  
- **`imputation.R`**: Script to perform multiple imputation on datasets with missing values. Requires datasets created from `missing.R`. Note: Running imputation on all datasets results in very long runtimes.  
- **`analysis.R`**: Script to create tables and plots for bias, coverage rate, and confidence interval width. Requires results from `imputation.R`.  
- **`plots.R`**: Script to generate additional plots. Requires datasets created from `datagen.R` and processed by `missing.R`.
  
## How to Use

1. Clone this repository to your local machine:
    ```bash
    git clone https://github.com/mspeckbacher/BT-MultipleImputation.git
    cd BT-MultipleImputation
    ```

2. **Generate Data**: Run `datagen.R` to create the datasets.
    ```bash
    Rscript datagen.R
    ```

3. **Induce Missing Values**: Run `missing.R` to introduce missing values into the datasets.
    ```bash
    Rscript missing.R
    ```

4. **Perform Imputation**: Use `imputation.R` to impute the missing values.
    ```bash
    Rscript imputation.R
    ```

5. **Analyze Results**: Run `analysis.R` to produce tables and plots for bias, coverage rate, and confidence interval width.
    ```bash
    Rscript analysis.R
    ```

6. **Generate Additional Plots**: Execute `plots.R` to create supplementary visualizations.
    ```bash
    Rscript plots.R
    ```
