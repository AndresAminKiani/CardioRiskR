# CardioRiskR

An R-based toolkit for cardiovascular risk modeling.  
Includes scripts for data cleaning, feature extraction, and Random Forest–based classification.  
Designed for reproducibility and reusability in clinical and epidemiologic data workflows.

## Features
- Automated package installation and environment setup  
- Data cleaning and transformation for large health datasets  
- Random Forest model training and parameter tuning  
- Model accuracy evaluation using `caret`  
- Feature importance visualization with `ggplot2` and `ggalt`  

## Requirements

- **R version:** ≥ 4.0  
- **Required packages:**  
  `randomForest`, `caret`, `cowplot`, `ggalt`, `dplyr`, `writexl`, `rfUtilities`

To install all dependencies at once:
```r
install.packages(c("randomForest", "caret", "cowplot", "ggalt", "dplyr", "writexl", "rfUtilities"))
