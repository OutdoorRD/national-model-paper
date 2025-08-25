# Repository Structure Template for Fixed Effects Analysis

## Overview
This template provides a suggested structure for organizing fixed effects analysis code in the national-model-paper repository.

## Recommended Directory Structure

```
national-model-paper/
├── README.md                          # Project overview and setup instructions
├── FIXED_EFFECTS_SEARCH_REPORT.md     # Search report (current findings)
├── analysis/                          # Statistical analysis scripts
│   ├── 01_data_preparation.R          # Data cleaning and preparation
│   ├── 02_exploratory_analysis.R      # Initial data exploration
│   ├── 03_fixed_effects_models.R      # Main fixed effects specifications
│   ├── 04_model_diagnostics.R         # Model validation and diagnostics
│   └── 05_sensitivity_analysis.R      # Robustness checks
├── models/                            # Model specification files
│   ├── visitor_demand_fe.R            # Visitor demand fixed effects model
│   ├── spatial_fixed_effects.R        # Spatial fixed effects specifications
│   └── temporal_fixed_effects.R       # Temporal fixed effects specifications
├── data/                              # Data files (if shareable)
│   ├── processed/                     # Clean, analysis-ready data
│   └── metadata/                      # Data documentation
├── results/                           # Model outputs and results
│   ├── tables/                        # Regression tables
│   ├── figures/                       # Plots and visualizations
│   └── model_objects/                 # Saved model objects
├── docs/                              # Documentation
│   ├── methods.md                     # Detailed methodology
│   └── variable_definitions.md        # Variable definitions
└── utilities/                         # Helper functions
    ├── model_functions.R              # Custom modeling functions
    └── plotting_functions.R           # Custom plotting functions
```

## Suggested Fixed Effects Model Categories

Based on the research topic "A National Model for US Public Land Visitation", the following types of fixed effects specifications might be relevant:

### 1. Spatial Fixed Effects
- **State Fixed Effects**: Control for state-level unobservables
- **County Fixed Effects**: Control for county-level characteristics
- **Park/Site Fixed Effects**: Control for site-specific attributes
- **Regional Fixed Effects**: Control for broader regional differences

### 2. Temporal Fixed Effects
- **Year Fixed Effects**: Control for time trends and shocks
- **Month Fixed Effects**: Control for seasonal patterns
- **Day-of-week Fixed Effects**: Control for weekly patterns

### 3. Individual/Group Fixed Effects
- **Visitor Type Fixed Effects**: Different visitor categories
- **Activity Type Fixed Effects**: Different recreation activities
- **Origin Fixed Effects**: Visitor origin location effects

## Example Code Structure

### Fixed Effects Model Template (R)
```r
# Example fixed effects specification for visitor demand
library(fixest)
library(lfe)

# State and year fixed effects
model1 <- feols(visitation ~ distance + population + income | 
                state + year, 
                data = visitor_data)

# Site and month fixed effects with clustering
model2 <- feols(visitation ~ distance + weather + activities | 
                site + month, 
                data = visitor_data,
                cluster = ~state)

# Three-way fixed effects
model3 <- feols(visitation ~ distance + income | 
                state + year + month, 
                data = visitor_data)
```

## Documentation Standards

When adding fixed effects specifications, please include:

1. **Model Description**: Clear description of what each model tests
2. **Fixed Effects Justification**: Why specific fixed effects are included
3. **Variable Definitions**: Clear definitions of all variables
4. **Estimation Method**: Which package/method is used and why
5. **Robustness Checks**: What sensitivity analyses were performed
6. **Results Interpretation**: How to interpret the fixed effects results

## Getting Started

1. Create the directory structure above
2. Add your analysis files to the appropriate directories
3. Update README.md with specific setup instructions
4. Document your model specifications clearly
5. Include data sources and variable definitions

## Notes for Contributors

- Follow consistent naming conventions
- Comment code thoroughly
- Include session info and package versions
- Use relative paths for reproducibility
- Consider using a package like `renv` for dependency management