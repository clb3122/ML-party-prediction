# Predicting Party Identification with ANES 2024 Data

## Overview
This project uses the 2024 American National Election Study (ANES) to predict party identification using various classification models. We compare the predictive power of demographic characteristics vs. policy opinions across five methods: decision trees, KNN, logistic regression, lasso, and ridge regression.

## Data
- **Source:** ANES 2024, multiply imputed via MICE
- **Outcome:** Party identification (`Democrat`, `Republican`, `Independent`)
- **Two samples:** Full sample including independents and a filtered two-party sample including Democrats and Republicans only

## Predictors

**Demographics** (17 variables): age, gender, transgender status, sexual orientation, race, birthplace, marital status, education, household income, rent/own home, occupation, union status, military service, 
gun ownership, urban/rural identity, religion, religious attendance.

**Issue opinions** (37 variables): Respondants opinions about government and democracy, economic policy, social and cultural issues, criminal justice, immigration, and foriegn policy.

## Notes
- For more detailed information about the dataset or variables, please refer to the [codebook](https://electionstudies.org/wp-content/uploads/2025/08/anes_timeseries_2024_userguidecodebook_20250808.pdf).
- Survey weights are not applied. These results describe the sample, not the general electorate.
- `set.seed(123)` is used for reproducibility.

## Contributions
**Graham**: variable selection, lasso model, and ridge regression model.

**Collin**: data cleaning, multiple imputation, logistic regression, KNN, and tree models.

