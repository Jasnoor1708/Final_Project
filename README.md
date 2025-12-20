## Project Overview

We study which child- and household-level factors predict school dropout and evaluate how changes in key risk factors (academic performance, household wealth, nutrition, household size) affect predicted dropout rates.

Methods used:
- Logistic regression (interpretable baseline)
- Random forest models with class imbalance handling
- Sensitivity analysis for policy simulation

---

## Data

### Original Dataset

This project uses data from the **Young Lives Longitudinal Study**, Younger Cohort, focusing on India and Peru.

- Data provider: Young Lives / UK Data Service  
- Access:  registration required to access 
- Data portal:  https://datacatalogue.ukdataservice.ac.uk/studies/study/6853#details
- UK Data Service catalogue: https://www.younglives.org.uk/


### Analytical Dataset

- `young_lives_dropout_r3_r4.csv`  
  Cleaned and merged dataset combining Round 3 and Round 4 data, constructed from the original Young Lives files.  
  Includes enrollment status, academic performance, health indicators, and household characteristics.

---

## Repository Structure

### Main Analysis Files
- `Final_Policy_5.qmd`  
  Final paper with full analysis, results, figures, and references.

- `Final_Policy-2.qmd`, `Final_Policy-3.qmd`, `Final_Policy.qmd`  
  Intermediate drafts and revisions of the final paper.

### Data Preparation
- `FULL_DATA_MERGE_CODE.R`  
  Script used to clean and merge Young Lives data across rounds.

- `R4_R3_COMBINED_CODE.R`  
  Script constructing the dropout variable and analytical sample.

### Parametric Analysis
- `younglives_parametric.R`  
  Logistic regression models and related diagnostics.

### Machine Learning and Sensitivity Analysis
- `non parameter training code.R`  
  Random forest models with class imbalance handling.

- `sensitivity_bmi_size.R`  
  Sensitivity simulations for BMI and household size.

- `sensitivity_results.csv`  
  Output from sensitivity analysis.


### Reproducibility
- `Final_Project.Rproj`  
  RStudio project file.

- `.gitignore`  
  Standard Git ignore file.

---

## How to Reproduce the Analysis

1. Obtain access to the Young Lives data through the UK Data Service.
2. Run `FULL_DATA_MERGE_CODE.R` and `R4_R3_COMBINED_CODE.R` to construct the analytical dataset.
3. Run `younglives_parametric.R` for logistic regression results.
4. Run `non parameter training code.R` and `sensitivity_bmi_size.R` for machine learning and policy simulations.
5. Render `Final_Policy-5.qmd` to reproduce the final report.

---

## Contributors

- Jasnoor Anand  
- Sanaa Kashif  
- Mehria Saadat Khan  

---

## Notes

This project was completed as part of a graduate-level data science course.  
All analysis decisions, limitations, and assumptions are documented in the final paper.
