# Examining-the-Association-between-Sex-and-Health-Events

This is a project for SPH BS 723 (Introduction to Statistical Computing) using SAS and SPH BS 730 (Introduction to R: Software for Statistical Computing) where I analyzed the association between sex and specific health events including: hypertension, diabetes, myocardial infarction, COPD, etc., using a subset of data from the 2014 Native Hawaiian and Pacific Islander National Health Interview Survey (NHPI NHIS) containing 2590 observations and 14 variables. The same analysis was done in both SAS and R.

Data preparation:
- Recoded certain responses to certain variables to missing (. in SAS and NA in R)
- Created two new variables
- Recoded a number of numerical variables to categorical/dichotomous
- Recoded one categorical variable to have less levels by combining and reorganizing categories

Statistical Analyses:
- Descriptive statistics for continuous and categorical variables
- Preliminary t-tests and chi-square tests for the association of health events between males and females
- 1-sample t-test of male and female BMI in this dataset against national mean BMIs
- 2-sample t-test for significant differences in BMI between males and females in our sample
- Chi-square test of independence for association between cancer and sex among current smokers only
