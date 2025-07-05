# 🦠 COVID-19 Death Analysis by Local Authority in England
**Course:** MSc Data Science

**Submitted by:** Harsh Kakkad

**Submission Date:** 4 January 2024

## 📘 Overview
This project investigates COVID-19 death tolls across districts in England (March 2020 – April 2021), focusing on household-related socio-economic factors.
The goal was to identify variables—such as transportation access, deprivation levels, and family structure—that may explain variance in death rates across local authorities.


🏫 COVID-19 death toll data was provided by my university. I independently chose to explore household-level variables as the analytical focus for this project.

## 🎯 Objectives

Evaluate correlation between household factors and COVID-19 deaths

Test statistical significance of these relationships using hypothesis testing

Determine most influential variables through multiple linear regression

Quantify each factor’s contribution to death rate variance

## 📦 Dataset Overview

Dependent Variable: Deaths per 1000 people (district-level, Mar 2020–Apr 2021)

Independent Variables (17 total) grouped into:

 🚗 Transportation Availability (e.g. car ownership per household)

👨‍👩‍👧‍👦 Family Structure (e.g. single-parent homes, elderly residents)

♿ Dependency (e.g. children, unemployment, long-term illness)

📉 Deprivation (based on 4 UK census dimensions: education, employment, health, housing)


## 📊 Methodology

🔍 Data Cleaning & Preparation

Merged multiple government datasets using district codes

Removed inconsistent districts and duplicate columns

Ensured all variables were numeric for correlation analysis

✅ Normality Testing (Target Variable)

Boxplot, Q-Q plot, Histogram, and Kolmogorov-Smirnov test

Outcome: Deaths per 1000 people is normally distributed

🔧 Feature Reduction
Removed low-correlation variables (< 0.08)

Performed independent variable correlation filtering

Used partial correlation and VIF to eliminate multicollinearity

## 📈 Modeling & Findings

🔬 Multiple Linear Regression

Built model using 9 filtered variables

Found that household factors explain ~18% of death rate variance

Most impactful factor:

Dependent_children_in_house → contributes 41% of model variance

🧪 Hypothesis Testing

p-value < 0.05 → failed to reject the null hypothesis

COVID-19 deaths are significantly linked to household characteristics

📉 Residual Diagnostics

Residuals not perfectly normally distributed

Scatter plot showed randomness, confirming valid model structure


## 🧠 Conclusion

While household-level variables don’t fully explain COVID-19 deaths, they do have a statistically significant impact. This highlights the importance of:

Local deprivation

Family composition

Physical/financial dependence during lockdown

### 📌 These findings are a foundation for preparing better support systems in future pandemics.

## 📚 References

NHS (2023). How COVID-19 spreads

ONS (2021). Household Deprivation Variables

