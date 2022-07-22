# statistical-models-car-insurance
This repo contains the source code of my project carried out during my academic experience at the University of Bologna and subsequently updated to improve the results.

## Goal
The primary goal of the project is to estimate the pure premium of an auto insurance portfolio and create tariff classes to identify policyholders' risk factors.

## Data and Tools
Data comes from the R [`CASdatasets`](http://cas.uqam.ca/pub/web/CASdatasets-manual.pdf) package, a collection of datasets originally for the book *["Computational Actuarial Science with R"](https://www.amazon.it/Computational-Actuarial-Science-Arthur-Charpentier/dp/1466592591)* edited by Arthur Charpentier. 

In particular, the datasets used are `freMTPLfreq`, which contains the risk characteristics and the number of claims per policy (413,169 policies), and `freMTPLsev`, which contains the claim amount and the corresponding policy ID.


## Summary of the project

#### Data preparation and EDA:

- Converted numerical features into categorical/ordinal (age of the driver, age of the car, population density) to create tariff risk classes

- Removed policies with large claims (100th percentile of the distribution of the claim amount variable)

- Exploratory analysis of features and outcomes

#### Modeling:
Following the actuarial practice, the pure premium is obtained by multiplying two components, the estimated claim frequency and cost.

Therefore, two models are estimated separately, one for the claim frequency and one for the average claim amount (severity).

As the premium and the new tariff classes will also have to be applied to future policies, Cross Validation techniques are used to select the most relevant features and the most accurate predictive models.

The analysis is structured in the following steps:

- Splitting data into training and test set

- Feature selection using the [Random Forest](https://en.wikipedia.org/wiki/Random_forest) algorithm

- Comparison of different counting data models ([Poisson Regression](https://en.wikipedia.org/wiki/Poisson_regression), Negative Binomial, [Zero-Inflated](https://en.wikipedia.org/wiki/Zero-inflated_model) and [Hurdle models](https://en.wikipedia.org/wiki/Hurdle_model)), evaluated using the [Dawid-Sebastiani scoring rule](https://stats.stackexchange.com/questions/71720/error-metrics-for-cross-validating-poisson-models).

- Gamma Regression for Severity modeling

#### Pricing and Relativities calculation:

After choosing the best models using the test sample, they are fitted on the full dataset to calculate the pure premium and then the accuracy of the prediction is evaluated with respect to the observed data, using MAE and RMSE.

The coefficients estimated on the full sample of the two chosen models are exponentiated, and the relativities of the risk factors are calculated from these values.


