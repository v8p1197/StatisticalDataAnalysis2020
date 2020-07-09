# StatisticalDataAnalysis2020

Statistical Data Analysis project for Università degli Studi di Salerno. Academic year 2019/2020. 

## Team members

* [Giovanni Ammendola](https://github.com/giorge1)
* [Vincenzo Petrone](https://github.com/v8p1197)
* [Andrea Valitutto](https://github.com/andrewvali)
* [Salvatore Ventre](https://github.com/salventre)

## Documentation

See the [documentation](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/ReportSDA2020.pdf) to understand the goals of the project.

## Usage

### Python

To use the [ColabSDA2020.ipynb](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/Python/ColabSDA2020.ipynb) Python colab notebook, download it and save a copy in your drive.

### R

* To correctly use the script, run [packages.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/packages.R) and [models.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/models.R) first
* [preprocessing.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/preprocessing.R) will merge the datasets into [preprocessed_complete.csv](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/Data/preprocessed_complete.csv)
  - There is no need to run it, you can just import the final dataset
* [linear_regression.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/linear_regression.R) contains fits a multiple linear regression for all the models
* [resampling_methods.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/resampling_methods.R) applies the Validation Set Approach and K-Fold Cross-Validation to estimate the test MSE and the Bootstrap to estimate the coefficients standard errors
* [fit_approach_linear.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/fit_approach_linear.R) applies subset selection methods to the linear model
* [fit_approach_poly2.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/fit_approach_poly2.R) applies subset selection methods to the polynomial model of degree 2
* [fit_approach_poly3.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/fit_approach_poly3.R) applies subset selection methods to the polynomial model of degree 3
* [fit_approach_poly4.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/fit_approach_poly4.R) applies subset selection methods to the polynomial model of degree 4
* [regularization_linear.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/regularization_linear.R) applies Ridge and LASSO regularization to the linear model
* [regularization_poly2.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/regularization_poly2.R) applies Ridge and LASSO regularization to the polynomial model of degree 2
* [regularization_poly3.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/regularization_poly3.R) applies Ridge and LASSO regularization to the polynomial model of degree 3
* [regularization_poly4.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/regularization_poly4.R) applies Ridge and LASSO regularization to the polynomial model of degree 4

* [pcr_pls_linear.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/pcr_pls_linear.R) applies PCR and PLS methods to the linear model
* [pcr_pls_poly2.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/pcr_pls_poly2.R) applies PCR and PLS methods to the polynomial model of degree 2
* [pcr_pls_poly3.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/pcr_pls_poly3.R) applies PCR and PLS methods to the polynomial model of degree 3
* [pcr_pls_poly4.R](https://github.com/v8p1197/StatisticalDataAnalysis2020/blob/master/R/pcr_pls_poly4.R) applies PCR and PLS methods to the polynomial model of degree 4
