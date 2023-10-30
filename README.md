# 732A94 Advanced Programming in R Lab Bonus

[![R-CMD-check](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab_Bonus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab_Bonus/actions/workflows/R-CMD-check.yaml)

# Overview

The `ridgereg` package provides a lightweight framework for ridge linear regression in R. This vignette serves as a guide to understanding and using the main functions provided by the package.

# Installation

To install the `labbonus` package, use the following commands:


```
devtools::install_github("qqyfly/732A94_Advanced_Programming_in_R_Lab_Bonus", build_vignettes = TRUE)
```

# Linear Regression with ridgereg

The core function in the `labbonus` package is `ridgereg` S3 class, which initializes a linear regression model with all the calculated values stored as a list.

This example demonstrates how to create a ridgereg object and how to use all the exported functions of this `ridgereg` S3 class.

## Load required libraries

In this step, we need to load the installed library `labbonus`. The code is showed below.

The required packages will be download and installed automatically.

```
library(labbonus)
```

## Create a ridgereg object

Let's create a simple linear regression model using $iris$ data set with the formula: $Petal.Length \thicksim Species$.

```{r create-labbonus-object-normal}
ridgereg_mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)
```

or you can also use qr to calculate a ridgereg object

```{r create-labbonus-object-qr}
ridgereg_mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1, qr = TRUE)
```


## Print a Linreg object

This function use the pre created ridgereg object as a parameter, and print out some information of the regression model.

```{r print-labbonus-object}
print(ridgereg_mod)
```

## Get coef values of regression model

This function use the pre created ridgereg object as a parameter, and return the coef values of regression model.

```
coef(ridgereg_mod)
```

## Predict new values using model

This function use the pre created ridgereg object as a parameter, and return the coef values of regression model.

```
predict(ridgereg_mod,x=c(1,2,3,4))
```

## Visualize Delay & Distance Relationship

```
visualize_airport_delays()
```