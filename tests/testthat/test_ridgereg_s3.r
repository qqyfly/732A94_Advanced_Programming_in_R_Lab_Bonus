#context("ridgereg")

data("iris")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_s3_class(ridgereg_mod, "ridgereg")
})

test_that("print() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_output(print(ridgereg_mod),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris")
  expect_output(print(ridgereg_mod),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("predict() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  expect_equal(round(unname(predict(ridgereg_mod,c(1,2,3,4))),2) ,  c(-2.29, -1.15, -0.01,1.12))    
})

test_that("coef() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_true(all(round(unname(coef(ridgereg_mod)),2) %in% c(3.76, -0.58, 1.47)))
})


