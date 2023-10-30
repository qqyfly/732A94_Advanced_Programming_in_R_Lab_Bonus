## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB Bonus
##
## Date Created: 2023-10-28
##
## Copyright (c) MIT
## ---------------------------

#' A S3 class provide ridgereg which provide same functions like linear regression lm().
#'
#' @name ridgereg
#' @param formula Formula
#' @param data Dataframe
#' @param lambda Lambda
#' @param qr use qr decomposition or not
#' @return ridgereg object(list) contain all the calculated values regarding the linear regression
#' @importFrom stats model.matrix pt sd
#' @export 
ridgereg <- function(formula, data, lambda = 0, qr = FALSE) {
  
  # object to save all the meta data and calculated values
  obj <- list(formula = formula,
              data = data,
              data_name = substitute(data),
              lambda = lambda,
              qr = qr,
              coefficients = NULL,
              fitted_values = NULL,
              x_name = NULL,
              x_data = NULL,
              x_sd = NULL,
              x_mean = NULL
             )
  
  # set class name
  class(obj) <- "ridgereg"
  
  #----------------START OF INIT CODE----------------
  # get all the variables
  vars <- all.vars(formula)
  
  var_count <- length(vars)
  
  # get dependent variable name and data
  y_name <- vars[1]
  y_data <- as.matrix(data[y_name])
  
  # create the design matrix X
  x_data <- model.matrix(formula, data)
  obj$x_name <- vars
  obj$x_data <- x_data
  
  
  # get row number of data
  n <- nrow(x_data)
  
  # normalize all co variates (X and Y)
  x_data_norm <- scale(x_data[,-1])
  x_data_norm <- cbind(x_data[,1],x_data_norm)
  
  x_sd <- apply(x_data[,-1], 2, sd)
  obj$x_sd <- x_sd 
  
  x_mean <- colMeans(x_data[,-1])
  obj$x_mean <- x_mean 
  
  # transpose of X_data and other calculation for later usage
  x_data_norm_t <- t(x_data_norm)
  xtx <- x_data_norm_t %*% x_data_norm
  
  # create a identity matrix
  I <- diag(ncol(x_data))
  
  # calc beta_ridge
  if(qr == FALSE){
    beta_ridge <- (solve(xtx + lambda * I) %*% x_data_norm_t) %*% y_data 
  }else{
    QR <- qr(x_data)
    Q <- qr.Q(QR)
    R <- qr.R(QR)
    beta_ridge <- (solve(t(R) %*% R  + lambda * I) %*% x_data_norm_t) %*% y_data
  }
  
  obj$coefficients <- beta_ridge
  
  # calc fitted_values (y_hat)
  fitted_values <- x_data_norm %*% beta_ridge
  obj$fitted_values <- fitted_values
  
  #----------------END OF INIT CODE----------------
  obj
}

#' print out formula, data and Coefficients of current linear regression.
#'
#' @param x ridgereg object
#' @param ... other objects
#' @export print.ridgereg
#' @export 

print.ridgereg <- function(x,...) {
  obj <- x
  cat("Call:\n")
  data_str <- paste("data = ",obj$data_name, sep="")
  formula_str <- paste("ridgereg(formula =",deparse(obj$formula))
  qr_str <- paste("qr = ",obj$qr,sep="")
  lambda_str <- paste("lambda = ",obj$lambda,")",sep="")
  
  firstPart <- paste(formula_str,data_str, qr_str, lambda_str, sep=", ")
  cat(firstPart)
  
  cat("\n\nCoefficients:\n")
  
  cat(colnames(obj$x_data))
  cat("\n")
  cat(obj$coefficients)
}

#' predict  value
#'
#' @param obj ridgereg object
#' @param x value X
#' @return predicted value
#' @export predict
#' @export
#' 
predict <- function(obj,x){
  if (is.null(x)){
    stop("input is null")
  }
  
  x_norm <- scale(x)
  prediction <- (as.matrix(cbind(1,x_norm)) %*% obj$coefficients[-1])[,]
  return(prediction)
}

#' get coefficients values
#'
#' @param obj ridgereg object
#' @export coef
#' @export

coef <- function(obj){
  obj$coefficients
}