# Name: ShinyCaret server
# Version: 1.0 
# Platform: R (3.4.0)
# Author: Zhen Wang
# Date: 2017/07/19

# Load neccessary packages.
# The version of the packages should be compatible with R version.
# Other packges required by 'caret' may be loaded automatically if necessary.
# Packages for each model can be found at http://topepo.github.io/caret/available-models.html.
# Note: Functions in 'shiny' and 'caret' would be greatly changed across versions.  
library(shiny)		# version: 1.0.3 (http://shiny.rstudio.com/)
library(caret)		# version: 6.0-76 (http://topepo.github.io/caret/index.html)

# Tree model package
library(randomForest)		# Random forest
library(rpart)			# Classification and regression tree (CART)
library(gbm)			# Stochastic gradient boosting
# Linear model packages
library(glmnet)			# Lasso and elastic-net regularized generalized linear model
# Spline package		# Multivariate adaptive regression spline
library(earth)
# SVM package
library(kernlab)		# Support vector machine
# Neural network package
library(nnet)			# Neural network
library(deepnet)		# Stacked auto-encoder deep neural network 

# Upload file size limitation (default 5M; set to 30M)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

# The function receives events from user input (UI) as reactive expressions.
# Server functions such as 'observe', 'observeEvent' and 'render***' are responsible for dealing with the reactive expressions.
shinyServer(function(input, output, session) {
	# Source for each panel	    
	source("server/preprocess.R", local = T)    
	source("server/training.R", local = T)
	source("server/prediction.R", local = T)
	source("server/filtering.R", local = T)
})

