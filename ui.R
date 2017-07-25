# Name: ShinyCaret UI
# Version: 1.0 
# Platform: R (3.4.0)
# Author: Zhen Wang
# Date: 2017/07/19

# Load neccessary packages.
# The version of the packages should be compatible with R version.
library(shiny)			# version: 1.0.3 (http://shiny.rstudio.com/)

# Present user interface
shinyUI(fluidPage(
	titlePanel("ShinyCaret v1.0"),

	# Each main step is a 'tabPanel'.
	# Each tabPanel is a 'siderbarLayout', with 'siderbarPanel' displays inputs and 'mainPanel' displays outputs.   
  	tabsetPanel(
		# Step 1: preprocess
    		tabPanel("Preprocess", sidebarLayout(
			sidebarPanel(
				h4("Training data set"),
				checkboxInput('header', 'Header', TRUE),
      				radioButtons('sep', 'Separator', inline = T,
                   			choices = c(Comma = ',', Tab = '\t', Semicolon = ';'), selected = ','
				),
				radioButtons('quote', 'Quote', inline = T,
					choices = c(None = '', 'Double quote' = '"', 'Single quote' = "'"), selected = ''
				),
				radioButtons('missing', 'Missing value', inline = T,
					choices = c(Blank = "", 'NA' = "NA"), selected = ""
				),
				fileInput('infile', 'Choose data file',
                			accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
				),

				
				hr(),
				h4("Display and filter feature"),
				selectInput("displyFeatX", "Feature X", choices = NULL),
				selectInput("displyFeatY", "Feature Y", choices = NULL),
				radioButtons("displyType", "Plot type", choices = c("X", "Y~X"), inline = T),
				div(actionButton("delFeat", "Delete feature X"), style = "text-align: center;"),

				hr(),
				h4("Pre-process"),

				# Frequently used preprocess options in caret
				# http://topepo.github.io/caret/pre-processing.html
				checkboxGroupInput("preOption", "Option", inline = T,
					choices = c(Center = "center", Scale = "scale", 
						"Impute by median" = "medianImpute",
						"Near-zero variance filter" = "nzv",
						"High correlation filter" = "corr")
				),
				div(actionButton("preProc", "Apply option"), style = "text-align: center;")
			),
			mainPanel(
		 		dataTableOutput('contents'),
				plotOutput("featPlot")
    			)
		)),

		# Step 2: filtering (feature selection)
		tabPanel("Filtering", sidebarLayout(
			sidebarPanel(
				h4("Feature selection control"),
				radioButtons("filterType", "Filter type", inline = T,
					choices = c("Recursive elimination" = "rfe", "Univariate filter" = "sbf") 
				),

				# Model for evaluating filter in caret
				# http://topepo.github.io/caret/recursive-feature-elimination.html
				# For 'rfe', the complete model name should be **Funcs; for 'sbf', it should be **SBF.
				# Generally, the model could be constructed by caretFuncs, but only pre-defined models are listed here.
				selectInput("filterModel", "Filter model", choices = c(
					"Random forest" = "rf", "Bagged trees" = "treebag",
					"Linear regression" = "lm", "Naive Bayes" = "nb"
				)),

				hr(),
				selectInput("filterOutcome", "Set outcome", choices = NULL),
				selectInput("filterMetric", "Set metric", choices = NULL),
				numericInput("filterSetNum", "Number of feature set (recursive elimination only)", 5, min = 1, step = 1),

				hr(),
				radioButtons("filterResampleMed", "Resample method", inline = T,
             				choices = c("Cross validation" = "repeatedcv", "Bootstrap" = "boot"
				)),
				sliderInput("filterResampleNum", "Bootstrap times / K-fold cross validation", 3, 10, 5),
				sliderInput("filterResampleRep", "Repeat times", 1, 10, 1),
				checkboxInput('filterResampleMet', 'Preserve all resample metric', TRUE),

				hr(),
				div(actionButton("filterStart", "Start feature selection"), style = "text-align: center;"),

				hr(),
				h4("Display filter and apply"),
				selectInput("filterSel", "Select filter", choices = NULL),
				
				actionButton("filterFeat", "Show selected features"),
				actionButton("filterResample", "Show resample"),

				hr(),
				actionButton("filterApply", "Apply selected filter"),
				downloadButton('filterSave', 'Save data')

			),
			mainPanel(
				verbatimTextOutput("filterSum"),
				plotOutput("filterPlot")
			)
		)),

		# Step 3: training
		tabPanel("Training", sidebarLayout(
			sidebarPanel(
				h4("Training control"),

				# Popular learning models supported by caret.
				# Corresponding packages should be installed on the server.
				# Caret method index: http://topepo.github.io/caret/available-models.html
				# Popular R packages: http://www.sohu.com/a/125507307_465975
				# Package categories: http://www.sohu.com/a/147336859_324765
				selectInput("setModel", "Set model", choices = c(
					# Tree model
					"Random forest" = "rf",
				        "Classification and regression tree" = "rpart",
					"Stochastic gradient boosting" = "gbm",
					# Linear model
					"Generalized linear model" = "glm",
					"Regularized generalized linear model" = "glmnet",
					# K-nearest neighbors
					"N-nearest neighbors" = "knn",
					# Spline
					"Multivariate adaptive regression spline" = "earth",
					# SVM
					"Support vector machine (linear kernel)" = "svmLinear",
					"Support vector machine (polynomial kernel)" = "svmPoly",
					"Support vector machine (radial basis kernel)" = "svmRadial",
					# Neural network
					"Neural network" = "nnet", 
					"Stacked auto-encoder " = "dnn"
				)),
				actionButton("showModel", "Show model"),
				actionButton("showGrid", "Hyperparameter grid"),

				hr(),
				selectInput("setOutcome", "Set outcome", choices = NULL),
				selectInput("setMetric", "Set metric", choices = NULL),

				hr(),
				radioButtons("resampleMed", "Resample method", inline = T,
             				choices = c("Cross validation" = "repeatedcv", "Bootstrap" = "boot"
				)),
				sliderInput("resampleNum", "Bootstrap times / K-fold cross validation", 3, 10, 5),
				sliderInput("resampleRep", "Repeat times", 1, 10, 1),
				checkboxInput('resampleMet', 'Preserve all resample metric', TRUE),
				
				hr(),
				div(actionButton("startTrain", "Start training"), style = "text-align: center;"),

				hr(),
				h4("Display model fit"),
				selectInput("selectFit", "Select fit", choices = NULL),
				radioButtons("fitPlotType", "Plot type", inline = T,
					     choices = c("Resample final" = "final", "Resample all" = "all"
				)),
				actionButton("showFeature", "Show feature importance"),
				actionButton("showResample", "Show resample"),
				
				hr(),
				downloadButton('saveModel', 'Save model')

			),
			mainPanel(
				verbatimTextOutput("fitSum"),
				plotOutput("fitPlot")
			)		  
		)),

		# Step 4: prediction
    		tabPanel("Prediction", sidebarLayout(
			sidebarPanel(
				h4("Prediction data set"),

				radioButtons("useData", "Use data",
					choices = c("Upload new data" = "uploadData", "Use current data" = "currentData")
				),
				
				checkboxInput('newHeader', 'Header', TRUE),
      				radioButtons('newSep', 'Separator', inline = T,
                   			choices = c(Comma = ',', Tab = '\t', Semicolon = ';'), selected = ','
				),
				radioButtons('newQuote', 'Quote', inline = T,
					choices = c(None = '', 'Double quote' = '"', 'Single quote' = "'"), selected = ''
				),
				radioButtons('newMissing', 'Missing value', inline = T,
					choices = c(Blank = "", 'NA' = "NA"), selected = ""
				),
				     
				fileInput("inData", "Choose data file",
					accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
				),
				
				hr(),
				h4("Prediction model"),

				radioButtons("useModel", "Use model",
					choices = c("Upload saved model" = "uploadModel", "Use currrent model" = "currentModel")
				),
				fileInput("inModel", "Choose model file",
                			accept = c('Rdata', 'R Workspace', '.Rdata')
				),
			
				hr(),
				div(actionButton("startPred", "Start prediction"), style = "text-align: center;"),
				
				hr(),
				actionButton("showPred", "Show prediction"),
				downloadButton('savePred', 'Save prediction')
			),
			mainPanel(
				 dataTableOutput('dataDisply'),
				 verbatimTextOutput("fitDisply")
			)
		))
  	)
))

