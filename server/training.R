# Name: ShinyCaret server
# Version: 1.0 
# Platform: R (3.4.0)
# Author: Zhen Wang
# Date: 2017/07/19

######### Training panel ##########
### Required global variable:
# server/preprocess.R: data		# training data set after preprocessing 
# server/preprocess.R: raw.data		# training data set before preprocessing
# server/preprocess.R: preOption	# preprocessing options


### Global variable
fits<-list()	# Lists of model fit

# Event: 'Show model' actionButton
observeEvent(input$showModel, {
	model<-input$setModel		# 'Set model' selectInput 
	showModal(modalDialog(
		img(src = "information.png"),
		# Information include: prarameters, for regression or classification, for probability 
		renderTable(modelLookup(model)),
		title = "Information"
	))	
})

# Event: 'Hyperparameter grid' actionButton
observeEvent(input$showGrid, {
	model<-input$setModel		# 'Set model' selectInput
       	# Get model information with precision match	
	info<-getModelInfo(model, regex = F)
	# Default grid is given as a function
	showModal(modalDialog(
		img(src = "information.png"),
		renderPrint(info[[model]]$grid),
		title = "Information"
	))	
})

# Function: update 'Set metric' selectInput according to outcome type (factor or numeric)
# Parameters: names of 'Set outcome' and 'Set metric' selectInput 
# This function can be shared by feature selection and model training
update.metric<-function(outcomeInput, metricInput) {
	outcome<-input[[outcomeInput]]		# 'Set outcome' selectInput
	if (outcome == '') return(NULL)
	
	if (is.factor(data[[outcome]]) && length(levels(data[[outcome]])) == 2) {
		# Outcome is a factor with two levels (ROC could be used)
		updateSelectInput(session, metricInput, "Set metric", choices = c("ROC", "Accuracy", "Kappa"))
	} else if (is.factor(data[[outcome]]) && length(levels(data[[outcome]])) > 2) {
		# Outcome is a factor with more than two levels
		updateSelectInput(session, metricInput, "Set metric", choices = c("Accuracy", "Kappa"))
	} else if (is.numeric(data[[outcome]])) {
		# Outcome is numeric
		updateSelectInput(session, metricInput, "Set metric", choices = c("RMSE", "Rsquared"))
	}	
}

# Event: 'Set outcome' selectInput
observeEvent(input$setOutcome, {
	update.metric("setOutcome", "setMetric")
})

# Function: check model settings before training and return possile errors
check.model.setting<-function() {
	model<-input$setModel			# 'Set model' selectInput
	outcome<-input$setOutcome		# 'Set outcome' selectInput
	metric<-input$setMetric			# 'Set metric' selectInput
	modelTab<-modelLookup(model)		# Model information table

	if (is.factor(data[[outcome]]) && !all(modelTab$forClass)) {
		# Return the error if the model is not suitable for predicting factor 
		return(list(rst = F, meg = paste(
			"Outcome", strong(outcome), "is factor but not supported by model", strong(model)
		)))
	} else if (is.numeric(data[[outcome]]) && !all(modelTab$forReg)) {
		# Return the error if the model is not suitable for predicting numeric values
		return(list(rst = F, meg = paste(
			"Outcome", strong(outcome), "is numeric but not supported by model", strong(model)
		)))
	} else if (metric == 'ROC' && !all(modelTab$probModel)) {
		# Return the error if the model cannot give predition probability required by ROC
		return(list(rst = F, meg = paste(
			"Metric", strong(metric), "requires probability model but not supported by model", strong(model)
		)))
	} else if (any(is.na(raw.data[[outcome]]))) {
		# NA in outcome is generally not allowed for training
		return(list(rst = F, meg = paste(
			"Outcome", strong(outcome), "do not allow missing values"
		)))
	}

	# Note: Whether missing value is allowed in predictors depends on the model, so we do not check it here.
	# If NA is not allowed in predictors, unexpected error will be thrown during training
	# We recommond imputation during pre-processing, but it does not applied for factors. 	

	# No errors above
	return(list(rst = T, meg = ""))	
}

# Event: 'Start training' actionButton
observeEvent(input$startTrain, {
	if (is.null(data)) {
		showModal(modalDialog(
			img(src = "error.png"), "No data is selected for training",
			title = "Error"
		))
		return(NULL)
	}

	# Check model setting and return the error message
	check<-check.model.setting()
	if (!check$rst) {
		showModal(modalDialog(
			img(src = "error.png"), HTML(check$meg),
			title = "Error"
		))
		return(NULL)	
	}

	# Get resample method and setting
	number<-input$resampleNum		# 'Bootstrap times / K-fold cross validation' sliderInput
	repeats<-input$resampleRep		# 'Repeat times' sliderInput
	if (input$resampleMed == 'boot') number<-number * repeats

	outcome<-input$setOutcome		# 'Set outcome' selectInput
	model<-input$setModel			# 'Set model' selectInput

	# Key step: set train control
	fitControl<-trainControl(method = input$resampleMed, number = number, repeats = repeats, 
		# User choose if all resample metrics should be preserved; otherwise only summary metric is preserved
		returnResamp = ifelse(input$resampleMet, 'all', 'final'),
		# twoClassSummary is used for ROC
		summaryFunction = ifelse(input$setMetric == 'ROC', twoClassSummary, defaultSummary),
		# Prediction probability is computed if model support and outcome is a factor	
		classProbs = ifelse(all(modelLookup(model)$probModel) && is.factor(data[[outcome]]), T, F)
	)
 
	# Use the progress dialog to inactive the page because the training below may be slow
	showModal(modalDialog(
		div(img(src = "progress.gif", height = 72, width = 72), style = "text-align: center;"),	# Center the image
		div("Training the model...", style = "text-align: center;"),
		title = "Progress",
		footer = NULL			# The dialog cannot be dismissed by users
	))
	
	# Key step: model training
	# The process may have unexpected error, so use tryCatch 
	fit<-tryCatch(
		# Training should applied to raw data before pre-processing
		# Integrating pre-processing into training is convenient for future prediction on new data
		train(y = raw.data[[outcome]], x = raw.data[names(raw.data) != outcome],
                 	method = model, 
                 	trControl = fitControl,
			metric = input$setMetric,
			preProcess = preOption
		), 

		# If unexpected error occur, throw the error dialog
		error = function(e) {
			showModal(modalDialog(
				img(src = "error.png"), "Unexpected error occurred",
				renderPrint(e$message),
				title = "Error"
			))
			return(NULL)
		}
	)

	# If the training is successful
	if (!is.null(fit)) {
		# Remove the progress dialog
		removeModal()
		# Add the new fit to the fit list
		fits[[length(fits) + 1]]<<-fit
		# Update 'Select fit' selectInput
		updateSelectInput(session, "selectFit", "Select fit", choices = length(fits):1)
	}
})

# Reactive: 'Select fit' selectInput
select.fit<-reactive({
	# Get selected fit from the fit list
	fitId<-input$selectFit
	if (fitId == '') return(NULL)
	fits[[as.numeric(fitId)]]
})

# Output: 'Fit summary' verbatimTextOutput
output$fitSum<-renderPrint({
	# Get selected fit
	fit<-select.fit()
	if (!is.null(fit)) {
		# 'fit' is a summary of metrics by hyperparamters based on resamples
		# 'fit$finalModel' is information of the best model
		list("Resample evaluation summary" = fit, "Final model information" = fit$finalModel)
	}
})

# Reactive: 'Plot type' radioButtons
# The plot is used for visializing resample metrics by hyperparameter grid
get.fit.plot.type<-reactive({
	type<-input$fitPlotType
})

# Output: 'Fit plot' plotOutput
output$fitPlot<-renderPlot({
	fit<-select.fit()			# Get selected fit
	type<-get.fit.plot.type()		# 'Plot type' radioButtons

	if (!is.null(fit)) {
		if (nrow(fit$results) == 1) {
			# For model without hyperparameter or with fixed hyperparameter, do not plot
			showModal(modalDialog(
				img(src = "warning.png"), "This fit has no hyperparameter grid for tuning",
				title = "Warning"
			))	
		} else if (type == 'final') {
			# Only plot summary metric of resamples (one metric for one hyperparameter setting)
			plot(fit)
		} else if (type == 'all' && fit$control$returnResamp == 'all') {
			# Plot metrics of all resamples (one metric for one resample)
		       	# All resample metrics should be preserved	
			xyplot(fit)
		} else if (type == 'all' && fit$control$returnResamp != 'all') {
			# Throw a warning if not all resample metrics are preserved
			showModal(modalDialog(
				img(src = "warning.png"), "Please select", strong("Preserve all resample metric"), "for the plot",
				title = "Warning"
			))	
		}
	}
})

# Event: 'Show feature importance' actionButton
observeEvent(input$showFeature, {
	fit<-select.fit()		# Get selected fit
	if (is.null(fit)) {
		showModal(modalDialog(
			img(src = "error.png"), "No fit is selected for displaying",
			title = "Error"
		))
	} else {
		# Get feature importance
		feat.imp<-varImp(fit)
		feat.mat<-feat.imp$importance

		# Display feature importance by DataTable in a dialog
		showModal(modalDialog(
			img(src = "information.png"),
			strong(feat.imp$model, " feature importance"),
			renderDataTable(options = list(pageLength = 5), 
				cbind(Feature = rownames(feat.mat), feat.mat)
			),
			title = "Information",
			size = ifelse(ncol(feat.mat) > 2, "l", "m")
		))
	}	
})

# Event: 'Show resample' actionButton
observeEvent(input$showResample, {
	fit<-select.fit()		# Get selected fit
	if (is.null(fit)) {
		showModal(modalDialog(
			img(src = "error.png"), "No fit is selected for displaying",
			title = "Error"
		))
	} else {
		# Display resample metrics by DataTable in a dialog
		showModal(modalDialog(
			img(src = "information.png"),
			renderDataTable(options = list(pageLength = 5), fit$resample),
			title = "Information",
			size = "l"
		))
	}	
})

# Output: 'Save model' downloadButton
output$saveModel<-downloadHandler(
	# Save the model as .Rdata file
    	filename = 'fit.Rdata',
    	content = function(file) {
		fit<-select.fit()		# Get selected fit
		if (is.null(fit)) {
			showModal(modalDialog(
				img(src = "error.png"), "No model is selected for saving",
				title = "Error"
			))
		}

		# Saved model is a 'train' object named 'fit'
		# It also contains options for pre-processing
		save(fit, file = file)
    	}
)

