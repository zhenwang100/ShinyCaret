# Name: ShinyCaret server
# Version: 1.0 
# Platform: R (3.4.0)
# Author: Zhen Wang
# Date: 2017/07/19

######### Prediction panel ##########
### Required global variable:
# server/preprocess.R: raw.data		# training data set before preprocessing


### Required global function:
# server/training.R: select.fit()	# get selected model in the 'training' panel


### Global variable
data.pred<-NULL		# Data set for prediction
fit.pred<-NULL		# Model used for prediction
rst.pred<-NULL		# Prediction result

# Reactive: read data set for prediction
# The data set should be before pre-processing
read.new.data<-reactive({
	# 'Use data' radioButtons
	if (input$useData == 'uploadData') {
		# Option 1: upload new data set
		inData<-input$inData		# 'Choose data file' fileInput
		if (!is.null(inData)) {
			# Input file format
    			read.csv(inData$datapath, header = input$newHeader, sep = input$newSep, quote = input$newQuote, na.string = input$newMissing)
		}
	} else if (input$useData == 'currentData') {
		# Option 2: use current data set in the 'Preprocess' panel (before pre-processing)
		return(raw.data)
	}
})

# Output: display data dataTableOutput
output$dataDisply<-renderDataTable(options = list(pageLength = 5, scrollX = '400px'), {
	data.pred<<-read.new.data()
})

# Reactive: read model for prediction
# The model is a 'train' object named 'fit'
read.model<-reactive({
	# 'Use model' radioButtons
	if (input$useModel == 'uploadModel') {
		# Option 1: load previously saved model from .Rdata file
		inFile<-input$inModel		# 'Choose model file' fileInput
    		if (is.null(inFile)) return(NULL)

		objs<-load(inFile$datapath)	# Load all objects in the .Rdata file
		
		# Check if there is a 'train' object named 'fit'
		if (length(grep("fit", objs)) == 0 || class(fit)[1] != "train") {
			showModal(modalDialog(
				img(src = "error.png"), 
				"Uploaded file does not contain a", strong("train"), "object named", strong("fit"),
				title = "Error"
			))
			return(NULL)
		} else {
			return(fit)
		}
	} else if (input$useModel == 'currentModel') {
		# Option 2: use model in the 'traning' panel 
		fit<-select.fit()		# Get selected model
		return(fit)
	}
})

# Output: display model verbatimTextOutput
output$fitDisply<-renderPrint({
	fit.pred<<-read.model()			# Get model for display
	if (!is.null(fit.pred)) {
		list("Resample evaluation summary" = fit.pred, "Final model information" = fit.pred$finalModel)
	}
})

# Function: check consitence between data set and model before prediction and return possible errors	
check.predictor<-function() {
	# Predictors used in the model
	# Note: All predictors in the model (excluding outcome) should exist in the data set.
	# But the data set can contain features not used in the model. 
	predictor<-predictors(fit.pred)

	# Whether all predictors is contained in the data set
	pred.exist<-predictor %in% colnames(data.pred)
	if (!all(pred.exist)) {
		# Missed predictors in the data set
		missed<-predictor[!pred.exist]
		return(list(rst = F, meg = paste(
			"Predictors", strong(paste(missed, collapse = " ")), " in model do not exist in data for prediction"
		)))
	}

	return(list(rst = T, meg = ""))
}

# Event: 'Start prediction' actionButton
observeEvent(input$startPred, {
	if (is.null(fit.pred) || is.null(data.pred)) {
		showModal(modalDialog(
			img(src = "error.png"), "Use a data set and a model for prediction",
			title = "Error"
		))
		return(NULL)
	}

	# Check predictors between the model and data
	check<-check.predictor()
	if (!check$rst) {
		showModal(modalDialog(
			img(src = "error.png"), HTML(check$meg),
			title = "Error"
		))
		return(NULL)
	}

	# Use the progress dialog to inactive the page because the prediction may be slow
	showModal(modalDialog(
		div(img(src = "progress.gif", height = 72, width = 72), style = "text-align: center;"),	# Center the image
		div("Predicting the data...", style = "text-align: center;"),
		title = "Progress",
		footer = NULL			# The dialog cannot be dismissed by users
	))

	# If this is a probability model for factor prediction, return the prediction probability
	model<-modelLookup(fit.pred$method)
	classProb<-ifelse(all(model$probModel) && fit.pred$modelType == 'Classification', "prob", "raw")
	
	# Key step: prediction
	# Use tryCatch to deal with unexpected errors
	pred<-tryCatch(predict(fit.pred, newdata = data.pred, type = classProb),
		error = function(e) {
			showModal(modalDialog(
				img(src = "error.png"), "Unexpected error occurred",
				renderPrint(e$message),
				title = "Error"
			))
			return(NULL)
		}
	)

	# If prediction is successful
	if (!is.null(pred)) {
		# Remove progress dialog
		removeModal()
		# Preserve prediction result as data.frame
		# 'pred' may be a vector or a data.frame determined by dimensions
		rst.pred<<-data.frame(prediction = pred)
	}
})

# Event: 'Show prediction' actionButton
observeEvent(input$showPred, {
	if (is.null(rst.pred)) {
		showModal(modalDialog(
			img(src = "error.png"), "There is no prediction result",
			title = "Error"
		))
		return(NULL)
	} else {
		# Show prediction result as a DataTable in a dialog
		showModal(modalDialog(
			img(src = "information.png"), strong("Prediction results"),
			renderDataTable(options = list(pageLength = 5), round(rst.pred, 3)),
			title = "Information",
			size = ifelse(ncol(rst.pred) > 3, "l", "m")
		))
	}
})

# Output: 'Save prediction' downloadButton
output$savePred<-downloadHandler(
	# Save the prediction result as .csv file
    	filename = "prediction.csv",
    	content = function(file) {
		if (is.null(rst.pred)) {
			showModal(modalDialog(
				img(src = "error.png"), "No prediction result for saving",
				title = "Error"
			))
		}

		# Combine the prediction result and features
		write.csv(x = data.frame(rst.pred, data.pred), file = file, quote = F, row.names = F)
    	}
)

