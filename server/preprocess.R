# Name: ShinyCaret server
# Version: 1.0 
# Platform: R (3.4.0)
# Author: Zhen Wang
# Date: 2017/07/19

######### Preprocess panel ##########

# Global variable 
raw.data<-NULL			# Raw data for training (before preprocessing)
data<-NULL			# Data after preprocessing for visualization
preOption<-NULL			# Methods for pre-processing; used in traning 

# Reactive: 'Choose data file' fileInput
read.infile<-reactive({
	inFile<-input$infile
    	if (is.null(inFile)) return(NULL)

	# Read data according to input format
    	raw.data<<-read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, na.string = input$missing)
	# Copy raw data for preprocessing
	data<<-raw.data

	# Update features for display
	update.feat()
})

# Event: observe 'Choose data file' fileInput
observe({read.infile()})

# Function: update features for diaplaying in all panels if features are changed
# This function is shared across all panels
update.feat<-function() {
	if (is.null(data)) return(NULL)

	# Update 'content' dataTableOutput
	# Note: the render* function for 'output$contents' will be over-ride each time, so do not change it latter
	output$contents<-renderDataTable(options = list(pageLength = 5, scrollX = '400px'), {
		return(data)
  	})

	# Features for displaying
	updateSelectInput(session, "displyFeatX", "Feature X", choices = colnames(data))
	updateSelectInput(session, "displyFeatY", "Feature Y", choices = colnames(data))

	# Outcome used in 'training' panel
	updateSelectInput(session, "setOutcome", "Set outcome", choices = colnames(data))
	
	# Outcome used in 'filtering' panel
	updateSelectInput(session, "filterOutcome", "Set outcome", choices = colnames(data))
}


# Reactive: "Feature X/Y" selectInput, "Plot type" radioButtons
# Disaply features according to feature type and plot type 
display.feature<-reactive({
	feat.x<-input$displyFeatX	# 'Feature X' selectInput
	feat.y<-input$displyFeatY	# 'Feature Y' selectInput
	if (is.null(data)) return(NULL)

	# Number of missing values in feature X
	missing<-length(data[[feat.x]][is.na(data[[feat.x]])])

	if (input$displyType == "X" && is.numeric(data[[feat.x]])) {
		# One numeric feature plot (hist)
		hist(data[[feat.x]], xlab = feat.x, main = paste("# missing values:", missing, collapse = " "))
	} else if (input$displyType == "X" && is.factor(data[[feat.x]])) {
		# One factor feature plot (barplot)
		plot(data[[feat.x]], xlab = feat.x, ylab = 'Frequency', main = paste("# missing values:", missing, collapse = " "))
	} else if (input$displyType == "Y~X") {
		# Two features plot (scatter plot / boxplot)
		plot(data[[feat.y]]~data[[feat.x]], xlab = feat.x, ylab = feat.y, main = '')
	}
})

# Output: 'feature plot' plotOutput
output$featPlot<-renderPlot({
	display.feature()
})

# Event: 'Delete feature X' button
observeEvent(input$delFeat, {
	feat<-input$displyFeatX		# 'Feature X' selectInput
	if (feat == "") {
		showModal(modalDialog(
			img(src = "error.png"), "No feature is selected for deleting",
			title = "Error"
		))
		return(NULL)
	}

	# Delete the feature from raw data. This action cannot be reverted.
	raw.data[feat]<<-NULL
	data[feat]<<-NULL
	
	# Updating features and re-rendering data table
	update.feat()

	showModal(modalDialog(
		img(src = "warning.png"), "Selected feature is deleted. This action cannot be reverted.",
		title = "Warning"
	))

})

# Event: 'Apply option' actionButton
observeEvent(input$preProc, {
	if (is.null(data)) {
		showModal(modalDialog(
			img(src = "error.png"), "No data is selected for pre-processing",
			title = "Error"
		))
		return(NULL)
	}	
		
	# Preserve the pre-processing options	
	preOption<<-input$preOption		# 'Option' checkboxGroupInput
	if (is.null(preOption)) {
		# Show raw data if no pre-process option
		data<<-raw.data
		showModal(modalDialog(
			img(src = "information.png"), "No preprocessing is applied to the data",
			title = "Information"
		))
	} else {
		# Pre-processing is only for data but not raw data.
		# Note: Pre-processing here is for visualization only. The options will be incoporated in training.
		preProc<-preProcess(raw.data, method = preOption)
		data<<-predict(preProc, raw.data)

		# Show pre-processing summary
		# Note: Pre-processing can be reverted
		showModal(modalDialog(
			img(src = "information.png"), strong("Filter summary"),
	       		renderPrint(preProc),	
			"Note: The options of preprocessing can be reverted.",
			title = "Information"
		))
	}

	# Updating features and re-rendering data table
	update.feat()
})

