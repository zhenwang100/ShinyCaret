# Name: ShinyCaret server
# Version: 1.0 
# Platform: R (3.4.0)
# Author: Zhen Wang
# Date: 2017/07/19

######### Filtering panel ##########
### Required global variable:
# server/preprocess.R: data		# training data set after preprocessing 
# server/preprocess.R: raw.data		# training data set before preprocessing 

### Required global function:
# server/traning.R: update.metric()	# set metrics according to outcome type (factor or numeric)
# server/preprocess.R: update.feat()	# Update features for displaying in case of change

### Global variable
filters<-list()		# a list of filters


# Event: 'Set outcome' selectInput
observeEvent(input$filterOutcome, {
	# Set metrics according to outcome type (factor or numeric)
	update.metric("filterOutcome", "filterMetric")
})

# Function: check filter settings before feature selection and return possible errors
check.filter.setting<-function() {
	outcome<-input$filterOutcome	# 'Set outcome' selectInput
	model<-input$filterModel	# 'Filter model' selectInput

	# There seems no general way to check the filter setting, so we have to check case by case 
	if (is.factor(data[[outcome]]) && model == 'lm') {
		# lm* functions do not support factor
		return(list(rst = F,  meg = paste(
			"Outcome", strong(outcome), "is factor but not supported by model", strong(model))
		))
	} else if (is.numeric(data[[outcome]]) && model == 'nb') {
		# nb* functions do not support numeric
		return(list(rst = F,  meg = paste(
			"Outcome", strong(outcome), "is numeric but not supported by model", strong(model))
		))
	} else if (any(is.na(raw.data[[outcome]]))) {
		# NA in outcome is generally not allowed
		return(list(rst = F, meg = paste(
			"Outcome", strong(outcome), "do not allow missing values")
		))
	}

	# Note: Whether missing value is allowed in predictors depends on the model, so we do not check it here.
	# If NA is not allowed in predictors, unexpected error will be thrown during feature selection.
	# We recommond imputation during pre-processing, but it does not applied for factors. 	

	# No errors above
	return(list(rst = T, meg = ""))
}

# Function: get filter model name according to filter type
get.filter.model<-function() {
	type<-input$filterType			# 'Filter type' radioButtons	
	model<-input$filterModel		# 'Filter model' selectInput (model name suffix) 

	model<-if (type == 'rfe') {
		# For recursive elimination, add 'Funcs'
		paste(model, "Funcs", sep = '')
	} else if (type == 'sbf') {
		# For univariant selection, add 'SBF'
		paste(model, "SBF", sep = '')
	}
}


# Event: 'Start feature selection' actionButton
observeEvent(input$filterStart, {
	if (is.null(data)) {
		showModal(modalDialog(
			img(src = "error.png"), "No data is selected for feature selection",
			title = "Error"
		))
		return(NULL)
	}

	# Check user setting and return possible errors
	check<-check.filter.setting()
	if (!check$rst) {
		showModal(modalDialog(
			img(src = "error.png"), HTML(check$meg),
			title = "Error"
		))
		return(NULL)	
	}

	type<-input$filterType			# 'Filter type' radioButtons
	outcome<-input$filterOutcome		# 'Set outcome' selectInput		
	metric<-input$filterMetric		# 'Set metric' selectInput

	# Convert filter model from character to symbol for evaluation
	model<-get.filter.model()
	model<-eval(as.symbol(model))

	# Change summary function to twoClassSummary if metric is ROC 
	model$summary<-ifelse(metric == 'ROC', twoClassSummary, defaultSummary)

	# Get resample method and setting
	number<-input$filterResampleNum		# 'Bootstrap times / K-fold cross validation' sliderInput
	repeats<-input$filterResampleRep	# 'Repeat times' sliderInput
	if (input$filterResampleMed == 'boot') number<-number * repeats		# 'Resample method' RadioButtons

	# Number of feature set for evaluation (only used in 'Recursive elimination')
	filterSetNum<-input$filterSetNum	# 'Number of feature set' numericInput
	
	# Set number should larger than 1 and smaller than all predictors 
	if (filterSetNum < 1 || filterSetNum > ncol(data) - 1) {
		showModal(modalDialog(
			img(src = "error.png"), 
			HTML(paste(strong("Number of feature set"), "should be greater than", strong(1), "and smaller than", strong(ncol(data) - 1))),
			title = "Error"
		))
		return(NULL)
	}

	# Construct a range of feature set size according to the set number
	sizes<-as.integer(seq(from = 1, to = ncol(data) - 1, length.out = filterSetNum))

	# Use the progress dialog to inactive the page because the feature selection may be slow
	showModal(modalDialog(
		div(img(src = "progress.gif", height = 72, width = 72), style="text-align: center;"),	# Center the image
		div("Performing feature selection...", style="text-align: center;"),	
		title = "Progress",
		footer = NULL				# The dialog cannot be dismissed by users
	))

	# Key step: feature selection
	# The process may have unexpected error, so use tryCatch
	filter<-tryCatch(
		if (type == 'rfe') {
			# Recursive elimination (rfe) control
			# 'rerank = T' will result in exception for some models, so do not use
			ctrl<-rfeControl(functions = model, method = input$filterResampleMed,
                		number = number, repeats = repeats, verbose = FALSE,
				# Choose if all resample metrics should be preserved; otherwise only summary metric is preserved
				returnResamp = ifelse(input$filterResampleMet, 'all', 'final')
			)

			# Start rfe process
			# As pre-processing is not applicable to feature selection, use pre-processed data but not raw data
			# Also, the filter cannot be applied to new data set because pre-processing information is missing
			rfe(y = data[[outcome]], x = data[names(data) != outcome],
		    		sizes = sizes, metric = metric, rfeControl = ctrl
			)
		} else if (type == 'sbf') {
			# Univariant filter (sbf) control
			ctrl<-sbfControl(functions = model, method = input$filterResampleMed,
                		number = number, repeats = repeats, verbose = FALSE,
				returnResamp = ifelse(input$filterResampleMet, 'all', 'final')
			)

			# Start sbf
			sbf(y = data[[outcome]], x = data[names(data) != outcome],
				metric = metric, sbfControl = ctrl
			)
		},

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

	# If feature selection is successful
	if (!is.null(filter)) {
		# Remove progress dialog
		removeModal()
		# Add the outcome name to the filter, which is useful to distinguish outcome and predictors in filtering latter  
		filter$.outcome<-outcome
		# Add current filter to the filter list
		filters[[length(filters) + 1]]<<-filter
		# Update 'Select filter' selectInput
		updateSelectInput(session, "filterSel", "Select filter", choices = length(filters):1)			
	}	
})

# Reactive: 'Select filter' selectInput
select.filter<-reactive({
	id<-input$filterSel
	if (id == '') return(NULL)
	# Return the selected filter
	filters[[as.numeric(id)]]
})

# Output: 'Filter summary' verbatimTextOutput
output$filterSum<-renderPrint({
	# Get selected filter
	filter<-select.filter()
	# Show summary information of the filter
	if (!is.null(filter)) filter
})

# Output: 'Filter plot' plotOutput
output$filterPlot<-renderPlot({
	# Get selected filter
	filter<-select.filter()

	# Plot resample metrics according to filter type and resample preserved because different plot functions shoule be used
	if (class(filter) == 'rfe' && filter$control$returnResamp == 'all') {
		# Plot metrics of all resamples for rfe (one metric for one resample)
		# 'Preserve all resample metric' should be selected for the filter
		xyplot(filter, type = c("g", "p", "smooth"))
	} else if (class(filter) == 'rfe' && filter$control$returnResamp == 'final') {
		# Plot summary metrics of resamples for rfe (one metric for one feature set)
		plot(filter, type = c("g", "o"))
	} else if (class(filter) == 'sbf') {
		# Plot for sbf (it always returns all resamples for only the final feature set)
		histogram(filter)
	}
})

# Event: 'Show selected feature' actionButton
observeEvent(input$filterFeat, {
	# Get selected filter
	filter<-select.filter()
	if (is.null(filter)) {
		showModal(modalDialog(
			img(src = "error.png"), "No filter is selected for displaying",
			title = "Error"
		))
	} else {
		# Display selected features by DataTable in a dialog
		# Caution: 'predictors' try to rank the features, but it seems not supported by all models!
		# Check the rank by 'print(filter, top = 5)' to find the top features
		feature<-predictors(filter)
		showModal(modalDialog(
			img(src = "information.png"),
			renderDataTable(options = list(pageLength = 5), data.frame(Rank = 1:length(feature), Feature = feature)),
			title = "Information"
		))
	}	
})

# Event: 'Show resample' actionButton
observeEvent(input$filterResample, {
	# Get selected filter
	filter<-select.filter()
	if (is.null(filter)) {
		showModal(modalDialog(
			img(src = "error.png"), "No filter is selected for displaying",
			title = "Error"
		))
	} else {
		# Display resample metrics by DataTable in a dialog
		showModal(modalDialog(
			img(src = "information.png"),
			renderDataTable(options = list(pageLength = 5), filter$resample),
			title = "Information",
			size = "l"
		))
	}	
})

# Event: 'Apply selected filter' actionButton
# Filter features in data (this action cannot be reverted)
observeEvent(input$filterApply, {
	# Get selected filter
	filter<-select.filter()
	if (is.null(filter)) {
		showModal(modalDialog(
			img(src = "error.png"), "No filter is selected for displaying",
			title = "Error"
		))
	} else {
		# Features preserved by the filter, including the outcome 
		feature<-c(predictors(filter), filter$.outcome)
		
		# Filter features in both data and raw data
		data<<-data[feature]
		raw.data<<-data[feature]
		
		# Flush the filter list because the filter can only be applied once
		filters<<-list()
		updateSelectInput(session, "filterSel", "Select filter", choices = "")

		# Update features for displaying in all places
		update.feat()

		# Show information
		showModal(modalDialog(
			img(src = "warning.png"), "Only selected features are preserved. This action cannot be reverted.",
			title = "Warning"
		))
	}
})

# Output: 'Save data' downloadButton
# The data should be filtered before saving
output$filterSave<-downloadHandler(
	# Save the data as .csv file
    	filename = "filter.csv",
    	content = function(file) {
		if (is.null(data)) {
			showModal(modalDialog(
				img(src = "error.png"), "No data set for saving",
				title = "Error"
			))
		}
		# Save the raw data before pre-processing, which can be re-uploaded for training
		write.csv(x = raw.data, file = file, quote = F, row.names = F)
    	}
)

