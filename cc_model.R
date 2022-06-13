

library(zoo)

#modelop.init
begin <- function(){
	load("model_artifacts.RData")
	logreg <<- logreg_model
	predictor <<- predictor
	hos_cleanup <<- ho_cleanup
	threshold <<- threshold
}

make_prediction <- function(datum){
	datum$earliest_cr_line <- as.yearmon(datum$earliest_cr_line, 
					     format="%b-%Y")
	datum$int_rate <- as.numeric(sub("%", "", datum$int_rate, 
					     fixed=TRUE))/100
	datum$logit_int_rate <- sapply(datum$int_rate, qlogis)
	datum$log_loan_amnt <- sapply(datum$loan_amnt, log)
	datum$log_annual_inc <- sapply(datum$annual_inc, log)
	datum$home_ownership <- sapply(datum$home_ownership, hos_cleanup)
	datum$credit_age = Sys.yearmon() - datum$earliest_cr_line
	preds <- unname(predict(logreg, datum, type="response"))
	return(preds)
}

#modelop.score
action <- function(datum){
	preds <- make_prediction(datum)
	outcome <- sapply(preds, predictor)
	output <- list(outome = outcome, propensity = preds)
	emit(output)
}

#modelop.metrics
metrics <- function(data){
	preds <- make_prediction(data)
	outcomes <- sapply(preds, predictor)
	cm <- table(factor(outcomes), factor(data$loan_status))
	metrics <- list(confusionMatrix=cm["table"])
	emit(metrics)
}
