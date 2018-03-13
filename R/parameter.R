#' Default parameters.
#'
#' Default parameters used for training the model.
#'
#' @export
#' @return named list of default parameters
get.default.parameter <- function() {

  params = list(objective        = "multi:softmax",
                num_class        = NA,
                eta              = 0.3,
                gamma            = 0,
                max_depth        = 6,
                min_child_weight = 1,
                subsample        = 1,
                colsample_bytree = 1,
                eval_metric      = 'merror',
                nthread          = 0)
  return(params)
}

#' Set parameter.
#'
#' Write (custom) parameters to file 'parameters'. To use the default parameters
#' call this function without any argument.
#'
#' @param parameter list of parameters (for all parameters  not included in this
#'  list, the default values are used)
#' @importFrom data.table fwrite
#' @export
set.parameter <- function(output_dir, input_params=NA, parameter=list())  {

  if(is.na(input_params)){
    defaultParams <- get.default.parameter()
  } else {
    defaultParams <- fread(input_params)
  }

  if (length(parameter != 0)) {
    if (!all(names(parameter) %in% names(defaultParams))) {
      stop("You are trying to set an unknown parameter.")
    }

    for(i in names(parameter))  {
      defaultParams[i] <- parameter[i]
    }
  }

  fwrite(defaultParams, paste(output_dir, "train_parameter", sep = "/"))
}

#' Get parameter.
#'
#' Read 'parameter' file created with \link{set.parameter} and return requested
#' parameters.
#'
#' @param parameter parameters
#' @importFrom data.table fread
#' @export
get.parameter <- function(params, parameter)  {

  if (length(parameter) > 0 &&
      !all(names(parameter) %in% names(get.default.parameter()))) {
    stop("You are trying to read an unknown parameter.")
  }
  prm <- fread(params)
  return(as.numeric(as.list(prm)[parameter]))   # TODO will be NA for non-numeric params
}


