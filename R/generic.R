#' getData
#' @param object ML.data object
#' @param IDs A vector of IDs for each data object. When specified, data only
#' with these IDs will be output
#' @param Cols A vector of colnames. When specified, data only for
#' these colunm will be output.
#' @param  Filters A string like "Col1 == 1 | Col2 == 2& Col3 >2". This will
#' filter data based on the criteria.
#' @return data for machine learning.
#' @exportMethod getData
#' @docType methods
setGeneric(name="getData",
           def=function(object,IDs,Cols,Filters){standardGeneric("getData")})



#' getClass
#' @param object ML.data object
#' @param IDs A vector of IDs for each data object. When specified, class info only
#' with these IDs will be output
#' @return Classification info.
#' @exportMethod getClass
#' @docType methods
setGeneric(name="getClass",
           def=function(object,IDs){standardGeneric("getClass")})


#' getPred
#' @param object ML.data object
#' @param IDs A vector of IDs for each data object. When specified, class info only
#' with these IDs will be output
#' @return Classification info.
#' @exportMethod getPred
#' @docType methods
setGeneric(name="getPred",
           def=function(object,IDs){standardGeneric("getPred")})

