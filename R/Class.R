### set class
#'
#' An S4 class contains 5 slots: methylation, expression, sample information, probe
#' information and gene information. MEE.data class are the main input for main functions.
#' @slot data A data.frame contains data for ML.
#' each column is one individual features
#' @slot classes A data.frame contains data for ML. Columns are IDs, CLASS, ...
#' @slot pred A data.frame contains prediction. Columns are IDs, PRED, PROB, ...
#' @exportClass ML.data
setClass("ML.data",
         representation = representation(data="data.frame",classes="data.frame",
                                         pred="data.frame"),
         validity=function(object){
           message("~~~ ML.data: inspector ~~~")
           if(!is.null(data) & !is.null(classes)){
             if(!all(sort(data$ID) %in% sort(classes$ID)) ){
               warning("[ML.data: validation] class doesn't contain all ID
                       the information for the data.")
             }
           }
           if(!is.null(data) & !is.null(pred)){
             if(!all(sort(data$ID) == sort(pred$ID)) ){
               warning("[ML.data: validation] pred doesn't contain all ID
                       the information for the data.")
             }
           }
           return(TRUE)
           }
         )
