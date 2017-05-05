###ML.data ----------------------------------------------------
# # initialize
setMethod(f="initialize",signature="ML.data",
          definition=function(.Object,data, classes, pred){
            message("~~~ ML.data: initializator ~~~ ")
            # data is a data.frame
            if(!missing(data)) .Object@data<-data
            # class is a data.frame
            if(!missing(classes)) .Object@classes<-classes
            # pred is a data.frame
            if(!missing(pred)) .Object@pred <- pred
            return(.Object)
          }
)

# show
setMethod (f="show","ML.data",
           function(object){
             cat("*** Class ML.data, method show *** \n")
             cat("* data \n"); print(str(object@data))
             cat("* classes \n"); print(str(object@classes))
             if(!is.null(object@classes)){
               print(table(object@classes$CLASS));
               print(table(object@classes$CLASS)/nrow(object@classes))
             }
             cat("* pred \n"); print(str(object@pred))
             cat("******* End Print (ML.data) ******* \n")
           }
)

setMethod (f="summary","ML.data",
           function(object){
             cat("*** Class ML.data, method summary *** \n")
             cat("* data \n"); print(str(object@data))
             cat("* classes \n"); print(str(object@classes))
             if(!is.null(object@classes)){
               print(table(object@classes$CLASS));
               print(table(object@classes$CLASS)/nrow(object@classes))
             }
             cat("* pred \n"); print(str(object@pred))
             cat("******* End Print (ML.data) ******* \n")
           }
)

#Accessor Getter
#' @rdname getData
#' @aliases getData
#' @importFrom dplyr filter_ select_ filter
#' @examples
#'
setMethod(f="getData",signature="ML.data",
          definition=function(object,IDs,Cols,Filters){
            if(missing(IDs)){
              IDs = object@data$IDs
            }
            IDs = paste0("IDs %in% c(",paste(IDs, collapse = ","), ")")
            if(missing(Cols)){
              Cols = paste0("c(",paste(c(colnames(object@data), "IDs"), collapse = ","), ")")
            }else{
              Cols = paste0("c(",paste(c(Cols, "IDs"), collapse = ","), ")")
            }

            if(! missing(Filters)){
              data = object@data %>% filter_(Filters) %>% select_(Cols) %>% filter_(IDs)
            }else{
              data = object@data %>%  select_(Cols) %>% filter_(IDs)
            }
            return(data)
          })


#' @rdname getClass
#' @aliases getClass
#' @importFrom dplyr filter_
#' @examples
#'
setMethod(f="getClass",signature="ML.data",
          function(object,IDs){
            if(missing(IDs)){
              classes = object@classes
            }else{
              IDs = paste0("IDs %in% c(",paste(IDs, collapse = ","), ")")
              classes = object@classes %>% filter_(IDs)
            }
            return(classes)
          })


#' @rdname getPred
#' @aliases getPred
#' @importFrom dplyr filter_
#' @examples
#'
setMethod(f="getPred",signature="ML.data",
          function(object,IDs){
            if(missing(IDs)){
              pred = object@pred
            }else{
              IDs = paste0("IDs %in% c(",paste(IDs, collapse = ","), ")")
              pred = object@pred %>% filter_(IDs)
            }
            return(pred)
          })


## Construct data structure-----------------------------------------------------
#' GenData
#' @aliases GenData
#' @param data A data frame or path of file.
#' @param class.var A vecter to specify the variable in data is classification information.
#' @param pred.var A vecter to specify the variable in data is prediction variable.
#' @param ... Parameters for read file
#' @return ML.data class
#' @export
#' @importFrom dplyr select one_of
#' @examples
GenData <- function(data = NULL, class.var = NULL, pred.var = NULL, ...){
  if(class(data) == "character"){
    if(grepl(".csv$", data)){
      data = read.csv(data, stringsAsFactors = F, check.names = F,...)
    }else if(grepl(".txt$", data) | grepl(".tsv$", data)){
      data = read.delim(data, stringsAsFactors = F, check.names = F,...)
    }
  }else if(class(data) != "data.frame" ){
    stop(sprintf("Class of data is %s. Data should be either file path (character class) or data.frame format.", class(data)))
  }

  data$IDs = 1:nrow(data)
  dataCols = colnames(data)
  dataCols = dataCols[!dataCols %in% c(class.var, pred.var)]
  if(!is.null(class.var)){
    classes = data %>% select(one_of(c("IDs", class.var)))
  }else{
    classes = NULL
  }

  if(!is.null(pred.var)){
    pred = data %>% select(one_of(c("IDs", pred.var)))
  }else{
    pred = NULL
  }

  data = data %>% select(one_of(dataCols))
  args <- list(data=data,classes=classes,pred=pred)
  args <- args[!unlist(lapply(args,is.null))]
  args$Class <- "ML.data"
  MLData <- do.call(new,args)
  return(MLData)
}




