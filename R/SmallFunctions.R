#' check if a variable is categorical variable
#' @param x a vector of data
#' @param maxLevel A number to specify the max number of categories for categorical data.
#' @return
#'
#' @examples
#' CheckCateg(x)
#'
CheckCateg = function(x, maxLevel = 10){
  Levels = length(unique(x))
  if(Levels > maxLevel){
    return(FALSE)
  }else if(Levels <= maxLevel){
    return(TRUE)
  }
}
