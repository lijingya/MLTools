#' Generate distribution plot and summary
#' @param x Input data ML.data
#' @param outDir Path to directory to save the plot and table.
#' If it is specified, the plots will be saved. Default is NULL
#' @param colClass A boolen. If it is TRUE, the ditribution will colored by
#' Classes. Default is FALSE.
#' @param maxLevel A number to specify the max number of categories
#'  for categorical data.
#' @param ... Parameters in getData
#' @return plots and summary for continuous variable and catergorical variables.
#'
#' @examples
#' DistriSum(x)
#'
DistriSum = function(x, outDir = NULL, colCalss = FALSE, maxLevel = 10, ...){
  data = getData(x, ...)
  if(colClass){
    args = list(...)
    if("IDs" %in% names(args)){
      Classes = getClass(x, args[["IDs"]])
    }else{
      Classes = getClass(x)
    }
    data$CLASS = Classes[[2]]
  }
  Vars = colnames(data)
  Vars = Vars[!Vars %in% c("IDs","CLASS")]
  CatCon = unlist(lapply(Vars, function(x){CheckCateg(data[[x]], maxLevel)}))
  CatVars = Vars[CatCon]
  ConVars = Vars[!CatCon]

  ## format data ##
  if(colCalss){
    data = melt(data, id.vars = c("IDs", "CLASS"), measure.vars = Vars)
  }else{
    data = melt(data, id.vars = "IDs", measure.vars = Vars)
  }

  numPlots = 1
  ## if there is Categorical variable, plot it
  if(length(CatVars) > 0){
    dataCat = data %>% filter(variable %in% CatVars)
    dataCatSum = dataCat %>%
      count(variable, value) %>%
      mutate(freq = n / sum(n))

    P[[numPlots]] = ggplot(dataCat, aes(x = value))+
      geom_bar(aes(y = (..count..)/sum(..count..)))+
      scale_y_continuous(labels=percent) +
      facet_wrap(~ variable, scales = "free")+
      theme(plot.title = element_text(color="black", face="bold")) +
      theme(axis.title = element_text(color="black", face="bold")) +
      theme_bw()
    numPlots = numPlots + 1
  }

  ## if there are continuous variables, plot it
  if(length(ConVars) > 0){
    dataCon =  data %>% filter(variable %in% ConVars)
    dataConSum = dataCon %>% group_by(variable) %>% summarise(count    = n(),
                                                              mean     = mean(value, na.rm = T),
                                                              median   = median(value, na.rm = T),
                                                              variance = var(value, na.rm = T),
                                                              sd       = sd(value, na.rm = T),
                                                              `25%`    =quantile(value, probs=0.25, na.rm = T),
                                                              `50%`    =quantile(value, probs=0.5, na.rm = T),
                                                              `75%`    =quantile(value, probs=0.75, na.rm = T),
                                                              skewness = skewness(value, na.rm = T),
                                                              kurtosis = skewness(value, na.rm = T),
                                                              missingValue = sum(is.na(value))
                                                              )
    P[[numPlots]] = ggplot(dataCon, aes(x = value))+
      geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5)+
      geom_density(colour = 'blue') +
      geom_text(data = dataConSum, aes(x=Inf,
                                y=Inf,
                                hjust=1,
                                vjust=1,
                                label=format(skewness, digits=2, nsmall=2)))+
      facet_wrap(~ variable, scales = "free", ncol = 10)+
      theme(plot.title = element_text(color="black", face="bold")) +
      theme(axis.title = element_text(color="black", face="bold")) +
      theme_bw()
  }

  if(!is.null(outDir)){
    height = 3 + floor(max(length(ConVars), length(CatVars))/10) * 1.5
    width = 14
    pdf(sprintf("%s/distribution.plots.pdf", outDir), width = width, height = height)
    invisible(lapply(P, print))
    dev.off()
  }
  write.table(dataConSum, file = sprintf("%s/ContinuousVars.summary.tsv", outDir), sep = "\t", quote = F, row.names = F)
  write.table(dataCatSum, file = sprintf("%s/CategoricalVars.summary.tsv", outDir), sep = "\t", quote = F, row.names = F)
  return(list(plots = P, ConSum = dataConSum, CatSum = dataCatSum))
}
