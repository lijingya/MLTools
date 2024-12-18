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

rocplot.multiple <- function(test.data.list, groupName = "super_label", predName = "pred_cv_threshold", title = "ROC Plot", p.value = TRUE, size=22) {
  plotdata <- plyr::llply(test.data.list, function(x) with(x, .rocdata(grp = eval(parse(text = groupName)), pred = eval(parse(text = predName)))))
  plotdata <- list(roc = plyr::ldply(plotdata, function(x) x$roc),
                   stats = plyr::ldply(plotdata, function(x) x$stats)
  )
  
  if (p.value == TRUE){
    annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (P=", signif(p.value, 2), ")", sep=""))
  } else {
    annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (95% CI ", signif(ci.upper, 2), " - ", signif(ci.lower, 2), ")", sep=""))
  }
  
  p <- ggplot(plotdata$roc, aes_string(x = "x", y = "y")) +
    geom_line(aes_string(colour = ".id")) +
    geom_segment(x=0, y=0, xend=1, yend=1, linetype=2) +
    scale_x_continuous("False Positive Rate (1-Specificity)") +
    scale_y_continuous("True Positive Rate (Sensitivity)") +
    scale_colour_brewer(palette="Set1", breaks = names(test.data.list), 
                        labels = paste(names(test.data.list), ": ", annotation, sep = "")) +
    ggtitle(title) +
    theme(text = element_text(size=size)) +
    theme(plot.title = element_text(size=size), 
          axis.title.x = element_text(size=size),
          axis.title.y = element_text(size=size, angle=90),
          legend.justification=c(1,0), 
          legend.position=c(1,0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text=element_text(size=size),
          axis.text.x = element_text(size=size),
          axis.text.y = element_text(size=size)) +
    guides(colour=guide_legend(override.aes = list(size=3)))
  
  return(p)
}



.rocdata <- function(grp, pred){
	# Arguments: grp - labels classifying subject status
	#            pred - values of each observation
	# Output: List with 2 components:
	#         roc = data.frame with x and y co-ordinates of plot
	#         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
	
	if (length(pred) != length(grp)) {
		stop("The number of classifiers must match the number of data points")
	} 
	
	#     ### NOTE this is incorrect as the cutoff itself is never accounted for
	#     ### results in n-1 levels
	#     ### removed 12/30/2015
	#     cut <- unique(pred)
	#     tp <- sapply(cut, function(x) sum(pred > x & grp == levels(grp)[2]))
	#     fn <- sapply(cut, function(x) sum(pred < x & grp == levels(grp)[2]))
	#     fp <- sapply(cut, function(x) sum(pred > x & grp == levels(grp)[1]))
	#     tn <- sapply(cut, function(x) sum(pred < x & grp == levels(grp)[1]))
	
	levels <- sort(unique(grp))
	grp <- ordered(grp, levels = levels)
	if (length(levels(grp)) != 2) {
		stop("There must only be 2 values for the classifier")
	}
	n.pos <- sum(as.character(grp) == as.character(levels[2]))
	n.neg <- sum(as.character(grp) == as.character(levels[1]))
	
	pred.order <- order(pred, decreasing = TRUE)
	pred.sorted <- pred[pred.order]
	
	tp <- cumsum(grp[pred.order] == as.character(levels[2]))
	fp <- cumsum(grp[pred.order] == as.character(levels[1]))
	dups <- rev(duplicated(rev(pred.sorted)))
	tp <- c(0, tp[!dups])
	fp <- c(0, fp[!dups])
	fn <- n.pos - tp
	tn <- n.neg - fp
	tpr <- tp / (tp + fn)
	fpr <- fp / (fp + tn)
	
	roc = data.frame(x = fpr, y = tpr)
	roc <- roc[order(roc$x, roc$y),]
	
	i <- 2:nrow(roc)
	auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
	
	## Hanley and McNeil (1982) method of Variance for AUC
	pos <- pred[grp == levels(grp)[2]]
	neg <- pred[grp == levels(grp)[1]]
	q1 <- auc/(2-auc)
	q2 <- (2*auc^2)/(1+auc)
	se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
	ci.upper <- auc + (se.auc * 0.96)
	ci.lower <- auc - (se.auc * 0.96)
	
	se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
	z <- (auc - 0.5)/se.auc.null
	p <- 2*stats::pnorm(-abs(z))
	
	stats <- data.frame (auc = auc,
											 p.value = p,
											 ci.upper = ci.upper,
											 ci.lower = ci.lower
	)
	
	return (list(roc = roc, stats = stats))
}
