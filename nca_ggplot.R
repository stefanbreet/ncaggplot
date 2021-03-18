

nca_ggplot <- function(model = NA, 
                       data = NA,
                       label = NA,  
                       levels = NA, 
                       point.color = "#0B2B5D",
                       point.size = 4,
                       opacity = 0.7,
                       legend = TRUE,
                       CE.FDH = TRUE,
                       CR.FDH = TRUE,
                       regression.line = TRUE,
                       scope.line = TRUE,
                       scope.line.color = "grey") {
  
  ##########################################################################
  # 1. Initial Checks
  ##########################################################################
  
  # Check whether the ggplot2 function is installed and load it
  require("ggplot2")
  
  
  # Check if the data.set is stored as a data.frame
  if(is.data.frame(data) == FALSE) {
    stop("Data is not a data.frame. Provide the dataset in the data.frame format.")
  }
  
  ##########################################################################
  # 2. Extract NCA results
  ##########################################################################
  
  # Store the model results
  l <- model[[1]][[1]]
  
  # Store the names of the variables
  variable.names <- l[[4]]
  
  # Store the names of the independent and dependent variables
  # separately
  dependent.variable <- variable.names[2]
  independent.variable <- variable.names[1]
  
  # Store the dataset
  data.set <- as.data.frame(data)
  
  # Store the dv as a data.frame to check if the
  # values are numerical
  dv.column <- select(data.set, dependent.variable)
  
  # Select the rows for which the dependent variable 
  # is not NA and subset the dataset
  number.of.NAs <- sum(is.na(dv.column))
  
  # Subset the dataset
  data.set <- data.set[!is.na(dv.column), ]
  
  # Create a message
  message.first.part <- "Removed"
  message.middle.part <- as.character(number.of.NAs)
  message.last.part <- "observations due to missing data of the dependent variable"
  
  full.message <- paste(message.first.part, message.middle.part,
                        message.last.part)
  
  # Provide a message
  print(full.message)
  
  # Store the iv as a data.frame to check if the
  # values are numerical
  iv.column <- select(data.set, independent.variable)
  
  # Select the rows for which the dependent variable 
  # is not NA and subset the dataset
  number.of.NAs <- sum(is.na(iv.column[,1]))
  
  # Subset the dataset
  data.set <- data.set[!is.na(iv.column), ]
  
  # Create a message
  message.first.part <- "Removed"
  message.middle.part <- as.character(number.of.NAs)
  message.last.part <- "observations due to missing data of the independent variable"
  
  full.message <- paste(message.first.part, message.middle.part,
                        message.last.part)
  
  # Provide a message
  print(full.message)
  
  ####################################
  # Store plot parameters
  ####################################  
  
  # Store plot scope
  xmin <- model$summaries[[1]]$global[3]
  xmax <- model$summaries[[1]]$global[4]
  ymin <- model$summaries[[1]]$global[5]
  ymax <- model$summaries[[1]]$global[6]

  
  # Store OLS regression line
  ols.intercept <-  l$lines$ols$coefficients[1] # Access the intercept
  ols.slope     <-  l$lines$ols$coefficients[2] # Access the slope
  
  # Ceiling Envelopment 
  x <-  l$lines$ce_fdh[1] # Access Lines lines$ce_fdh[[1]]
  y <-  l$lines$ce_fdh[2] # Access Lines lines$ce_fdh[[2]]
  
  # Create a data.frame with ce_fdh results
  ce_fdh <- data.frame(x, y) # create an empty data.frame
  names(ce_fdh) <- c("x", "ce_fdh") # Rename the columns
  minimum <- data.frame(x = xmin, ce_fdh = NA) # Add the minum value of the scope 
  ce_fdh <- rbind(minimum, ce_fdh) # Combine the two datasets

  
  if(is.null(l$lines$cr_fdh) == FALSE ) {
    
    # Ceiling Regression 
    cr_fdh.intercept <- as.numeric(l$lines$cr_fdh$coefficients[1])
    cr_fdh.slope <- as.numeric(l$lines$cr_fdh$coefficients[2])
    
    # Create a data.frame with cr_fdh results
    cr_fdh <- data.frame(x = ce_fdh$x, cr_fdh = NA) # Use the same X values as ce_fdh
    cr_fdh$cr_fdh <- cr_fdh.intercept + c(cr_fdh$x*cr_fdh.slope) # Calculate the scores
    
    # Remove the X variable
    cr_fdh$x <- NULL
    
    # Create one dataset that contains CR_FDR, CR_FDH and the regression line
    lines <- cbind(ce_fdh, cr_fdh)
    
    # Make sure that the CR-FDH line is cutoff at the maximum value of Y or 
    # at the maximum value of X, depending on the line
    # Find the value of X that correspond to the maximum value of Y 
    x.max.cr_fdh <- c(ymax - cr_fdh.intercept) / cr_fdh.slope
    
    # Check if the value of x that corresponds to the maximum value of
    # Y is smaller than the plot scope
    if(x.max.cr_fdh <= xmax) {
      
      # If the value of x that corresponds to the maximum value of
      # y is smaller than the plot scope, add the vlaues to a row that
      # can be added to the dataset with line coordinates
      maximum.cr_fdh <- data.frame(x = x.max.cr_fdh, ce_fdh = NA, cr_fdh = ymax)
      
      # Add the row to the dataset with line coordinates
      lines <- rbind(lines, maximum.cr_fdh)
      
      # Sort the dataset by x
      lines <- arrange(lines, x)
      
      # Because the maximum of x is smaller than the maximum of y, the
      # value of y that corresponds to the maximum of x should be set to
      # NA such that it is not displayed on the plot
      lines$cr_fdh[lines$x == xmax] <- NA
      
    } else {
      
      # If the value of x that corresponds to the maximum value of
      # Y is lager than the plot scope, calculate the new maximum value of Y
      maximum.cr_fdh <- c(xmax*cr_fdh.slope) + cr_fdh.intercept
      
      # Add the row to the dataset
      maximum.cr_fdh <- data.frame(x = xmax, ce_fdh = NA, cr_fdh = maximum.cr_fdh)
    }
    
    # Arrange the x variable
    lines <- arrange(lines, x)
    
    # Set all cr_fdh scores higher than the maximum cr_fdh to NA
    lines$cr_fdh[lines$cr_fdh > ymax] <- NA
    
    # Set the ce_fdh score of x.max.cr_fdh to the value of the x before
    position.x <- match(x.max.cr_fdh, lines$x)
    lines$ce_fdh[position.x] <- lines$ce_fdh[position.x-1]
    
  } else {
    
    CR.FDH <- FALSE
    
    message("Ceiling Regression line could not be computed")
    
    lines <- ce_fdh
  }
  
  match.iv <- match(independent.variable, colnames(data.set))
  match.dv <- match(dependent.variable, colnames(data.set))

  
  ##########################################################################
  # 3. Create plot
  ##########################################################################
  
  # Load the ggplot2 library
  library(ggplot2)
  
  
  # Create a ggplot2 object
  p <- ggplot(data = data.set)
  
  
  # Show scope lines
  if(scope.line == TRUE) {
    # Add lines for the scope (minmum and maximum values of Y and X)
    p <- p + geom_vline(xintercept = xmin, color = scope.line.color, linetype = "dotted")
    p <- p + geom_vline(xintercept = xmax, color = scope.line.color, linetype = "dotted")
    p <- p + geom_hline(yintercept = ymin, color = scope.line.color, linetype = "dotted")
    p <- p + geom_hline(yintercept = ymax, color = scope.line.color, linetype = "dotted")
  }
  

  # Check if a "levels" variable is specified
  
  # If not, use one color
  if(is.na(levels) == TRUE) {
    
    # Draw the points
    p <- p + geom_point(aes(x = data.set[,match.iv], y = data.set[,match.dv]),
                        size = point.size, alpha = opacity, colour = point.color)
    
  } else {
    
    # Store the position of the column that contains the factor variable with
    # levels
    match.levels <- match(levels, colnames(data.set))
    
    # Draw the points
    p <- p + geom_point(aes(x = data.set[,match.iv], y = data.set[,match.dv],
                            colour = data.set[,match.levels]),
                        size = point.size, alpha = opacity)
  }
  
  # Add the titles of the x and y axis
  p <- p + xlab(independent.variable) + ylab(dependent.variable)
  
  if(is.na(label) == FALSE) {
    
    # Store the position of the column that contains the label
    match.label <- match(label, colnames(data.set))
    
    # Add labels
    p <- p + geom_text(x = data.set[,match.iv], y = data.set[,match.dv], 
                       label = as.character(data.set[,match.label]))

  }
  
  if(CE.FDH == TRUE) {
    # Add slope of the ce_fdh stepwise function line
    p <- p + geom_step(data = lines, aes(x = x,y = ce_fdh, linetype = "CE-FDH"), color = "Black")
  }
  
  if(CR.FDH == TRUE) {
    # Add slope of the [ADD EXPLANATION] line
    p <- p + geom_line(data = lines, aes(x = x,y = cr_fdh, linetype = "CR-FDH"), color = "Black")
  }
  
  if(regression.line == TRUE) {
    # Add slope of the Regression line
    p <- p + geom_abline(slope = ols.slope, intercept = ols.intercept, linetype = "dotted" , color = "Black")
  }

  if(legend == TRUE){
    # Change the title of the legend
    p <- p + labs(linetype="Ceiling Technique")
    
    # Manually set the linetypes
    p <- p + scale_linetype_manual(values = c("dashed", "solid"))
    
  } else {
    # Remove the legend
    p <- p + guides(linetype=FALSE)
    
  }
  
  p
  
}

  
