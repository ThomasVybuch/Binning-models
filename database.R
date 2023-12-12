################################
### Storage of functions
################################



# Create decision tree for one regressor at the time
STROM <- function(Y, X, cp, method) {
  # Control variables
  controls <- rpart.control(maxcompete = 2, cp = cp, maxdepth = 2)
  
  # Fitting of decision tree  - rpart
  model <- rpart(formula = as.formula(paste(Y, "~", X)),
                 data = data,
                 control = controls,
                 method = method)
  
  return(model)
}