# Define the range of x values
x <- seq(-5, 5, length.out = 100)

binar<-function(x){
  y=c()
  for(i in 1:length(x)){
    if(x[i]>0){y[i]=1}else{y[i]=0}
  }
  return(y)
}
# List of popular activation functions
activation_functions <- list(
  Binary_Step = binar(x),
  Linear = x,
  Sigmoid = 1 / (1 + exp(-x)),
  Tanh = tanh(x),
  ReLU = pmax(0, x),
  Leaky_ReLU = pmax(0.01 * x, x)
)

# List of function formulas
function_formulas <- list(
  Binary_Step = NULL,
  Linear = expression(y = x),
  Sigmoid = expression(y = frac(1, 1 + exp(-x))),
  Tanh = "tanh(x)",
  ReLU = "max(0, x)",
  Leaky_ReLU = "max(0.01 * x, x)"
  #,Softmax = expression(y = frac(e^x, sum(e^x)))
)

# Create and arrange plots using par(mfrow)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))  # Add outer margin at the bottom for the formulas

for (i in 1:length(activation_functions)) {
  name <- names(activation_functions)[i]
  
  plot(x, activation_functions[[name]], type = "l", col = "blue",
       main = name,
       xlab = "x", ylab = "f(x)",
       xlim=c(-3,3),
       ylim = c(-1.2, 1.2))  # Set the y-axis limit
  
  text(1.4, -0.6, function_formulas[[name]],
       col = "black")
  
  abline(h = 0, v = 0, col = "gray", lty = 2)  # Add origin lines
}

# Adjust plot size and margins if necessary
# Adjust mar and oma values to make sure the formulas fit well in the plots
