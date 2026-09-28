# Load necessary libraries
library(neuralnet)
library(readxl)
library(scales)  # For data normalization

# Load and prepare data
data <- read_excel("exchangeUSD.xlsx")
exchange_rates <- data[[3]]  # Assuming 3rd column is USD/EUR exchange rates

# Function to normalize data
normalize_data <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize the data
norm_exchange_rates <- normalize_data(exchange_rates)

# Split the data into training and testing sets
train_data <- norm_exchange_rates[1:400]
test_data <- norm_exchange_rates[401:500]

# Creating Input/Output Matrices
create_ar_matrix <- function(data, lags) {
  n <- length(data)
  max_lag <- max(lags)
  X <- sapply(lags, function(lag) data[(1 + lag):(n - max_lag + lag)])
  Y <- data[(max_lag + 1):n]
  return(list(input = as.matrix(X), output = Y))
}

# Model Training and Experimentation
train_models <- function(train_data, lags, hidden_layers) {
  models <- list()
  for (lag in lags) {
    for (hidden_layer in hidden_layers) {
      io_matrix <- create_ar_matrix(train_data, lag)
      nn <- neuralnet(output~., data=as.data.frame(io_matrix), hidden=hidden_layer, linear.output=TRUE)
      models[[paste("Lag", lag, "Hidden", toString(hidden_layer), sep="_")]] <- nn
    }
  }
  models
}

# Configuration parameters
lags <- 1:3  # Adjusted for complexity
hidden_layers <- list(c(5), c(10), c(5, 5), c(3, 3, 3))  # Adjusted for variety

# Train the models
models <- train_models(train_data, lags, hidden_layers)

# Compute statistics
compute_statistics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual))
  smape <- mean(2 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
  c(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smape)
}

# Model Evaluation
evaluate_models <- function(models, test_data) {
  results <- list()
  for (name in names(models)) {
    lag <- as.numeric(strsplit(name, "_")[[1]][2])
    hidden_config <- strsplit(name, "_")[[1]][4]
    model <- models[[name]]
    io_matrix <- create_ar_matrix(test_data, lag)
    predictions <- compute(model, io_matrix$input)$net.result
    statistics <- compute_statistics(io_matrix$output, predictions)
    results[[name]] <- c(statistics, Description=paste("Lag:", lag, "Hidden Layers:", hidden_config))
  }
  results
}

# Apply evaluation
evaluation_results <- evaluate_models(models, test_data)

# Results Presentation
# Create a comparison table
comparison_table <- do.call(rbind, evaluation_results)

# Print and plot results
print(comparison_table)


plot(best_io_matrix$output, type='l', col='blue', lwd=2, main="Best Model Predictions vs Actual", xlab="Time", ylab="Normalized Exchange Rate", ylim=c(min(best_io_matrix$output, best_predictions)-0.05, max(best_io_matrix$output, best_predictions)+0.05))
lines(best_predictions, col='red', lty=2, lwd=2)
points(best_io_matrix$output, pch=20, col='darkblue', cex=0.7)
points(best_predictions, pch=18, col='darkred', cex=0.7)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=c(1, 2), lwd=2, pch=c(20, 18))
