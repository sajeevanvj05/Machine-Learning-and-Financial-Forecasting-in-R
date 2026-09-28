# Load necessary libraries
library(neuralnet)
library(readxl)
library(Metrics)  # Load Metrics package
library(scales)

# Load and prepare data
data <- read_excel("ExchangeUSD.xlsx")
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

# Function to create lagged input/output matrices
create_ar_matrix <- function(data, lag) {
  n <- length(data)
  max_lag <- max(lag)
  X <- sapply(lag, function(l) data[(1 + l):(n - max_lag + l)])
  Y <- data[(max_lag + 1):n]
  return(list(input = as.matrix(X), output = Y))
}

# Model Training and Experimentation
train_models <- function(train_data, lags, hidden_layers) {
  models <- list()
  for (lag in lags) {
    io_matrix <- create_ar_matrix(train_data, lag)
    for (hidden_layer in hidden_layers) {
      nn <- neuralnet(output ~ ., data = as.data.frame(cbind(io_matrix$input, output = io_matrix$output)), hidden = hidden_layer, linear.output = TRUE)
      models[[paste("Lag", toString(lag), "Hidden", toString(hidden_layer), sep = "_")]] <- nn
    }
  }
  models
}

# Configuration parameters
lags <- list(1, 1:2, 1:3, 1:4)  # Different lag configurations
hidden_layers <- list(c(5), c(10), c(5, 5), c(3, 3, 3))  # Different hidden layer configurations

# Train the models
models <- train_models(train_data, lags, hidden_layers)

# Function to compute statistics (RMSE, MAE, MAPE, SMAPE) using Metrics package
compute_statistics <- function(actual, predicted) {
  rmse <- rmse(actual, predicted)
  mae <- mae(actual, predicted)
  mape <- mape(actual, predicted)
  smape <- smape(actual, predicted)
  c(RMSE = rmse, MAE = mae, MAPE = mape, SMAPE = smape)
}

# Model Evaluation
evaluate_models <- function(models, test_data) {
  results <- list()
  for (name in names(models)) {
    lag <- as.numeric(unlist(strsplit(name, "_"))[2:length(unlist(strsplit(name, "_"))) - 2])
    hidden_config <- unlist(strsplit(name, "_"))[length(unlist(strsplit(name, "_")))]
    model <- models[[name]]
    io_matrix <- create_ar_matrix(test_data, lag)
    predictions <- predict(model, io_matrix$input)$net.result
    statistics <- compute_statistics(io_matrix$output, predictions)
    results[[name]] <- c(statistics, Description = paste("Lag:", paste(lag, collapse = ","), "Hidden Layers:", hidden_config))
  }
  results
}

# Apply evaluation
evaluation_results <- evaluate_models(models, test_data)

# Results Presentation
# Create a comparison table
comparison_table <- do.call(rbind, evaluation_results)

# Print comparison table
print(comparison_table)

# Plot results for the best model (based on RMSE)
best_model_name <- names(evaluation_results)[which.min(sapply(evaluation_results, function(x) x["RMSE"]))]
best_model <- models[[best_model_name]]
best_io_matrix <- create_ar_matrix(test_data, as.numeric(unlist(strsplit(best_model_name, "_"))[2:length(unlist(strsplit(best_model_name, "_"))) - 2]))
best_predictions <- predict(best_model, best_io_matrix$input)$net.result

plot(best_io_matrix$output, type = 'l', col = 'blue', lwd = 2, main = "Best Model Predictions vs Actual", xlab = "Time", ylab = "Normalized Exchange Rate", ylim = c(min(best_io_matrix$output, best_predictions) - 0.05, max(best_io_matrix$output, best_predictions) + 0.05))
lines(best_predictions, col = 'red', lty = 2, lwd = 2)
points(best_io_matrix$output, pch = 20, col = 'darkblue', cex = 0.7)
points(best_predictions, pch = 18, col = 'darkred', cex = 0.7)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = c(1, 2), lwd = 2, pch = c(20, 18))
