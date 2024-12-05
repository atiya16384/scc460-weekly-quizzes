# Parameters for the Lotka-Volterra model
alpha <- 0.05          # Birth rate of rabbits
beta <- 1.2e-4         # Rate of foxes eating rabbits
gamma <- 0.04          # Death rate of foxes
initial_rabbits <- 60  # Initial population of rabbits (R1)
initial_foxes <- 30    # Initial population of foxes (F1)
time_steps <- 104      # Total time steps (104 weeks for a 2-year period)

# Initialization function to set up population vectors
initialize_population <- function(initial_rabbits, initial_foxes, time_steps) {
  rabbit_population <- numeric(time_steps)  # Vector to store rabbit population over time
  fox_population <- numeric(time_steps)     # Vector to store fox population over time
  rabbit_population[1] <- initial_rabbits   # Set initial number of rabbits
  fox_population[1] <- initial_foxes        # Set initial number of foxes
  list(rabbit_population, fox_population)   # Return vectors as a list
}

# Initialize population vectors
populations <- initialize_population(initial_rabbits, initial_foxes, time_steps)
rabbit_population <- populations[[1]]
fox_population <- populations[[2]]

# Function for the stochastic Lotka-Volterra model
stochastic_lotka_volterra <- function(rabbit_population, fox_population, alpha, beta, gamma, time_steps) {
  set.seed(17540)  # Set seed for reproducibility
  
  # Loop through each time step, calculating population changes
  for (t in 2:time_steps) {
    # Binomial stochastic processes for births, deaths, and predation
    rabbits_born <- rbinom(1, rabbit_population[t-1], alpha)
    rabbits_eaten <- rbinom(1, rabbit_population[t-1] * fox_population[t-1], beta)
    foxes_died <- rbinom(1, fox_population[t-1], gamma)
    
    # Update rabbit population, ensuring non-negative values
    rabbit_population[t] <- max(0, rabbit_population[t-1] + rabbits_born - rabbits_eaten)
    
    # Update fox population, ensuring non-negative values (rabbits eaten = new foxes)
    fox_population[t] <- max(0, fox_population[t-1] + rabbits_eaten - foxes_died)
  }
  
  # Return the final populations
  list(rabbits = rabbit_population, foxes = fox_population)
}

# Run the stochastic model
sto_results <- stochastic_lotka_volterra(rabbit_population, fox_population, alpha, beta, gamma, time_steps)
sto_rabbits <- sto_results$rabbits
sto_foxes <- sto_results$foxes

# Display the last few values of the stochastic model results
cat("Stochastic Model - Last few values:\n")
cat("Rabbits:\n", paste(tail(sto_rabbits, n = 5), collapse = "\n"), "\n")
cat("Foxes:\n", paste(tail(sto_foxes, n = 5), collapse = "\n"), "\n")

