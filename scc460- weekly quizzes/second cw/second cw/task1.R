# Parameters for the Lotka-Volterra model
alpha <- 0.05  # Birth rate of rabbits
beta <- 1.2e-4  # Rate of foxes eating rabbits
gamma <- 0.04  # Death rate of foxes

# Initial values
R1 <- 60  # Initial number of rabbits
F1 <- 30  # Initial number of foxes

# Time steps (104 weeks total: week 1 + 103 more weeks)
time_steps <- 104

# Vectors to store the populations over time
rabbit_population <- numeric(time_steps)
fox_population <- numeric(time_steps)

# Set initial values for week 1
rabbit_population[1] <- R1
fox_population[1] <- F1

# Implement the deterministic model in a for loop
for (t in 2:time_steps) {
  rabbit_population[t] <- rabbit_population[t-1] + alpha * rabbit_population[t-1] - beta * rabbit_population[t-1] * fox_population[t-1]
  fox_population[t] <- fox_population[t-1] + beta * rabbit_population[t-1] * fox_population[t-1] - gamma * fox_population[t-1]
}

# Print the last few values of rabbits and foxes (do not print the entire vectors)
tail(rabbit_population, n = 5)
tail(fox_population, n = 5)

