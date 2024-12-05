# Task 3: Preparing data for visualization
library(ggplot2)

# Create a long data frame with time, group, and size
LV <- data.frame(
  time = rep(1:time_steps, 4),
  group = rep(c("rabbits_deterministic", "foxes_deterministic", "rabbits_stochastic", "foxes_stochastic"), each = time_steps),
  size = c(rabbit_population_det, fox_population_det, rabbit_population_sto, fox_population_sto)
)

ggplot(LV, aes(x = time, y = size, color = group, linetype = group)) +
  geom_line(linewidth = 1) +  # Adjust line thickness using linewidth
  labs(
    title = "Lotka-Volterra Model: Deterministic vs Stochastic",
    x = "Time (weeks)",
    y = "Population Size"
  ) +
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(
    legend.position = "top"  # Place legend at the top for better visibility
  )

