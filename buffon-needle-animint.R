library(animint2)
library(data.table)

set.seed(123)
n_drops <- 100
L <- 0.8  # Needle length
D <- 1.0  # Distance between lines (L < D)
total_lines <- 5

y_center <- runif(n_drops, 0, total_lines) 
theta <- runif(n_drops, 0, pi)
x_center <- runif(n_drops, 0, 10)

dy <- (L/2) * sin(theta)
dx <- (L/2) * cos(theta)

# Coordinates
y1_all <- y_center - dy
y2_all <- y_center + dy
x1_all <- x_center - dx
x2_all <- x_center + dx

is_cross <- floor(y1_all) != floor(y2_all)

# Statistics
cumulative_hits <- cumsum(is_cross)
iterations <- 1:n_drops
pi_estimates <- (2 * L * iterations) / (D * cumulative_hits)
# Clean up early division by zero for the plot
pi_estimates[is.infinite(pi_estimates) | is.na(pi_estimates)] <- 0

# Table for the estimation plot (added label_text)
estimate_dt <- data.table(
  iteration = iterations,
  estimate = pi_estimates,
  hits = cumulative_hits,
  label_text = paste0("Pi: ", round(pi_estimates, 3))
)

# Table for the needles
needle_dt <- rbindlist(lapply(iterations, function(i) {
  data.table(
    iteration = i,
    x1 = x1_all[1:i],
    x2 = x2_all[1:i],
    y1 = y1_all[1:i],
    y2 = y2_all[1:i],
    cross = factor(is_cross[1:i], levels = c(TRUE, FALSE))
  )
}))

# Plot 1: The Floor (theme_bw)
floor_plot <- ggplot() +
  geom_hline(yintercept = 0:total_lines, color = "black", size = 1) + 
  geom_segment(data = needle_dt,
               aes(x = x1, xend = x2, y = y1, yend = y2, color = cross),
               showSelected = "iteration") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "green")) +
  geom_text(data = estimate_dt,
            aes(x = 5, y = total_lines + 0.5, 
                label = sprintf("Drops: %d | Hits: %d | Pi Est: %.4f", iteration, hits, estimate)),
            showSelected = "iteration", size = 5, fontface = "bold") +
  labs(title = "Buffon's Needle Simulation", x = "Width", y = "Lines") +
  coord_equal() +
  theme_bw() + # Removed theme_minimal
  theme(legend.position = "none")

# Plot 2: Convergence (Smoothness, Scales, and Labels)
conv_plot <- ggplot() +
  geom_hline(yintercept = pi, linetype = "dashed", color = "blue", size = 1) +
  geom_line(data = estimate_dt, aes(x = iteration, y = estimate), color = "black", alpha = 0.3) +
  # THE RED POINT
  geom_point(data = estimate_dt, aes(x = iteration, y = estimate),
             showSelected = "iteration", clickSelects = "iteration",
             color = "red", size = 4) +
  # THE MOVING LABEL
  geom_text(data = estimate_dt, aes(x = iteration, y = estimate, label = label_text),
            showSelected = "iteration", vjust = -1.2, fontface = "bold") +
  # Explicit Scales and Theme
  scale_x_continuous(breaks = seq(0, n_drops, by = 10)) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 0.5)) +
  labs(title = "Convergence to Pi", x = "Number of Drops", y = "Estimate") +
  theme_bw() # Removed theme_minimal

viz <- animint(
  floor = floor_plot,
  convergence = conv_plot,
  time = list(variable = "iteration", ms = 600),
  duration = list(iteration = 300), 
  title = "Buffon's Needle Pi Approximation (Final Fix)"
)

# Output
if(dir.exists("buffon_needle_fix")) unlink("buffon_needle_fix", recursive = TRUE)
animint2dir(viz, out.dir = "buffon_needle_fix", open.browser = TRUE)