library(animint2)
library(data.table)

# 1. Data Setup
set.seed(123)
n_points <- 50
FUN <- function(x) x - x^2

x_points <- runif(n_points)
y_points <- FUN(x_points)
estimates <- cumsum(y_points) / seq_len(n_points)

# Data for the points appearing over time
rect_dt <- rbindlist(lapply(seq_len(n_points), function(i) {
  data.table(iteration = i, x = x_points[1:i], y = y_points[1:i])
}))

# Explicit data for the red curve (replacing stat_function)
x_curve <- seq(0, 1, length.out = 100)
curve_dt <- data.table(x = x_curve, y = FUN(x_curve))

# Data for the running estimate line
line_dt <- data.table(
  iteration = seq_len(n_points),
  integral_est = estimates
)

# 2. Render using explicit data layers
viz <- animint(
  pointsPlot = ggplot() +
    # The background curve (static)
    geom_line(data = curve_dt, aes(x = x, y = y), 
              color = "red", size = 1) +
    # The points (dynamic)
    geom_point(data = rect_dt, 
               aes(x = x, y = y),
               showSelected = "iteration",
               color = "steelblue", size = 4) +
    labs(title = "Sample Mean Monte Carlo", x = "x", y = "f(x)") +
    theme_minimal(),
  
  linePlot = ggplot() +
    geom_line(data = line_dt, 
              aes(x = iteration, y = integral_est), 
              color = "red", alpha = 0.4) +
    geom_point(data = line_dt, 
               aes(x = iteration, y = integral_est), 
               showSelected = "iteration",
               clickSelects = "iteration",
               color = "red", size = 3) +
    geom_hline(yintercept = 1/6, linetype = "dashed", color = "blue") +
    labs(title = "Integral Estimate", x = "Iteration", y = "Estimated Integral") +
    theme_minimal(),
  
  time = list(variable = "iteration", ms = 500),
  duration = list(iteration = 250),
  first = list(iteration = 1)
)

# 3. Output
if(dir.exists("mc_samplemean")) unlink("mc_samplemean", recursive = TRUE)
animint2dir(viz, out.dir = "mc_samplemean", open.browser = TRUE)