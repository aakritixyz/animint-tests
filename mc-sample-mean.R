library(animint2)
library(data.table)

set.seed(123)
n_points <- 50
FUN <- function(x) x - x^2
x_points <- runif(n_points); y_points <- FUN(x_points)
estimates <- cumsum(y_points) / seq_len(n_points)

line_dt <- data.table(
  iteration = seq_len(n_points),
  integral_est = estimates,
  label_text = paste0("Est: ", round(estimates, 4))
)

viz_mc <- animint(
  pointsPlot = ggplot() +
    geom_line(data = data.table(x=seq(0,1,0.01), y=FUN(seq(0,1,0.01))), 
              aes(x, y), color = "red") +
    geom_point(data = rbindlist(lapply(1:n_points, function(i) data.table(iteration=i, x=x_points[1:i], y=y_points[1:i]))),
               aes(x, y), showSelected = "iteration", color = "steelblue") +
    theme_bw(), #theme-minimal removed
  
  linePlot = ggplot() +
    geom_line(data = line_dt, aes(iteration, integral_est), color = "red", alpha = 0.2) +
    geom_hline(yintercept = 1/6, linetype = "dashed", color = "blue") +
    # Red Point with Label
    geom_point(data = line_dt, aes(iteration, integral_est), 
               showSelected = "iteration", clickSelects = "iteration", color = "red", size = 4) +
    geom_text(data = line_dt, aes(iteration, integral_est, label = label_text),
              showSelected = "iteration", vjust = -1.5, fontface = "bold") +
    # Explicit Scales for Grid Lines
    scale_x_continuous(breaks = seq(0, n_points, by = 10)) +
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05)) +
    theme_bw() + 
    labs(title = "Integral Convergence", x = "Iteration", y = "Estimate"),
  
  time = list(variable = "iteration", ms = 1000),
  duration = list(iteration = 800) #  Smooth movement
)

animint2dir(viz_mc, out.dir = "mc_final", open.browser = TRUE)