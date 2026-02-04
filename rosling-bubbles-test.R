library(data.table)
library(animint2)

set.seed(42)
n_individuals <- 10
n_years <- 50

dt_list <- lapply(1:n_years, function(k) {
  data.table(
    id = factor(paste("Subject", 1:n_individuals)),
    year = k,
    x = rnorm(n_individuals, mean = k/5, sd = 2), 
    y = rnorm(n_individuals, mean = k/10, sd = 2),
    size1 = abs(rnorm(n_individuals, 2, 0.5)),
    size2 = abs(rnorm(n_individuals, 2, 0.5)),
    label = paste("Subj:", 1:n_individuals)
  )
})
rosling_data <- rbindlist(dt_list)

# Year labels data (for the big background text)
year_labels <- unique(rosling_data[, .(year)])
year_labels[, year_text := as.character(year)]
# We add a constant 'key_id' to help animint2 track this single text object
year_labels[, key_id := 1]

#bubble-plot
bubbles_plot <- ggplot() +
  # Big Background Year: Key moved inside aes() to silence warnings
  geom_text(data = year_labels,
            aes(x = 5, y = 2.5, label = year_text, key = key_id),
            showSelected = "year",
            size = 40, color = "grey92") +
  # Moving Bubbles
  geom_point(data = rosling_data,
             aes(x = x, y = y, size = size1, fill = id, key = id),
             showSelected = "year",
             shape = 21, color = "black", alpha = 0.7) +
  # Subject Labels
  geom_text(data = rosling_data,
            aes(x = x, y = y + 0.8, label = label, key = id),
            showSelected = "year",
            size = 4, fontface = "bold") +
  scale_size_continuous(range = c(5, 20), name = "Size") +
  scale_fill_brewer(palette = "Set3", name = "ID") +
  labs(title = "Animint2 Test: Bubbles",
       subtitle = "Clean Replication of Rosling.bubbles()",
       x = "X Axis", y = "Y Axis") +
  theme_bw()

#rectangles

rectangles_plot <- ggplot() +
  # Big Background Year: Key moved inside aes()
  geom_text(data = year_labels,
            aes(x = 5, y = 2.5, label = year_text, key = key_id),
            showSelected = "year",
            size = 40, color = "grey92") +
  # Moving Rectangles
  geom_rect(data = rosling_data,
            aes(xmin = x - size1, xmax = x + size1,
                ymin = y - size2, ymax = y + size2,
                fill = id, key = id),
            showSelected = "year",
            color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Set3", name = "ID") +
  labs(title = "Animint2 Test: Rectangles",
       subtitle = "Visualizing Matrix Dimensions as Symbols",
       x = "X Axis", y = "Y Axis") +
  theme_bw()

viz <- animint(
  bubbles = bubbles_plot,
  rects = rectangles_plot,
  # Speed: 1000ms per iteration for a "slow" feel
  time = list(variable = "year", ms = 1000), 
  duration = list(year = 1000),
  first = list(year = 1)
)

out_dir <- "yihui_test_final_clean"
if(dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
animint2dir(viz, out.dir = out_dir, open.browser = TRUE)