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

# Year labels data
year_labels <- unique(rosling_data[, .(year)])
year_labels[, year_text := as.character(year)]
year_labels[, key_id := 1]

# 1. BUBBLES PLOT
bubbles_plot <- ggplot() +
  # Big Background Year
  geom_text(data = year_labels,
            aes(x = 5, y = 2.5, label = year_text, key = key_id),
            showSelected = "year",
            size = 30, color = "grey92") +
  # Moving Bubbles
  geom_point(data = rosling_data,
             aes(x = x, y = y, size = size1, fill = id, key = id),
             showSelected = "year",
             shape = 21, color = "black", alpha = 0.7) +
 
# INCREASED LABEL SIZE & VISIBILITY
geom_text(data = rosling_data,
          aes(x = x, y = y + 1.2, label = label, key = id), # Nudged higher
          showSelected = "year",
          size = 6,              # Increased from 4 to 6
          fontface = "bold",
          color = "black") + 
  # =====================================================
scale_size_continuous(range = c(5, 20), name = "Size") +
  scale_fill_brewer(palette = "Set3", name = "ID") +
  labs(title = "Animint2 Test: Bubbles",
       subtitle = "Readable Labels",
       x = "X Axis", y = "Y Axis") +
  theme_bw()

# 2. RECTANGLES PLOT
rectangles_plot <- ggplot() +
  geom_text(data = year_labels,
            aes(x = 5, y = 2.5, label = year_text, key = key_id),
            showSelected = "year",
            size = 30, color = "grey92") +
  geom_rect(data = rosling_data,
            aes(xmin = x - size1, xmax = x + size1,
                ymin = y - size2, ymax = y + size2,
                fill = id, key = id),
            showSelected = "year",
            color = "black", alpha = 0.7) +
  # Adding labels to Rects too for better identification
  geom_text(data = rosling_data,
            aes(x = x, y = y, label = label, key = id),
            showSelected = "year",
            size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set3", name = "ID") +
  labs(title = "Animint2 Test: Rectangles",
       subtitle = "Visualizing Matrix Dimensions",
       x = "X Axis", y = "Y Axis") +
  theme_bw()

viz <- animint(
  bubbles = bubbles_plot,
  rects = rectangles_plot,
  time = list(variable = "year", ms = 1000), 
  duration = list(year = 1000),
  first = list(year = 1)
)

out_dir <- "rosling_bubbles_fix"
if(dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
animint2dir(viz, out.dir = out_dir, open.browser = TRUE)