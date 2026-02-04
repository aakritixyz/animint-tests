library(animint2)
library(data.table)

# Prepare the data
iris_dt <- as.data.table(iris)
iris_dt[, id := .I] # Create a unique ID for 'key' aesthetic

# The Selector Plot (Sepal)
plot_sepal <- ggplot() +
  geom_point(data = iris_dt, 
             aes(x = Sepal.Length, y = Sepal.Width, 
                 fill = Species, key = id),
             clickSelects = "Species", # Interaction
             size = 4, shape = 21, alpha = 0.6) +
  scale_fill_manual(values = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")) +
  theme_bw() +
  ggtitle("Sepal Data: Click a dot to select Species")

#The Target Plot (Petal)
plot_petal <- ggplot() +
  geom_point(data = iris_dt, 
             aes(x = Petal.Length, y = Petal.Width, 
                 fill = Species, key = id),
             showSelected = "Species", # Filtered by selection
             size = 4, shape = 21) +
  scale_fill_manual(values = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")) +
  theme_bw() +
  ggtitle("Petal Data: Showing Selected Species Only")

viz <- animint(
  sepalPlot = plot_sepal,
  petalPlot = plot_petal,
  title = "Iris Linked Brushing Demo",
  duration = list(Species = 500) # Smooth 0.5s transition
)

# Render locally
animint2dir(viz, out.dir = "gsoc_easy_test", open.browser = TRUE)