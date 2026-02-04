# Ten Random Points - Animint2 demo
# Author: Aakriti Kushwaha
# Purpose: GSOC animint2 Easy Test

library(animint2)

set.seed(1)
ten.points <- data.frame(
  x = 0:9,
  y = rnorm(10)
)

ten.points$group <- ifelse(ten.points$x < 5, "Left", "Right")

viz <- animint(
  point = ggplot() +
    geom_point(
      aes(x, y, color = group),
      clickSelects = "group",
      data = ten.points,
      size = 4
    ),
  title = "Ten Random Points (Animint2)",
  source = "https://github.com/aakritixyz/ten-points-animint"
)

animint2dir(
  viz,
  out.dir = "ten-points-animint",
  open.browser = TRUE
)