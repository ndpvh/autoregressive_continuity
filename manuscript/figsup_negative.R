library(ggplot2)

# Create data showing an oscillatory regulatory process with negative
# autoregressive effect-0.75
data <- data.frame(
  x = 1:10, 
  y = (-0.75)^seq(0, 9)
)

# Create the plot showing this regulation
plt <- ggplot(
  data, 
  aes(x = x, y = y)
) +
  geom_hline(
    yintercept = 0, 
    linetype = "solid", 
    color = "black",
    linewidth = 1.5
  ) +
  geom_line(
    color = "black", 
    linewidth = 1, 
    linetype = "dashed"
  ) +
  geom_point(
    shape = 21, 
    color = "black",
    fill = "cornflowerblue",
    size = 6
  ) +
  labs(
    title = "", 
    x = "Time", 
    y = "Process"
  ) +
  lims(y = c(-1, 1)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(
      fill = "white", 
      color = NA
    ),
    panel.border = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 1
    ),
    axis.title = element_text(
      size = 25
    ),
    axis.text = element_blank()
  )

# Save the plot in a predestined location
ggsave(
  file.path("manuscript", "figure_negative.png"),
  plt, 
  width = 1000 * 2, 
  height = 600 * 2, 
  unit = "px"
)
