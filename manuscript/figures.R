################################################################################
# PURPOSE:
#
# This script contains the code for the figures used in the manuscript. Each
# figure is references by its number and has a short description describing 
# its purpose.
#
# Author: Niels Vanhasbroeck
################################################################################

library(ggplot2)
library(ggpubr)



################################################################################
# Figure 1
#
# Comparison of continuous-time time series and samples thereof. Additionally 
# visualizing the exponential relationship between sampling rate and discrete-
# time temporal dependence.

# Create function for the Ornstein-Uhlenbeck model. Sampling/Approximation 
# through the Euler method. The parameter \mu defines the attractor of the 
# system, \theta is the drift rate which determines the speed and shape of the 
# drift towards this attractor, and \sigma defines the relative value of the 
# diffusion
euler <- function(N, parameters, dt = 1e-4) {
  # Extract the parameters (makes it easier to give them meaning)
  mu <- parameters[1]
  theta <- parameters[2]
  sigma <- parameters[3]

  # Define the numeric vector and the initial condition
  y <- numeric(N)
  y[1] <- rnorm(1, mean = mu, sd = sigma)
  for(i in 2:N) {
    y[i] <- rnorm(
      1, 
      mean = y[i - 1] + theta * (mu - y[i - 1]) * dt, 
      sd = sigma
    )
  }

  return(y)
}

# Define the parameters to be used when generating the data for the figures
theta <- 100
dt <- 1e-4

# Create a continuous time-series
set.seed(41) # Static Dress - Unexplainabletitlesleavingyouwonderingwhy
y <- euler(
  5000,
  c(0, theta, 1),
  dt = dt
)

# Sample from the continuous time process with two different sampling rates, 
# specifically one with 51 samples (short time interval) and one with 26 
# samples (long time interval)
y_short <- y[seq(1, length(y), length.out = 51)]
y_long <- y_short[seq(1, length(y_short), by = 2)]

# Combine all time-series in a single list with an identifier that will be used
# in the figure
y <- list(
  "Continuous process" = y, 
  "Sampled with shorter time interval" = y_short, 
  "Sampled with longer time interval" = y_long
)

# Create the time-series figures
plt <- lapply(
  seq_along(y), 
  function(i) {
    # Create data.frame to be plotted for ggplot2
    data <- data.frame(
      time = seq_along(y[[i]]) * length(y[[1]]) / length(y[[i]]),
      y = y[[i]]
    )

    # Create the plot
    plt <- ggplot(
      data, 
      aes(
        x = time, 
        y = y
      )
    ) +
      geom_line(
        color = "black",
        linewidth = 1
      ) +
      labs(
        title = names(y)[i],
        x = "Time",
        y = "Process"
      ) +
      lims(
        y = range(y[[1]])
      ) +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(
          fill = NA, 
          color = "black",
          linewidth = 1
        ), 
        panel.grid.major = element_line(
          color = "gray90",
          linewidth = 0.5
        ),
        panel.grid.minor = element_line(
          color = "gray95",
          linewidth = 0.25
        ),
        plot.title = element_text(
          size = 20, 
          hjust = 0.5
        ),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 10)
      )

    # If needed, add points to the plot to indicate the sampled
    # values
    if(i > 1) {
      plt <- plt + 
        geom_point(
          shape = 21, 
          size = 3,
          color = "black",
          fill = ifelse(
            i == 2, 
            "salmon",
            "cornflowerblue"
          )
        )
    }

    return(plt)
  }
)

# Bind the time-series plots together
time_series <- ggarrange(
  plotlist = plt, 
  ncol = 1
)

# Create an exponential function displaying the value of the autoregressive effect
# against the time interval. Important: Discrete-time sampling rates are about 
# time_interval 100 (~5000/50) and 200 (~5000/25)
data <- data.frame(
  time_interval = seq_len(400) * dt,
  y = exp(-theta * seq_len(400) * dt)
)

exponential <- ggplot(
  data, 
  aes(
    x = time_interval, 
    y = y
  )
) +
  geom_line(
    color = "black",
    linewidth = 1.5
  ) +
  labs(
    x = "Sampled time interval",
    y = "Autoregressive effect"
  ) +
  annotate(
    "segment",
    x = c(
      data$time_interval[100],
      0, 
      data$time_interval[200],
      0
    ),
    xend = c(
      data$time_interval[100],
      data$time_interval[100],
      data$time_interval[200],
      data$time_interval[200]
    ),
    y = c(
      0, 
      data$y[100],
      0, 
      data$y[200]
    ),
    yend = c(
      data$y[100],
      data$y[100],
      data$y[200],
      data$y[200]
    ),
    linewidth = 1, 
    linetype = "dashed",
    color = rep(c("salmon", "cornflowerblue"), each = 2)
  ) +
  annotate(
    "point",
    x = data$time_interval[c(100, 200)], 
    y = data$y[c(100, 200)],
    shape = 21, 
    color = "black",
    fill = c("salmon", "cornflowerblue"),
    size = 3
  ) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      fill = NA, 
      color = "black",
      linewidth = 1
    ), 
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    plot.title = element_text(
      size = 20, 
      hjust = 0.5
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 7),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Save the plots separately (to be bound together in the manuscript)
ggsave(
    file.path(
      "manuscript", 
      "figures", 
      "fig1_time_series.png"
    ),
    time_series,
    width = 800 * 3, 
    height = 780 * 3,
    unit = "px"
)

ggsave(
    file.path(
      "manuscript", 
      "figures", 
      "fig1_exponential.png"
    ),
    exponential,
    width = 800, 
    height = 780,
    unit = "px"
)



################################################################################
# Figure 2
#
# Comparison of the time intervals in the short and long time interval conditions
# across datasets.

# Read in the datasets containing information on the time intervals
buchner = readRDS(
  file.path(
    "data_processed",
    "BUCHNER.Rds"
  )
)
cloos = readRDS(
  file.path(
    "data_processed",
    "CLOOS.Rds"
  )
)
dejonckheere = readRDS(
  file.path(
    "data_processed",
    "DEJONCKHEERE.Rds"
  )
)

# Put these data in a loopable list
data <- list(
  "Büchner" = buchner, 
  "Dejonckheere" = dejonckheere,
  "Cloos" = cloos
)

# Loop over all data and create a big data.frame containing the data to be 
# plotted. In this, we delete (a) all non-usable pairs, (b) all NA intervals, 
# and (c) retain only one interval per beep
data <- lapply(
  names(data),
  function(name) data[[name]] |>
    dplyr::mutate(
      dataset = name
    ) |>
    dplyr::group_by(id, beep_number, condition) |>
    dplyr::mutate(
      to_keep = dplyr::row_number() == 1,
      pair_sum = response + response_lag
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !is.na(time_interval),
      to_keep,
      !is.na(pair_sum)
    ) |>
    dplyr::select(dataset, condition, time_interval)
)
data <- do.call("rbind", data)

# Make the figure itself
plt <- ggplot(
  data,
  aes(
    x = dataset,
    y = time_interval, 
    fill = factor(condition), 
  )
) +
  geom_boxplot(
    outliers = FALSE,
    width = 0.75
  ) +
  lims(
    y = c(0, 270)
  ) +
  labs(
    y = "Time interval",
    x = "",
    fill = "Time interval"
  ) +
  scale_fill_manual(
    values = c(
      "short_interval" = "salmon",
      "long_interval" = "cornflowerblue"
    ),
    labels = c(
      "short_interval" = "Short",
      "long_interval" = "Long"
    )
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
        fill = NA, 
        color = "black",
        linewidth = 1
    ), 
    panel.grid.major = element_line(
        color = "gray90",
        linewidth = 0.5
    ),
    panel.grid.minor = element_line(
        color = "gray95",
        linewidth = 0.25
    ),
    plot.title = element_text(
        size = 30, 
        hjust = 0.5
    ),
    axis.title = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25)
  ) +
  annotate(
    "segment",
    x = c(0.8, 0.8, 1.2, 1.8, 1.8, 2.2, 2.8, 2.8, 3.2),
    xend = c(0.8, 1.2, 1.2, 1.8, 2.2, 2.2, 2.8, 3.2, 3.2),
    y = c(240, 245, 245, 160, 165, 165, 185, 190, 190),
    yend = c(245, 245, 240, 165, 165, 160, 190, 190, 185),
    color = "black",
    linewidth = 1
  ) +
  annotate(
    "label",
    x = c(1, 2, 3), 
    y = c(255, 175, 200),
    label = rep("p < 0.001", 3),
    size = 6,
    linewidth = NA
  )

# Save the resulting figure
ggsave(
  file.path(
    "manuscript",
    "figures",
    "fig2_intervals.png"
  ),
  plt, 
  width = 900 * 2.5, 
  height = 1000 * 2.5,
  unit = "px"
)



################################################################################
# Figure 3
#
# Visualization of our comparison procedures, comparing the autoregressive 
# effect across conditions (targeting the dependence assumption) and comparing
# autoregressive effects within a condition to a continuous-time estimate 
# (targeting the exponential assumption).

# Define a sequence of x over which to draw the normal distributions and a 
# grouping variable to be able to create a legend of the plot
x <- seq(-0.1, 1, 0.01)
y <- rep(-1, length(x))
group <- rep(
  c("Long", "Short"), 
  length.out = length(x)
)

# Define a margin within which the arrows will operate
margin <- 0.05

# Create the plot itself
plt <- ggplot(
  data.frame(x = x, y = y, group = group),
  aes(x = x, y = y, fill = group)
) +
  # To ensure the legend works
  geom_point(
    shape = 21
  ) +

  # Normal distributions
  stat_function(
    fun = \(x) dnorm(x, mean = 0.40, sd = 0.1),
    geom = "area",
    fill = "cornflowerblue",
    color = "black",
    linewidth = 1,
    alpha = 0.5
  ) +
  stat_function(
    fun = \(x) dnorm(x, mean = 0.70, sd = 0.1),
    geom = "area",
    fill = "salmon",
    color = "black",
    linewidth = 1,
    alpha = 0.5
  ) +

  # Vertical lines
  geom_vline(
    xintercept = 0.10,
    color = "black",
    linewidth = 2,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0.40,
    color = "cornflowerblue",
    linewidth = 2,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0.70,
    color = "salmon",
    linewidth = 2,
    linetype = "dashed"
  ) +

  # Arrows at the top
  geom_segment(
    x = 0.10 + margin, 
    xend = 0.40 - margin,
    y = 4.5, 
    yend = 4.5,
    linewidth = 1,
    arrow = arrow(
      ends = "both",
      length = unit(20, "pt"),
      type = "open"
    )
  ) +
  geom_segment(
    x = 0.40 + margin, 
    xend = 0.70 - margin,
    y = 4.5, 
    yend = 4.5,
    linewidth = 1,
    arrow = arrow(
      ends = "both",
      length = unit(20, "pt"),
      type = "open"
    )
  ) +
  annotate(
    "text",
    x = 0.25, 
    y = 4.75,
    label = "b",
    size = 15
  ) + 
  annotate(
    "text",
    x = 0.55,
    y = 4.75,
    label = "a",
    size = 15
  ) +

  # Information
  labs(
    title = "Comparison Strategy",
    y = "Density",
    x = "Autoregressive Effect"
  ) +
  lims(
    x = range(x), 
    y = c(0, 5)
  ) + 
  scale_fill_manual(
    name = "Time interval:  ",
    values = c(
      "Long" = "cornflowerblue", 
      "Short" = "salmon"
    )
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(size = 15)
    )
  ) +

  # Theme
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 1.5
    ),
    plot.title = element_text(
      size = 40, 
      hjust = 0.5
    ),
    axis.title = element_text(size = 35),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 35, face = "plain"),
    legend.text = element_text(size = 35)#,
    # legend.box.background = element_rect(fill = NA, linewidth = 0.5)
  )

# Save the plot
ggsave(
  file.path(
    "manuscript", 
    "figures", 
    "fig3_comparison.png"
  ),
  plt, 
  width = 1050 * 4,
  height = 800 * 4,
  unit = "px"
)



################################################################################
# Figure 1, Supplementary Materials
#
# Visualization of the typical behavior of the autoregressive model under 
# negative values for its dynamic parameter.

# Create an impulse-response function for the autoregressive model with the 
# negative autoregressive effect -0.75
data <- data.frame(
  x = 1:10, 
  y = (-0.75)^seq(0, 9)
)

# Create the figure itself
plt <- ggplot(
  data, 
  aes(x = x, y = y)
) +
  geom_hline(
    yintercept = 0, 
    linetype = "solid", 
    color = "black",
    linewidth = 2
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
  lims(
    y = c(-1, 1)
  ) +
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

# Save the figure
ggsave(
  file.path(
    "manuscript", 
    "figures", 
    "figs1_negative.png"
  ),
  plt, 
  width = 1000 * 2, 
  height = 600 * 2, 
  unit = "px"
)



################################################################################
# Figure 2, Supplementary Materials
#
# Forest plots for the meta-analyses. Only a few of these plots is actually 
# included in the supplementary materials. All plots can be created through the 
# code below.

# Define the conditions and datasets for which to create the forest plots
conditions <- cbind(
  rep(c("BUCHNER", "CLOOS", "DEJONCKHEERE"), each = 3),
  rep(c("condition", "continuous_s2l", "continuous_l2s"), times = 3)
)

# Define some bounds based on the estimated confidence intervals
bounds <- lapply(
  seq_len(nrow(conditions)),
  function(i) readRDS(
    file.path(
      "results", 
      paste0(
        conditions[i, 1],
        "_meta-",
        conditions[i, 2],
        ".Rds"
      )
    )
  )$yi |>
    as.numeric()
) |> 
  unlist() |> 
  as.numeric()

# Derive the limits themselves. Round the alims to 0.5 precision, allowing for 
# a cleaner number of steps in the plots. Note that we round to the ceiling in 
# both the negative and positive direction, widening the limits of the plot to 
# ensure that all confidence intervals are fully included in the plot
alims <- sign(range(bounds)) * ceiling(abs(range(bounds) * 2)) / 2
xlims <- alims + c(-0.5, 0.5) * diff(alims)

# Create forest plots that summarize the meta-analysis
for(i in seq_len(nrow(conditions))) {
  # Read in the results of the meta-analysis for the specific dataset and
  # condition
  result <- readRDS(
    file.path(
      "results", 
      paste0(
        conditions[i, 1],
        "_meta-",
        conditions[i, 2],
        ".Rds"
      )
    )
  )

  # Create the plots themselves
  pdf(
    file.path(
      "manuscript",
      "figures", 
      paste0(
        "figs2_", 
        conditions[i, 1],
        "_meta-", 
        conditions[i, 2], 
        ".pdf"
      )
    ),
    width = 8, 
    height = 5 * nrow(result$data) / 19 + 1.5
  )

  par(mar = rep(2, 4))

  metafor::forest(
    result, 
    xlab = "Cohen's d",
    slab = result$data$item, 
    header = "Item",
    alim = alims,
    xlim = xlims,
    refline = ifelse(
      0 > xlims[1] & 0 < xlims[2],
      0,
      NA
    ),
    width = c(0.1, 0.8, 0.1),
    psize = 1,
    cex = 1,
    digits = 1,
    steps = 8
  )

  dev.off()
}



