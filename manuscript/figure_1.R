################################################################################
# Figure displaying continuous-time, different sample rates, and relationship
# between parameters
################################################################################

library(ggplot2)
library(ggpubr)

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

# Define the parameters to be used everywhere
theta <- 100
dt <- 1e-4

# Create a continuous time-series
set.seed(41) # Static Dress - Unexplainabletitlesleavingyouwonderingwhy
y <- euler(
    5000,
    c(0, theta, 1),
    dt = dt
)
plot(seq_along(y), y, type = "l")

# Differentiate between the continuous series and two different discrete time 
# series differing in the time interval between observations
y_short <- y[seq(1, length(y), length.out = 51)]
y_long <- y_short[seq(1, length(y_short), by = 2)]

# Combine all time-series in a single list
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
                        "cornflowerblue",
                        "salmon"
                    )
                )
        }

        return(plt)
    }
)

# Bind these plots together
time_series <- ggarrange(
    plotlist = plt, 
    ncol = 1
)

# Create an exponential function displaying the value of the autoregressive effect
# against the time interval. Important: Discrete-time sampling rates are at 
# time_interval 100 (5000/50) and 200 (5000/25)
data <- data.frame(
    time_interval = seq_len(400) * dt,
    y = exp(-theta * seq_len(400) * dt)
)
plot(data$time_interval, data$y)

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
        color = rep(c("cornflowerblue", "salmon"), each = 2)
    ) +
    annotate(
        "point",
        x = data$time_interval[c(100, 200)], 
        y = data$y[c(100, 200)],
        shape = 21, 
        color = "black",
        fill = c("cornflowerblue", "salmon"),
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
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 7),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )

# Bind both plots together to create the final plot
ggsave(
    file.path("manuscript", "fig1_time_series.png"),
    time_series,
    width = 800 * 3, 
    height = 780 * 3,
    unit = "px"
)

ggsave(
    file.path("manuscript", "fig1_exponential.png"),
    exponential,
    width = 800, 
    height = 780,
    unit = "px"
)
