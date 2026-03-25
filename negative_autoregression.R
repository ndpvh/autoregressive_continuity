################################################################################
# VISUALIZATION
# 
# Visualize the two types of drift. Either:
#   - Real values for the drift parameter \theta of the Ornstein-Uhlenbeck model,
#     leads to strictly positive values of the autoregressive effect
#   - Imaginary parts for the drift parameter \theta of the Ornstein-Uhlenbeck 
#     model, leads to negative and/or positive values of the autoregressive 
#     effect, depending on the measured time interval

by <- 0.05

png(file.path("figures", "real-complex.png"), width = 1000, height = 550)

par(mfrow = c(1, 2))
plot(
    seq(by, 10, by = by), 
    exp(complex(real = log(0.5), imaginary = 0) * seq(by, 10, by = by)),
    type = "l",
    main = "Real Drift",
    xlab = "Time Interval",
    ylab = "Autoregressive effect",
    ylim = c(-1, 1),
    lwd = 3,
    cex.main = 3,
    cex.lab = 2
) |>
    suppressWarnings()

plot(
    seq(by, 10, by = by), 
    exp(complex(real = log(0.5), imaginary = pi) * seq(by, 10, by = by)),
    type = "l",
    main = "Complex Drift",
    xlab = "Time Interval",
    ylab = "Autoregressive effect",
    ylim = c(-1, 1),
    lwd = 3,
    cex.main = 3,
    cex.lab = 2
) |>
    suppressWarnings()

dev.off()


################################################################################
# TRANSFORMATION
# 
# Check whether we can compute the expected value of the autoregressive effect
# after a time interval \Delta t_1 when we already computed the autoregressive 
# effect after a time interval \Delta t_0. 
#
# This simulation study goes as follows:
#   - Create the expected trajectories of the autoregressive effect based on 
#     the OU (as visualized above)
#   - Draw a random value for the autoregressive effects after a time interval 
#     \Delta t_0 and \Delta t_1
#   - Compute the value of the autoregressive effect \phi_1 after a time interval
#     \Delta t_1 based on this time interval, \Delta t_0, and \phi_0
#
# ASSUMPTION: In the transformation, we will always assume that the imaginary
# part of the drift parameters under a negative autoregressive effect is equal 
# to pi, effectively communicating that this value is at a turning point in the 
# cycle and won't become more negative when changing the time interval slightly. 
# This is a necessary assumption to take for identifiability purposes and can be
# defended based on the autoregressive framework, where a negative autoregressive
# effect creates sawtooth-like dynamics that can only be displayed in the 
# assumed case (note that this, of course, assumes equal time intervals).
#
# This assumption becomes problematic in the following cases (as seen in the 
# simulation study):
#   - When the first autoregressive effect ar_0 comes from an oscillating 
#     dynamical system (imaginary part of theta is not 0) but is positive. In 
#     this case, we transform the positive value of the autoregressive effect 
#     according to a real exponential rather than the complex one that would be
#     required for correct derivations.
#   - When the two time intervals are not integer multiples of each other such
#     that a rotation has happened. While not impossible and accounted for 
#     somewhat through the ratio of time intervals, it does make it difficult
#     to map the derived values of the autoregressive effect to the actual 
#     correct ones when time intervals fall between the discrete-time gaps.

# Create a transformer function, transforming the autoregressive effect to a new 
# time interval
#
# @param ar_0 Numeric denoting the autoregressive effect based on a time 
# interval dt_0
# @param dt_0 Numeric denoting the time interval at which the autoregressive 
# effect ar_0 has been measured
# @param dt_1 Numeric denoting the time interval for which you would like to 
# compute the expected autoregressive effect
#
# @return The expected value for the autoregressive effect as measured at a 
# time interval dt_1 (i.e., ar_1)
transformer <- function(ar_0,
                        dt_0, 
                        dt_1) {

    # Based on the value of the autoregressive effect, we create a complex 
    # number with either no imaginary parts (ar > 0) or with an imaginary part
    # at an angle pi (ar < 0).
    if(ar_0 >= 0) {
        theta <- complex(
            real = log(ar_0),
            imaginary = 0
        )
    } else {
        theta <- complex(
            real = log(abs(ar_0)),
            imaginary = pi
        )
    }

    # Transform to the expected value at a time interval dt_1. Only use the 
    # real parts of this number
    return(Re(exp(theta * dt_1 / dt_0)))
}

# Create a true drift function, describing what the autoregressive effect would 
# be like at particular time intervals when using the drift parameter of the OU
#
# @param theta Complex number denoting the drift parameter theta
# @param dt Discretized time step by which the time interval increases over time.
# Defaults to 1e-2.
# @param max_time Numeric denoting the maximal value for the time interval. 
# Defaults to 5.
#
# @return Dataframe containing the time interval ("interval") and the 
# corresponding autoregressive effect ("ar") under the specification of the OU
drift <- function(theta,
                  dt = 1e-2,
                  max_time = 5) {
    
    # Create the time intervals 
    interval <- seq(dt, max_time, by = dt)

    # Create and return the requested data.frame
    return(
        data.frame(
            time_interval = interval, 
            ar = Re(exp(-theta * interval))
        )
    )
}

# Run the simulation study itself
set.seed(6622537) # Sesame Street is No Place for Romance - A Lot Like Birds
result <- lapply(
    1:500, 
    function(i) {
        # Generate random values for the drift parameter within particular bounds
        theta <- complex(
            real = runif(1, 1e-2, 2),
            imaginary = runif(1, -pi + 1e-2, pi - 1e-2)
        )

        # Generate the expected autoregressive effects at particular time 
        # intervals for this drift parameter
        autoregressive <- drift(theta)

        # Select two random rows within this data.frame. The first occurring 
        # value will be treated as the given autoregressive effect ar_0 at a 
        # particular time interval dt_0, while the second value will be taken 
        # as the time interval dt_1 that we want to transform to
        idx <- sample(1:nrow(autoregressive), 2) |>
            sort()

        # Transform the ar_0 to ar_1
        times <- autoregressive$time_interval[idx]
        transformed <- transformer(
            ar_0 = autoregressive$ar[idx[1]],
            dt_0 = times[1],
            dt_1 = times[2]
        )

        # Return a single rowed data.frame containing all useful information
        return(
            data.frame(
                ar_0 = autoregressive$ar[idx[1]],
                dt_0 = times[1],
                dt_1 = times[2],
                ar_1_true = autoregressive$ar[idx[2]],
                ar_1_transformed = transformed,
                ar_1_difference = abs(transformed - autoregressive$ar[idx[2]]),
                theta_real = Re(theta),
                theta_imaginary = Im(theta),
                oscillating = abs(Im(theta)) > 5e-1,
                dt_difference = abs(times[2] / times[1] - round(times[2] / times[1]))
            )
        )
    }
)
result <- do.call("rbind", result)

# Plot the result
png(file.path("figures", "simulation.png"), width = 750, height = 850)
par(mfrow = c(1, 1))
plot(
    result$ar_1_true, 
    result$ar_1_transformed, 
    type = "n",
    main = "Simulation Study Results",
    xlab = "True AR",
    ylab = "Derived AR",
    xlim = c(-1, 1),
    ylim = c(-1, 1),
    cex.main = 3,
    cex.lab = 2
)
abline(
    a = 0, 
    b = 1, 
    col = "black",
    lwd = 3
)
points(
    result$ar_1_true,
    result$ar_1_transformed, 
    col = "black",
    bg = "cornflowerblue",
    cex = 3,
    pch = 21
)
dev.off()
