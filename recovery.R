################################################################################
# PURPOSE: Investigate recovery of temporal dependence under the following 
#          circumstances:
#              - Different sample sizes
#              - One following the other or effective pairs
#              - Examination of autoregressive effect or autocorrelation
################################################################################

# Define the objective function of the autoregressive model that accounts for 
# only effective pairs
objective_function <- function(x, y) {
    # Define the parameters
    delta <- x[1]
    phi <- x[2]
    sigma <- x[3]

    # Define the expectation (y_hat) based on the parameters and the initial 
    # conditions
    y_hat <- delta + phi * y[, 1]

    # Compare the observations against the expectations and define the 
    # likelihood
    L <- dnorm(
        y[, 2], 
        mean = y_hat, 
        sd = sigma
    )

    # Return the min-log-likelihood for the minimizer
    return(-sum(log(L)))
}

# Define the different conditions
sample_size <- seq(5, 50, 5)

fx <- list(
    "autoregression" = function(N, parameters) {
        # Create the matrix that will hold all effective pairs
        y <- matrix(0, nrow = N, ncol = 2)

        # Define the initial condition
        y[1, 1] <- rnorm(
            1,
            mean = parameters[1] * (1 - parameters[2])^(-1),
            sd = parameters[3]
        )

        # Loop over all remaining points
        for(i in 1:N) {
            y[i, 2] <- rnorm(
                1, 
                mean = parameters[1] + parameters[2] * y[i, 1],
                sd = parameters[3]
            )
            if(i != N) {
                y[i + 1, 1] <- y[i, 2]
            }
        }
        
        return(y)
    },
    "pairs" = function(N, parameters) {
        # Create the matrix that will hold all effective pairs
        y <- matrix(0, nrow = N, ncol = 2)

        # Sample all initial conditions separately from the same normal 
        # distribution (no one string of observations, but loose strings)
        y[, 1] <- rnorm(
            N, 
            mean = parameters[1] * (1 - parameters[2])^(-1),
            sd = parameters[3]
        )

        # Compute the remaining points
        y[, 2] <- rnorm(
            N, 
            mean = parameters[1] + parameters[2] * y[, 1], 
            sd = parameters[3]
        )

        return(y)
    }
)
statistics <- list(
    "autoregressive" = function(data) {
        # Compute the autoregressive model through minimizing the objective 
        # function through DEoptim
        result <- DEoptim::DEoptim(
            \(x) objective_function(x, data), 
            lower = c(0, -1 + 1e-5, 1e-5), 
            upper = c(1, 1 - 1e-5, 0.5),
            control = DEoptim::DEoptim.control(
                itermax = 500,
                NP = 100,
                strategy = 6,
                p = 0.5,
                trace = FALSE
            )
        )

        # Return the autoregressive effect
        return(result$optim$bestmem[2])
    },
    "autocorrelation" = function(data) {
        return(cor(data[, 1], data[, 2]))
    }
)

# Loop over the different ways of generating the data
N <- 1000

set.seed(1)
for(i in seq_along(fx)) {
    # Loop over the different sample sizes
    for(j in sample_size) {
        # Generate the parameters for each simulation
        parameters <- cbind(
            runif(N, min = 0, max = 1),
            runif(N, min = -0.9, max = 0.9),
            runif(N, min = 1e-2, max = 0.4)
        )

        # Create a data.frame that will contain all needed information
        results <- data.frame(
            simulated = parameters[, 2],
            estimated = numeric(N),
            correlation = numeric(N)
        )

        # Loop over the number of simulations to run for each recovery study
        for(k in 1:N) {
            cat(paste0("\r", names(fx)[i], ": ", j, ": ", k))

            # Simulate data
            y <- fx[[i]](j, parameters[k, ])

            # Estimate the autoregressive effect and the autocorrelation
            results$estimated[k] <- statistics[["autoregressive"]](y)
            results$correlation[k] <- statistics[["autocorrelation"]](y)
        }

        # Save the results
        write.table(
            results,
            file.path("results", paste0(names(fx)[i], "_", j, ".txt")),
            sep = ","
        )
    }

    # Once all sample sizes have been completed, we can create a plot for this
    plt <- lapply(
        seq_along(sample_size),
        function(x) {
            # Load in data
            results <- read.table(
                file.path("results", paste0(names(fx)[i], "_", sample_size[x], ".txt")),
                sep = ","
            )

            # Create plot with the autoregressive effects
            plt_1 <- ggplot2::ggplot(
                results, 
                ggplot2::aes(
                    x = simulated,
                    y = estimated
                )
            ) +
                ggplot2::geom_abline(
                    intercept = 0, 
                    slope = 1,
                    color = "black",
                    linewidth = 2
                ) +
                ggplot2::geom_point(
                    shape = 21,
                    color = "black",
                    fill = "cornflowerblue",
                    size = 3, 
                    alpha = 0.25
                ) +
                ggplot2::labs(
                    title = sample_size[x],
                    x = "Simulated",
                    y = "Estimated"
                ) + 
                ggplot2::lims(
                    x = range(results[, c(1, 2)]),
                    y = range(results[, c(1, 2)])
                )

            # Create a plot with the autocorrelation 
            plt_2 <- ggplot2::ggplot(
                results, 
                ggplot2::aes(
                    x = simulated,
                    y = correlation
                )
            ) +
                ggplot2::geom_abline(
                    intercept = 0, 
                    slope = 1,
                    color = "black",
                    linewidth = 2
                ) +
                ggplot2::geom_point(
                    shape = 21,
                    color = "black",
                    fill = "cornflowerblue",
                    size = 3, 
                    alpha = 0.25
                ) +
                ggplot2::labs(
                    x = "Simulated",
                    y = "Autocorrelation"
                ) + 
                ggplot2::lims(
                    x = range(results[, c(1, 3)]),
                    y = range(results[, c(1, 3)])
                )

            # Return the combination of both
            return(
                ggpubr::ggarrange(
                    plt_1, 
                    plt_2,
                    ncol = 1
                )
            )
        }
    )

    # Add all plots together
    plt <- ggpubr::ggarrange(
        plotlist = plt,
        nrow = 1
    )

    # Save figure
    ggplot2::ggsave(
        file.path("figures", paste0(names(fx)[i], ".png")),
        plt,
        width = length(sample_size) * 750,
        height = length(statistics) * 775,
        unit = "px"
    )

    if(i == length(fx)) {
        cat("\n")
    }
}

