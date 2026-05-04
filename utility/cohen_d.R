# Function that computes the Cohen's d for the different types of comparison we
# run in the analyses. Will also be used in the meta-analysis.
#
# @param t Numeric denoting the value of the t-statistic in the comparison
# @param n Numeric or numeric vector containing the value(s) of the sample size
# @param paired Logical denoting whether the comparison was paired or not
cohen_d <- function(t, 
                    n,
                    paired = FALSE) {

  # Some checks first:
  if(is.na(t) || any(is.na(n))) {
    return(
      data.frame(
        d = NA_real_, 
        dvar = NA_real_
      )
    )
  }

  # Distinguish between the different cases:
  #   - Independent sample: 2 sample sizes provided, not paired
  #   - One sample: 1 sample size provided, not paired
  #   - Paired sample: 1 sample provided, paired
  #
  # Independent sample
  if(length(n) == 2 & !paired) {
    d <- t * sqrt(1 / n[1] + 1 / n[2])
    dvar <- (n[1] + n[2]) / (n[1] * n[2]) + (d^2 / (2 * (n[1] + n[2])))

  # One sample
  } else if(length(n) == 1 & !paired) {
    d <- t / sqrt(n)
    dvar <- (1 / n) + (d^2 / (2 * n))

  # Paired sample
  } else if(length(n) == 1 & paired) {
    d <- t / sqrt(n)
    dvar <- (1 / n) + (d^2 / (2 * n))

  } else {
    stop("Input does not match any of the types of d's.")
  }

  return(
    data.frame(
      d = as.numeric(d), 
      dvar = as.numeric(dvar)
    )
  )
}

