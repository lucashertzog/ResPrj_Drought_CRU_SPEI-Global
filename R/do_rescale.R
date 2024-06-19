# tar_load(dat_cru)

do_rescale <- function(
    dat_cru
){
  

# Function to calculate six-month rolling sums
calc_six_month_sum <- function(x) {
  n <- length(x)
  roll_sum <- numeric(n)
  for (i in seq_len(n)) {
    if (i >= 6) {
      roll_sum[i] <- sum(x[(i-5):i], na.rm = TRUE)
    } else {
      roll_sum[i] <- NA
    }
  }
  return(roll_sum)
}

# Function to scale percentiles to the range -4 to +4
scale_to_range <- function(x, min = -4, max = 4) {
  ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) * (max - min) + min
}

# Calculate six-month rolling sums of cru_value
dat_cru[, six_month_sum := calc_six_month_sum(cru_value), by = .(adm0, adm1, adm2)]

# Remove rows with NA values in six_month_sum
dat_cru <- dat_cru[!is.na(six_month_sum)]

# Calculate the six-month rainfall percentiles
dat_cru[, six_month_percentile := rank(six_month_sum, na.last = "keep") / .N, by = .(adm0, adm1, adm2)]

# Scale the percentiles to lie between -4 and +4
dat_cru[, scaled_rainfall := scale_to_range(six_month_percentile, min = -4, max = 4), by = .(adm0, adm1, adm2)]

return(dat_cru)
}