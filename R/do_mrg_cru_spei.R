# tar_load(cru_rescale)
# tar_load(dat_spei)

mrg <- merge(cru_rescale, dat_spei, by = c("date", "group_id"), all = FALSE)

lso <- mrg[adm1.x=="Leribe"]



# Optionally add a regression line
abline(lm(spei_value ~ scaled_rainfall, data = mrg), col = "red")

# Fit a linear regression model
model <- lm(spei_value ~ scaled_rainfall, data = lso)

# Summary of the model
summary(model)

# Coefficient for scaled_rainfall
slope <- coef(model)["scaled_rainfall"]
slope

coefficients <- coef(model)
intercept <- coef(model)["(Intercept)"]

# Calculate the change in SPEI value for a change in scaled rainfall
change_in_spei <- slope * (-1)


# Create the scatter plot
plot(lso$scaled_rainfall, lso$spei_value,
     xlab = "RP",
     ylab = "SPEI Value",
     main = "",
     pch = 20)
# Add the regression line
abline(model, col = "red")

# Highlight the change for a specific decrease
points(x = -1, y = intercept + slope * -1, col = "green", pch = 19, cex = 2)
text(x = -1, y = intercept + slope * -1, labels = paste("Change in SPEI:", round(change_in_spei, 3)), pos = 4, col = "green")

# Create the initial plot with SPEI value
plot(lso$date, lso$spei_value,
     type = "l",  # Line plot
     col = "blue",
     xlab = "Date",
     ylab = "Value",
     main = "SPEI Value (Blue) and Scaled Rainfall (Red) over Time")

# Add the scaled rainfall to the plot
lines(lso$date, lso$scaled_rainfall, col = "red")

# Add a legend to the plot
legend("topright", legend = c("SPEI Value", "Scaled Rainfall"),
       col = c("blue", "red"), lty = 1, cex = 0.8)
