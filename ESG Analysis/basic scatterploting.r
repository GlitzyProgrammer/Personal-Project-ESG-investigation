
library(ggplot2)
library(readxl)

original_dataframe <- readxl::read_excel("C:/Users/ljwil/OneDrive/Desktop/Personal Projects/ESG Analysis/SP 500 ESG Risk Ratings.xlsx", sheet = "SP 500 ESG Risk Ratings")

# Cleanning data
rows_df <- original_dataframe[!is.na(original_dataframe$`Total ESG Risk score`) &
                              !is.na(original_dataframe$`Full Time Employees`) &
                              original_dataframe$`Full Time Employees` != 'N/A', ]

total_esg_colum_y <- rows_df$`Total ESG Risk score`
full_time_values_x <- rows_df$`Full Time Employees`


# Fit the linear model (OLS regression)
model <- lm(total_esg_colum_y ~ full_time_values_x, data = rows_df)


summary(model)

#R stats
multiple_r <- sqrt(summary(model)$r.squared)
multiple_r_squared <- summary(model)$r.squared 
adjusted_r_squared <- summary(model)$adj.r.squared
residuals <- model$residuals
rse <- sqrt(sum(residuals^2) / (length(residuals) - 2))  # residual standard error (RSE)
num_observations <- length(residuals)


Regression_Summary <- data.frame(
  "Multiple R" = multiple_r,
  "R Square" = multiple_r_squared,
  "Adjusted R Square" = adjusted_r_squared,
  "Standard Error" = rse,
  "Number of Observations" = num_observations
)


print("Regression Statistics Summary")
# printing out regression summary
for (i in 1:nrow(Regression_Summary)) {
  for (j in 1:ncol(Regression_Summary)) {
    # Get the column name
    col_name <- colnames(Regression_Summary)[j]
    
    # Get the value at that position
    value <- Regression_Summary[i, j]
    
    # Print the column name and the value
    cat(col_name, ": ", value, "\n")
  }
}





# Add regression line
plot(full_time_values_x, total_esg_colum_y,
 main ="Relationship between total employers and ESG Scores",xlab = "Full Time Employees", ylab = "Total ESG Risk Score", pch = 19)  # Scatter plot
abline(model, col = "red")