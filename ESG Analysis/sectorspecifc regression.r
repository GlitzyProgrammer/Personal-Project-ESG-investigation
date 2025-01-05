
library(ggplot2)
library(readxl)
library(jsonlite)

original_dataframe <- readxl::read_excel("C:/Users/ljwil/OneDrive/Desktop/Personal Projects/ESG Analysis/SP 500 ESG Risk Ratings.xlsx", sheet = "SP 500 ESG Risk Ratings")

Sector_names <- unique(original_dataframe$Sector)
Sector_names <- Sector_names[-12]  #removing NA

regression_frame <- data.frame(
    "Sector Name" = character(0),
    "Multiple R" = numeric(0),
    "R Square" = numeric(0),
    "Adjusted R Square" = numeric(0),
    "Standard Error" = numeric(0),
    "Number of Observations" = numeric(0)
  )

for (sector_name in Sector_names) {

  rows_df <- original_dataframe[
  !is.na(original_dataframe$`Total ESG Risk score`) & 
  !is.na(original_dataframe$`Full Time Employees`) & 
  original_dataframe$`Full Time Employees` != 'N/A' & 
  original_dataframe$`Sector` == sector_name, 
  ]
  
  total_esg_colum_y <- rows_df$`Total ESG Risk score`
  full_time_values_x <- rows_df$`Full Time Employees`
   
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
    "Sector Name" = sector_name,
    "Multiple R" = multiple_r,
    "R Square" = multiple_r_squared,
    "Adjusted R Square" = adjusted_r_squared,
    "Standard Error" = rse,
    "Number of Observations" = num_observations
  )
  regression_frame <- rbind(regression_frame, Regression_Summary)
}

graphics.off()
#storing Regressoin summaries in json file for later usage
json_data <- toJSON(regression_frame, pretty = TRUE)
write(json_data, file = "regression_summary.json")


png("boxplot_output.png", width = 1000, height = 600)
par(mar = c(5, 9, 5, 2)) 
#visualizing the  r^2 values 
categories <- regression_frame$`Sector.Name`
values <- regression_frame$`R.Square`

barplot(
  values, 
  names.arg = categories, 
  main = "Sector R^2 values Bar Graph", 
  xlab = "R^2 Values", 
  col = "#F7879A", 
  border = "black",
  las = 2, 
  cex.names = 0.8,
  horiz = TRUE
)
legend("topright", 
       legend = "Sectors",  
       fill = "#F7879A",            
       bty = "n",                   
       cex = 0.8                   
)
graphics.off()