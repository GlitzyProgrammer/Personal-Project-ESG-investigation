library(readxl)

# Read the Excel file
orignal_dataframe <- read_excel("C:/Users/ljwil/OneDrive/Desktop/Data Analytics/Personal Projects/ESG Analysis/SP 500 ESG Risk Ratings.xlsx", 
                                sheet = "SP 500 ESG Risk Ratings")

# Filter rows where columns are not NA and 'Full Time Employees' is not 'N/A'
rows_to_add <- orignal_dataframe[!is.na(orignal_dataframe$`Total ESG Risk score`) & 
                                !is.na(orignal_dataframe$`Full Time Employees`) & 
                                !is.na(orignal_dataframe$Sector) & 
                                orignal_dataframe$`Full Time Employees` != 'N/A', ]

# Reset row names
rownames(rows_to_add) <- NULL

# Assign columns to variables
total_esg_column_y <- rows_to_add$`Total ESG Risk score`
total_worker_x <- rows_to_add$`Full Time Employees`
sector_column <- rows_to_add$Sector

print(sector_column)