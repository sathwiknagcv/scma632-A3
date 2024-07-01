# Load necessary libraries
library(data.table)
library(AER)
install.packages('AER')
# Load the data
data <- fread('C:\\Users\\Admin\\Downloads\\Bootcamp Assignement\\NSSO68.csv', na.strings = "")

# Subset data for state 'KA'
df <- data[, .(MPCE_URP, Whether_owns_any_land, hhdsz, Religion, Social_Group, Regular_salary_earner)]

# Check for missing values
print(sum(is.na(df$MPCE_URP)))
print(sum(is.na(df$Whether_owns_any_land)))
print(sum(is.na(df$hhdsz)))
print(sum(is.na(df$Religion)))
print(sum(is.na(df$Social_Group)))
print(sum(is.na(df$Regular_salary_earner)))

# Drop rows with any remaining NaN values
df <- df[complete.cases(df)]

# Check for missing values after dropping
print(sum(is.na(df$MPCE_URP)))
print(sum(is.na(df$Whether_owns_any_land)))
print(sum(is.na(df$hhdsz)))
print(sum(is.na(df$Religion)))
print(sum(is.na(df$Social_Group)))
print(sum(is.na(df$Regular_salary_earner)))

# Convert the target variable to binary based on the specified condition
df[, MPCE_URP := ifelse(MPCE_URP < 380, 0, 1)]

# Define the independent variables (X) and the dependent variable (y)
X <- df[, .(Whether_owns_any_land, hhdsz, Religion, Social_Group, Regular_salary_earner)]
y <- df$MPCE_URP

# Add a constant term for the intercept
X <- cbind(1, X)  # Adding a column of ones for the intercept

# Fit the Tobit model
tobit_model <- tobit(MPCE_URP ~ Whether_owns_any_land + hhdsz + Religion + Social_Group + Regular_salary_earner, left = 0, right = Inf, data = df)

# Print the summary of the model
summary(tobit_model)

