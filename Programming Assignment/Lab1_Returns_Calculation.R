# DataCamp / Coursera
# Introduction to Computational Finance and Financial Econometrics
# Lab 1 Returns Calculation




# Load the monthly Starbucks return data

# Assign the URL to the CSV file
data_url <- "http://assets.datacamp.com/course/compfin/sbuxPrices.csv"

# Load the data frame using read.csv
sbux_df <- read.csv(data_url, header = T, stringsAsFactors = F)




# Get a feel for the data

# Check the structure of 'sbux_df'
str(sbux_df)

# Check the first and last part of 'sbux_df'
head (sbux_df)
tail (sbux_df)

# Get the class of the Date column of 'sbux_df'
class(sbux_df$Date)

names(sbux_df)




# Extract the price data

# You can use square brackets to extract data from the sbux_df data frame like this: 
sbux_df[rows, columns]

sbux_df[1:5, "Adj.Close"]
sbux_df[1:5, 2]
sbux_df$Adj.Close[1:5]

# Assign to the variable closing_prices all the adjusted closing prices while preserving the dimension information.
closing_prices = sbux_df[,2, drop = F]

# Print
closing_prices




# Find indices associated with the dates 3/1/1994 and 3/1/1995

# Useful package for time-series http://www.rdocumentation.org/packages/xts
# The which() function returns the indices for which a condition is TRUE. 
# For example: which(sbux_df$Date == "3/1/1994") returns the position of the date 3/1/1994.
# Which indicates in this case the row number in the sbux_df data frame.
which()

# Find indices associated with the dates 3/1/1994 and 3/1/1995
index_1 <- which(sbux_df$Date == "3/1/1994")
index_2 <- which(sbux_df$Date == "3/1/1995")

# Extract prices between 3/1/1994 and 3/1/1995
some_prices <- sbux_df[index_1:index_2, "Adj.Close"]




# Subset directly on dates

# Create a new data frame that contains the price data with the dates as the row names
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) <- sbux_df$Date
head(sbux_prices_df)

# With Dates as rownames, you can subset directly on the dates.
# Find indices associated with the dates 3/1/1994 and 3/1/1995.
price_1 <- sbux_prices_df["3/1/1994",1]
price_2 <- sbux_prices_df["3/1/1995",1]




# Plot the price data
# Let us make a better plot by adding the following arguments to the plot function: 

type="l"                                 # specifies a line plot, 
col="blue"                               # indicates that the line should be blue, 
lwd=2                                    # doubles the line thickness, 
ylab="Adjusted close"                    #adds a y-axis label 
main="Monthly closing price of SBUX"     # adds a title.


plot(sbux_df$Adj.Close,
     type = "l", 
     col = "blue",
     lwd = 2,
     ylab = "Adjusted close",
     main = "Monthly closing price of SBUX")
legend(x='topleft',legend='SBUX', lty=1, lwd=2, col='blue')




# Calculate simple returns

# Denote n the number of time periods
n <- nrow(sbux_prices_df)

# Calculate simple returns
# The fact that R is vectorized makes that relatively easy. 
# In case you would like to calculate the price difference over time, you can use:
sbux_ret <- (sbux_prices_df[2:n,1] - sbux_prices_df[1:(n-1),1])/sbux_prices_df[1:(n-1),1]




# Add dates to simple return vector
names(sbux_ret) <- sbux_df [2:n, 1]




# Compute continuously compounded 1-month returns
# Formula: ln(Pt)-ln(Pt-1)
sbux_ccret <- log(sbux_prices_df[2:n, 1]) - log(sbux_prices_df[1:(n-1), 1])

# Assign names to the continuously compounded 1-month returns
names(sbux_ccret) <- sbux_df[2:n,1]




# Compare simple and continuously compounded returns

# You can use the cbind() function to paste the two vectors that contain both types of returns next to each other in a matrix.


# Compare the simple and cc returns
head(cbind(sbux_ccret, sbux_ret))




# Graphically compare the simple and continuously compounded returns

# Plot the returns on the same graph
plot(sbux_ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Returns on SBUX")

# Add horizontal line at zero
abline(h=0)

# Add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))

# Add the continuously compounded returns
lines(sbux_ccret,
      col = "red",
      lwd = 2)




# Calculate growth of $1 invested in SBUX

# Would it have been a good idea to invest in the SBUX stock over the period in our data set? 
# In case you invested $1 in SBUX on 3/31/1993 (the first day in sbux_df), 
# how much would that dollar be worth on 3/3/2008 (the last day in sbux_df)? 
# What was the evolution of the value of that dollar over time?

# R can help you to quickly come up with an answer to these questions. 
# Remember that when you use simple returns, the total return over a period can be obtained by taking the cumulative product of the gross returns.
# R has a handy cumprod() function that calculates that cumulative product.
cumprod()

# Assign to the variable sbux_gret the gross returns (which is the simple return + 1).
# Assign to sbux_fv a vector that contains the future values of the $1 invested in SBUX for every time period.
# Have a look at the plot. Would it have been a good idea to invest in SBUX?

# Compute gross returns
sbux_gret <- 1 + sbux_ret

# Compute future values
sbux_fv <- cumprod(sbux_gret)

# Plot the evolution of the $1 invested in SBUX as a function of time
plot(sbux_fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")




# Question 1
# Use the data in sbux. What is the simple monthly return between the end of December 2004 and the end of January 2005?
price <- c(31.18, 27.00, 25.91, 25.83, 24.76, 27.40, 25.83, 26.27, 24.51, 25.05, 28.28, 30.45, 30.51)

# Create date vector
sbux_df2 <- data.frame(
  date=rep(as.Date(seq(as.Date("2004/12/1"), as.Date("2005/12/1"), by = "1 month ")),1),
  sbux=price
)

q1 <- ((sbux_df2[[2,2]] - sbux_df2[[1,2]])/ sbux_df2[[1,2]])  

# OR

q1 <- ((sbux_df2[sbux_df2$date == "2005-01-01",2] - sbux_df2[sbux_df2$date == "2004-12-01",2])/ sbux_df2[sbux_df2$date == "2004-12-01",2])




# Question 2  
# Compute one continuously compounded Starbucks return  
# What is the continuously compounded monthly return between December 2004 and January 2005?

q2 <- log(sbux_df2[sbux_df2$date == "2005-01-01",2]) - log(sbux_df2[sbux_df2$date == "2004-12-01",2])




# Question 3
# Monthly compounding
# Assume that all twelve months have the same return as the simple monthly return between the end of December 2004 and the end of January 2005. 
# What would be the annual return with monthly compounding in that case?

q3 <- ((1+q1)^12)-1




# Question 4
# Simple annual Starbucks return
# Use the data in sbux and compute the actual simple annual return between December 2004 and December 2005.

q4 <- ((sbux_df2[sbux_df2$date == "2005-12-01",2] - sbux_df2[sbux_df2$date == "2004-12-01",2])/ sbux_df2[sbux_df2$date == "2004-12-01",2])




# Question 5
# Use the data sbux and compute the actual annual continuously compounded return between December 2004 and December 2005.

q5 <- log(sbux_df2[sbux_df2$date == "2005-12-01",2]) - log(sbux_df2[sbux_df2$date == "2004-12-01",2])