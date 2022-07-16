# ====================================================================================================================================
#                                           Air Lab #2: Statistical practice with PM data                                            #
# ====================================================================================================================================
# Last Updated: 2/22/2022 by MD
# Learning outcomes: (1) Understand how to compute the mass concentration of PM using data obtained from VPC
#                    (2) Understand basic statistics (mean, median, quantiles and sample standaerd deviation)
#                    (3) Learn about t-test and how to use t-test in R
#                    (4) Visualize your data and statistical results in R
#
# Note: Before starting this lab section, you should already have loaded your HazeL data ("HazeL_data_raw.txt") 
#       to your computer.
#
# ------------------------------------------------------------------------------------------------------------------------------------
## Load libraries
library(ggplot2)
library(dplyr)

## Work with clean slate
rm(list = ls())

# Set your working directory
setwd('C:/Users/monad/OneDrive - Harvard University/ESE_6_2022/Air_Lab/') # Change to your working directory
# Check working directory
getwd()

# -----------------------------------------------------------------------------------------------------------------------
## Step 1: Read HazeL data
# First use source() to read read.hazel.beta.r file in your working directory to define the function read.hazel
source("read.hazel.beta.r")

# Read HazeL
HazeL_data_example <- read.hazel(datafile = '220208_230003_data.txt')

# View PM data
head(HazeL_data_example) # Note which time periods you are interested in for your t-test

# ----------------------------------------------------------------------------------------------------------------
## Step 2: Save PM data (X0.3um) for total particle count number greater than 0.3 micrometers; units: per 0.1 Liter
#          Save out only time periods you are interested in after viewing your data table.
#          Try to compare data sets with roughly the same number of data points.

# Place 1
harvard_yd <- HazeL_data_example[500:1500,] 
harvard_yd <- data.frame(harvard_yd['UTC_timestamp'], harvard_yd['X0.3um'])

# Place 2
harvard_sq <- HazeL_data_example[3000:4000,]
harvard_sq <- data.frame(harvard_sq['UTC_timestamp'], harvard_sq['X0.3um'])

# ----------------------------------------------------------------------------------------------------------------
## Step 3: Calculate statistics.
#          Are these distributions what you expect? Is there a difference between Place 1 & Place 2? 
#          Can you explain why?

## Place 1
# Look at summary statistics
summary(harvard_yd$X0.3um)
# Find mean
harvard_yd_mu <- mean(harvard_yd$X0.3um)
# Find standard deviation
harvard_yd_sd <- sd(harvard_yd$X0.3um)

## Repeat for Place 2
summary(harvard_sq$X0.3um)
harvard_sq_mu <- mean(harvard_sq$X0.3um)
harvard_sq_sd <- sd(harvard_sq$X0.3um)

# ----------------------------------------------------------------------------------------------------------------
## Step 4: Visualize data by plotting distributions
#          Below we want to plot (visualize) the > 0.3 um PM data distribution for our 2 places in the same figure;
#          First, we will plot the counts using a geom_hist & geom_density.
#          Then, we will plot the normal distribution, which mimics t-distribution when sample number (N) 
#          is large; To visualize the normal distribution, we should know 
#                       a. The sample mean [e.g., mean(harvard_yd$X0.3um)]
#                       b. Sample standard deviation [e.g., sd(harvard_yd$X0.3um)]
#          We can also use the function 'dnorm(x,df)' to plot the probability density against x, and the PM values
#          in different locations. Then, visually, we can see in one figure that between 2 places, how much is the 
#          difference between their mean PM values and what is the extent of
#          deviation of the PM measurements in each location.

## Plot counts
# Define bin width for histogram
binw = 25
ggplot() +
     # Plot histograms for places 1 and 2
     ## Place 1 - Harvard Square
     # Histogram
     geom_histogram(aes(x = X0.3um, fill = "#5ab4ac"), # define what's being plotted, color
                    color = "white", # create outline of each bar
                    alpha = 0.8, # change transparency
                    position = "stack", # plot on top of other graphs
                    binwidth = binw,
                    data = harvard_yd) +
     # Density plot
     geom_density(
          aes(x = X0.3um, y = binw * ..count..),
          color = "brown",
          lwd = 1, position = "stack",
          show.legend = F,
          data = harvard_yd) +
     
     ## Place 2 - Harvard Yard
     # Histogram
     geom_histogram(aes(x = X0.3um, fill = "#d8b365"),
                    color = "white",
                    alpha = 0.8,
                    position = "stack",
                    binwidth = binw,
                    data = harvard_sq) +
     # Density Plot
     geom_density(
          aes(x = X0.3um, y = binw * ..count..),
          color = "blue",
          lwd = 1,
          position = "stack",
          show.legend = F,
          data = harvard_sq) +
     
     # Style
     # labels
     labs(title = ">0.3um PM Count", x = "Total >0.3um PM (per 0.1 L)", y = "Count") + 
     # format legend
     guides(color = guide_legend(override.aes=list(size=1))) + 
     # change colors
     scale_fill_manual(name = "Place", values = c("#d8b365", "#5ab4ac"), labels = c("Harvard Square", "Harvard Yard")) +
     # scale_color_manual(values = c("#d8b365", "#5ab4ac")) +
     theme(legend.position=c(0.80, 0.85)) # reposition legend
     
## Plot normal distribution
# Place 1
x1norm <- seq(harvard_yd_mu - unique(range(harvard_yd_mu)/2), harvard_yd_mu + unique(range(harvard_yd_mu)/2), length.out = 1000) # Create range of x-vals to plot over
y1norm <- dnorm(x1norm, mean = harvard_yd_mu, sd = harvard_yd_sd) # Create normally distributed data from newly created x-vals
# Create new data.frame
harvard_ydnorm <- data.frame(x1norm, y1norm)

# Repeat for Place 2
x2norm <- seq(harvard_sq_mu - unique(range(harvard_sq_mu)/2), harvard_sq_mu + unique(range(harvard_sq_mu)/2), length.out = 1000) # Create range of x-vals to plot over
y2norm <- dnorm(x2norm, mean = harvard_sq_mu, sd = harvard_sq_sd) # Create normally distributed data from newly created x-vals
harvard_sqnorm <- data.frame(x2norm, y2norm)

# Reset bin width
binw = 75
ggplot() +
     # Place 1
     geom_histogram(aes(x = X0.3um, y = ..density.., fill = "1"), 
                    color = "white", alpha = 0.8,
                    position = "stack",
                    binwidth = binw,
                    data = harvard_yd) +
     geom_line(aes(x = x1norm, y = y1norm, color = "1"), 
               lwd = 1,
               show.legend = F,
               data = harvard_ydnorm)  +
     # Vertical line at mean
     geom_vline(aes(xintercept = harvard_yd_mu, color = "1"), lwd = 1, linetype = "dashed", , show.legend = F) +
     # Place 2
     geom_histogram(aes(x = X0.3um, y = ..density.., fill = "2"), 
                    color = "white", alpha = 0.8,
                    position = "stack",
                    binwidth = binw,
                    data = harvard_sq) +
     geom_line(aes(x = x2norm, y = y2norm, color = "2"),
               lwd = 1,
               show.legend = F,
               data = harvard_sqnorm)  +
     # Vertical line at mean
     geom_vline(aes(xintercept = harvard_sq_mu, color = "2"), lwd = 1, linetype = "dashed", show.legend = F) +
     
     # Style
     # labels
     labs(title = ">0.3um PM Probability Density", x = "Total >0.3um PM (per 0.1 L)", y = "Density") + 
     # format legend
     guides(color = guide_legend(override.aes=list(size=1))) + 
     # change colors
     scale_fill_brewer(name = "Place", palette = "Set1", labels = c("Harvard Yard", "Harvard Square")) +
     scale_color_brewer(palette = "Set1") +
     theme(legend.position=c(0.80, 0.85)) # re-position legend

# ----------------------------------------------------------------------------------------------------------------
## Step 5: Perform t-test
#          Hypothesis test intro and visualization: PM in Harvard Yard vs. PM in Harvard Square
#        - In this section, we are going to perform hypothesis test on the PM data we obtain in different locations 
#          (say, Harvard Yard & Harvard Square), to test if the PM data in Harvard Yard is either significantly 
#          greater, less, or different from the PM data in Harvard Square.
#        - We are going to use t-test, a statistical inference tool to do our hypothesis test.
#        - Because we are interested to know the mean values of Harvard Yard are significantly different than those in Harvard Square,
#          we set the alternative hypothesis to ask whether the mean for Place 1 (Harvard Yard) is different than for Place 2 (Harvard Square); 
#          hence, alternative='two.sided'

## Perform t-test
ttest_PM = t.test(harvard_yd$X0.3um, harvard_sq$X0.3um, alternative='two.sided', var.equal = FALSE, conf.level = 0.95)

## Look at results
ttest_PM


# ----------------------------------------------------------------------------------------------------------------
## Step 6: Plot t-test results
#          Here we plot our t-distributions for our t-tests, inputs are:
#                  a. Probability Density = dt(x,df), where x is a vector of t-values 
#                     (we can imagine that t-value is the normalized mean value of our data)
#                     where df is the Degree of Freedom (ttest_PM$parameter), which has 
#                     been calculated using the t.test() function
#                  b. Critical t-value for the t-test , which defines as crit_val = qt(.95,ttest_PM2.5$parameter )
#                  c. t-value, defined as ttest_PM$statistic which has been calculated in our t-test as well
#          Similar as what we do above, to perform our t-test visually, we want to compare the t-value and critical 
#          t-value -- if |t-value| is greater than |critical t-value|, we reject the null 

# Calculate & save distribution
x_t <- seq(-6,6, length.out = nrow(harvard_yd))
y_t <- dt(x_t, df = ttest_PM$parameter)

# Save as data frame
ttest <- data.frame(x_t = x_t, y_t = y_t)

# Save Critical Value
crit_val_hi <- qt(0.975, ttest_PM$parameter)
crit_val_lo <- qt(0.025, ttest_PM$parameter)

# Create shaded area
shade1 <- rbind(c(crit_val_hi,0), cbind(subset(ttest, x_t > crit_val_hi),0), c(Inf,0))
shade2 <- rbind(c(crit_val_lo,0), c(-Inf,0), cbind(subset(ttest, x_t < crit_val_lo),0))

# Plot
ggplot(ttest, aes(x = x_t, y = y_t)) +
     # Plot t-test line
     geom_line() +
     
     # Plot shaded area - where you would reject null hypothesis
     geom_segment(aes(x = crit_val_hi, y = 0, xend = crit_val_hi, yend = dnorm(crit_val_hi))) +
     geom_segment(aes(x = crit_val_lo, y = 0, xend = crit_val_lo, yend = dnorm(crit_val_lo))) +
     geom_polygon(data = shade1, aes(x=x_t, y=y_t, fill="red")) +
     geom_polygon(data = shade2, aes(x=x_t, y=y_t, fill="red")) +
     
     # Plot horizontal & vertical lines at 0 (for aesthetics)
     geom_hline(yintercept = 0) +
     geom_vline(xintercept = ttest_PM$null.value, col = "red", lwd = 0.5) + 

     # Plot where your critical value i.s
     geom_vline(xintercept = ttest_PM$statistic, col = "blue", lwd = 2) + # Plot normalized mean for Place 2
     
     # No legend
     guides(fill="none") +
     
     # Add labels
     labs(title = "T-Test Results", x = "t-value", y = "Density")


# Are the hypothesis test conclusions you get from the two figures consistent with the conclusions 
# you get by comparing the p-value with 5%?


# ----------------------------------------------------------------------------------------------------------------
## Step 7: Time-Series Data
ggplot(data = HazeL_data_example) +
     geom_line(aes(x = UTC_timestamp, y = X0.3um)) +
     labs(title = "Time-Series Data", x = "Time", y = "Total Particle Count > 0.3um [per 0.1L]")
