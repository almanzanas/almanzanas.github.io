library(datasets)
library(tidyverse)
data("mtcars")

# Visualizations in Base-R
View(mtcars)
## Histogram
hist(mtcars$mpg)
### Customizing histogram
hist(mtcars$mpg, col = "lightblue",
     main = "Histogram of Miles per Gallon (MPG) ",
     xlab = "MPG",
     ylab = "Frequencies",
     breaks = "FD" # Freedman-Diaconis formula for Breaks
     )

## Boxplots
boxplot(mtcars$mpg, col = "lightblue",
        main = "Boxplot of Miles per Gallon",
        ylab = "Miles per Gallon")

# Do cars with automatic transmission make less miles 
# per gallon than the manual transmission vehicles?
boxplot(mtcars$mpg[mtcars$am == 0],
        mtcars$mpg[mtcars$am == 1],
        col = c("lightpink", "lightblue"),
        names = c("Automatic", "Manual"),
        ylab = "MPG",
        main = "Types of Transmission and the Fuel Efficiency")

# Using ~ notation to comparison:
# How mpg responds to the changes in cylinders
boxplot(mpg ~ cyl, data = mtcars,
        col = c("gold", "lightpink", "lightblue"),
        main = "Miles per Gallon by Number of Cylinders",
        xlab = "Cylinders", ylab = "Miles Per Gallon")

## Density Plot
# density calculation
dprop <- density(mtcars$mpg)
# plot
plot(dprop, main = "Density Plot of MPG")
# Fill shape
polygon(dprop, col = "lightgreen")
###
dp0 <- density(mtcars$mpg[mtcars$am == 0])
dp1 <- density(mtcars$mpg[mtcars$am == 1])
plot (dp0, main = "Density Plot of MPG" )
lines (dp1,)
legend(x=35, y=0.1,
       c("Automatic", "Manual"),
       fill = c("lightpink", "lightblue"),
       col = c("lightpink", "lightblue"))
polygon(dp0, col = "lightpink")
polygon(dp1, col = "lightblue")

# Scatterplot
plot(x = mtcars$hp,
     y = mtcars$mpg,
     main = "Effect of Horsepower on Miles per Gallon (MPG)",
     xlab = "Horsepower", ylab = "MPG",
     col = "blue", pch=16)

# Bar plot
cyl.count <- table(mtcars$cyl)
barplot(cyl.count, col = "lightblue",
        main = "Number of Cars by Cylinder",
        xlab = "Cylinders", ylab = "Number of Cars")
###
vs.count <- table(mtcars$vs)
barplot(vs.count, main = "Cars with V-Engine vs Straight",
        col = "lightblue",
        horiz = TRUE,
        names = c("V-Shaped", "Straight"))

# Line plot
data("AirPassengers")
plot (AirPassengers, col = "lightblue", lwd = 3,
      main = "Evolution of the Number of Air Passengers")

# Multiple Variables
## Scatter plot three variables
plot (mtcars$hp, mtcars$mpg,
      col = mtcars$cyl, cex = mtcars$cyl/3,
      xlab = "HP", ylab = "MPG",
      main = "Effect of Horsepower and Number of cylinder on Fuel Efficiency")
legend (140, 32, sort(unique(mtcars$cyl)),
        col = sort(unique(mtcars$cyl)),
        cex = sort(unique(mtcars$cyl)/6),
        pch = 1, # Circles not filled
        horiz = TRUE, yjust = 0.4
        )

# Plots side by side
par(mfrow = c(1,2))    # plot grid
plot(x = mtcars$disp, y = mtcars$hp,
     col = "darkgreen", pch=16,
     main = "HP by Displacement")
hist(mtcars$disp, col="lightgreen",
     main = "Histogram of Displacement")
par(mfrow = c(1,1))    # Reset and check the plot grid
par("mfrow")






