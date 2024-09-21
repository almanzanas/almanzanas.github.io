# https://github.com/PacktPublishing/Data-Wrangling-with-R

# Loading the dataset
txhousing <- read.csv("data/txhousing.csv")
# Let's view the content
View(txhousing)
# Checking the column type
sapply(txhousing, class)
str(txhouses)
# Creating a copy to preserve the original dataset
txhouses <- txhousing

## Data types
txhouses$Date <- as.Date(txhouses$Date, format = "%Y-%m-%d")

## Fast summary
summary(txhouses)

txhouses[is.na(txhouses)]
sum ( is.na(txhouses) )

# plots
txhouses <- as.data.frame(txhouses)
## grid for 8 plots
par(mfrow = c(4,2))
## hist
for (var in colnames(txhouses[3:8])) {
  hist(txhouses[,var],
       col = "blue",
       main = paste("Histogram of", var),
       border = "white" )
}
## Corr
cor.var <- c("Sales", "Median.Price", "Months.Inventory", "Dollar.Volume")
pairs(txhouses[, cor.var])

# OUTLIERS
# https://statsandr.com/blog/outliers-detection-in-r/
## IQR
par(mfrow = c(1,1))
boxplot(txhouses$Median.Price)
(out <- boxplot.stats(txhouses$Median.Price)$out )
in.out <- which(txhouses$Median.Price %in% c(out))
txhouses[in.out,]
