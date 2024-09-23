library(data.table)
library(stringr)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
df <- read.csv(url, header=F, strip.white = T, stringsAsFactors = F)
head(df)

# Assigning column names:
colnames(df) <- c("age", "workclass", "final_weight", "education",
                  "education_num", "marital_status", "occupation",
                  "relationship", "race", "sex", "capital_gain",
                  "capital_loss", "hours_week", "native_country", "target")

# Dimension of DF
dim(df)

# Data frame to data.table
dt <- as.data.table(df)

  ## Summarize
dt[, .N, by = sex] 
# .N is a special variable
# it counts the number of rows in each group
dt[, mean(age), by = sex]

    ### Education years barplot
educ.yr <- dt[, mean(education_num), by = education]
educ.yr <- educ.yr[order(V1)]
barplot(educ.yr$V1, names.arg = educ.yr$education, col = "lightblue",
        main = "Average years of education by grade level")


# Missing Values
df.na <- df[which(is.na(df)),]
dim(df.na)
remove(df.na)
unique(df["workclass"])
unique(df["occupation"])
  ## The '?' character will be treated as NA
for (i in names(df)) {
  df[[i]][df[[i]] == '?'] <- NA
}
# Also could be do it with 'gsub()' function

  ## Extracting NA values
sum(is.na(df)) # returns 4262
# If NA values is at most 5% of the data, it s safe to remove it.
sapply(df, function (x) mean(is.na(x)) * 100)
    ### Treatment: replacing NA with the most common value when NA > 5%
most.f <- names(table(df$workclass)[which.max(table(df$workclass))])
df$workclass[is.na(df$workclass)] <- most.f
most.f <- names(table(df$occupation)[which.max(table(df$occupation))])
df$workclass[is.na(df$occupation)] <- most.f
remove(most.f)
# For numeric variables we can replace NA with the mean

# Now only are NA in native_country representing 1.8% then let's remove them
df <- na.omit(df)

    ### Education mean by workclass
dt <- as.data.table(df)
dt[, mean(education_num), by = workclass][order(V1, decreasing = TRUE)]

# New variables
unique(df['sex'])
df$sex_bin <- ifelse(df$sex == 'Female', 1, 0)
# other possibility is to convert sex from character to Factor
df$education <- as.factor(df$education)
unique(df$education)
df$education <- ordered(df$education, levels = c("Preschool", "1st-4th",
                                                 "5th-6th","7th-8th",
                                                 "9th", "10th", "11th",
                                                 "12th", "HS-grad",
                                                 "Some-college", "Assoc-acdm",
                                                 "Assoc-voc",
                                                 "Bachelors", "Masters" ,
                                                 "Doctorate") )

    ### Adding taxes
df$total_gain <- df$capital_gain - df$capital_loss
df$tax <- ifelse(df$total_gain >= 15000, df$total_gain * 0.1, 0)





