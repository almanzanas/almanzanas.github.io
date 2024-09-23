install.packages('tidyverse')
library(tidyverse)

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

# Slicing

df %>% .[1:7, c(2:6)]

df %>% slice_min(age, prop = 0.1) %>% .[1:15,1:4]
df %>% slice_max(age, prop = 0.25) %>% .[10:20, 1:4]

  ## Sample

df %>% slice_sample(n=10, replace = FALSE) %>% .[, 1:4]

# Filtering

df %>% filter(age > 30) %>% .[10:20, 1:4]

df %>% filter(age > 30) %>%
  select(marital_status, education, age) %>%
  slice_sample(n=10)

df %>% distinct(sex)

# Grouping and Summarizing

df %>% group_by(workclass) %>% 
  summarise( age_avg = median(age) )

  ## Grouping with two variables returns a grouped_df object
(
  workclass_gp <- df %>%
    group_by(workclass, sex) %>%
    summarise(age_avg = mean(age) )
)
  ## Ungroup to return a tbl_df
(
  workclass_ungp <- df %>%
    group_by(workclass, sex) %>%
    summarise(age_avg = mean(age) ) %>%
    ungroup()
)
    ### ungrouped tibble requires less memory space

  ## Other useful summaries

    ### count
df %>% group_by(workclass) %>% summarise(n())

    ### sum not-NA
df %>% summarise(sum(!is.na(workclass)))

    ### first value in each group
df %>% group_by(workclass) %>% summarise(first(age))

    ### shows quantile 0.5
df %>% group_by(workclass) %>% summarise(quantile(age, 0.5))

    ### standard deviation
df %>% group_by(workclass %>% summarise(sd(age)))

    ### Accross applies a function to the selected columns
df %>% select(1, 3, 5, 11) %>%
  summarise(across(everything(), mean))

# Replacing and filling data

unique(df$occupation) # '?' is the missing value

for (i in colnames(df)) { # Count '?' on each column
  temp <- df[df[i] == '?',]
  print( paste ( i, nrow(temp) ) )
}

df.rep <- df %>%
  mutate(
    workclass = replace( workclass, workclass == "?", NA),
    native_country = replace (native_country, native_country == "?", NA),
    occupation = replace ( occupation, occupation == "?", NA)
  )

df.rep[ apply( df.rep, 1, function(x) any(is.na(x)) ), ]

  ## Filling NA with the next or previous valid value

df.rep %>% fill (native_country,
                 .direction = "down")

    ### For categorical, filling with the most frequent value

wrk.freq <- names( table( df$workclass)[which.max(table(df$workclass))])
occ.freq <- names( table( df$occupation)[which.max(table(df$occupation))])

#df.rep %>% replace_na(workclass, wrk.freq)
#df.rep %>% replace_na(occupation, occ.freq)

df.cln <- df.rep %>%
  replace_na( list( workclass = wrk.freq,
                    occupation = occ.freq ) )

df.cln <- df.cln %>% drop_na()

# Arranging Data

df.cln %>% arrange(native_country)
df.cln %>% arrange( desc(native_country) )

  ## Exploring

df.cln %>% group_by(education) %>%
  summarise( count = n(), avg_gain = mean(capital_gain - capital_loss)) %>%
  arrange( desc(avg_gain) )

# Creating new Variables

df.cln %>% separate( target, into = c("sign", "amt"), sep = "\\b")
#-- separate has 'remove=TRUE' by default
#-- unite() function is the complementary one
#-- creating a description identifier

df.cln %>% unite( sex, race, age, col="description",
                  sep = "_", remove = FALSE)

    ## Mutate function

#-- tax variable:
df.cln %>% mutate(total_gain = capital_gain - capital_loss,
                  tax = ifelse(total_gain >= 15000,
                               total_gain * 0.1, 0 ) ) %>% arrange (desc(tax))

#-- mutate() with recode() to mapping values
df.cln %>% mutate( over_under = recode(target, "<=50K"="under",
                                       ">50K"="over") ) %>%
  select(target, over_under)

#-- mutate() with cut() on age
df.cln %>%
  mutate(age_avg = mean(age),
         age_loc = cut(age, 
                       c(0, mean(age), max(age)), # from 0 to mean, mean to max
                       c("Lower_than_average", "Above_average")) ) %>%
  select(age, age_avg, age_loc)

# Reshaping Table






