# -----  B1703 Pre-Tableau Cleaning | 21.03.2024 -----]

ATP2023Correct <- readxl::read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023CorrectMatches.xlsx")
ATP2023Incorrect <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023SeasonIncorrectMatches.csv")

# Load the dplyr package for data manipulation
library(dplyr)
library(lubridate)

# Exclude columns 29 to 36 from ATP2023Correct
ATP2023Correct <- select(ATP2023Correct, -c(29:36))
ATP2023Incorrect <- select(ATP2023Incorrect, -c())

# Assuming the date column in ATP2023Incorrect is named 'Date'
# Convert the date from 'YYYYMMDD' to 'YYYY-MM-DD'
ATP2023Incorrect$tourney_date <- ymd(ATP2023Incorrect$tourney_date)

# Renaming variables
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    W1stIn = w_1stIn,
    W1stWon = w_1stWon,
    W2ndWon = w_2ndWon,
    L1stIn = l_1stIn,
    L1stWon = l_1stWon,
    L2ndWon = l_2ndWon
  )



