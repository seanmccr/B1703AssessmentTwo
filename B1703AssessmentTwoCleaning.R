# -----  B1703 Assessment 2 | Pre-Tableau Cleaning | 21.03.2024 -----]

# ----- 1. Loading in datasets -----
ATP2023Correct2 <- readxl::read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023CorrectMatches.xlsx")
ATP2023Incorrect2 <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023SeasonIncorrectMatches.csv")

##### 1.2. Libraries -----
# Load the dplyr package for data manipulation
library(dplyr)
library(lubridate)

# ----- 2. Changing variable types/names/removing variables -----
# Converting date formats
ATP2023Incorrect$tourney_date <- ymd(ATP2023Incorrect$tourney_date)

# Renaming 
ATP2023Incorrect <- ATP2023Incorrect %>%
     rename(
         WRank = winner_rank,
         LRank = loser_rank
      )

ATP2023Incorrect2 <- ATP2023Incorrect2 %>%
  rename(
    LSeed = LRank
  )

ATP2023Incorrect <- ATP2023Incorrect2

# Remove columns
ATP2023Incorrect <- select(ATP2023Incorrect, -c(1))

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
   rename(
    Tournament = tourney_name,
    Surface = surface,
    Date = tourney_date
)

# Remove columns
ATP2023Incorrect <- select(ATP2023Incorrect, -c(3,4,6,7,15))
ATP2023Incorrect <- select(ATP2023Incorrect, -c(5,12))

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    Score = score,
    LAge = loser_age
    )

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    BestOf = best_of,
    Round = round, 
    Minutes = minutes,
    LCountry = loser_ioc,
    WCountry = winner_ioc,
    WAge = winner_age,
    WHeight = winner_ht,
    WHand = winner_hand
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    Winner = winner_name,
    Loser = loser_name,
    LHand = loser_hand,
    LHeight = loser_ht,
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    WRankPts = winner_rank_points,
    LRankPts = loser_rank_points,
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    WAce = w_ace,
    WBPSave = w_bpSaved,
    WBPFaced = w_bpFaced,
    LAce = l_ace,
    LBPSave = l_bpSaved,
    LBPFaced = l_bpFaced
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
     WDoubleFault = w_df,
     LDoubleFault = l_df,
     WTotalSVPts = w_svpt,
     WSVGames = w_SvGms,
     LTotalSVPts = l_svpt,
     LSVGames = l_SvGms
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
   rename(
     W1stIn = w_1stIn,
     W1stWon = w_1stWon,
     W2ndWon = w_2ndWon,
     L1stIn = l_1stIn,
     L1stWon = l_1stWon,
     L2ndWon = l_2ndWon
)

ATP2023Incorrect$Date <- ymd(ATP2023Incorrect$Date)

# New Dataset as renamed
MASTERATP2023Incorrect <- ATP2023Incorrect

# Separating score into separate variables for better applicability
# 1. Split 'Score' by space to separate the sets
score_splits <- strsplit(ATP2023Incorrect$Score, " ")
View(score_splits)
score_splits <- strsplit(ATP2023Incorrect$Score, " ")
 
# Pre-make columns for new variables
num_matches <- length(ATP2023Incorrect$Score)
new_columns <- matrix(NA, nrow = num_matches, ncol = 10)
colnames(new_columns) <- c('W1', 'L1', 'W2', 'L2', 'W3', 'L3', 'W4', 'L4', 'W5', 'L5')
 
# 2. Separating set scores
for (i in 1:num_matches) {
   sets <- score_splits[[i]]
   for (j in 1:length(sets)) {
     set_score <- sets[j]
        # Check for any retirements
         if(set_score == "RET") {
             # Put NA in when retirements happen
           new_columns[i, (2*j-1):(2*j)] <- NA
                 next
               }
         # Split set score by '-', sperate winner from losers scores
           games <- unlist(strsplit(set_score, "-"))
           # Tiebreaks 
             games <- gsub("\\((.*?)\\)", "", games)
             # Put scores into new variables
               new_columns[i, (2*j-1):(2*j)] <- games[1:2]
             }
 }
 


new_df <- as.data.frame(new_columns)
DRAFTATP2023Incorrect <- cbind(ATP2023Incorrect, new_df)

# Remove variables, rename again
ATP2023Final <- select(ATP2023Final, -c(14))
ATP2023Final <- DRAFTATP2023Incorrect


# NA Count for each variable
na_count_ATP2023Final <- ATP2023Final %>% 
  summarise_all(~sum(is.na(.)))
print(na_count_ATP2023Final)

# Rename 
ATP2023FinalDraft <- ATP2023Final

ATP2023Final <- ATP2023Final %>%
  filter(!grepl("Davis Cup", Tournament))

# Datasets featuring NA values only to assess circumstance i.e NAs in many can be explained by Walkovers so can be ignored
ATP2023Final_NA_WAce <- filter(ATP2023FinalDraft, is.na(WAce))

ATP2023Final_NA_WRank <- filter(ATP2023Final, is.na(WRank))

ATP2023Final_NA_LRank <- filter(ATP2023Final, is.na(LRank))

ATP2023Final_NA_LAge <- filter(ATP2023Final, is.na(LAge))

ATP2023Final_NA_Minutes <- filter(ATP2023Final, is.na(Minutes))

ATP2023Final_NA_WHeight <- filter(ATP2023Final, is.na(WHeight))

ATP2023Final_NA_LHeight <- filter(ATP2023Final, is.na(LHeight))

# Updating age and Height
ATP2023Final <- ATP2023Final %>%
  mutate(LAge = ifelse(Loser == "Liam Krall", 21.7, LAge),
         LHeight = ifelse(Loser == "Liam Krall", 191, LHeight))
# Again
ATP2023Final <- ATP2023Final %>%
  mutate(LAge = ifelse(Loser == "Manas Dhamne", 15.1, LAge),
         LHeight = ifelse(Loser == "Manas Dhamne", 188, LHeight))

# Inputting match length (via ATP website)
ATP2023Final <- ATP2023FinalDraft %>%
  mutate(Minutes = ifelse(row_number() == 38, 73 , Minutes),
         Minutes = ifelse(row_number() == 35, 91, Minutes),
         Minutes = ifelse(row_number() == 27, 75, Minutes),
         Minutes = ifelse(row_number() == 24, 143, Minutes),
         Minutes = ifelse(row_number() == 15, 130, Minutes),
         Minutes = ifelse(row_number() == 45, 91, Minutes))

# Input heights via ATP website
ATP2023Final <- ATP2023Final %>%
  mutate(LHeight = ifelse(Loser == "Ben Shelton", 193, LHeight),
         LHeight = ifelse(Loser == "Gijs Brouwer", 191, LHeight),
         LHeight = ifelse(Loser == "Alexander Shevchenko", 185, LHeight),
         LHeight = ifelse(Loser == "Luca Van Assche", 178, LHeight),
         LHeight = ifelse(Loser == "Rinky Hijikata", 178, LHeight),
         LHeight = ifelse(Loser == "Stefanos Sakellaridis", 196, LHeight),
         LHeight = ifelse(Loser == "Flavio Cobolli", 183, LHeight),
         LHeight = ifelse(Loser == "Dalibor Svrcina", 178, LHeight),
         LHeight = ifelse(Loser == "Juncheng Shang", 180, LHeight),
         LHeight = ifelse(Loser == "Brandon Holt", 185, LHeight),
         LHeight = ifelse(Loser == "Luciano Darderi", 183, LHeight),
         LHeight = ifelse(Loser == "Matija Pecotic", 185, LHeight),
         LHeight = ifelse(Loser == "Camilo Ugo Carabelli", 185, LHeight),
         LHeight = ifelse(Loser == "Alexander Ritschard", 193, LHeight),
         LHeight = ifelse(Loser == "Jacopo Berrettini", 193, LHeight),
         LHeight = ifelse(Loser == "Riccardo Bonadio", 180, LHeight),
         LHeight = ifelse(Loser == "Aleksandar Kovacevic", 183, LHeight),
         LHeight = ifelse(Loser == "Francesco Passaro", 180, LHeight),
         LHeight = ifelse(Loser == "Ivan Gakhov", 191, LHeight),
         LHeight = ifelse(Loser == "Abedallah Shelbayh", 180, LHeight),
         LHeight = ifelse(Loser == "Pablo Llamas Ruiz", 188, LHeight),
         LHeight = ifelse(Loser == "Genaro Alberto Olivieri", 175, LHeight),
         LHeight = ifelse(Loser == "Ryan Peniston", 180, LHeight),
         LHeight = ifelse(Loser == "Jan Choinski", 196, LHeight),
         LHeight = ifelse(Loser == "Filip Misolic", 180, LHeight),
         LHeight = ifelse(Loser == "Alex Michelsen", 193, LHeight),
         LHeight = ifelse(Loser == "Ethan Quinn", 191, LHeight),
         LHeight = ifelse(Loser == "Dino Prizmic", 188, LHeight),
         LHeight = ifelse(Loser == "Sho Shimabukuro", 180, LHeight),
         LHeight = ifelse(Loser == "Gabriel Diallo", 203, LHeight),
         LHeight = ifelse(Loser == "Omni Kumar", 173, LHeight),
         LHeight = ifelse(Loser == "Yu Hsiou Hsu", 178, LHeight),
         LHeight = ifelse(Loser == "Titouan Droguet", 191, LHeight),
         LHeight = ifelse(Loser == "Jakub Mensik", 193, LHeight),
         LHeight = ifelse(Loser == "Philip Sekulic", 191, LHeight),
         LHeight = ifelse(Loser == "Alibek Kachmazov", 185, LHeight),
         LHeight = ifelse(Loser == "Beibit Zhukayev", 196, LHeight),
         LHeight = ifelse(Loser == "Terence Atmane", 193, LHeight),
         LHeight = ifelse(Loser == "Bu Yunchaokete", 185, LHeight),
         LHeight = ifelse(Loser == "Shintaro Mochizuki", 175, LHeight),
         LHeight = ifelse(Loser == "Giovanni Mpetshi Perricard", 203, LHeight),
         LHeight = ifelse(Loser == "Mark Lajal", 191, LHeight),
         LHeight = ifelse(Loser == "Billy Harris", 193, LHeight)
         )
   
# Again
ATP2023Final <- ATP2023Final %>%
  mutate(LHeight = ifelse(Loser == "Kiranpal Pannu", 185, LHeight),
         LHeight = ifelse(Loser == "Mattia Bellucci", 175, LHeight),
         LHeight = ifelse(Loser == "Oleksii Krutykh", 185, LHeight),
         LHeight = ifelse(Loser == "Alex Rybakov", 185, LHeight),
         LHeight = ifelse(Loser == "Clement Chidekh", 180, LHeight),
         LHeight = ifelse(Loser == "Mateus Alves", 193, LHeight),
         LHeight = ifelse(Loser == "Nick Chappell", 178, LHeight),
         LHeight = ifelse(Loser == "Rodrigo Pacheco Mendez", 188, LHeight),
         LHeight = ifelse(Loser == "Henrique Rocha", 180, LHeight),
         LHeight = ifelse(Loser == "Younes Lalami Laaroussi", 188, LHeight),
         LHeight = ifelse(Loser == "Valentin Vacherot", 193, LHeight),
         LHeight = ifelse(Loser == "Daniel Rincon", 185, LHeight),
         LHeight = ifelse(Loser == "Max Hans Rehberg", 183, LHeight),
         LHeight = ifelse(Loser == "Martin Landaluce", 191, LHeight),
         LHeight = ifelse(Loser == "Alvaro Lopez San Martin", 178, LHeight),
         LHeight = ifelse(Loser == "Eduardo Nava", 180, LHeight),
         LHeight = ifelse(Loser == "George Loffhagen", 188, LHeight),
         LHeight = ifelse(Loser == "Arthur Fery", 175, LHeight),
         LHeight = ifelse(Loser == "Eliot Spizzirri", 183, LHeight),
         LHeight = ifelse(Loser == "Yunseong Chung", 178, LHeight),
         LHeight = ifelse(Loser == "Andres Martin", 183, LHeight),
         LHeight = ifelse(Loser == "Jesper De Jong", 180, LHeight),
         LHeight = ifelse(Loser == "Skander Mansouri", 193, LHeight),
         LHeight = ifelse(Loser == "Alexis Galarneau", 180, LHeight),
         LHeight = ifelse(Loser == "Strong Kirchheimer", 185, LHeight),
         LHeight = ifelse(Loser == "Nicolas Moreno De Alboran", 185, LHeight),
         LHeight = ifelse(Loser == "Learner Tien", 180, LHeight),
         LHeight = ifelse(Loser == "Tao Mu", 180, LHeight),
         LHeight = ifelse(Loser == "Jie Cui", 183, LHeight),
         LHeight = ifelse(Loser == "Ye Cong Mo", 183, LHeight),
         LHeight = ifelse(Loser == "Rigele Te", 188, LHeight),
         LHeight = ifelse(Loser == "Denis Yevseyev", 185, LHeight),
         LHeight = ifelse(Loser == "Karl Friberg", 191, LHeight),
         LHeight = ifelse(Loser == "Alexander Blockx", 191, LHeight),
         LHeight = ifelse(Loser == "Matteo Martineau", 183, LHeight)
  )

# Again
ATP2023Final <- ATP2023Final %>%
  mutate(LHeight = ifelse(Loser == "Kiranpal Pannu", 185, LHeight))

# Replacing NA values in match length with 0 for Walkovers
ATP2023Final <- ATP2023Final %>%
  mutate(Minutes = ifelse(is.na(Minutes), 0, Minutes))


# Load necessary libraries
library(dplyr)
library(lubridate)


# Update ATP2023Correct with the tournament start date for each match
ATP2023Correct <- ATP2023Correct %>%
  group_by(Tournament) %>%
  mutate(Date = min(Date)) %>%
  ungroup()

# Summarize the variable types in ATP2023Final
str(ATP2023Final)
str(ATP2023Correct)

# Extract unique Surface values from ATP2023Final dataset
unique_surfaces_final <- unique(ATP2023Final$Surface)

# Print the unique surfaces from ATP2023Final
print(unique_surfaces_final)

# Extract unique Surface values from ATP2023Correct dataset
unique_surfaces_correct <- unique(ATP2023Correct$Surface)

# Print the unique surfaces from ATP2023Correct
print(unique_surfaces_correct)

unique_series <- unique(ATP2023Correct$Series)
print(unique_series)


# Ensure all relevant columns are of the correct type (if not already converted)
ATP2023Correct <- ATP2023Correct %>%
  mutate(across(c(W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank), as.numeric))

# Prepare the data from ATP2023Correct with only the required variables for joining and the 'Series' variable
ATP2023Correct_Series <- ATP2023Correct %>%
  select(Series, W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank)

# Merge 'Series' into ATP2023Final based on the unique combination
# Note: ATP2023Final should also have W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank correctly formatted as numerical types before this step
ATP2023Final_withSeries <- merge(ATP2023Final, ATP2023Correct_Series, 
                                 by = c("W1", "L1", "W2", "L2", "W3", "L3", "W4", "L4", "W5", "L5", "WRank", "LRank"),
                                 all.x = TRUE)

# Optional: check if merge was performed correctly   
head(ATP2023Final_withSeries)


# Reordering Dataset
ATP2023Final_reordered <- ATP2023Final_withSeries %>%
  select(Tournament, Series, Surface, Date, Winner, WHand, WHeight, WCountry, WAge, Loser, LHand, LHeight, 
         LCountry, LAge, BestOf, Round, Minutes, WAce, WDoubleFault, WTotalSVPts, W1stIn, W1stWon, W2ndWon, 
         WSVGames, WBPSave, WBPFaced, LAce, LDoubleFault, LTotalSVPts, L1stIn, L1stWon, L2ndWon, LSVGames, LBPSave, 
         LBPFaced, WRankPts, LRankPts, WRank, LRank, W1, L1, W2, L2, W3, L3, W4, L4, W5, L5) 

# Checking for NA Values
na_count_ATP2023Final_reordered <- ATP2023Final_reordered %>% 
  summarise_all(~sum(is.na(.)))
print(na_count_ATP2023Final_reordered)


ATP2023Finalreordered_NA_Series <- filter(ATP2023Final_reordered, is.na(Series))



# Matching NA values to other tournament names to fill them
ATP2023Final_reordered <- ATP2023Final_reordered %>%
  group_by(Tournament) %>%
  mutate(
    Series = ifelse(is.na(Series),
                    first(Series[!is.na(Series)]), # Take the first non-NA Series value within the same tournament
                    Series) # Keeps original value
  ) %>%
  ungroup() # Ensure to ungroup at the end for subsequent operations not to be affected



ATP2023Final_reordered <- ATP2023Final_reordered %>%
  mutate(Series = case_when(
    Tournament %in% c("Lyon", "Geneva") ~ "ATP250",
    TRUE ~ Series # keeps the original Series value if condition is not met
  ))


# Remove 'Tournament' entries labelled 'NextGen Finals'
ATP2023Final_reordered <- ATP2023Final_reordered %>%
  filter(!grepl("NextGen Finals", Tournament))

ATPFinal <- ATP2023Final_reordered

ATP2023PointsDraft <- ATP2023Final

# Replace NA values in the 'Tournament' column with 'ATP500' for United Cup entries
ATP2023PointsDraft <- ATP2023PointsDraft %>%
  mutate(Series = if_else(Tournament == "United Cup" & is.na(Series), "ATP500", Series))


# Filtering the dataset to include only 'United Cup' entries
ATP2023PointsDraft_UnitedCup <- filter(ATP2023PointsDraft, Tournament == "United Cup")

# Filtering the dataset to exclude 'United Cup' entries
ATP2023PointsDraft_OtherGames <- filter(ATP2023PointsDraft, Tournament != "United Cup")







# Define a function for regular tournament point calculation (excluding United Cup)
getTournamentPoints <- function(series, round, winnerFlag = FALSE) {
  # Define the points structure including Finals (F) and Winner (W) points
  points <- list(
    'Grand Slam' = c(R128 = 10, R64 = 35, R32 = 45, R16 = 90, QF = 180, SF = 360, F = 480, W = 800),
    'Masters 1000' = c(R128 = 10, R64 = 15, R32 = 20, R16 = 45, QF = 90, SF = 180, F = 240, W = 400),
    'ATP500' = c(R64 = 0, R32 = 20, R16 = 25, QF = 45, SF = 90, F = 120, W = 200),
    'ATP250' = c(R64 = 0, R32 = 10, R16 = 10, QF = 25, SF = 45, F = 60, W = 100),
    'Masters Cup' = c(RR = 200, SF = 400, F = 0, W = 500)
  )
  
  # Adjust the logic to handle 'Final' round for both players and 'Winner' points
  seriesPoints <- points[[series]]
  if (is.null(seriesPoints)) {
    return(NA)  # Return NA if the Series is not found
  }
  
  finalPoints <- ifelse(round == 'F', seriesPoints['F'], 0)
  
  winnerBonus <- ifelse(winnerFlag & round == 'F', seriesPoints['W'], 0) +
    ifelse(winnerFlag & round != 'F', seriesPoints[round], 0)
  
  return(finalPoints + winnerBonus)
}

# Adjust the ATP2023PointsDraft dataset processing to use the updated function
# Assuming 'Winner' and 'Loser' variables exist in ATP2023PointsDraft to determine the bonus points for the winner

ATP2023PointsDraft_OtherGames <- ATP2023PointsDraft_OtherGames %>%
  rowwise() %>%
  mutate(
    WPoints = getTournamentPoints(Series, Round, TRUE),
    LPoints = if_else(Round == "F", getTournamentPoints(Series, Round, FALSE), 0)
  ) %>%
  ungroup()

# The rest of your original `B1703AssessmentTwoCleaning.R` code follows here,
# ensuring it properly integrates with the updated points calculation logic.


calculateUnitedCupPoints <- function(Round, LRank) {
  points <- case_when(
    Round == "RR" & LRank >= 1 & LRank <= 10 ~ 80,
    Round == "RR" & LRank >= 11 & LRank <= 20 ~ 65,
    Round == "RR" & LRank >= 21 & LRank <= 30 ~ 55,
    Round == "RR" & LRank >= 31 & LRank <= 50 ~ 40,
    Round == "RR" & LRank >= 51 & LRank <= 100 ~ 35,
    Round == "RR" & LRank >= 101 & LRank <= 250 ~ 25,
    Round == "RR" & LRank > 250 ~ 20,
    Round == "SF" & LRank >= 1 & LRank <= 10 ~ 130,
    Round == "SF" & LRank >= 11 & LRank <= 20 ~ 105,
    Round == "SF" & LRank >= 21 & LRank <= 30 ~ 90,
    Round == "SF" & LRank >= 31 & LRank <= 50 ~ 60,
    Round == "SF" & LRank >= 51 & LRank <= 100 ~ 40,
    Round == "SF" & LRank >= 101 & LRank <= 250 ~ 35,
    Round == "SF" & LRank > 250 ~ 25,
    Round == "F" & LRank >= 1 & LRank <= 10 ~ 180,
    Round == "F" & LRank >= 11 & LRank <= 20 ~ 140,
    Round == "F" & LRank >= 21 & LRank <= 30 ~ 120,
    Round == "F" & LRank >= 31 & LRank <= 50 ~ 90,
    Round == "F" & LRank >= 51 & LRank <= 100 ~ 60,
    Round == "F" & LRank >= 101 & LRank <= 250 ~ 40,
    Round == "F" & LRank > 250 ~ 35,
    TRUE ~ 0 # Default to 0 points for scenarios not defined above
  )
  
  return(points)
}

# Usage within the 'ATP2023PointsDraft_UnitedCup' dataset;
# Modifying the dataset to include the updated points for Winners while
# ensuring Losers receive 0 points by default
ATP2023PointsDraft_UnitedCup <- ATP2023PointsDraft_UnitedCup %>%
  mutate(
    WPoints = calculateUnitedCupPoints(Round, LRank), # Allocate points only to Winners
    LPoints = 0 # Explicitly set Losers' points to 0 for all entries
  )




DRAFTATP2023Incorrect <- DRAFTATP2023Incorrect %>%
  rename(
    WRank = winner_rank,
    LRank = loser_rank,
  )


DRAFTATP2023Incorrect <- DRAFTATP2023Incorrect %>%
  mutate(across(c(W1, L1, W2, L2, W3, L3, W4, L4, W5, L5), ~as.numeric(gsub("[^0-9.]", "", .))))

DRAFTATP2023Incorrect <- DRAFTATP2023Incorrect %>%
  mutate(across(c(W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank), as.numeric))

# Select necessary columns from DRAFTATP2023Incorrect 
DRAFTATP2023Incorrect_selected <- DRAFTATP2023Incorrect %>%
  select(WRank, LRank, Date, W1, L1, W2, L2, W3, L3, W4, L4, W5, L5, WSeed, LSeed)

# Merge the selected columns into ATP2023PointsDraft_OtherGames
ATP2023PointsDraft_UnitedCup2 <- ATP2023PointsDraft_UnitedCup %>%
  left_join(DRAFTATP2023Incorrect_selected, 
            by = c("WRank", "LRank", "Date", "W1", "L1", "W2", "L2", "W3", "L3", "W4", "L4", "W5", "L5"))

# Merge the selected columns into ATP2023PointsDraft_OtherGames
ATP2023PointsDraft_OtherGames_updated2 <- ATP2023PointsDraft_OtherGames %>%
  left_join(DRAFTATP2023Incorrect_selected, 
            by = c("WRank", "LRank", "Date", "W1", "L1", "W2", "L2", "W3", "L3", "W4", "L4", "W5", "L5"))



ATP2023PointsDraft_OtherGames <- ATP2023PointsDraft_OtherGames_updated2

ATP2023PointsDraft_UnitedCup <- ATP2023PointsDraft_UnitedCup2


# Combining the datasets
ATP2023FinalClean <- bind_rows(ATP2023PointsDraft_OtherGames, ATP2023PointsDraft_UnitedCup)

















roundOrder <- c("R128", "R64", "R32", "R16", "QF", "SF", "F")


detectByes <- function(data) {
  # Append a unique identifier for each match combining Tournament and Date
  data <- mutate(data, TournamentID = paste(Tournament, Date))
  
  # Identify unique players
  players <- unique(c(data$Winner, data$Loser))
  
  # Initialize a Bye data frame
  byeData <- data.frame(Player=character(), TournamentID=character(), Round=character(), stringsAsFactors=FALSE)
  
  # Loop through each player
  for (player in players) {
    # Filter matches involving the player
    playerMatches <- filter(data, Winner == player | Loser == player)
    
    # Identify unique tournaments the player participated in
    tournaments <- unique(playerMatches$TournamentID)
    
    # Check for Byes in each tournament
    for (tournament in tournaments) {
      # Filter matches in this specific tournament
      tournamentMatches <- filter(playerMatches, TournamentID == tournament)
      
      # Within your detectByes function, just after obtaining tournamentMatches
      tournamentRounds <- unique(tournamentMatches$Round)
      startingRound <- min(match(tournamentRounds, roundOrder))
      
      # Now, check against the actual startingRound of the tournament
      firstRound <- min(match(tournamentMatches$Round, roundOrder))
      
      # Adjusted condition
      if (!is.na(firstRound) && firstRound > startingRound) {
        previousRound <- roundOrder[firstRound - 1]
        byeEntry <- data.frame(Player=player, TournamentID=tournament, Round=previousRound)
        byeData <- rbind(byeData, byeEntry)
      }
    }
  }
  
  # Return the Bye data
  return(byeData)
}

# Detect Byes in the ATP2023PointsDraft_OtherGames dataset
byeData <- detectByes(ATP2023PointsDraft_OtherGames)
































# Save ATP2023Incorrect dataset as a new .csv file
write.csv(ATP2023Final, "/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023Final.csv", row.names = FALSE)



