# Load necessary libraries
library(dplyr)

# Load the dataset and select the required columns
vax_tweet <- read.csv("vax_tweet.csv") %>%
  select(user_description, text)

# View the first few rows to ensure it's loaded correctly
head(vax_tweet)

library(stringr)

# Clean the text column
vax_tweet$text <- vax_tweet$text %>%
  # Replace mentions with whitespace
  str_replace_all("@\\S+", " ") %>%
  # Replace newline characters with whitespace
  str_replace_all("\\n", " ") %>%
  # Replace hashtags with whitespace
  str_replace_all("#", " ")

# Print the cleaned text of the first 5 tweets
head(vax_tweet$text, 5)

# Load the syuzhet package

install.packages("slam", repos = "https://cran.rstudio.com", type = "source")
install.packages("textshape", repos = "https://cran.rstudio.com", type = "source")
install.packages("remotes")
remotes::install_github("mjockers/syuzhet")
library(syuzhet)

# Calculate sentiment scores for each tweet using get_sentiment
vax_tweet$sentiment_score <- get_sentiment(vax_tweet$text)

# Calculate and print the mean sentiment score
mean_score <- mean(vax_tweet$sentiment_score)
print(mean_score)

# Determine if the mean score is positive or negative
if (mean_score > 0) {
  print("The sentiment is positive.")
} else {
  print("The sentiment is negative.")
}

# Ensure doctor_tweets and non_doctor_tweets subsets are created properly

# Define the doctor pattern
doctor_pattern <- "(?i)\\b(phd|m\\.d\\.?|md)\\b"

# Add a new column to indicate if the user is a doctor or not
vax_tweet$is_doctor <- grepl(doctor_pattern, vax_tweet$user_description)

# Split data into doctor and non-doctor subsets
doctor_tweets <- vax_tweet[vax_tweet$is_doctor == TRUE, ]
non_doctor_tweets <- vax_tweet[vax_tweet$is_doctor == FALSE, ]

# Check if the subsets are created correctly
print(paste("Number of doctor tweets:", nrow(doctor_tweets)))
print(paste("Number of non-doctor tweets:", nrow(non_doctor_tweets)))

# Calculate the average sentiment score for doctors, handling empty subsets
if (nrow(doctor_tweets) > 0 && any(!is.na(doctor_tweets$sentiment_score))) {
  avg_sentiment_doctors <- mean(doctor_tweets$sentiment_score, na.rm = TRUE)
} else {
  avg_sentiment_doctors <- NA  # or set to 0 if you prefer
}

# Calculate the average sentiment score for non-doctors, handling empty subsets
if (nrow(non_doctor_tweets) > 0 && any(!is.na(non_doctor_tweets$sentiment_score))) {
  avg_sentiment_non_doctors <- mean(non_doctor_tweets$sentiment_score, na.rm = TRUE)
} else {
  avg_sentiment_non_doctors <- NA  # or set to 0 if you prefer
}

# Print the average sentiment scores
print(paste("Average sentiment score for doctors:", avg_sentiment_doctors))
print(paste("Average sentiment score for non-doctors:", avg_sentiment_non_doctors))

# Compare average sentiment scores, handling NA values
if (!is.na(avg_sentiment_doctors) && !is.na(avg_sentiment_non_doctors)) {
  if (avg_sentiment_doctors > avg_sentiment_non_doctors) {
    print("Doctors have a higher average sentiment score.")
  } else if (avg_sentiment_doctors < avg_sentiment_non_doctors) {
    print("Non-doctors have a higher average sentiment score.")
  } else {
    print("Doctors and non-doctors have the same average sentiment score.")
  }
} else {
  print("One or both groups have no valid sentiment scores.")
}

