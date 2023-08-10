


# Automated Heart Score Pipeline


# STEPS:

### 1) Gather relevant notes 
# 1a. Current note: Gather time stamp, note type, and provider type, for all 
#         notes related to the current encounter
# 1b. Current & Past Notes: Gather same data for all previous notes and then 
#         append to results of 1a
# 1c. Past notes only (needed for specifying previous vs. past encounter notes
#         for one-pass prompt heart score)

### 2) Run GPT analysis with respective prompts on each text from step 1
# 2a. History
# 2b. EKG
# 2c. Risks
# 2d. Age
# 2e. Troponin
# 2f. Calculate score with one pass prompt

### 3) Calculate heart score
# 3a. Extract bracketed answers from GPT responses
# 3b. Calculate  scores

### 4) Calculate price

### 5) Examining variability



# Load necessary libraries
library(tidyverse) # Includes dplyr and ggplot2, among others
library(stringr)
library(stringi)
library(glue) 
library(httr) #For GPT API queries
library(future) # For parallelization of GPT queries
library(purrr) # For parallelization of GPT queries
library(jsonlite) # For GPT API queries


################################################################################
################################################################################

### (1) Gather relevant notes 

################################################################################

# (1a.) Current note: Gather time stamp, note type, and provider type, for all 
#         notes related to the current encounter


# Read the csv spreadsheet into a dataframe called "allNotes"
allNotes <- read.csv('/Users/conradsafranek/Documents/DECILE/Automated\ Heart\ Score\ via\ LLM/simulatedPatientNotes_jdatExtractSample.csv',
                       header = TRUE)

# Convert 'LastEditedInstant' column to POSIXct (recognizable/readable date format)
allNotes$LastEditedInstant <- as.POSIXct(allNotes$LastEditedInstant, format="%Y-%m-%d %H:%M:%S")

# Order "allNotes" by PAT_ENC_ID and then by LastEditedInstant
allNotes <- arrange(allNotes, PAT_ENC_ID, LastEditedInstant)

# Add a new column "compiledText" to "allNotes"
allNotes <- mutate(allNotes,
                     compiledText = paste0("Last Edited: ", LastEditedInstant, 
                                           "\nType: ", Type, 
                                           "\nProvider: ", Provider, 
                                           "\n\n\n", TEXT))
# Preview result:
# cat(allNotes$compiledText[1])


# Create a new df called "currentVisitNotes" that is a subset of allNotes,
#    but filters only for the columns PAT_ENC_ID and PAT_ID
currentVisitNotes <- select(allNotes, PAT_ENC_ID, PAT_ID)

# Filter for when the value of PAT_ENC_ID includes the string "visCurrent"
currentVisitNotes <- filter(currentVisitNotes, grepl("visCurrent", PAT_ENC_ID))

# Filter down so that there's only unique rows (i.e. no duplicates)
currentVisitNotes <- distinct(currentVisitNotes)

# Prepare for loop:
currentVisitNotes$compiledAllText_current = ""
counter = 1

for (n in 1:length(currentVisitNotes$PAT_ENC_ID)){
  for (i in 1:length(allNotes$PAT_ENC_ID)){
    if (currentVisitNotes$PAT_ENC_ID[n] == allNotes$PAT_ENC_ID[i]){
      currentText = currentVisitNotes$compiledAllText_current[n]
      currentVisitNotes$compiledAllText_current[n] = paste0(currentText,
                                      "#####################################",
                                      "\ NOTE [#", counter, "] IN CURRENT ENCOUNTER",
                                      "\n\n\n", allNotes$compiledText[i], "\n\n\n")
      counter = counter + 1
    }
  }
  # Reset the counter:
  counter = 1
}

# Preview result:
# cat(currentVisitNotes$compiledAllText_current[1])

################################################################################

# 1b. Current & Past Notes: Gather same data for all previous notes and then 
#       append to results of 1a


# Create a new df called "allPtNotes" that will eventually contain all the relevant
#       pt notes from a pt's history leading up to the current encounter
allPtNotes <- select(allNotes, PAT_ENC_ID, PAT_ID, LastEditedInstant)

# Filter for when the value of PAT_ENC_ID includes the string "visCurrent"
allPtNotes <- filter(allPtNotes, grepl("visCurrent", PAT_ENC_ID))

# Get the row with the max LastEditedInstant for each unique PAT_ENC_ID
allPtNotes <- allPtNotes %>%
  group_by(PAT_ENC_ID) %>%
  slice_max(LastEditedInstant)

# Initialize an empty column for compiled text
allPtNotes$compiledAllText_currentAndPast = ""

counter = 1

for (n in 1:length(allPtNotes$PAT_ENC_ID)){
  for (i in 1:length(allNotes$PAT_ENC_ID)){
    # For the given visit, find all previous notes from the same PAT_ID where the
    #    LastEditedInstant was equal to or prior to the last encounter note in the current visit
    if (allPtNotes$PAT_ID[n] == allNotes$PAT_ID[i] & 
        allPtNotes$LastEditedInstant[n] >= allNotes$LastEditedInstant[i]){
      currentText = allPtNotes$compiledAllText_currentAndPast[n]
      allPtNotes$compiledAllText_currentAndPast[n] = paste0(currentText,
                                                  "#####################################",
                                                  "\nENCOUNTER NOTE #", counter, 
                                                  "\n\n\n", allNotes$compiledText[i], "\n\n\n")
      counter = counter + 1
    }
  }
  # Reset the counter for each patient
  counter = 1
}

# Preview result:
# cat(allPtNotes$compiledAllText_currentAndPast[1])



################################################################################

# 1c. Past notes only (needed for specifying previous vs. past encounter notes
#         for one-pass prompt heart score)

# Create a new df called "ptPastNotes" that will eventually contain all the relevant
#       pt notes from a pt's history leading up to the current encounter
ptPastNotes <- select(allNotes, PAT_ENC_ID, PAT_ID, LastEditedInstant)
ptPastNotes <- filter(ptPastNotes, grepl("visCurrent", PAT_ENC_ID))

# Get the row with the minimum LastEditedInstant for each unique PAT_ENC_ID
ptPastNotes <- ptPastNotes %>%
  group_by(PAT_ENC_ID) %>%
  slice_min(LastEditedInstant)

# Initialize an empty column for compiled text
ptPastNotes$compiledAllText_PastOnly = ""

counter = 1

for (n in 1:length(ptPastNotes$PAT_ENC_ID)){
  for (i in 1:length(allNotes$PAT_ENC_ID)){
    # For the given visit, find all previous notes from the same PAT_ID where
    #    the LastEditedInstant was equal to or prior to the current visit
    if (ptPastNotes$PAT_ID[n] == allNotes$PAT_ID[i] & 
        ptPastNotes$LastEditedInstant[n] > allNotes$LastEditedInstant[i]){
      currentText = ptPastNotes$compiledAllText_PastOnly[n]
      ptPastNotes$compiledAllText_PastOnly[n] = paste0(currentText,
                                                            "#####################################",
                                                            "\nENCOUNTER NOTE #", counter, 
                                                            "\n\n\n", allNotes$compiledText[i], "\n\n\n")
      counter = counter + 1
    }
  }
  # Reset the counter for each patient
  counter = 1
}

# Preview result:
# cat(ptPastNotes$compiledAllText_PastOnly[1])



################################################################################
################################################################################

### 2) Run GPT analysis with respective prompts on each text from step 1

#Load prompts:
prompts <- suppressWarnings(read.csv('/Users/conradsafranek/Documents/DECILE/Automated\ Heart\ Score\ via\ LLM/prompt_iteration_for_GPT.csv',
                                     header = TRUE))

#Prepare overall df for results
results <- left_join(allPtNotes, select(currentVisitNotes, PAT_ENC_ID, compiledAllText_current), by = "PAT_ENC_ID")
results <- left_join(results, select(ptPastNotes, PAT_ENC_ID, compiledAllText_PastOnly), by = "PAT_ENC_ID")


### Run GPT query

#load preferred model with settings (uncomment out desired line, check tpm & rpm)
# RPM (requests per min) and TPM (tokens per min) is information used later to calculate a max rate at which you can parallelize the GPT requests. See: https://platform.openai.com/docs/guides/rate-limits 
# Retrieve the rate limits from your "Rate Limits" accounts page once you've logged in on OpenAI: https://platform.openai.com/account/rate-limits
MODEL <- "gpt-3.5-turbo-16k"; rpm <- 3500; tpm <- 180000; 
     input_price_per_1k_tokens <- 0.003;  output_price_per_1k_tokens <- 0.004; # gpt-3.5-turbo-16k
#MODEL <- "gpt-4-8k"; rpm <- 3500; tpm <- 90000; 
    # input_price_per_1k_tokens <- 0.03;  output_price_per_1k_tokens <- 0.06; # gpt-4-8k
#MODEL <- "gpt-4-32k"

#Instructional phrase that defines the role of the model:
instructionalPhrase = prompts$Prompt[6]

#Set model temperature
Temperature = 0.5

# Set this value of n_future based on the results from calculations below (first run this code for a subset of your data, then use the result of "n_future calculated as: " below to set n_future)
#   !!!!  WILL NEED TO UPDATE once we switch to gpt-4  !!!!
n_future = 3; plan(multisession, workers = n_future)
source("/Users/conradsafranek/Documents/DECILE/Automated\ Heart\ Score\ via\ LLM/helperFunctionHEART_gptAutoQuery.R")

# !!!! Potentially consider a different parallel package ("do parallel" -David)

# DOCUMENTATION ON TOKENS: https://platform.openai.com/docs/guides/gpt/managing-tokens



################################################################################
# REPEAT many fold to study variability:

# N: desired number of repeats
N = 25

# Repeat each row N times
results <- results[rep(1:nrow(results), each = N), ]

# Modify value of PAT_ENC_ID to have a unique value for each replication
results <- results %>%
  group_by_all() %>%
  mutate(PAT_ENC_ID = paste0(PAT_ENC_ID, "-", 1:N)) %>%
  ungroup()

################################################################################



# 2a. History

# Fetch relevant prompt for history
gptQuery_generalString <- prompts$Prompt[1]

#Prepare GPT query with encounter notes (only include current visit notes):
encounterNotes_list <- results$compiledAllText_current
results$historyQ <- glue(gptQuery_generalString, EncounterNotes = encounterNotes_list)
# cat(results$historyQ[1]) #Preview question

#Run GPT queries
n_future = 4; plan(multisession, workers = n_future)
results <- results %>% mutate(hist_gptResponse = map_chr(historyQ, call_chat_gpt))
# cat(results$hist_gptResponse[1]) #Preview result

################################################################################

# 2b. EKG

# Fetch relevant prompt for history
gptQuery_generalString <- prompts$Prompt[2]

#Prepare GPT query with encounter notes (only include current visit notes):
encounterNotes_list <- results$compiledAllText_current
results$ekgQ <- glue(gptQuery_generalString, EncounterNotes = encounterNotes_list)
# cat(results$ekgQ[1]) #Preview question

### Run GPT queries
n_future = 4; plan(multisession, workers = n_future)
results <- results %>% mutate(ekg_gptResponse = map_chr(ekgQ, call_chat_gpt))

# cat(results$ekg_gptResponse[1]) #Preview result

################################################################################

# 2c. Risks

# Fetch relevant prompt for history
gptQuery_generalString <- prompts$Prompt[3]

#Prepare GPT query with encounter notes (only include current AND PAST visit notes):
encounterNotes_list <- results$compiledAllText_currentAndPast
results$risksQ <- glue(gptQuery_generalString, EncounterNotes = encounterNotes_list)
# cat(results$risksQ[1]) # Preview question

# Run GPT queries
n_future = 1; plan(multisession, workers = n_future)
results <- results %>% mutate(risks_gptResponse = map_chr(risksQ, call_chat_gpt))
# cat(results$risks_gptResponse[1]) #Preview result

################################################################################

# 2d. Age


# Fetch relevant prompt for history
gptQuery_generalString <- prompts$Prompt[4]

#Prepare GPT query with encounter notes (only include current AND PAST visit notes):
encounterNotes_list <- results$compiledAllText_current
results$ageQ <- glue(gptQuery_generalString, EncounterNotes = encounterNotes_list)
# cat(results$ageQ[1]) #Preview question


# Run GPT queries
n_future = 4; plan(multisession, workers = n_future)
results <- results %>% mutate(age_gptResponse = map_chr(ageQ, call_chat_gpt))
# cat(results$age_gptResponse[1]) #Preview result


################################################################################

# 2e. Troponin

# Fetch relevant prompt for history
gptQuery_generalString <- prompts$Prompt[5]

#Prepare GPT query with encounter notes (only include current AND PAST visit notes):
encounterNotes_list <- results$compiledAllText_current
results$tropQ <- glue(gptQuery_generalString, EncounterNotes = encounterNotes_list)
# cat(results$tropQ[1]) #Preview question


# Run GPT queries
n_future = 4; plan(multisession, workers = n_future)
results <- results %>% mutate(trop_gptResponse = map_chr(tropQ, call_chat_gpt))
# cat(results$trop_gptResponse[1]) #Preview result


################################################################################

# 2f. Calculate score with one pass prompt

# Fetch relevant prompt for history
gptQuery_generalString <- prompts$Prompt[7]

#Prepare GPT query with encounter notes (only include current AND PAST visit notes):
currentEncounterNotes <- results$compiledAllText_current
pastEncounterNotes <- results$compiledAllText_PastOnly
results$onePassQ <- glue(gptQuery_generalString, CurrentEncounterNotes = currentEncounterNotes, PastEncounterNotes = pastEncounterNotes)
# cat(results$tropQ[1]) #Preview question


# Run GPT queries
n_future = 1; plan(multisession, workers = n_future)
results <- results %>% mutate(onepass_gptResponse = map_chr(onePassQ, call_chat_gpt))
# cat(results$onepass_gptResponse[1]) #Preview result


################################################################################

# Checking speeds for future parallization

# Fine tuning the speed of parallel calls (to not exceed rate limit):
# Percent rate limit (rather than exactly hitting our rate limit)
percent_RateLimit = 0.75
n_future_hist       <- calculate_n_future(DF = results, TPM = tpm, RPM = rpm, GPT_QUERY = "historyQ", GPT_RESPONSE = "hist_gptResponse",     PERCENT_RATELIMIT = percent_RateLimit)
n_future_ekg        <- calculate_n_future(DF = results, TPM = tpm, RPM = rpm, GPT_QUERY = "ekgQ",     GPT_RESPONSE = "ekg_gptResponse",      PERCENT_RATELIMIT = percent_RateLimit)
n_future_risks      <- calculate_n_future(DF = results, TPM = tpm, RPM = rpm, GPT_QUERY = "risksQ",   GPT_RESPONSE = "risks_gptResponse",    PERCENT_RATELIMIT = percent_RateLimit)
n_future_age        <- calculate_n_future(DF = results, TPM = tpm, RPM = rpm, GPT_QUERY = "ageQ",     GPT_RESPONSE = "age_gptResponse",      PERCENT_RATELIMIT = percent_RateLimit)
n_future_trop       <- calculate_n_future(DF = results, TPM = tpm, RPM = rpm, GPT_QUERY = "tropQ",    GPT_RESPONSE = "trop_gptResponse",     PERCENT_RATELIMIT = percent_RateLimit)
n_future_onepass    <- calculate_n_future(DF = results, TPM = tpm, RPM = rpm, GPT_QUERY = "onePassQ", GPT_RESPONSE = "onepass_gptResponse",  PERCENT_RATELIMIT = percent_RateLimit)



################################################################################
################################################################################

### (3) Calculate heart score

################################################################################

# 3a. Extract bracketed answers from GPT responses


# Function to extract ONLY the last bracketed phrase from a string
extract_bracketed_info <- function(input_string) {
  # Extract all bracketed phrases using regex
  bracketed_info <- str_extract_all(input_string, "\\[[^\\]]+\\]")[[1]]
  
  # Check if any bracketed phrases were found
  if (length(bracketed_info) > 0) {
    # Return the last bracketed phrase
    return(tail(bracketed_info, 1))
  } else {
    # If no bracketed phrases were found, return NA
    return("ERROR - No bracketed phrase")
  }
}

# Function to extract the number from the parentheses within the bracketed information
extract_number_from_brackets <- function(input_string) {
  # Get the bracketed information using the previous function
  bracketed_info <- extract_bracketed_info(input_string)
  
  # Check if the bracketed information contains "[Not enough information to determine..."
  if (grepl("\\[Not enough information to determine", bracketed_info)) {
    return("ERROR - Not enough info")
  }
  
  # Extract the number from within the parentheses using regex
  number_within_parentheses <- str_match(bracketed_info, "\\((\\d+)\\)")[, 2]
  
  # If the number is found, return it; otherwise, return "ERROR"
  if (length(number_within_parentheses) > 0) {
    return(number_within_parentheses)
  } else {
    return("ERROR - No number")
  }
}

# TESTS:
# extract_bracketed_info(results$hist_gptResponse[1])
# extract_bracketed_info(results$ekg_gptResponse[1])
# extract_bracketed_info(results$risks_gptResponse[1])
# extract_bracketed_info(results$age_gptResponse[1])
# extract_bracketed_info(results$trop_gptResponse[1])
# extract_number_from_brackets(results$hist_gptResponse[1])
# extract_number_from_brackets(results$ekg_gptResponse[1])
# extract_number_from_brackets(results$risks_gptResponse[1])
# extract_number_from_brackets(results$age_gptResponse[1])
# extract_number_from_brackets(results$trop_gptResponse[1])

################################################################################

# 3b. Calculate  score

#Calulate individual subscores:
results$histScore <- sapply(results$hist_gptResponse, extract_number_from_brackets)
results$ekgScore <- sapply(results$ekg_gptResponse, extract_number_from_brackets)
results$risksScore <- sapply(results$risks_gptResponse, extract_number_from_brackets)
results$ageScore <- sapply(results$age_gptResponse, extract_number_from_brackets)
results$tropScore <- sapply(results$trop_gptResponse, extract_number_from_brackets)


#Calculate overall heart score from summed subscores:
results <- results %>%
  rowwise() %>%
  mutate(
    heartSCORE = {
      cols <- c(histScore, ekgScore, risksScore, ageScore, tropScore)
      if (all(!is.na(suppressWarnings(as.numeric(cols))))) {
        sum(as.numeric(cols), na.rm = TRUE)
      } else {
        NA_real_
      }
    }
  )


#Calculate heart score from one-pass prompt
   #  Function to extract numerical answer from bracket response for onepassHeartScore
extract_points <- function(string) {
  num <- as.numeric(str_extract(string, "\\d+"))
  ifelse(is.na(num), NA, num)
}
results <- results %>%
  mutate(onepassScoreBracketAnswer = extract_bracketed_info(onepass_gptResponse),
         onepassScore = extract_points(onepassScoreBracketAnswer))






################################################################################
################################################################################


# CHECK consistency:
# results$hist_gptResponse[1] == results$hist_gptResponse[2]
# results$hist_gptResponse[3] == results$hist_gptResponse[4]
# results$ekg_gptResponse[1] == results$ekg_gptResponse[2]
# results$ekg_gptResponse[3] == results$ekg_gptResponse[4]
# results$age_gptResponse[1] == results$age_gptResponse[2]
# results$age_gptResponse[3] == results$age_gptResponse[4]
# results$risks_gptResponse[1] == results$risks_gptResponse[2]
# results$risks_gptResponse[3] == results$risks_gptResponse[4]

################################################################################
################################################################################
#resultsV2 <- results
#write.csv(results, file = "/Users/conradsafranek/Desktop/resultsV5_new_Aug8.csv")
#resultsV5 <- results
#resultsV4 <- results
#results <- read_csv("/Users/conradsafranek/Documents/DECILE/Automated\ Heart\ Score\ via\ LLM/Results/resultsV4_Aug7.csv")

#results <- resultsV5
#results <- resultsV4

################################################################################
################################################################################

# 4) Calculate price and statistics

################################################################################

# PRICE

MODEL <- "gpt-3.5-turbo-16k"; rpm <- 3500; tpm <- 180000; 
#input_price_per_1k_tokens <- 0.003;  output_price_per_1k_tokens <- 0.004; # gpt-3.5-turbo-16k
#MODEL <- "gpt-4-8k"; rpm <- 3500; tpm <- 90000; 
 input_price_per_1k_tokens <- 0.03;  output_price_per_1k_tokens <- 0.06; # gpt-4-8k
#MODEL <- "gpt-4-32k"


# Calculate price via subscores:
GPT_QUERIES = c("historyQ", "ekgQ", "risksQ", "ageQ", "tropQ"); 
GPT_RESPONSES = c("hist_gptResponse", "ekg_gptResponse", "risks_gptResponse", "age_gptResponse", "trop_gptResponse")
total_subscore_price <- calculate_price(DF = results, GPT_QUERIES = GPT_QUERIES, GPT_RESPONSES = GPT_RESPONSES, input_price_per_1k = input_price_per_1k_tokens, output_price_per_1k = output_price_per_1k_tokens)
print(paste("Total price for subscore calculations across", nrow(results), 
            " rows is: [ $", round(ceiling(total_subscore_price * 100) / 100, 2), "]"))
print(paste("price per individual row: [ $", round(total_subscore_price/nrow(results), 4), "]"))

# Calculate price via subscores:
GPT_QUERIES = c("onePassQ"); 
GPT_RESPONSES = c("onepass_gptResponse")
total_onepass_price <- calculate_price(DF = results, GPT_QUERIES = GPT_QUERIES, GPT_RESPONSES = GPT_RESPONSES, input_price_per_1k = input_price_per_1k_tokens, output_price_per_1k = output_price_per_1k_tokens)
print(paste("Total price for onepass calculations across", nrow(results), 
            " rows is: [ $", round(ceiling(total_onepass_price * 100) / 100, 2), "]"))
print(paste("price per individual row: [ $", round(total_onepass_price/nrow(results), 4), "]"))

# GRAND TOTAL
print(paste("GRAND TOTAL: [ $", round(ceiling((total_onepass_price + total_subscore_price) * 100) / 100, 2), "]"))


################################################################################

# WORD COUNTING

avg_word_count <- function(column) {
  column %>%
    # Replace any non-word or non-space character with nothing.
    str_replace_all("[^\\w\\s]", "") %>%
    # Split the string into words based on spaces.
    str_split(" ") %>%
    # Get the length of the resulting list of words.
    sapply(length) %>%
    # Compute the average.
    mean(na.rm = TRUE)
}

# For ptPastNotes
avg_past <- avg_word_count(ptPastNotes$compiledAllText_PastOnly)

# For currentVisitNotes
avg_current <- avg_word_count(currentVisitNotes$compiledAllText_current)

print(paste("Average word count for ptPastNotes: ", round(avg_past, 2)))
print(paste("Average word count for currentVisitNotes: ", round(avg_current, 2)))




################################################################################
################################################################################

# 5) Examine variability of results:

################################################################################


# BAR GRAPH - Count non-numeric values for each score by PAT_ID

# Function to determine if value is non-numeric
is_non_numeric <- function(x) {return(is.na(x) | !is.na(as.character(x)) & is.na(as.numeric(as.character(x))))}

# Count non-numerical results for each subscore
non_numeric_counts <- results %>% summarise(
  histScore = sum(is.na(as.numeric(histScore))),
  ekgScore = sum(is.na(as.numeric(ekgScore))),
  risksScore = sum(is.na(as.numeric(risksScore))),
  ageScore = sum(is.na(as.numeric(ageScore))),
  tropScore = sum(is.na(as.numeric(tropScore))),
  heartSCORE = sum(is.na(as.numeric(heartSCORE))),
  onepassScore = sum(is.na(as.numeric(onepassScore)))
)

# Count NAs and non-numeric values for each score by PAT_ID
na_nonnum_counts_by_pat <- results %>%
  gather(key="score", value="value", histScore, ekgScore, risksScore, ageScore, tropScore, heartSCORE, onepassScore) %>%
  group_by(score, PAT_ID) %>%
  summarize(count_invalid = sum(is_non_numeric(value)))

# Setting order
ordered_names <- names(non_numeric_counts)
na_nonnum_counts_by_pat$score <- factor(na_nonnum_counts_by_pat$score, levels = ordered_names)

# Create a stacked bar chart
ggplot(na_nonnum_counts_by_pat, aes(x=score, y=count_invalid, fill=PAT_ID)) + 
  geom_bar(stat="identity", position="stack") + #add color outline:  color="gray60", size=0.3
  scale_fill_brewer(palette="Pastel1") +
  labs(title = paste("Count of NA Results by Score by Patient   [N repeats per pt =", N, "]"),
       x="Score Type", 
       y="Count of Non-numerical and NA Results") +
  theme_minimal() +
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=12)) 


################################################################################
# BOX PLOT - Score Distribution

data_long_values <- gather(results, key="score", value="value", histScore, ekgScore, risksScore, ageScore, tropScore, heartSCORE, onepassScore)

# Ensure the data is numeric
data_long_values$value <- as.numeric(data_long_values$value)

# Setting order
data_long_values$score <- factor(data_long_values$score, levels = ordered_names)


# Calculate counts of numeric values for each score by PAT_ID
numeric_counts <- data_long_values %>%
  filter(!is.na(value) & !is.character(value)) %>%
  group_by(score, PAT_ID) %>%
  tally(name = "count")

# Create boxplot with adjusted outline and count labels
ggplot(data_long_values, aes(x=score, y=value, fill=PAT_ID)) + 
  geom_boxplot(color="gray45", lwd=0.3) + 
  geom_text(data=numeric_counts, aes(y=Inf, label=paste0("n=", count)), 
            position=position_dodge(width=0.75), vjust=2, size=3) +  # Place count label above each box
  scale_fill_brewer(palette="Pastel1") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure y-axis breaks are integers
  labs(title="Score Distribution by Patient", 
       x="Score Type", 
       y="Value") +
  theme_minimal() +
  theme(legend.position="top")

################################################################################
# JITTER PLOT - Score Distribution

ggplot(data_long_values, aes(x=score, y=value, color=PAT_ID)) + 
  geom_jitter(position=position_jitterdodge(jitter.width = 0.4, dodge.width = 0.75), 
              size=1.5, alpha=1) +
  geom_text(data=numeric_counts, aes(y=Inf, label=paste0("n=", count)), 
            position=position_dodge(width=0.75), vjust=1, size=3.5) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values = c("#D98880", "#5499C7")) +  # Earthy pink and blue
  labs(title="Score Jitter Plot by Patient", 
       x="Score Type", 
       y="Value") +
  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank())



################################################################################
# MODE SCORE

# Convert scores in results to numeric or NA
cols_to_convert <- c("histScore", "ekgScore", "risksScore", "ageScore", "tropScore", "heartSCORE", "onepassScore")
resultsForMODE <- results
resultsForMODE[cols_to_convert] <- lapply(results[cols_to_convert], function(x) {
  num_values <- as.numeric(x)
  num_values[is.na(num_values)] <- NA
  return(num_values)
})

# Create the long-format data frame
new_data_long <- resultsForMODE %>%
  select(PAT_ID, histScore, ekgScore, risksScore, ageScore, tropScore, heartSCORE, onepassScore) %>%
  pivot_longer(cols = -PAT_ID, names_to = "score", values_to = "value")

# Get mode
get_mode <- function(v) {
  uniqv <- unique(v)
  mode_val <- uniqv[which.max(tabulate(match(v, uniqv)))]
  return(mode_val)
}

mode_data <- new_data_long %>%
  group_by(score, PAT_ID) %>%
  summarise(mode = get_mode(value)) %>%
  ungroup()

# Adjust the factor levels for PAT_ID to make sure pt2 appears below pt1
mode_data$PAT_ID <- factor(mode_data$PAT_ID, levels = c("pt2", "pt1"))
mode_levels <- sort(unique(mode_data$mode[!is.na(mode_data$mode)]))

# Separate the color mapping for NA and other values
mode_data$fill_color <- ifelse(is.na(mode_data$mode), "lightgray", as.factor(mode_data$mode))

# Create the new color mapping, excluding NA
colors <- scales::seq_gradient_pal("white", "#8FBC8F")(seq(0, 1, length.out = length(mode_levels)))
colors <- c(colors, "lightgray")

# Create heatmap with the new ordering and color mapping
ggplot(mode_data, aes(x = factor(score, levels = cols_to_convert), y = PAT_ID, fill = fill_color)) + 
  geom_tile(color = "gray40") +
  geom_text(aes(label = ifelse(is.na(mode), "NA", mode)), color = "black", size = 4) +
  scale_fill_manual(values = colors) +
  labs(title = "Mode of Subscores by Patient", 
       x = "Score Type", 
       y = "Patient ID") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())



