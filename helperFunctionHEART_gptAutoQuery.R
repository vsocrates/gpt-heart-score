# library(httr)
# library(jsonlite)
# library(purrr)
# library(dplyr)
# library(furrr)
# library(stringr)
# library(stringi)

# PLAN:
# Plan for multicore computing. n_future is the number of cores to be used.
# Ensure this is within your RPM and TPM limits.
#n_future <- 10
#plan(multisession, workers = n_future)

OPENAI_API_KEY <- Sys.getenv("OPENAI_KEY")

#These values are set in the main document:
#MODEL <- "gpt-3.5-turbo"
#instructionalPhrase = "You are a helpful assistant."

chat_completion <- function(message) {
  url <- paste0("https://api.openai.com/v1/chat/completions")
  
  headers <- add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", OPENAI_API_KEY)
  )
  
  body <- list(
    model = MODEL,
    messages = list(
      list(role = "system", content = instructionalPhrase),
      list(role = "user", content = message)
    ),
    temperature = Temperature
  )
  
  response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
  content <- content(response, "parsed")
  
  return(content$choices[[1]]$message$content)
}


call_chat_gpt <- function(gpt_request) {
  tryCatch({
    chat_completion(gpt_request)
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

# call_chat_gpt <- function(gpt_request) {
#   chat_completion(gpt_request)
# }


########################################

calculate_n_future <- function(DF, TPM, RPM, GPT_QUERY, GPT_RESPONSE, PERCENT_RATELIMIT) {
  # Count tokens in the requests and responses
  DF <- DF %>%
    mutate(
      tokens_in_query = stri_count_words(.data[[GPT_QUERY]]),
      tokens_in_response = stri_count_words(.data[[GPT_RESPONSE]])
    )
  
  # Calculate average tokens in queries and responses
  avg_tokens_in_query <- mean(DF$tokens_in_query)
  avg_tokens_in_response <- mean(DF$tokens_in_response)
  
  # Calculate n_future based on the rate limits and average tokens
  MRPS <- RPM / 60  # maximum requests per second
  MTPS <- TPM / 60  # maximum tokens per second
  
  avg_tokens_per_query <- avg_tokens_in_query + avg_tokens_in_response  # average tokens per query
  
  max_requests_per_second_based_on_tokens <- MTPS / avg_tokens_per_query  # maximum requests per second based on tokens
  
  max_n_future <- min(MRPS, max_requests_per_second_based_on_tokens)  # the minimum of MRPS and maximum requests per second based on tokens
  
  n_future <- floor(max_n_future * PERCENT_RATELIMIT) # use percent_RateLimit to reduce the number of requests
  
  return(n_future)
}

########################################

calculate_price <- function(DF, GPT_QUERIES, GPT_RESPONSES, input_price_per_1k, output_price_per_1k) {
  
  if(length(GPT_QUERIES) != length(GPT_RESPONSES)){
    stop("Mismatch in the length of GPT_QUERIES and GPT_RESPONSES")
  }
  
  DF <- DF %>%
    rowwise() %>%
    mutate(
      combined_tokens_in_query = sum(c_across(all_of(GPT_QUERIES)) %>% map_dbl(~stri_count_words(.))),
      combined_tokens_in_response = sum(c_across(all_of(GPT_RESPONSES)) %>% map_dbl(~stri_count_words(.))),
      combined_input_price = combined_tokens_in_query/1000 * input_price_per_1k,
      combined_output_price = combined_tokens_in_response/1000 * output_price_per_1k
    )
  
  total_combined_price <- sum(DF$combined_input_price + DF$combined_output_price)
  
  return(total_combined_price)
}
