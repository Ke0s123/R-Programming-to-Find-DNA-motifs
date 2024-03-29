```R
# Function to generate a frequency table of k-mers in the text
FrequencyTable <- function(Text, k) {
  freqMap <- list()
  n <- nchar(Text)
  for (i in 1:(n - k + 1)) {
    Pattern <- substr(Text, i, i + k - 1)
    if (is.null(freqMap[[Pattern]])) {
      freqMap[[Pattern]] <- 1
    } else {
      freqMap[[Pattern]] <- freqMap[[Pattern]] + 1
    }
  }
  return(freqMap)
}

# Function to find the maximum value in the frequency map
MaxMap <- function(freqMap) {
  max_value <- max(unlist(freqMap))
  return(max_value)
}

# Function to find the most frequent k-mers in the text
BetterFrequentWords <- function(Text, k) {
  frequentPatterns <- c() # Initialize an empty array of strings
  freqMap <- FrequencyTable(Text, k)
  max_freq <- MaxMap(freqMap)
  
  for (Pattern in names(freqMap)) {
    if (freqMap[[Pattern]] == max_freq) {
      frequentPatterns <- c(frequentPatterns, Pattern)
    }
  }
  return(frequentPatterns)
}

# Example usage:
Text <- "ACGTTGCATGTCGCATGATGCATGAGAGCT"
k <- 4
result <- BetterFrequentWords(Text, k)
print(result)
```
