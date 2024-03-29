# Most Frequent k-mer Problem
A pattern of k-mer can be considered the most frequent k-mer if it maximizes the Count(Text, Pattern) 

The pseudocode for **FrequentWords** is shown below. However when the Text and k become larger it will become very slow.
```
FrequentWords(Text, k)
    FrequentPatterns ← an empty set
    for i ← 0 to |Text| − k
        Pattern ← the k-mer Text(i, k)
        Count(i) ← PatternCount(Text, Pattern)
    maxCount ← maximum value in array Count
    for i ← 0 to |Text| − k
        if Count(i) = maxCount
            add Text(i, k) to FrequentPatterns
    remove duplicates from FrequentPatterns
    return FrequentPatterns
```
A better algorithm should include a **frequency table** which allows the indices to become arbitary numbers (strings for this case) as well as, a **MaxMap** function that takes a map of strings to integers as an input and returns the maximum value of this map as output
## BetterFrequentWords
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
