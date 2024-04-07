PatternCount <- function(Text, Pattern) {
  count <- 0
  TextLength <- nchar(Text)
  PatternLength <- nchar(Pattern)
  for (i in 1:(TextLength - PatternLength + 1)) {
    if (substr(Text, i, i + PatternLength - 1) == Pattern) {
      count <- count + 1
    }
  }
  return(count)
}

lines <- readLines("/Users/KEOGH/Desktop/R-Programming-to-Find-DNA-motifs/dataset_30272_6.txt")
Text <- lines[1]
Pattern <- lines[2]
result <- PatternCount(Text, Pattern)

print(result) 