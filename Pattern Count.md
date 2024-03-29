
```R
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

lines <- readLines("your_file.txt")
Text <- lines[1]
Pattern <- lines[2]
result <- PatternCount(Text, Pattern)

print(result)  

```
