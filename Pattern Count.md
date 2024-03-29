To compute Count(Text, Pattern), create a window sliding down Text, checking whether each k-mer substring of Text matches Pattern. The k-mer starting at position i of Text will be refered to as Text(i, k). Remember to use 0-based indexing.

##Sample Input: DNA String; and DNA Pattern to find

GCCGGGAAAGCGATTTAACGGGTTCACAATTGAACCTAGCCGCTAGCTAGCGGGACGAAAGGCCACGTCCTTGGGCGGTAATAAGCCTTTCAGCCTAACTGTTTCATCCAAGGTTGGTCACCGTGGACGGGATTTCTCTGTCCCCAGTTATCACTATGGTCTTGTTGCATAGTCTGTTACCAGTCGTGTGTGTACACGCATTTAGGGCAAGC

GCC

##Sample Output: Count of Pattern found in the string

5
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
