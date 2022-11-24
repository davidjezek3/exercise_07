library(Biostrings)
Overlap_Calc <- function(seq1, seq2) {
  for (i in 1:(length(seq1)-1)) {
    if ((length(seq1)-i) > length(seq2)) {
      next
    }
    else {
      if (seq1[(1+i):length(seq1)] == seq2[1:(length(seq1)-i)]) {
        return(length(seq1)-i)
        break
      }
    }
    if (i ==(length(seq1)-1)) {
      return(0)
    }
  }
}

Overlap_Calc(DNAString("CATGC"), DNAString("CTAAGT"))


S <- DNAStringSet(c("CATGC", "CTAAGT", "GCTA", "TTCA", "ATGCATC"))
S <- c(DNAString("CATGC"), DNAString("CTAAGT"))
M <- matrix(0,length(S),length(S))

for (k in 1:length(S)) {
  for (l in 1:length(S)) {
    if (k != l) {
      M[k,l] <- Overlap_Calc(S[k], S[l])
    }
  }
}

Overlap_Calc(S[1], S[2])
