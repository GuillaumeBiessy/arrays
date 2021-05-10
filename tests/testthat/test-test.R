x <- 1:10
dn <- list(x = 1:5, g = c("H", "F"))

(A <- x %>% aMake(dn))
dim(A)
dimnames(A)
A %>% c %>% aMake(dn)

aSums(A, 1)
aSums(A, 1, F)
aSums(A, 2)

A %>% array_to_df("E") %>% df_to_array(covariates = c("x", "g"), "E")

A %>% aInsert("g", "F", 12)

aSums(A, 1) %>% aMorph(l = list(x = 1:7))

A %>% aMorph(list(x = 1:4), TRUE)
A %>% aMorph(list(g = c("H", "F", "B")))
