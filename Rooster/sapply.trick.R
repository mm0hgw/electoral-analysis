
a1 <- seq(10)
a2 <- lapply(a1,function(x){seq(10)+x})
print(a2)
a3 <- sample(a1,1)
print(a3)
a4 <- sapply(a2,'[[',a3)
print(a4)
