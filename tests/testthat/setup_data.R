data(adeliepenguin)

# simulate an initial vector
set.seed(1234)
Cranevec1 <- runif(5)
Cranevec1 <- Cranevec1/sum(Cranevec1) #scales the vector to sum up to 1
