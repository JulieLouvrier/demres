data(adeliepenguin)

# simulate an initial vector
set.seed(1234)
penguinvec1 <- runif(5)
penguinvec1 <- penguinvec1/sum(penguinvec1) #scales the vector to sum up to 1
