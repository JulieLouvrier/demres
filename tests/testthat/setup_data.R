data(adeliepenguin)

# simulate an initial vector
set.seed(125435)
vec <- round(runif(2) * 100 )
vector_list <- lapply(1:28, function(i) vec)
