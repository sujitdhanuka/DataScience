
example <- iris

head(example)
table(example)


tapply(example$Sepal.Length, example$Species, mean)

tapply(example$Sepal.Width,example$Species, mean)
tapply(example$Petal.Length, example$Species, mean)
tapply(example$Petal.Width, example$Species, mean)
