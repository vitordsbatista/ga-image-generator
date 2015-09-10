#packages
library("EBImage")
library("genalg")
#Lendo imagem
img = readImage("test.jpg")
display(img)

#Criando dados de entrada da rede neural
x <- dim(img@.Data)[1]
y <- dim(img@.Data)[2]
total <- x*y
img <- resize(img, x/2, y/2)
display(img)

img_gray <- apply(img@.Data, 1:2, gray)
display(img_gray)

img_bin <- apply(img_gray, 1:2, binary)
display(img_bin)

#AG

#fitness
fitness <- function(x) {
  x <- matrix(x, dim(img_bin)[1], dim(img_bin)[2])
  length(which(img_bin == x))/total
}
#generations
gen = 1000
#pop size
pop = 300
#mutation
mut = 0.2

#GA
GAmodel <- ga(type="binary", fitness=fitness, 
              popSize=pop, maxiter=gen, nBits=total, 
              selection=gabin_tourSelection,
              crossover=gabin_uCrossover,
              parallel=TRUE)
#best solution
best <- matrix(GAmodel@solution[1,], x, y)
display(best)

#solutions x available
cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints)))





#############________________-----------_----------------_________________##############
#Functions
#best
bestSolution <- function(GAmodel){
  a <- summary(GAmodel)
  a <- substr(a, nchar(a)-total, nchar(a)-2)
  a <- strsplit(a, " ")[[1]]
  a <- as.numeric(a)
  a
}
#gray
gray <- function(p){
  p <- p[1]*0.299 + p[2]*0.587 + p[3]*0.144
  p
}
#gray
binary <- function(p){
  if(p>.66) {
    p <- 1
  }else{
    p <- 0
  }
  p
}
