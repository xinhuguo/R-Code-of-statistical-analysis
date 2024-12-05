#install.packages("plspm")
library(plspm)
data <- read.table("1.txt", header = TRUE)

inner_model <- matrix(c(
  0,0,0,0,0, #"YC"
  1,0,0,0,0, #"SP"
  1,1,0,0,0, #"DMA"
  1,1,1,0,0, #"GP"
  1,1,1,1,0 #"Y"
  ),nrow = 5, ncol=5, byrow = TRUE)
colnames(inner_model)<-rownames(inner_model)<-c("YC","SP","DMA","GP","Y")
outer_model=list(1:2,3:4,5:10,11:13,14)
modes<-c("A","A","A","A","A")
pls_model<-plspm(data,inner_model,outer_model, modes)

plot(pls_model,what="loadings",arr.width=0.1,show.values = TRUE,lcol='gray')
summary(pls_model)

plot(pls_model,what="weight",
   arr.width=0.1,
   colpos="firebrick1",
   colneg="darkgreen",
  show.values=TRUE,
  lcol="black")

pls_model$path_coefs
pls_model$inner_model
pls_model$gof
pls_model$inner_summary
