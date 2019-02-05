x <-matrix(0,nrow=15,ncol=15) 
y <-matrix(0,nrow=15,ncol=15) 
x[1,4]<-x[4,1]<-1 
x[3,5]<-x[5,3]<-3 
x[3,6]<-x[6,3]<-1 
x[4,5]<-x[5,4]<-3 
 
 
x[7,8]<-x[8,7]<-2 
x[7,10]<-x[10,7]<- 2 
x[7,13]<-x[13,7]<-4 
x[8,9]<-x[9,8]<-2 
x[9,14]<-x[14,9]<-3 
 
x[10,11]<-x[11,10]<-1 
x[10,12]<-x[12,10]<-3 



g <- igraph::graph.adjacency(adjmatrix=x, 
                             weighted=NULL,
                             mode = "undirected")
plot(g)

put <- 0
dor <- c()
S <- 1
P <- 1
u <- 1

#конкретное расстояние от s до p
s <- 1
p <- 10

point <- 1:nrow(x) #массив вершин
visit <- rep(F, times = ncol(x)) #массив посещений

path_length <- c() #здесь будут храниться метки
for(i in 1:nrow(x)){
  path_length[i] <- Inf
}


pred <- seq(1:ncol(x)) #массив предков

tab <- rbind(point,path_length,visit)

pitu <- function(S,P){
  u <- S
  tab[2,u] <- 0
  
  
  while(u != 0 || u != P){
    for(i in 1:ncol(x)){
      if(tab[3,i] == F & x[u,i] != 0){
          
         #если вершина смежна с u-той и не посещена
        h <- tab[2,i]
        tab[2,i] <- min(tab[2,i],tab[2,u]+x[u,i])
        
        if(tab[2,i]<h){
          pred[i] <- u
        }}}
    
    tab[3,u] <- T #отмечаем посещенной
    
    min_nevisit <- f(tab[2,],tab[3,]) #ищем мин из непосещеннных
    u <- min_nevisit
  }
  if(is.infinite(tab[2,u]) == T){
    put <- 0
  }else{
    put <- tab[2,u] 
    
    #для последующего восстанавления пути через массив предков
    m <- pred[P]
    dor <- c(m,P)
    while(m != S ){
      m <- pred[m]
      dor <- c(m,dor)}
    
    if(S == s & P == p){
      print("The way")
      print(dor)}
  }
  return(put)
}


#поиск наименьшей из непосещенных
f <- function(x,y){
  mn <- Inf
  imn <- 0
  f <- c()
  for(i in 1:length(x)){
    if(x[i] < mn & y[i] == F){
      mn <- x[i]
      imn <- i
    }}
  return(imn)
}


#создание матрицы с расстояниями от i вершины до j    
for(S in 1:ncol(x)){
  for(P in 1:ncol(x)){
    y[S,P] <- pitu(S,P)}}
 
print(y)


#восстанавливаем путь от s до p через массив предков

R <- pitu(s,p)
print(R)





#ищем столицу графа 
q <- c()
for(i in 1:ncol(x)){
  q[i] <- sum(y[i,])
  if(q[i] == 0){
    q[i] <- Inf
  }}
capital <- which.min(q)
print("The capital")
print(capital)