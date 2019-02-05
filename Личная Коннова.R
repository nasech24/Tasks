P<-matrix(0,nrow=10,ncol=10) 

P[1,6]<-P[6,1]<-1
P[2,4]<-P[4,2]<-1
P[1,4]<-P[4,1]<-1 
P[3,5]<-P[5,3]<-1 
P[3,6]<-P[6,3]<-1
P[3,5]<-P[5,3]<-1 
P[3,4]<-P[4,3]<-1
P[2,6]<-P[6,2]<-1
P[4,5]<-P[5,4]<-1 
P[1,7]<-P[7,1]<-1
P[9,7]<-P[7,9]<-1 
P[8,7]<-P[7,8]<-1
P[9,8]<-P[8,9]<-1 
P[9,4]<-P[4,9]<-1 
P[10,9]<-P[9,10]<-1 
P[10,2]<-P[2,10]<-1

g <- igraph::graph.adjacency(adjmatrix=P, 
                             weighted=NULL,
                             mode = "undirected")
plot(g)

n <- nrow(P)
#Функция, которая находит какие кольца связаны с кольцом S
smezh<-function(G,S){

  L <- c()
  for(i in 1:n){
    if(G[S,i]!=0){
      L <- append(L,i)
    }
  }
  return(L)
}


#Функция, которая находит всевозможные пути от одного кольца до другого
puti<-function(G,L,A,S,P){
  L[S] <- 1
  for(i in smezh(G,S)){
    if(L[i]==0){
      if(i!=P){
        A <- c(A,i)
        M <- puti(G,L,A,i,P)
        Q <- append(Q,M)
        A <- A[-length(A)] 
      }else{
        A <- c(A,P)
        Q <- append(Q,list(A))
        A <- A[-length(A)]
      }
    }
  }
  return(Q) #это таблица всех путей из одной вершины в другую
}

#Функция,которая находит, сколько же у кольца смежных колец
pow <- function(G,S){
  A <- G[S,]
  a <- length(which(A!=0))
  return(a)
}
#Для того, чтобы путь нам подходил, он должен быть цепью и удовлетворять определенным услвоиям
#Функция, которая проверяет путь на соблюдение этих условий
prov <- function(G,S,P,A){
  M <- c()
  if(pow(G,S)==1 && pow(G,P)==1){
    for(i in 2:(length(A)-1)){
      M <- append(M,pow(G,A[i]))
    }
    if(all(M==2)==TRUE){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
}

#Функция, которая показывет, какие кольца удаляем
ydali <- function(A,n){
  Z <- c(1:n)
  for(i in 1:length(A)){
    Z[A[i]] <- 0
  }
  Z <- Z[-which(Z==0)]
  return(Z)
}

Q <- list()
#Все самое главное происходит здесь
main <- function(G){
  n <- nrow(G)
  TR <- list()
  L <- rep(0,n)
  G1 <- G
  for(i in 1:n){
    A <- c(i)
    for(j in 1:i){
      if(i!=j){
        M <- puti(G,L,A,i,j)
        
        for(k in 1:length(M)){
          current <- M[[k]]
          G1[ydali(current,n),] <- 0
          G1[,ydali(current,n)] <- 0
          if(prov(G1,i,j,current)==TRUE){
            TR <- append(TR,list(current))
          }
          G1 <- G
        }
      }
    }
  }
  return(TR)
}

#Сортируем пути от самого длинного к самому короткому
sortirovka<-function(A){
  for(i in (length(A)-1):1){
    for(j in 1:i){
      if(length(A[[j]])<length(A[[j+1]])){
        W<-A[[j+1]]
        A[[j+1]]<-A[[j]]
        A[[j]]<-W
      }
    }
  }
  return(A)
}

#и вуаля!
A <- main(P)
A <- sortirovka(A)
otvet <- A[[1]]
delete <- ydali(otvet,n)
print("Максимальная цепочка:")
print(otvet)
print("Нужно удалить:")
print(delete)
