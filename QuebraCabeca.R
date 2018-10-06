setwd("/Users/samara/Downloads/basicAI_Search-master")
source("Estado.R")

## Classe e m??todos para o problema dos 3 Mission??rios e 3 Canibais
QuebraCabeca <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuebraCabeca", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para compara????o entre estados
Ops.QuebraCabeca = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da fun????o gen??rica "print" do R
print.QuebraCabeca <- function(obj) {
  cat("(P11 P12 P13 P21 P22 P23 P31 P32 P33): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da fun????o gen??rica "heuristica", definida por Estado.R
heuristica.QuebraCabeca <- function(atual){
  
  ## Se uma peca nao estiver na sua posicao objetivo,
  ## somar a distancia entre sua posicao nos eixos x
  ## e y e incrementar a heuristica
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = M + C + B
  return(0) ##sum(atual$desc)
}

geraFilhos.QuebraCabeca <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()

  desc <- obj$desc
  
  ## gera filhos usando todos os operadores  
  
  filhosDesc <- sapply(1:length(desc),
                          function(i) {
                            fDesc1 <- desc
                            fDesc2 <- desc
                            fDesc3 <- desc
                            fDesc4 <- desc
                            if( (i-3) > 0 && (i-3) < 10 && desc[i-3] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc1[i]
                              fDesc1[i] <- fDesc1[i-3]
                              fDesc1[i-3] <- temp
                              fDesc4 <- fDesc1
                            }
                            if( (i-1) > 0 && (i-1) < 10 && desc[i-1] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc2[i]
                              fDesc2[i] <- fDesc2[i-1]
                              fDesc2[i-1] <- temp
                              fDesc4 <- c(fDesc1,fDesc2)
                            }
                            if( (i+1) > 0 && (i+1) < 10 && desc[i+1] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc3[i]
                              fDesc3[i] <- fDesc3[i+1]
                              fDesc3[i+1] <- temp
                              fDesc4 <- c(fDesc1,fDesc2,fDesc3)
                            }
                            if( (i+3) > 0 && (i+3) < 10 && desc[i+3] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc4[i]
                              fDesc4[i] <- fDesc4[i+3]
                              fDesc4[i+3] <- temp
                              fDesc4 <- c(fDesc1,fDesc2,fDesc3,fDesc4)
                            }
                            print("teste")
                            fDesc4
                          })
  
  ## gera os objetos QuebraCabeca para os filhos
  for(filhoDesc in filhosDesc){
    filho <- QuebraCabeca(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}