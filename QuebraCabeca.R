source("Estado.R")

## Classe e métodos para o problema dos 3 Missionários e 3 Canibais
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

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuebraCabeca = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuebraCabeca <- function(obj) {
  cat("(P11 P12 P13 P21 P22 P23 P31 P32 P33): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuebraCabeca <- function(atual){
  
  ## Se uma peca nao estiver na sua posicao objetivo,
  ## somar a distancia entre sua posicao nos eixos x
  ## e y e incrementar a heuristica
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = M + C + B
  return(sum(atual$desc))
}

geraFilhos.QuebraCabeca <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()

  desc <- obj$desc
  
  bAtual <- as.numeric(desc[8]) ##?
  
  bNovo <- as.numeric(bAtual != 1) ##?
  
  ## gera filhos usando todos os operadores  
  
  filhosDesc <- sapply(1:length(desc),
                          function(i) {
                            fDesc <- desc
                            if( (i-3) > 0 && (i-3) < 10 && desc[i-3] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc[i]
                              fDesc[i] <- fDesc[i-3]
                              fDesc[i-3] <- temp
                              fDesc
                            }
                            else if( (i-1) > 0 && (i-1) < 10 && desc[i-1] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc[i]
                              fDesc[i] <- fDesc[i-1]
                              fDesc[i-1] <- temp
                              fDesc
                            }
                            else if( (i+1) > 0 && (i+1) < 10 && desc[i+1] == -1){ ## Se a posicao esta vazia
                                temp <- fDesc[i]
                              fDesc[i] <- fDesc[i+1]
                              fDesc[i+1] <- temp
                              fDesc
                            }
                            else if( (i+3) > 0 && (i+3) < 10 && desc[i+3] == -1){ ## Se a posicao esta vazia
                              temp <- fDesc[i]
                              fDesc[i] <- fDesc[i+3]
                              fDesc[i+3] <- temp
                              fDesc
                            }
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