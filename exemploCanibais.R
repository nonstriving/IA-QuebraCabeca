debugSource("QuebraCabeca.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- QuebraCabeca(desc = c(P1 = c(2,2), P2 = c(3,1), P3 = c(2,1), P4 = c(2,3), P5 = c(1,3), P6 = c(1,2), P7 = c(1,1), P8 = c(3,2)))

objetivo <- QuebraCabeca()
objetivo$desc <- c(P1 = c(1,3), P2 = c(2,3), P3 = c(3,3), P4 = c(3,2), P5 = c(3,1), P6 = c(2,1), P7 = c(1,1), P8 = c(1,2))

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))