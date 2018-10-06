setwd("/Users/samara/Downloads/basicAI_Search-master")

debugSource("QuebraCabeca.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- QuebraCabeca(desc = c(P11 = 5, P12 = 4, P13 = -1, P21 = 6, P22 = 1, P23 = 8, P31 = 7, P32 = 3, P33 = 2))

objetivo <- QuebraCabeca()
objetivo$desc <- c(P11 = 1, P12 = 2, P13 = 3, P21 = 8, P22 = -1, P23 = 4, P31 = 7, P32 = 6, P33 = 5)

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