
# Trabalho 1: SNA - Social Network Analysis 

## (libera mem�ria de eventuais objetos manipulados antes)
rm(list=ls())

install.packages("sna")
library(sna)
install.packages("rgl")
library(rgl)
install.packages("network")
library(network)

# Trabalha a partir de uma rede aleat�ria
rede <- read.table("D:/0 Aula 28_07_18 Redes sociais/SNA e Text Mining Paulista T4/Trabalho 1/Rede One Mode_Trab1.csv",header=TRUE,sep = ";", dec=",")
rede

# Adaptando o data.frame rede para que possa servir para a montagem da rede
grede <- rede[,2:17]
rownames(grede) <- rede[,1]

# Construindo a rede a partir da matriz de rela��es (0 e 1)
par(mfrow=c(1,1))
gplot(grede)
gplot(grede,gmode="graph",displaylabels = TRUE)
gplot(grede,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)

# Explorando a rede
degree(grede,gmode="graph",cmode="indegree")
closeness(grede,gmode="graph")
# qto maior o closeness, mais proximo
betweenness(grede,gmode="graph")
# qto maior, mais ponte � (n�o pode ser eliminado, pois rompe a rede)
# zero significa que pode ser eliminado que n�o impacta a rede

# Aprimorando a representa��o da rede
gplot(grede,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=degree(grede,gmode="graph",cmode="indegree")/3)

gplot(grede,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=degree(grede,gmode="graph",cmode="indegree"))

gplot(grede,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=closeness(grede,gmode="graph")*2)

gplot(grede,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=round(closeness(grede,gmode="graph"),digits=2))

gplot(grede,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=betweenness(grede,gmode="graph")/3+1)

gplot(grede,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=betweenness(grede,gmode="graph"))

# Gr�fico 3D da rede
gplot3d(grede)

# Interprete as m�tricas de centralidade de grau, proximidade e intermedia��o


############################

# Altera��o: eliminar n�s F e L
rede1 <- read.table("D:/0 Aula 28_07_18 Redes sociais/SNA e Text Mining Paulista T4/Trabalho 1/Rede One Mode_Trab1_alt.csv",header=TRUE,sep = ";", dec=",")
rede1

# Adaptando o data.frame rede para que possa servir para a montagem da rede
grede1 <- rede1[,2:15]
rownames(grede1) <- rede1[,1]

# Construindo a rede a partir da matriz de rela��es (0 e 1)
par(mfrow=c(1,1))
gplot(grede1)
gplot(grede1,gmode="graph",displaylabels = TRUE)
gplot(grede1,gmode="graph",displaylabels = TRUE,edge.col="gray",usearrows=FALSE)

# Explorando a rede
degree(grede1,gmode="graph",cmode="indegree")
closeness(grede1,gmode="graph")
# qto maior o closeness, mais proximo
betweenness(grede1,gmode="graph")
# qto maior, mais ponte � (n�o pode ser eliminado, pois rompe a rede)
# zero significa que pode ser eliminado que n�o impacta a rede

# Aprimorando a representa��o da rede
gplot(grede1,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=degree(grede1,gmode="graph",cmode="indegree")/3)

gplot(grede1,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=degree(grede1,gmode="graph",cmode="indegree"))

gplot(grede1,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=closeness(grede1,gmode="graph")*2)

gplot(grede1,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=round(closeness(grede1,gmode="graph"),digits=2))

gplot(grede1,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=betweenness(grede1,gmode="graph")/3+1)

gplot(grede1,gmode="grede",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=betweenness(grede1,gmode="graph"))

# Gr�fico 3D da rede
gplot3d(grede1)


############################

# Outras an�lises (extens�es sna, network ou igraph)

####################################################################
##PESQUISA NO PDF DO PROF:
# SNA

closeness(dat, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
          tmaxdev=FALSE, cmode="directed", geodist.precomp=NULL,
          rescale=FALSE, ignore.eval=TRUE)

betweenness(dat, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
            tmaxdev=FALSE, cmode="directed", geodist.precomp=NULL,
            rescale=FALSE, ignore.eval=TRUE)

degree(dat, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
       tmaxdev=FALSE, cmode="freeman", rescale=FALSE, ignore.eval=FALSE)

gplot(dat, g = 1, gmode = "digraph", diag = FALSE,
      label = NULL, coord = NULL, jitter = TRUE, thresh = 0,
      thresh.absval=TRUE, usearrows = TRUE, mode = "fruchtermanreingold",
      displayisolates = TRUE, interactive = FALSE, interact.bycomp = FALSE,
      xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, pad = 0.2,
      label.pad = 0.5, displaylabels = !is.null(label), boxed.labels = FALSE,
      label.pos = 0, label.bg = "white", vertex.enclose = FALSE,
      vertex.sides = NULL, vertex.rot = 0, arrowhead.cex = 1, label.cex = 1,
      loop.cex = 1, vertex.cex = 1, edge.col = 1, label.col = 1,
      vertex.col = NULL, label.border = 1, vertex.border = 1, edge.lty = NULL,
      edge.lty.neg=2, label.lty = NULL, vertex.lty = 1, edge.lwd = 0,
      label.lwd = par("lwd"), edge.len = 0.5, edge.curve = 0.1,
      edge.steps = 50, loop.steps = 20, object.scale = 0.01, uselen = FALSE,
      usecurve = FALSE, suppress.axes = TRUE, vertices.last = TRUE,
      new = TRUE, layout.par = NULL, ...)

##################################################################################

# NETWORK

# ?????????????????????????????????? N� SEI FAZER !!!!!!!!!

##################################################################################

# IGRAPH

closeness(graph, vids = V(graph), mode = c("out", "in", "all", "total"),
          weights = NULL, normalized = FALSE)
estimate_closeness(graph, vids = V(graph), mode = c("out", "in", "all",
                                                    "total"), cutoff, weights = NULL, normalized = FALSE)

degree(graph, v = V(graph), mode = c("all", "out", "in", "total"),
       loops = TRUE, normalized = FALSE)
degree_distribution(graph, cumulative = FALSE, ...)

estimate_betweenness(graph, vids = V(graph), directed = TRUE, cutoff,
                     weights = NULL, nobigint = TRUE)
betweenness(graph, v = V(graph), directed = TRUE, weights = NULL,
            nobigint = TRUE, normalized = FALSE)
edge_betweenness(graph, e = E(graph), directed = TRUE, weights = NULL)

####################################################################

# Extens�o SNA
# Medidas de Centralidade
degree(grede,gmode="digraph",cmode="freeman")
closeness(grede,gmode="digraph")
betweenness(grede,gmode="digraph")

# Gr�ficos Closeness
gplot(grede,gmode="digraph",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,vertex.cex=closeness(grede,gmode="digraph")*2)
gplot(grede,gmode="digraph",displaylabels = TRUE,
      edge.col="gray",usearrows=FALSE,label=round(closeness(grede,gmode="digraph"),digits=2))

##########

# Extens�o NETWORK

# ???????????????????????????? N�O SEI FAZER

##########

# Extens�o IGRAPH

install.packages("igraph")
library(igraph)

g<- graph.formula(A-M, A-O, A-C, A-N, A-B, A-P, A-D, A-J, A-K, B-D, B-J, B-P, B-K, B-A, B-O, B-C, C-N, C-D, C-P, C-K, C-A, C-M, C-B, D-J, D-E, D-A, D-B, D-O, D-C, D-N, E-F, E-H, E-G, E-M, E-K, E-J, E-D, E-P, E-I, F-G, F-E, F-K, G-I, G-F, G-E, G-M, G-J, G-K, H-I, H-E, H-J, I-H, I-L, I-G, I-E, I-K, I-M, J-K, J-N, J-B, J-P, J-D, J-A, J-M, J-E, J-G, J-H, K-N, K-C, K-O, K-O, K-B, K-A, K-M, K-E, K-G, K-I, K-F, K-J, L-I, M-I, M-G, M-E, M-K, M-O, M-A, M-C, M-J, N-O, N-D, N-P, N-J, N-K, N-A, N-C, O-M, O-A, O-K, O-P, O-D, O-B, O-N, P-A, P-E, P-B, P-N, P-C, P-O, P-J)
g

plot(g)
V(g)
E(g)
get.adjacency(g)

dg<- graph.formula(A-+M, A-+O, A-+C, A-+N, A-+B, A-+P, A-+D, A-+J, A-+K, B-+D, B-+J, B-+P, B-+K, B-+A, B-+O, B-+C, C-+N, C-+D, C-+P, C-+K, C-+A, C-+M, C-+B, D-+J, D-+E, D-+A, D-+B, D-+O, D-+C, D-+N, E-+F, E-+H, E-+G, E-+M, E-+K, E-+J, E-+D, E-+P, E-+I, F-+G, F-+E, F-+K, G-+I, G-+F, G-+E, G-+M, G-+J, G-+K, H-+I, H-+E, H-+J, I-+H, I-+L, I-+G, I-+E, I-+K, I-+M, J-+K, J-+N, J-+B, J-+P, J-+D, J-+A, J-+M, J-+E, J-+G, J-+H, K-+N, K-+C, K-+O, K-+O, K-+B, K-+A, K-+M, K-+E, K-+G, K-+I, K-+F, K-+J, L-+I, M-+I, M-+G, M-+E, M-+K, M-+O, M-+A, M-+C, M-+J, N-+O, N-+D, N-+P, N-+J, N-+K, N-+A, N-+C, O-+M, O-+A, O-+K, O-+P, O-+D, O-+B, O-+N, P-+A, P-+E, P-+B, P-+N, P-+C, P-+O, P-+J)
dg
plot(dg)

dg11<- graph.formula(A++M, A++O, A++C, A++N, A++B, A++P, A++D, A++J, A++K, B++D, B++J, B++P, B++K, B++O, B++C, C++N, C++D, C++P, C++K, C++M, D++J, D++E, D++O, D++N, E++F, E++H, E++G, E++M, E++K, E++J, E++P, E++I, F++G, F++K, G++I, G++M, G++J, G++K, H++I, H++J, I++L, I++K, I++M, J++K, J++N, J++P, J++M, K++N, K++O, K++M, M-+O, N++O, N++P, O++P)
dg11
plot(dg11)

dg22<- graph.formula(A++M, A+-O, A+-C, A+-N, A+-B, A+-P, A+-D, A++J, A++K, B-+D, B-+J, B-+P, B-+K, B-+O, B-+C, C-+N, C-+D, C-+P, C-+K, C-+M, D-+J, D-+E, D-+O, D-+N, E++F, E+-H, E+-G, E++M, E++K, E++J, E+-P, E+-I, F+-G, F++K, G-+I, G-+M, G-+J, G-+K, H-+I, H-+J, I-+L, I-+K, I-+M, J++K, J+-N, J+-P, J++M, K+-N, K+-O, K++M, M++O, N-+O, N-+P, O-+P)
dg22
plot(dg22)

plot(g.tree, layout=layout.circle)
plot(g.tree, layout=layout.reingold.tilford)
plot(g.tree, layout=layout.reingold.tilford(g.tree, circular=T))

############################

# Cluster Analysis

# Extens�o para gr�ficos e para An�lise de Agrupamentos

install.packages("ggplot2")
library(ggplot2)
install.packages("ggdendro")
library(ggdendro)

rede2 <- read.table("D:/0 Aula 28_07_18 Redes sociais/SNA e Text Mining Paulista T4/Trabalho 1/Rede Two Mode_Trab1.csv",header=TRUE,sep = ";", dec=",")
head(rede2)
row.names(rede2) <- rede2$X

# Implementa o algoritmo hier�rquico e apresenta o dendrograma
hc <- hclust(dist(rede2), "average")  # explorar com outros m�todos de dist�ncia
p <- ggdendrogram(hc, rotate=FALSE)
print(p)
ggdendrogram(hc, rotate=TRUE)

hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrograma dos Consumidores")

# "Cortando" a �rvore em 4 grupos
grupos <- cutree(hc,k=4)
grupos

# Analisando as principais vari�veis a partir dos grupos
boxplot(rede2$iPhone ~ grupos, col = "blue", main = 'Box Plot do iPhone')
boxplot(rede2$iPad ~ grupos, col = "blue", main = 'Box Plot do iPad')
boxplot(rede2$Livro.Harry.Potter ~ grupos, col = "blue", main = 'Box Plot do Livro Harry Potter')
boxplot(rede2$jogo.MineCraft ~ grupos, col = "blue", main = 'Box Plot do Jogo MineCraft')
boxplot(rede2$Camisa.do.Corinthians ~ grupos, col = "blue", main = 'Box Plot da Camisa do Corinthians')
boxplot(rede2$Bola.de.Futebol ~ grupos, col = "blue", main = 'Box Plot da Bola de Futebol')
boxplot(rede2$Flauta.Transversal ~ grupos, col = "blue", main = 'Box Plot da Flauta Transversal')
boxplot(rede2$Lista.Telef�nica ~ grupos, col = "blue", main = 'Box Plot da Lista Telef�nica')
boxplot(rede2$Caixa.de.F�sforos ~ grupos, col = "blue", main = 'Box Plot da Caixa de F�sforos')
boxplot(rede2$Calculadora ~ grupos, col = "blue", main = 'Box Plot da Calculadora')
boxplot(rede2$Detergente ~ grupos, col = "blue", main = 'Box Plot do Detergente')

# Refazendo os clusters, agora com as vari�veis padronizadas
rede2_padr <- rede2
for (i in 2:12) rede2_padr[,i] <- scale(rede2_padr[,i])

# Implementa o algoritmo hier�rquico e apresenta o dendrograma
hc2 <- hclust(dist(rede2_padr), "average")  # explorar com outros m�todos de dist�ncia
p <- ggdendrogram(hc2, rotate=FALSE)
print(p)
ggdendrogram(hc2, rotate=TRUE)

hcdata2 <- dendro_data(hc2)
ggdendrogram(hcdata2, rotate=TRUE, size=2) + labs(title="Dendrograma dos Consumidores (padronizado)")

# "Cortando" a �rvore em 4 grupos
grupos2 <- cutree(hc2,k=4)
grupos2

# Analisando as principais vari�veis a partir dos grupos2
boxplot(rede2$iPhone ~ grupos2, col = "red", main = 'Box Plot do iPhone')
boxplot(rede2$iPad ~ grupos2, col = "red", main = 'Box Plot do iPad')
boxplot(rede2$Livro.Harry.Potter ~ grupos2, col = "red", main = 'Box Plot do Livro Harry Potter')
boxplot(rede2$jogo.MineCraft ~ grupos2, col = "red", main = 'Box Plot do Jogo MineCraft')
boxplot(rede2$Camisa.do.Corinthians ~ grupos2, col = "red", main = 'Box Plot da Camisa do Corinthians')
boxplot(rede2$Bola.de.Futebol ~ grupos2, col = "red", main = 'Box Plot da Bola de Futebol')
boxplot(rede2$Flauta.Transversal ~ grupos2, col = "red", main = 'Box Plot da Flauta Transversal')
boxplot(rede2$Lista.Telef�nica ~ grupos2, col = "red", main = 'Box Plot da Lista Telef�nica')
boxplot(rede2$Caixa.de.F�sforos ~ grupos2, col = "red", main = 'Box Plot da Caixa de F�sforos')
boxplot(rede2$Calculadora ~ grupos2, col = "red", main = 'Box Plot da Calculadora')
boxplot(rede2$Detergente ~ grupos2, col = "red", main = 'Box Plot do Detergente')

#####################

# Desenhar os gr�ficos de rede
compras1 <- read.table("D:/0 Aula 28_07_18 Redes sociais/SNA e Text Mining Paulista T4/Trabalho 1/Rede Two Mode_Trab1.csv",header=TRUE,sep = ";", dec=",")
compras1

gcompras1<- compras1[, 2:12]
rownames(gcompras1) <- compras1[,1]
gcompras1

# Construindo a rede a partir da matriz de rela��es (0 e 1)
gplot(gcompras1)
gplot(gcompras1,gmode="twomode",displaylabels = TRUE)
gplot(gcompras1,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE)

# Explorando a rede
degree(gcompras1,gmode="twomode",cmode="indegree")
closeness(gcompras1,gmode="twomode")
betweenness(gcompras1,gmode="twomode")

# Aprimorando a representa��o da rede
gplot(gcompras1,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE,
      vertex.cex = closeness(gcompras,gmode="twomode")*3)

# Desafio alterar cores: produto branco e pessoas colorido
gplot(gcompras1,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE,
      vertex.cex = closeness(gcompras,gmode="twomode")*3, vertex.col= cbind(rep("#33CCCC", length(gcompras1[,1])), rep('white', length(gcompras1[1,]))))

# Analise:
# Voc� acha que as medidas de centralidade de proximidade e intermedia��o
# s�o �teis no contexto da rede Two Mode? 



