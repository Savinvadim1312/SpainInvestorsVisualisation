install.packages("quanteda")
install.packages("stringr")
install.packages("xlsx")
install.packages("igraph")
install.packages("zoom")
install.packages("visNetwork")

library(quanteda)
library(stringr)
library (tidyverse)
library(xlsx)
library(igraph)
library(plotly)
library(ggplot2)
library(visNetwork)


#Cargo los datos de inversores en España
input <- as.data.frame(read.xlsx("C:/Users/ramadr/Dropbox/Fitenium/Investment & Financing/Spanish Startup ecosystem/Freddy_agr_15apr19_El-referente_2019_rev.xlsx", sheetName = "Fondos de inversion esp", encoding="UTF-8"))


#Hago un Document Frequency Matrix a partir de las inversiones (inversores x participadas)
invpart <- input$Inversiones %>% 
  str_replace_all(" ","") %>%
  str_replace_all(","," ")  %>%
  dfm(tolower = FALSE) %>%
  as.data.frame()

inv_name <- as.data.frame(input$Nombre)

#Elimino las columnas de inversores vacías que me genera la dfm
na_v <- which(!complete.cases(input$Nombre))

inv_name <- as.data.frame(inv_name[-na_v,])
invpart <- invpart[-na_v,]
invpart <- invpart[,-1]

#Cambio los nombres de las columnas
rownames(invpart) <- inv_name$`inv_name[-na_v, ]`

#Creo la matriz de participadas 
part2 <- data.frame(matrix(0, dim(invpart)[2], dim(invpart)[2])) 
rownames(part2) <- colnames(invpart)
colnames(part2) <- colnames(invpart)


#Junto las participadas con inversores x participadas
n1 <- rbind(part2,invpart)

#Creo la matriz de inversores y la matriz de participadas x inversores
inv2 <- data.frame(matrix(0, dim(invpart)[1], dim(invpart)[1]))
partinv <- data.frame(matrix(0, dim(invpart)[2], dim(invpart)[1]))
colnames (inv2) <- inv_name$`inv_name[-na_v, ]`
colnames (partinv) <- inv_name$`inv_name[-na_v, ]`

#Junto las participadas x inversores con inversores.
n2 <- rbind(partinv,inv2)

#Junto las 4 matrices
network <- as.matrix(cbind(n1,n2))

#Defino la red para trabajar en iGraph a partir de una matriz de adyacentes
inv_esp <- graph.adjacency(network, mode = "directed", diag = FALSE)


set.seed(3959)
#transformo la red que había creado con iGraph para visualizarla con visNetwork 
test.visn <- toVisNetworkData(inv_esp)

#Diferencio los inversores de las participadas
test.visn$nodes$type.label[(nrow(part2)+1):nrow(n2)] <-"Inversor"
test.visn$nodes$type.label[1:nrow(part2)] <-"Startup"
test.visn$nodes$group <- test.visn$nodes$type.label

#Modifico conexiones a voluntad
test.visn$edges$color <- "gray"
test.visn$edges$width <- 0.05

#Modifico nodos a voluntad
test.visn$nodes$color.background[(nrow(part2)+1):nrow(n2)] <- "lightblue" #Inversores en azul
test.visn$nodes$color.border[(nrow(part2)+1):nrow(n2)] <- "grey"
test.visn$nodes$color.highlight.background[(nrow(part2)+1):nrow(n2)] <- "cadetblue"
test.visn$nodes$color.highlight.border[(nrow(part2)+1):nrow(n2)] <- "gray21"

test.visn$nodes$color.background[1:nrow(part2)] <- "lightsalmon" #Participadas en salmon
test.visn$nodes$color.border[1:nrow(part2)]<- "grey"
test.visn$nodes$color.highlight.background[1:nrow(part2)] <- "coral"
test.visn$nodes$color.highlight.border[1:nrow(part2)] <- "gray21"

test.visn$nodes$size  <- degree(inv_esp, mode = "total") 
test.visn$nodes$size[1:nrow(part2)] <- test.visn$nodes$size[1:nrow(part2)]*2 

test.visn$nodes$title[(nrow(part2)+1):nrow(n2)] <- as.vector(input$Descripción)[1:nrow(inv2)] # Text on click

draw <- visNetwork(test.visn$nodes, 
                   test.visn$edges, 
                   background="#eeefff",
                   main="StartUp investments in Spain", 
                   submain="Reported by elReferente 2019 guide") %>%  
#footer= "Hyperlinks and mentions among media sources"))   
  
  visOptions(highlightNearest = TRUE, 
             selectedBy = "type.label") #%>% 
  
  #visIgraphLayout(layout = "layout_with_kk")

draw







#Defino los atributos que quiero representar en mi red a partir del input
#set_vertex_attr(inv_esp, "CompanyType", 1:nrow(part2), value = "Startup")
#set_vertex_attr(inv_esp, "CompanyType", (nrow(part2)+1):nrow(n2), value = "Investor")
#set_vertex_attr(inv_esp, "Description", (nrow(part2)+1):nrow(n2), input$Descripción[1:nrow(inv2)])



#Coloreo las participadas en rojo y los inversores en azul
#for (i in 1:length(test.visn$nodes$color[])) {
#  ifelse(i <= dim(part2)[1], 
#         test.visn$nodes$color[i] <- "lightsalmon",
#         test.visn$nodes$color[i] <- "lightblue")
#}


#Quiero jugar con los nodos?
#V(inv_esp)$degree <- degree(inv_esp)
#hist(V(inv_esp)$degree, breaks = 100, xlab = "Degree of nodes")



#Definimos el layout a usar
#l1 <- layout.fruchterman.reingold(inv_esp, niter = 1000)
#l2 <- layout_in_circle(inv_esp)
#l3 <- layout_with_kk(inv_esp)
#l4 <- layout_with_lgl(inv_esp)

#Aqui lo que estoy haciendo es asignar atributos a los vertices y conexiones

#Vertex color
#V(inv_esp)$color <- "grey" #Todos los nodos grises
#V(inv_esp)[degree(inv_esp, mode = "in")>3]$color <- "yellow" #Mas de tres inv. en amarillo
#V(inv_esp)[degree(inv_esp, mode = "out")>3]$color <- "blue" #Mas de tres inv. en rojo

#Vertex sizes
#  V(inv_esp)$size=degree(inv_esp, mode = "in")*5 #Tamaño de los nodos varía respecto a los entrantes

#Edges option
#  E(inv_esp)$color <- "grey" #Color de las conexiones

#Working with visNetwork 
## convert to VisNetwork-list


#Clustering [Error fatal por memoria]
#  clp <- cluster_optimal(inv_esp)
#  class(clp)

#plot(inv_esp, 
#     #vertex.label=NA,
#     vertex.label.cex=0.3,
#     layout = l3,
#     vertex.label.color="Black",
#     #edge.arrow.mode= "-", #Con esto especifico que no quiero flechas
#     edge.arrow.size=0.012)


