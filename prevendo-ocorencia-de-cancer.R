# Colectando os Dados
install.packages("caret")
#library(caret)

        
library(gmodels)
library(readr)
dados <- read.csv("bc_data.csv", stringsAsFactors = FALSE)
dim(dados)
str(dados)

#Boa pratica excluir as colunas IDs, pois nao uteis e podem causar resultados errados no modelo
# Remover a primeira coluna
dados <- dados[-1]
str(dados)

# Observando valores NA
any(is.na(dados))

# Multiclassificadores requerem que as variaveis sejam do tipo factores
# Para a coluna diagnostio temos duas categorias, B e M
# B - Benigno, M - Maligno
table(dados$diagnosis)

# Converter a variavel diagnosis de char, para tipo factor
dados$diagnosis <- factor(dados$diagnosis, levels = c('B','M'), labels = c('Benigno', 'Maligno'))
str(dados$diagnosis)

#Verificando a porporcao/percentual das observacoes

round(prop.table(table(dados$diagnosis))*100, digit=1)

# Medidas de tendencia Cenrtral 
# Observando as 3 colunas

summary(dados[c("radius_mean", "area_mean","smoothness_mean")])
# Detectamos aqui um problema na escala usada 
# O Calcula da distancia feito pelo KNN 'e dependente das medidas de escala nos dados de entrada

# Para isso temos que normalizar

normalizar <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}


# testando a normalizacao de dados com 2 vectores
# as escalas retornadas devem ser identicas

vetor1 <- normalizar(c(1,2,3,4,5))
vetor2 <- normalizar(c(10,20,30,40,50)) 

# testando
vetor1
vetor2


# Normalizando os dados
# lapply retorna uma lista, entao 'e necessario converter para um dataframe 
dados_norm <- as.data.frame(lapply(dados[2:31], normalizar))



# verificando a normalizacao
summary(dados[c("radius_mean", "area_mean","smoothness_mean")])
summary(dados_norm[c("radius_mean", "area_mean","smoothness_mean")])



#####################################################################################
################################### Treinando o Modelo ##############################


install.packages("class")
library(class)
?knn

# Criando dados de treino e de teste
# Fazendo slicing/fatiamento dos dados e separando
# Metodo de fatiamento manual, e sem randomizar os dados
# IDeal criar varios tipos de maneiras de Slicing e comparar qual o melhor com maior performace
dados_treino <- dados_norm[1:469, ]
dados_teste <- dados_norm[470:569, ]

# Criando os Labels

dados_treino_labels <- dados_norm[1:469,1 ]
dados_teste_labels <- dados_norm[470:569,1 ]

# Cirando o dito modelo

modelo <- knn(train = dados_treino,
              test = dados_teste,
              cl  = dados_treino_labels,
              k = 21)


# Criando modelo com caret
?trainControl


# A funcao knn reorna objecto do tipo factor
class(modelo)

#####################################################################################
############################### Interpretando o Modelo ##############################


#library(gmodels)
 #criando uma tabela cruzada

 # Criando uma cross table - tabela cruzada
 # prop.chisq = FALSE - Nao mostrar o teste de q quadrado
CrossTable(x = dados_teste_labels, y = modelo, prop.chisq = FALSE )

CrossTable(x = dados_teste_labels, y = modelo, prop.chisq = FALSE)



# Optimizando performace do modelo

# Padronizando o score-z com  funcao scale()
?scale()
# permite padronizar, "normalizar" com uma outra tecnica
head(dados)
dados_z <- as.data.frame(scale(dados[-1]))

# medidas de tendencia central
summary(dados_z$area_mean)

# Criando novos datasets

dados_treino <- dados_z[1:469, ]
dados_teste <- dados_z[470:569, ]

dados_treino_labels <- dados_z[1:469, 1]
dados_teste_labels <- dados_z[470:569, 1]


# Reclassificando

modelo_2 <- knn(train = dados_treino,
                test = dados_teste,
                cl = dados_treino_labels,
                k = 21)

# Criando uma CrossTable

CrossTable(x = dados_teste_labels, y = modelo_2, prop.chisq = FALSE)

# E necessario criar os processos varias vezes, modeificando o valor de K,
# para avaliar a performace do modelo, se melhora ou nao, e ficar com a menor taxa possivel de erro



# Calculado a taxa de erro

prev = NULL
taxa_erro = NULL


for(i in 1:20){
  set.seed(101)
  prev = knn(train = dados_treino, test = dados_teste, cl = dados_treino_labels, k=i)
  taxa_erro[i] = mean(dados$diagnosis != prev)
}

# Obtendo var de k e taxa de erros
#library(ggplot2)

#k.value <- 1:20
#df_erro <- data.frame(taxa_erro, k.value)

