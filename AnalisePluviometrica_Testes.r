#Importando os Arquivos:
## Definindo o diretório:
setwd("D:\\Estudo\\Oficina de Pesquisa\\Dados Tratados")

## Criando vetor de arquivos:
tabelas <- list.files(pattern= '*.csv')
tabelas # Imprimindo os arquivos, só pra ver se foi direitinho.

#OBS:
#  No R, a utilização do r"..." não funciona muito bem pra corrigir o 
#problema da barra invertida '\', ou seja, nesse caso, precisa-se de fato
#duplicar as barras invertida '\\'.No python dá o mesmo problema, mas 
#o r"..." resolve esse problema, aqui nãoresolve.

#OBS 2:
#  Olha como o R facilita as coisas, se eu precisa-se colocar esse códigico
#no python, precisaria montar uma estrutura de repetição, aqui no R, já 
#existe um método só pra isso. Não que fosse coisa de outro mundo montar 
#uma estrutura de repetição, mas no R é mais simples de fato né.

# Criar dataframe vazio para os resultados dos testes
resultados <- data.frame(Estações = character(),
                         t_FevxMar = numeric(),
                         t_Fev31xMar = numeric(),
                         t_FevxMarN = numeric(),
                         t_Fev31xMarN = numeric(),
                         ks_FevxMar = numeric(),
                         ks_Fev31xMar = numeric(),
                         ks_FevxMarN = numeric(),
                         ks_Fev31xMarN = numeric(),
                         stringsAsFactors = FALSE)

# Fazendo os Testes:
for (i in 1:length(tabelas)) {
  # Lendo Dataframe:
  df <- read.csv(tabelas[i])
  
  # Aplicando t-test:
  ## Valores Absolutos:
  t_FevxMar <- t.test(df$FevNatural, df$Março)
  t_Fev31xMar <- t.test(df$Fev31, df$Março)
  
  ## Valores Normalizados:
  t_FevxMarN <- t.test(df$FevNaturalN, df$MarçoN)
  t_Fev31xMarN <- t.test(df$Fev31N, df$MarçoN)
  
  # Aplicando ks-test:
  ## Valores Absolutos:
  ks_FevxMar <- ks.test(df$FevNatural, df$Março)
  ks_Fev31xMar <- ks.test(df$Fev31, df$Março)
  
  ## Valores Normalizados:
  ks_FevxMarN <- ks.test(df$FevNaturalN, df$MarçoN)
  ks_Fev31xMarN <- ks.test(df$Fev31N, df$MarçoN)
  
  # Tratar nome da estação
  nome_estacao <- gsub("_", " ", gsub(".csv", "", tabelas[i]))
  nome_estacao <- gsub("Final", "", nome_estacao)
  
  # Criar dataframe com os resultados dos testes para esta tabela
  resultados_tabela <- data.frame(Estações = nome_estacao,
                                  t_FevxMar = c(t_FevxMar$statistic, t_FevxMar$p.value),
                                  t_Fev31xMar = c(t_Fev31xMar$statistic, t_Fev31xMar$p.value),
                                  t_FevxMarN = c(t_FevxMarN$statistic, t_FevxMarN$p.value),
                                  t_Fev31xMarN = c(t_Fev31xMarN$statistic, t_Fev31xMarN$p.value),
                                  ks_FevxMar = c(ks_FevxMar$statistic, ks_FevxMar$p.value),
                                  ks_Fev31xMar = c(ks_Fev31xMar$statistic, ks_Fev31xMar$p.value),
                                  ks_FevxMarN = c(ks_FevxMarN$statistic, ks_FevxMarN$p.value),
                                  ks_Fev31xMarN = c(ks_Fev31xMarN$statistic, ks_Fev31xMarN$p.value),
                                  stringsAsFactors = FALSE)
  
  # Mesclar resultados da tabela com o dataframe principal
  resultados <- rbind(resultados, resultados_tabela)
}

# OBS 3:
#   A função cat() é usada para imprimir o nome da tabela atual
#antes de imprimir os resultados dos testes. Ela permite imprimir 
#vários argumentos separados por vírgulas e não gera erros ao lidar
#com valores 'NA'.

# Salvar dataframe como um arquivo CSV
write.csv(resultados, "resultados.csv", row.names = FALSE)