#Importando os dados:
teste <- read.csv2(file="D:\\Estudo\\Oficina de Pesquisa\\teste\\urca_FinalN.csv", sep=',')
print(teste)

#identifica o tipo da coluna:
print(sapply(teste, class))


#Corrige o Tipo da coluna:
data_frame_mod <- transform(
  teste, Fev31N = as.numeric(as.character(Fev31N)))

teste <- transform(
  data_frame_mod, FevNaturalN  = as.numeric(as.character(FevNaturalN)))

#Fazendo os teste:
print(t.test(teste$Fev31N,
             teste$FevNaturalN))
(ks.test(teste$Fev31N,
         teste$FevNaturalN))
