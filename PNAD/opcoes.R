
library(hash)

# Tabela(s) a ser(em) gerada(s)
opcoes.tabela["Grupamento por atividade"] <- 0
opcoes.tabela["Posição na ocupação"] <- 0
opcoes.tabela["Cargo/Função"] <- 0


# Variáveis indexadoras
opcoes.indexadoras <- hash()
opcoes.indexadoras["Local"] <- 1
opcoes.indexadoras["Sexo"] <- 1 
opcoes.indexadoras["Idade"] <- 1
opcoes.indexadoras["Raça"] <- 1
opcoes.indexadoras["Nível de instrução"] <- 0
opcoes.indexadoras["Contribuição previdenciária"] <- 0

opcoes.valores["Local"] <- c("", "Ceará")


# Resuktados                                      
opcoes.resultados["Total de respostas"] <- 0
opcoes.resultados["Total com pesos"] <- 1
opcoes.resultados["Pessoas ocupadas"] <- 1
opcoes.resultados["Total na força de trabalho"] <- 1
opcoes.resultados["Taxa de desocupação"] <- 1
opcoes.resultados["Nível de ocupação"] <- 1
opcoes.resultados["Nível de participação"] <- 1

opcoes.resultados["Renda habitual principal"] <- 1
opcoes.resultados["Renda efetiva principal"] <- 1
opcoes.resultados["Renda habitual total"] <- 1
opcoes.resultados["Renda efetiva total"] <- 1

opcoes.resultados["Renda domiciliar"] <- 0
opcoes.resultados["Renda domiciliar per capita"] <- 0

opcoes.resultados["Horas habituais principal"] <- 1
opcoes.resultados["Horas efetivas principal"] <- 1
opcoes.resultados["Horas habituais total"] <- 1
opcoes.resultados["Horas efetivas total"] <- 1

opcoes.resultados["Anos de escolaridade"] <- 1
