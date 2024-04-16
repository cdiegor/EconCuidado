library(PNADcIBGE)
library(dplyr)
library(survey)
library(readr)

leitura <- function(pnadtxt)
{
  # Leitura e salvamento dos dados
  pnadc <- read_pnadc(pnadtxt, "input_PNADC_trimestral.txt")
  pnadc <- pnadc_labeller(data_pnadc=pnadc, "dicionario_PNADC_microdados_trimestral.xls")
  pnadc <- pnadc_deflator(pnadc, "deflator_PNADC_2023_trimestral_101112.xls")
  
  pnadfile = substring(pnadtxt, 1, nchar(pnadtxt)-4)
  
  # Salvando um objeto RDS
  saveRDS(pnadc, pnadfile)
  return (pnadfile)
}


preprocessamento <- function(pnadfile)
{
  # Abrindo os dados
  pnadc <- as_tibble(readRDS(file = pnadfile))
  
  # Idade
  pnadc$idade = pnadc$V2009
  
  pnadc <- subset(pnadc, idade >= 14)
  
  # Utilizando o design da PNAD estabelecido pelo IBGE
  spnadc <- pnadc_design(data_pnadc = pnadc)
  
  
  # Sexo
  pnadc$sexo = pnadc$V2007
  #pnadc$sexo[pnadc$sexo==1] <- 'Masculino'
  #pnadc$sexo[pnadc$sexo==2] <- 'Feminino'
  
  # UFNomeado
  pnadc$UFN = pnadc$UF
  pnadc$UFN[pnadc$UF==11] <- 'Rondônia'
  pnadc$UFN[pnadc$UF==12] <- 'Acre'
  pnadc$UFN[pnadc$UF==13] <- 'Amazonas'
  pnadc$UFN[pnadc$UF==14] <- 'Roraima'
  pnadc$UFN[pnadc$UF==15] <- 'Pará'
  pnadc$UFN[pnadc$UF==16] <- 'Amapá'
  pnadc$UFN[pnadc$UF==17] <- 'Tocantins'
  pnadc$UFN[pnadc$UF==21] <- 'Maranhão'
  pnadc$UFN[pnadc$UF==22] <- 'Piauí'
  pnadc$UFN[pnadc$UF==23] <- 'Ceará'
  pnadc$UFN[pnadc$UF==24] <- 'Rio Grande do Norte'
  pnadc$UFN[pnadc$UF==25] <- 'Paraíba'
  pnadc$UFN[pnadc$UF==26] <- 'Pernambuco'
  pnadc$UFN[pnadc$UF==27] <- 'Alagoas'
  pnadc$UFN[pnadc$UF==28] <- 'Sergipe'
  pnadc$UFN[pnadc$UF==29] <- 'Bahia'
  pnadc$UFN[pnadc$UF==31] <- 'Minas Gerais'
  pnadc$UFN[pnadc$UF==32] <- 'Espírito Santo'
  pnadc$UFN[pnadc$UF==33] <- 'Rio de Janeiro'
  pnadc$UFN[pnadc$UF==35] <- 'São Paulo'
  pnadc$UFN[pnadc$UF==41] <- 'Paraná'
  pnadc$UFN[pnadc$UF==42] <- 'Santa Catarina'
  pnadc$UFN[pnadc$UF==43] <- 'Rio Grande do Sul'
  pnadc$UFN[pnadc$UF==50] <- 'Mato Grosso do Sul'
  pnadc$UFN[pnadc$UF==51] <- 'Mato Grosso'
  pnadc$UFN[pnadc$UF==52] <- 'Goiás'
  pnadc$UFN[pnadc$UF==53] <- 'Distrito Federal'
  
  
  # Raça
  pnadc$raca = pnadc$V2010
  
  pnadc$raca[pnadc$raca==1] = 'Branca'
  pnadc$raca[pnadc$raca==2] = 'Preta'
  pnadc$raca[pnadc$raca==3] = 'Amarela'
  pnadc$raca[pnadc$raca==4] = 'Parda'
  pnadc$raca[pnadc$raca==5] = 'Indígena'
  pnadc$raca[pnadc$raca==6] = 'Ignorado'
  
  # Faixas etárias
  pnadc$faixa_etaria = pnadc$idade
  pnadc$faixa_etaria[pnadc$idade>=14 & pnadc$idade<=30] = "Jovem"
  pnadc$faixa_etaria[pnadc$idade>=30 & pnadc$idade<=50] = "Pleno"
  pnadc$faixa_etaria[pnadc$idade>=50 & pnadc$idade<=64] = "Senior"
  pnadc$faixa_etaria[pnadc$idade>=65] = "Idoso"
  
  # Renda média habitual de todos os trabalhos (VD4019)
  #pnadc$rendatrabtotal = pnadc$VD4019
  
  # Renda média habitual do trabalho principal (VD4019)
  pnadc$rendatrabtotal = pnadc$VD4016 * pnadc$Habitual 
  
  
  # Pessoas na força de trabalho
  pnadc$forca = pnadc$VD4001
  
  # Pessoas ocupadas e não ocupadas
  pnadc$ocupadas = pnadc$VD4002
  
  # Carteira assinada
  pnadc$contribuicao = pnadc$V4029
  
  # Horas habitualmente trabalhadas
  pnadc$horas = pnadc$V4039
  
  # Posição na ocupação
  pnadc$posicao = pnadc$V4010
  
  
  pnadc$posicao[pnadc$V4010 >= 1111 & pnadc$V4010 <= 1439] = "Diretores e gerentes"
  pnadc$posicao[pnadc$V4010 >= 2111 & pnadc$V4010 <= 2659] = "Profissionais das ciências e intelectuais"
  pnadc$posicao[pnadc$V4010 >= 3111 & pnadc$V4010 <= 3522] = "Técnicos e profissionais de nível médio"
  pnadc$posicao[pnadc$V4010 >= 4110 & pnadc$V4010 <= 4419] = "Trabalhadores de apoio administrativo"
  pnadc$posicao[pnadc$V4010 >= 5111 & pnadc$V4010 <= 5419] = "Trabalhadores dos serviços, vendedores dos comércios e mercados"
  pnadc$posicao[pnadc$V4010 >= 6111 & pnadc$V4010 <= 6225] = "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca"
  pnadc$posicao[pnadc$V4010 >= 7111 & pnadc$V4010 <= 7549] = "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios"
  pnadc$posicao[pnadc$V4010 >= 8111 & pnadc$V4010 <= 8350] = "Operadores de instalações e máquinas e montadores"
  pnadc$posicao[pnadc$V4010 >= 9111 & pnadc$V4010 <= 9629] = "Ocupações elementares"
  pnadc$posicao[pnadc$V4010 >= 0110 & pnadc$V4010 <= 0512] = "Membros das forças armadas, policiais e bombeiros militares"
  
  # Atividade no trabalho principal
  pnadc$atividade = pnadc$V4012
  
  #pnadc$atividade[pnadc$V4013 >= 01101 & pnadc$V4013 <= 03002] = "Agricultura, pecuária, produção florestal, pesca e aquicultura"
  #pnadc$atividade[pnadc$V4013 >= 05000 & pnadc$V4013 <= 39000] = "Indústria geral"
  #pnadc$atividade[pnadc$V4013 >= 41000 & pnadc$V4013 <= 43000] = "Construção"
  #pnadc$atividade[pnadc$V4013 >= 45010 & pnadc$V4013 <= 48100] = "Comércio, reparação de veículos automotores e motocicletas"
  #pnadc$atividade[pnadc$V4013 >= 49010 & pnadc$V4013 <= 53002] = "Transporte, armazenagem e correio"
  #pnadc$atividade[pnadc$V4013 >= 55000 & pnadc$V4013 <= 56020] = "Alojamento e alimentação"
  #pnadc$atividade[pnadc$V4013 >= 58000 & pnadc$V4013 <= 82009] = "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
  #pnadc$atividade[pnadc$V4013 >= 84011 & pnadc$V4013 <= 88000] = "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
  #pnadc$atividade[pnadc$V4013 >= 97000 & pnadc$V4013 <= 97000] = "Serviços domésticos"
  
  # Grandes regiões
  pnadc$regioes = factor(
    substr(pnadc$UPA, 1, 1),
    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  )
  
  # Escolaridade
  pnadc$escolaridade = as.numeric(gsub("([0-9]+).*$", "\\1", pnadc$VD3005))
  #pnadc %>% mutate(escolaridade=parse_number(escolaridade))
  
  return (pnadc)
  
}
