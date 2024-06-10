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
  
  inicial <- ncol(pnadc)
  
  #Pesos calibrados
  pnadc$pesoscalibrados = pnadc$V1028
  
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
  pnadc$faixa_etaria[pnadc$idade>=14 & pnadc$idade<=24] = "Jovem"
  pnadc$faixa_etaria[pnadc$idade>=25 & pnadc$idade<=29] = "Junior"
  pnadc$faixa_etaria[pnadc$idade>=30 & pnadc$idade<=49] = "Pleno"
  pnadc$faixa_etaria[pnadc$idade>=50 & pnadc$idade<=64] = "Senior"
  pnadc$faixa_etaria[pnadc$idade>=65] = "Idoso"
  
  
  # Renda média habitual de todos os trabalhos (VD4019)
  pnadc$rendahabtotal = pnadc$VD4019 * pnadc$Habitual 
  pnadc$rendaefetotal = pnadc$VD4020 * pnadc$Efetivo
  
  # Renda média habitual do trabalho principal (VD4016)
  pnadc$rendahabprincipal = pnadc$VD4016 * pnadc$Habitual 
  pnadc$rendaefeprincipal = pnadc$VD4017 * pnadc$Efetivo 
  
  # Renda média habitual do trabalho principal (VD4016)
  #pnadc$rendatrabtotal = pnadc$VD4016 * pnadc$Habitual 
  
  
  # Pessoas na força de trabalho
  pnadc$forca = pnadc$VD4001
  
  # Pessoas ocupadas e não ocupadas
  pnadc$ocupadas = pnadc$VD4002
  
  # Contribuição previdenciária
  pnadc$contribuicao = pnadc$VD4012
  
  # Horas habitualmente trabalhadas
  pnadc$horashabprincipal = pnadc$V4039
  pnadc$horasefeprincipal = pnadc$V4039C
  pnadc$horashabtotal = pnadc$VD4031
  pnadc$horasefetotal = pnadc$VD4032
  
  # Posição na ocupação
  pnadc$posicao = pnadc$V4010
  
  pnadc$posicao[as.numeric(pnadc$V4010) >= 1111 & as.numeric(pnadc$V4010) <= 1439] = "Dirigentes e gerentes"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 2111 & as.numeric(pnadc$V4010) <= 2659] = "Profissionais das ciências e intelectuais"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 3111 & as.numeric(pnadc$V4010) <= 3522] = "Técnicos e profissionais de nível médio"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 4110 & as.numeric(pnadc$V4010) <= 4419] = "Trabalhadores de apoio administrativo"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 5111 & as.numeric(pnadc$V4010) <= 5419] = "Trabalhadores dos serviços, vendedores dos comércios e mercados"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 6111 & as.numeric(pnadc$V4010) <= 6225] = "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 7111 & as.numeric(pnadc$V4010) <= 7549] = "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 8111 & as.numeric(pnadc$V4010) <= 8350] = "Operadores de instalações e máquinas e montadores"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 9111 & as.numeric(pnadc$V4010) <= 9629] = "Ocupações elementares"
  pnadc$posicao[as.numeric(pnadc$V4010) >= 110 & as.numeric(pnadc$V4010) <= 512] = "Membros das forças armadas, policiais e bombeiros militares"
  
  
  # Função
  pnadc$funcao = pnadc$V4012
  
  # Especificação
  pnadc$especificacao = pnadc$VD4009
  
  # CNPJ
  pnadc$CNPJ = pnadc$V4019
  
  # Atividade no trabalho principal
  pnadc$atividade = pnadc$V4013
  
  pnadc$atividade[as.numeric(pnadc$V4013) >=  1101 & as.numeric(pnadc$V4013) <=  3002] <- "Agricultura, pecuária, produção florestal, pesca e aquicultura"
  pnadc$atividade[as.numeric(pnadc$V4013) >=  5000 & as.numeric(pnadc$V4013) <= 39000] <- "Indústria"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 41000 & as.numeric(pnadc$V4013) <= 43000] <- "Construção"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 45010 & as.numeric(pnadc$V4013) <= 48100] <- "Comércio, reparação de veículos automotores e motocicletas"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 49010 & as.numeric(pnadc$V4013) <= 53002] <- "Transporte, armazenagem e correio"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 55000 & as.numeric(pnadc$V4013) <= 56020] <- "Alojamento e alimentação"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 58000 & as.numeric(pnadc$V4013) <= 82009] <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 84011 & as.numeric(pnadc$V4013) <= 88000] <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 90000 & as.numeric(pnadc$V4013) <= 96090 | as.numeric(pnadc$V4013) == 99000] <- "Outros serviços"
  pnadc$atividade[as.numeric(pnadc$V4013) >= 97000 & as.numeric(pnadc$V4013) <= 97000] <- "Serviços domésticos"
  
  pnadc$grupamento = pnadc$atividade
   
  # Divisão em grupamentostividade no trabalho principal - correspondência Isic 4 x CNAE20
  pnadc$grupamento[as.numeric(pnadc$V4013) >=  1000 & as.numeric(pnadc$V4013) <=  3220] <- "A: Agricultura, pecuária, produção florestal, pesca e aquicultura"
  pnadc$grupamento[as.numeric(pnadc$V4013) >=  5000 & as.numeric(pnadc$V4013) <=  9900] <- "B: Indústrias extrativas"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 10000 & as.numeric(pnadc$V4013) <= 33200] <- "C: Indústrias de transformação"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 35000 & as.numeric(pnadc$V4013) <= 35300] <- "D: Eletricidade e gás"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 36000 & as.numeric(pnadc$V4013) <= 39000] <- "E: Água, esgoto, atividades de gestão de resíduos e descontaminação"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 41000 & as.numeric(pnadc$V4013) <= 43900] <- "F: Construção"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 45000 & as.numeric(pnadc$V4013) <= 47990] <- "G: Comércio; reparação de veículos automotores e motocicletas"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 49000 & as.numeric(pnadc$V4013) <= 53200] <- "H: Transporte, armazenagem e correio"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 55000 & as.numeric(pnadc$V4013) <= 56300] <- "I: Alojamento e alimentação"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 58000 & as.numeric(pnadc$V4013) <= 63990] <- "J: Informação e comunicação"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 64000 & as.numeric(pnadc$V4013) <= 66300] <- "K: Atividades financeiras, de seguro e serviços relacionados"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 68000 & as.numeric(pnadc$V4013) <= 68200] <- "L: Atividades imobiliárias"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 69000 & as.numeric(pnadc$V4013) <= 75000] <- "M: Atividades profissionais, científicas e técnicas"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 77000 & as.numeric(pnadc$V4013) <= 82990] <- "N: Atividades administrativas e serviços complementares"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 84000 & as.numeric(pnadc$V4013) <= 84300] <- "O: Administração pública, defesa e seguridade social"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 85000 & as.numeric(pnadc$V4013) <= 85500] <- "P: Educação"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 86000 & as.numeric(pnadc$V4013) <= 88900] <- "Q: Saúde humana e serviços sociais"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 90000 & as.numeric(pnadc$V4013) <= 93290] <- "R: Artes, cultura, esporte e recreação"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 94000 & as.numeric(pnadc$V4013) <= 96090] <- "S: Outras atividades de serviço"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 97000 & as.numeric(pnadc$V4013) <= 98200] <- "T: Serviços domésticos"
  pnadc$grupamento[as.numeric(pnadc$V4013) >= 99000 & as.numeric(pnadc$V4013) <= 99000] <- "U: Organismos internacionais e outras instituições extraterritoriais"
  pnadc$grupamento[as.numeric(pnadc$V4013) == 00000] <- "V: Atividades maldefinidas"
  
  
  
  # Grandes regiões
  pnadc$regioes = factor(
    substr(pnadc$UPA, 1, 1),
    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
  )
  
  # Escolaridade
  pnadc$escolaridade = as.numeric(gsub("([0-9]+).*$", "\\1", pnadc$VD3005))
  #pnadc %>% mutate(escolaridade=parse_number(escolaridade))
  
  final <- ncol(pnadc)
  
  n <- final - inicial
  
  pnadc = pnadc[,(ncol(pnadc)-n-1):ncol(pnadc)]
  
  return (pnadc)
  
}
