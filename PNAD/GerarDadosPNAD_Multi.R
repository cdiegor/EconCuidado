

taxas <- function(pnadc)
{
  
  total_lsir        <- sum(pnadc$pesoscalibrados)
  ocupadas_lsir     <- sum(subset(pnadc, (pnadc$ocupadas=="Pessoas ocupadas") & (pnadc$forca=="Pessoas na força de trabalho") )$pesoscalibrados )  
  forca_lsir        <- sum(subset(pnadc,  (pnadc$forca=="Pessoas na força de trabalho") )$pesoscalibrados )
  taxa_lsir         <- 100 * (1 - ocupadas_lsir/forca_lsir)
  nivel_lsir        <- 100*ocupadas_lsir/total_lsir
  participacao_lsir <- 100*forca_lsir/total_lsir
  
  return (paste(nrow(pnadc), ";",
    format(round(total_lsir, 0), nsmall = 0), ";", 
    format(round(ocupadas_lsir, 0), nsmall = 0), ";", 
    format(round(forca_lsir, 0), nsmall = 0), ";", 
    format(round(taxa_lsir, 2), nsmall = 0), ";", 
    format(round(nivel_lsir, 2), nsmall = 2), ";", 
    format(round(participacao_lsir, 2), nsmall = 2)))
}

numeros <- function(pnadc)
{
  
  total_lsir        <- sum(pnadc$pesoscalibrados)
  ocupadas_lsir     <- sum(subset(pnadc, (pnadc$ocupadas=="Pessoas ocupadas") & (pnadc$forca=="Pessoas na força de trabalho") )$pesoscalibrados )  
  forca_lsir        <- sum(subset(pnadc,  (pnadc$forca=="Pessoas na força de trabalho") )$pesoscalibrados )

  return (paste(nrow(pnadc), ";",
                format(round(total_lsir, 0), nsmall = 0), ";", 
                format(round(ocupadas_lsir, 0), nsmall = 0), ";", 
                format(round(forca_lsir, 0), nsmall = 0)))
}


rendas <- function(pnadc)
{
  
  total_lsir <- nrow(subset(pnadc))
  renda_media_hab_principal <- weighted.mean(pnadc$rendahabprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_media_efe_principal <- weighted.mean(pnadc$rendaefeprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_media_hab_total <- weighted.mean(pnadc$rendahabtotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_media_efe_total <- weighted.mean(pnadc$rendaefetotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
   
  horas_hab_principal <- weighted.mean(pnadc$horashabprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  horas_efe_principal <- weighted.mean(pnadc$horasefeprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  horas_hab_total <- weighted.mean(pnadc$horashabtotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  horas_efe_total <- weighted.mean(pnadc$horasefetotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  
  renda_dom_efe_total <- weighted.mean(pnadc$Renda_dom, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_dom_efe_percapita <- weighted.mean(pnadc$Renda_per_capita, w = pnadc$pesoscalibrados, na.rm = TRUE)
  escolaridade <- weighted.mean(pnadc$escolaridade, w = pnadc$pesoscalibrados, na.rm = TRUE)

  return (paste(
    format(round(renda_media_hab_principal, 2), nsmall = 2), ";", 
    format(round(renda_media_efe_principal, 2), nsmall = 2), ";",
    format(round(renda_media_hab_total, 2), nsmall = 2), ";",
    format(round(renda_media_efe_total, 2), nsmall = 2), ";",
    format(round(horas_hab_principal, 2), nsmall = 2), ";", 
    format(round(horas_efe_principal, 2), nsmall = 2), ";",
    format(round(horas_hab_total, 2), nsmall = 2), ";", 
    format(round(horas_efe_total, 2), nsmall = 2), ";",
    format(round(renda_dom_efe_total, 2), nsmall = 2), ";",
    format(round(renda_dom_efe_percapita, 2), nsmall = 2), ";",
    format(round(escolaridade, 2), nsmall = 2)))
  
}  
  
taxas_desocupados <- function(pnadc)
{
  total_lsir        <- sum(pnadc$pesoscalibrados)
  meses             <- weighted.mean(pnadc$temposemtrabalhar, w=pnadc$pesoscalibrados, na.rm = TRUE)
  return (paste(nrow(pnadc), ";",
                format(round(total_lsir, 0), nsmall = 0), ";",
                format(round(meses, 2), nsmall = 2) ) )
  
}
rendas_desocupados <- function(pnadc)
{
  renda_dom_efe_total <- weighted.mean(pnadc$Renda_dom, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_dom_efe_percapita <- weighted.mean(pnadc$Renda_per_capita, w = pnadc$pesoscalibrados, na.rm = TRUE)
  escolaridade <- weighted.mean(pnadc$escolaridade, w = pnadc$pesoscalibrados, na.rm = TRUE)
  
  return (paste(
    format(round(renda_dom_efe_total, 2), nsmall = 2), ";",
    format(round(renda_dom_efe_percapita, 2), nsmall = 2), ";",
    format(round(escolaridade, 2), nsmall = 2)))
  
}
  



gerar_planilha_base <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará", "Pernambuco", "Bahia", "Minas Gerais", "São Paulo", "Norte", "Nordeste", "Sul", "Sudeste", "Centro-Oeste", "Município de Fortaleza (CE)
")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;",
               "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;",
               "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade",  "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local
      pnsubl <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )

      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubs <- subset(pnsubl,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          if (faixa_etaria_ == "15 a 29")
          {
            pnsubf <- subset(pnsubs,  (idade >= 15 & idade <=29 ) )
          }
          else
          {
            pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          }
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubn <- subset(pnsubf,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubr <- subset(pnsubn,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubr <- subset(pnsubn,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubr <- subset(pnsubn,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                          taxas(pnsubr),";", 
                          rendas(pnsubr),";",
                          "\n" ),
                   file = arquivo_saida,
                   append = TRUE)
            }
          }
        }
      }
    }  
  }
}


gerar_atividades <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais", "15 a 29")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  todas_contribuicoes = c("", "Contribuinte", "Não contribuinte")
  todas_posicoes = c("",
                     "Dirigentes e gerentes", 
                     "Profissionais das ciências e intelectuais", 
                     "Técnicos e profissionais de nível médio", 
                     "Trabalhadores de apoio administrativo", 
                     "Trabalhadores dos serviços, vendedores dos comércios e mercados", 
                     "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca", 
                     "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios", 
                     "Operadores de instalações e máquinas e montadores", 
                     "Ocupações elementares",
                     "Membros das forças armadas, policiais e bombeiros militares"
                     )
  
  todas_atividades = c("",
                       "Agricultura, pecuária, produção florestal, pesca e aquicultura",
                       "Indústria",
                       "Construção",
                       "Comércio, reparação de veículos automotores e motocicletas",
                       "Transporte, armazenagem e correio",
                       "Alojamento e alimentação",
                       "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas",
                       "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                       "Outros serviços",
                       "Serviços domésticos")
  todos_grupamentos = c("",
                       "A: Agricultura, pecuária, produção florestal, pesca e aquicultura",
                       "B: Indústrias extrativas",
                       "C: Indústrias de transformação",
                       "D: Eletricidade e gás",
                       "E: Água, esgoto, atividades de gestão de resíduos e descontaminação",
                       "F: Construção",
                       "G: Comércio, reparação de veículos automotores e motocicletas",
                       "H: Transporte, armazenagem e correio",
                       "I: Alojamento e alimentação",
                       "J: Informação e comunicação",
                       "K: Atividades financeiras, de seguro e serviços relacionados",
                       "L: Atividades imobiliárias",
                       "M: Atividades profissionais, científicas e técnicas",
                       "N: Atividades administrativas e serviços complementares",
                       "O: Administração pública, defesa e seguridade social",
                       "P: Educação",
                       "Q: Saúde humana e serviços sociais",
                       "R: Artes, cultura, esporte e recreação",
                       "S: Outras atividades de serviço",
                       "T: Serviços domésticos",
                       "U: Organismos internacionais e outras instituições extraterritoriais",
                       "V: Atividades maldefinidas"
                       )
  
  #todas_atividades = c("")
  todas_posicoes = c("")
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Contribuição previdenciária?;", "Principal atividade;", "Grupamento;", 
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade",  "\n"),
        file = arquivo_saida,
        append = FALSE )

  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      pnsubl <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubs <- subset(pnsubl,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          if (faixa_etaria_ == "15 a 29")
          {
            pnsubf <- subset(pnsubs,  (idade >= 15 & idade <=29 ) )
          }
          else
          {
            pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          }
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubn <- subset(pnsubf,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubr <- subset(pnsubn,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubr <- subset(pnsubn,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubr <- subset(pnsubn,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (contribuicao_ in todas_contribuicoes)
              {
                # Contribuição previdenciária    
                pnsubc <- subset(pnsubr,  ( (contribuicao_ == "") | (contribuicao == contribuicao_ ) ) )
                
                for (atividade_ in c("",
                                     "Indústria",
                                     "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas",
                                     "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                                     "Outros serviços"))
                {
                  grupamento_ <- ""
                  
                  pnsuba <- subset(pnsubc,  ( (atividade_ == "") | (atividade == atividade_ ) ) )  
                  if (local_ == "") local_ <- "Brasil"
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              contribuicao_, ";", atividade_, ";", grupamento_, ";",
                              taxas(pnsuba),";", 
                              rendas(pnsuba),";",
                              "\n" ),
                       file = arquivo_saida,
                       append = TRUE)
                  
                }
                #{
                # Setor econômico da atividade
                #pnsuba <- subset(pnsubp,  ( (atividade_ == "") | (atividade == atividade_ ) ) )  
                for (grupamento_ in todos_grupamentos)
                {
                  atividade_ = ""
                  if (grupamento_ == "A: Agricultura, pecuária, produção florestal, pesca e aquicultura") atividade_ <- "Agricultura, pecuária, produção florestal, pesca e aquicultura"
                  if (grupamento_ == "B: Indústrias extrativas") atividade_ <- "Indústria"
                  if (grupamento_ == "C: Indústrias de transformação") atividade_ <- "Indústria"
                  if (grupamento_ == "D: Eletricidade e gás") atividade_ <- "Indústria"
                  if (grupamento_ == "E: Água, esgoto, atividades de gestão de resíduos e descontaminação") atividade_ <- "Indústria"
                  if (grupamento_ == "F: Construção") atividade_ <- "Construção"
                  if (grupamento_ == "G: Comércio, reparação de veículos automotores e motocicletas") atividade_ <- "Comércio, reparação de veículos automotores e motocicletas"
                  if (grupamento_ == "H: Transporte, armazenagem e correio") atividade_ <- "Transporte, armazenagem e correio"
                  if (grupamento_ == "I: Alojamento e alimentação") atividade_ <- "Alojamento e alimentação"
                  if (grupamento_ == "J: Informação e comunicação") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas" 
                  if (grupamento_ == "K: Atividades financeiras, de seguro e serviços relacionados") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                  if (grupamento_ == "L: Atividades imobiliárias") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                  if (grupamento_ == "M: Atividades profissionais, científicas e técnicas") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                  if (grupamento_ == "N: Atividades administrativas e serviços complementares") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                  if (grupamento_ == "O: Administração pública, defesa e seguridade social") atividade_ <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
                  if (grupamento_ == "P: Educação") atividade_ <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
                  if (grupamento_ == "Q: Saúde humana e serviços sociais") atividade_ <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
                  if (grupamento_ == "R: Artes, cultura, esporte e recreação") atividade_ <- "Outros serviços"
                  if (grupamento_ == "S: Outras atividades de serviço") atividade_ <- "Outros serviços"
                  if (grupamento_ == "T: Serviços domésticos") atividade_ <- "Serviços domésticos"
                  if (grupamento_ == "U: Organismos internacionais e outras instituições extraterritoriais") atividade_ <- "Outros serviços"
                  if (grupamento_ == "V: Atividades maldefinidas") atividade_ <- "Atividades maldefinidas"
                  
                  pnsubg <- subset(pnsubc,  ( (grupamento_ == "") | (grupamento == grupamento_ ) ) )  
                  if (local_ == "") local_ <- "Brasil"
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              contribuicao_, ";", atividade_, ";", grupamento_, ";",
                              taxas(pnsubg),";", 
                              rendas(pnsubg),";",
                              "\n" ),
                       file = arquivo_saida,
                       append = TRUE)
                  
                }
                #}      
              }
            }
          }
        }
      }
    }  
  }
}

todas_as_funcoes <- function(pnads)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais", "15 a 29")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  
  todas_funcoes = c("", 
                    "Trabalhador doméstico",
                    "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar",
                    "Empregado do setor privado",
                    "Empregado do setor público (inclusive empresas de economia mista)",
                    "Empregador",
                    "Conta própria",
                    "Trabalhador familiar não remunerado",
                    "Não aplicável")
  
  todas_especificacoes = c("",
                           "Empregado no setor privado com carteira de trabalho assinada",
                           "Empregado no setor privado sem carteira de trabalho assinada",
                           "Trabalhador doméstico com carteira de trabalho assinada",
                           "Trabalhador doméstico sem carteira de trabalho assinada",
                           "Empregado no setor público com carteira de trabalho assinada",
                           "Empregado no setor público sem carteira de trabalho assinada",
                           "Militar e servidor estatutário",
                           "Empregador",
                           "Conta-própria",
                           "Trabalhador familiar auxiliar")
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;","Raça;", 
                "Função;", "Especificação;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda efetiva total;", "Horas trabalhadas efet total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade",  "\n"),
        file = "resultados_funcoes.csv",
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          if (faixa_etaria_ == "15 a 29")
          {
            pnsubfaixa <- subset(pnsubsexo,  (idade >= 15 & idade <=29 ) )
          }
          else
          {
            pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          }
          
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (funcao_ in todas_funcoes)
              {
                # Posição da ocupação
                pnsubfuncao <- subset(pnsubraca,  ( (funcao_ == "") | (funcao == funcao_ ) ) )
                
                if (funcao_ == "Empregador" | funcao_ == "Conta própria")
                {
                  especificacoes = c("", "Sim", "Não")
                  for (especificacao_ in especificacoes)
                  {
                    pnsubespe <- subset(pnsubfuncao,  ( (especificacao_ == "") | (CNPJ == especificacao_ ) ) )
                    if (especificacao_ == "Sim") especificacao_ <- "Com CNPJ"
                    if (especificacao_ == "Não") especificacao_ <- "Sem CNPJ"
                    if (local_ == "")
                      local <- "Brasil"
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                                funcao_, ";", especificacao_, ";", 
                                taxas(pnsubespe),";", 
                                rendas(pnsubespe),";",
                                "\n" ),
                         file = "resultados_funcoes.csv",
                         append = TRUE)
                    
                  }                
                }
                else
                {
                  if (funcao_ == "Trabalhador doméstico")
                  {
                    todas_especificacoes = c("",
                                             "Trabalhador doméstico com carteira de trabalho assinada",
                                             "Trabalhador doméstico sem carteira de trabalho assinada")                
                  }
                  if (funcao_ == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar")
                  {
                    todas_especificacoes = c("Militar e servidor estatutário")                
                  }
                  if (funcao_ == "Empregado do setor privado")
                  {
                    todas_especificacoes = c("",
                                             "Empregado no setor privado com carteira de trabalho assinada",
                                             "Empregado no setor privado sem carteira de trabalho assinada")
                  }
                  if (funcao_ == "Empregado do setor público (inclusive empresas de economia mista)")
                  {
                    todas_especificacoes = c("",
                                             "Empregado no setor público com carteira de trabalho assinada",
                                             "Empregado no setor público sem carteira de trabalho assinada",
                                             "Militar e servidor estatutário")
                  }
                  if (funcao_ == "Trabalhador familiar não remunerado")
                  {
                    todas_especificacoes = c("",
                                             "Trabalhador familiar auxiliar")                
                  }
                  
                  for (especificacao_ in todas_especificacoes)
                  {
                    pnsubespe <- subset(pnsubfuncao,  ( (especificacao_ == "") | (especificacao == especificacao_ ) ) )
                    if (local_ == "") local_ <- "Brasil"
                    if (funcao_ == "Empregado do setor público (inclusive empresas de economia mista)" &
                        especificacao_ == "Militar e servidor estatutário") especificacao_ <- "Servidor estatutário"
                    if (funcao_ == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar")
                      especificacao_ <- "Militar"
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                funcao_, ";", especificacao_, ";", 
                                taxas(pnsubespe),";", 
                                rendas(pnsubespe),";",
                                "\n" ),
                         file = "resultados_funcoes.csv",
                         append = TRUE)
                    
                  }
                }
              }
            }
          }
        }
      }
    }  
  }
}

todas_as_posicoes <- function(pnads)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais", "15 a 29")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  todas_contribuicoes = c("", "Contribuinte", "Não contribuinte")
  todas_posicoes = c("",
                     "Dirigentes e gerentes", 
                     "Profissionais das ciências e intelectuais", 
                     "Técnicos e profissionais de nível médio", 
                     "Trabalhadores de apoio administrativo", 
                     "Trabalhadores dos serviços, vendedores dos comércios e mercados", 
                     "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca", 
                     "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios", 
                     "Operadores de instalações e máquinas e montadores", 
                     "Ocupações elementares",
                     "Membros das forças armadas, policiais e bombeiros militares"
  )
  

  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Contribuição previdenciária?;", "Posição;", 
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual total;", "Horas trabalhadas hab total;", "Escolaridade",  "\n"),
        file = "resultados_posicoes.csv",
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      pnsubl <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubs <- subset(pnsubl,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          if (faixa_etaria_ == "15 a 29")
          {
            pnsubf <- subset(pnsubs,  (idade >= 15 & idade <=29 ) )
          }
          else
          {
            pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          }
          
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubn <- subset(pnsubf,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubr <- subset(pnsubn,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubr <- subset(pnsubn,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubr <- subset(pnsubn,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (contribuicao_ in todas_contribuicoes)
              {
                # Contribuição previdenciária    
                pnsubc <- subset(pnsubr,  ( (contribuicao_ == "") | (contribuicao == contribuicao_ ) ) )
                
                for (posicao_ in todas_posicoes)
                {
                  #Setor econômico da atividade
                  pnsubp <- subset(pnsubc,  ( (posicao_ == "") | (posicao == posicao_ ) ) )  
                  
                  if (local_ == "") local_ <- "Brasil"
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              contribuicao_, ";", posicao_, ";",
                              taxas(pnsubp),";", 
                              rendas(pnsubp),";",
                              "\n" ),
                       file = "resultados_posicoes.csv",
                       append = TRUE)
                  
                }
                #}      
              }
            }
          }
        }
      }
    }  
  }
}


gerar_posicao_na_ocupacao <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará", "Bahia", "Pernambuco", "Rio Grande do Norte")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais", "15 a 29")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  posicao_na_ocupacao = c("", "Empregado", "Empregador", "Conta própria", "Trabalhador doméstico","Trabalhador familiar auxiliar", "Formal", "Informal")
  situacao_empregado = c("", "Setor privado com carteira", "Setor privado sem carteira",
                         "Setor público com carteira", "Setor público sem carteira", "Militar e servidor estatutário")
  situacao_domestico = c("", "Trabalhador doméstico com carteira", "Trabalhador doméstico sem carteira")
  situacao_empregador = c("", "Com CNPJ", "Sem CNPJ")
  situacao_contapropria = c("", "Com CNPJ", "Sem CNPJ")
  
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Posição;", "Situação;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade",  "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      
      pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          if (faixa_etaria_ == "15 a 29")
          {
            pnsubfaixa <- subset(pnsubsexo,  (idade >= 15 & idade <=29 ) )
          }
          else
          {
            pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          }
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (posicao_ in posicao_na_ocupacao)
              {
                
                if (posicao_ == "")
                {
                  situacao_ <- ""
                  pnsubsit <- pnsubraca
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              posicao_, ";", situacao_, ";", 
                              taxas(pnsubsit),";", 
                              rendas(pnsubsit),";",
                              "\n" ),
                       file = arquivo_saida,
                       append = TRUE)
                }
                
                else if (posicao_ == "Empregado")
                {
                  for (situacao_ in situacao_empregado)
                  {
                    
                    if (situacao_ == "Setor privado com carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Empregado no setor privado com carteira de trabalho assinada") )
                    }
                    else if (situacao_ == "Setor privado sem carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Empregado no setor privado sem carteira de trabalho assinada") )
                    }
                    else if (situacao_ == "Setor público com carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Empregado no setor público com carteira de trabalho assinada") )
                    }
                    else if (situacao_ == "Setor público sem carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Empregado no setor público sem carteira de trabalho assinada") )
                    }
                    else if (situacao_ == "Militar e servidor estatutário")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Militar e servidor estatutário") )
                    }
                    else
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Trabalhador doméstico") |
                                           (funcao == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar") |
                                           (funcao == "Empregado do setor privado") |
                                           (funcao == "Empregado do setor público (inclusive empresas de economia mista)") )
                    }
                    
                    
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                                posicao_, ";", situacao_, ";", 
                                taxas(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
                else if (posicao_ == "Empregador")
                {
                  for (situacao_ in situacao_empregador)
                  {
                    if (situacao_ == "Com CNPJ")
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Empregador") & (CNPJ == "Sim") )
                    }
                    else if (situacao_ == "Sem CNPJ")
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Empregador") & (CNPJ == "Não") )
                    }
                    else
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Empregador") )
                    }
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                                posicao_, ";", situacao_, ";", 
                                taxas(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
                else if (posicao_ == "Conta própria")
                {
                  for (situacao_ in situacao_empregador)
                  {
                    if (situacao_ == "Com CNPJ")
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Conta própria") & (CNPJ == "Sim") )
                    }
                    else if (situacao_ == "Sem CNPJ")
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Conta própria") & (CNPJ == "Não") )
                    }
                    else
                    {
                      pnsubsit <- subset((pnsubraca), (funcao == "Conta própria") )
                    }
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                                posicao_, ";", situacao_, ";", 
                                taxas(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
                else if (posicao_ == "Trabalhador doméstico")
                {
                  for (situacao_ in situacao_domestico)
                  {
                    if (situacao_ == "Trabalhador doméstico com carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") )
                    }
                    else if (situacao_ == "Trabalhador doméstico sem carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                    }
                    else
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") | 
                                                        (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                    }
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                                posicao_, ";", situacao_, ";", 
                                taxas(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
                else if (posicao_ == "Trabalhador familiar auxiliar")
                {
                  situacao_ <- ""
                  pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador familiar auxiliar") )
                  
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              posicao_, ";", situacao_, ";", 
                              taxas(pnsubsit),";", 
                              rendas(pnsubsit),";",
                              "\n" ),
                       file = arquivo_saida,
                       append = TRUE)
                  
                }
                else if (posicao_ == "Formal")
                {
                  situacao_ <- ""
                  pnsubsit <- subset((pnsubraca), (especificacao == "Empregado no setor privado com carteira de trabalho assinada") |
                                                  (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") |
                                                  (especificacao == "Empregado no setor público com carteira de trabalho assinada") |
                                                  (especificacao == "Empregado no setor público sem carteira de trabalho assinada") |
                                                  (especificacao == "Militar e servidor estatutário") |
                                                  (especificacao == "Empregador" & CNPJ == "Sim") |
                                                  (especificacao == "Conta-própria" & CNPJ == "Sim"))
                  
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              posicao_, ";", situacao_, ";", 
                              taxas(pnsubsit),";", 
                              rendas(pnsubsit),";",
                              "\n" ),
                       file = arquivo_saida,
                       append = TRUE)
                }
                else if (posicao_ == "Informal")
                {
                  situacao_ <- ""
                  pnsubsit <- subset((pnsubraca), (especificacao == "Empregado no setor privado sem carteira de trabalho assinada") |
                                                  (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") |
                                                  (especificacao == "Empregador" & CNPJ == "Não") |
                                                  (especificacao == "Conta-própria" & CNPJ == "Não") |
                                                  (especificacao == "Trabalhador familiar auxiliar") )
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                              posicao_, ";", situacao_, ";", 
                              taxas(pnsubsit),";", 
                              rendas(pnsubsit),";",
                              "\n" ),
                       file = arquivo_saida,
                       append = TRUE)
                }
              }
            }
          }  
        }
      }
    }  
  }
}


gerar_trabalhadores_domesticos <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe","Bahia", "Nordeste")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  posicao_na_ocupacao = c("Trabalhador doméstico")
  situacao_domestico = c("", "Trabalhador doméstico com carteira", "Trabalhador doméstico sem carteira")

  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Posição;", "Situação;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade (anos)",
               "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      if (local_ == "Nordeste")
      {
        pnsublocal <- subset(pnsub, (pnsub$regiao == local_))
      }
      else
      {
        pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
      }
      
      if (local_ == "") local <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (posicao_ in posicao_na_ocupacao)
              {
                
                if (posicao_ == "Trabalhador doméstico")
                {
                  for (situacao_ in situacao_domestico)
                  {
                    if (situacao_ == "Trabalhador doméstico com carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") )
                    }
                    else if (situacao_ == "Trabalhador doméstico sem carteira")
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                    }
                    else
                    {
                      pnsubsit <- subset((pnsubraca), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") | 
                                           (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                    }
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                                posicao_, ";", situacao_, ";", 
                                taxas(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
              }
            }
          }  
        }
      }
    }  
  }
}

gerar_trabalhadores_cuidadores <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe","Bahia", "Nordeste")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  CODs        = c("5311", "5312", "5321", "5322", "5329", "Geral", "")
  Denominacao = c("Cuidadores de crianças", 
                  "Ajudantes de professores", 
                  "Trabalhadores de cuidados pessoais em instituições", 
                  "Trabalhadores de cuidados pessoais a domicílios", 
                  "Trabalhadores de cuidados pessoais nos serviços de saúde não classificados anteriormente",
                  "")
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Situação;",  "COD;", "Denominação;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade (anos)",
               "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }

              for (i in  1:length(CODs))
              {
                # Domésticos e externos
                cod_ <- CODs[i]
                denominacao_ <- Denominacao[i]

                if (CODs[i] == "Geral")
                {
                  pnsubcod <- subset(pnsubraca)
                }
                else if (CODs[i] == "")
                {
                  pnsubcod <- subset((pnsubraca), (COD == "5311" |
                                                   COD == "5312" |
                                                   COD == "5321" | 
                                                   COD == "5322" |
                                                   COD == "5329")  )
                  cod_ = "Agregado"
                }
                else
                {
                  pnsubcod <- subset((pnsubraca), (pnsubraca$COD == cod_))
                }
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                            ";", cod_, ";", denominacao_, ";", 
                            taxas(pnsubcod),";", 
                            rendas(pnsubcod),";",
                            "\n" ),
                     file = arquivo_saida,
                     append = TRUE)
              }
              for (i in  1:length(CODs))
              {
                # Apenas domésticos
                cod_ <- CODs[i]
                denominacao_ <- Denominacao[i]
                
                if (CODs[i] == "Geral")
                {
                  pnsubcod <- subset((pnsubraca), (funcao == "Trabalhador doméstico"))
                }
                else if (CODs[i] == "")
                {
                  pnsubcod <- subset((pnsubraca), (COD == "5311" |
                                                   COD == "5312" |
                                                   COD == "5321" | 
                                                   COD == "5322" |
                                                   COD == "5329")  &  funcao == "Trabalhador doméstico")
                  cod_ = "Agregado"
                }
                else
                {
                  pnsubcod <- subset((pnsubraca), (pnsubraca$COD == cod_ &  funcao == "Trabalhador doméstico"))
                }
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                            "Trabalhador doméstico;", cod_, ";", denominacao_, ";", 
                            taxas(pnsubcod),";", 
                            rendas(pnsubcod),";",
                            "\n" ),
                     file = arquivo_saida,
                     append = TRUE)
              }
              
              for (i in 1:length(CODs))
              {
                # Apenas externos
                cod_ <- CODs[i]
                denominacao_ <- Denominacao[i]
                if (CODs[i] == "Geral")
                {
                  pnsubcod <- subset((pnsubraca), (funcao != "Trabalhador doméstico"))
                }
                else if (CODs[i] == "")
                {
                  pnsubcod <- subset((pnsubraca), (COD == "5311" |
                                                   COD == "5312" |
                                                   COD == "5321" | 
                                                   COD == "5322" |
                                                   COD == "5329") &  funcao != "Trabalhador doméstico")
                  cod_ = "Agregado"
                }
                else
                {
                  pnsubcod <- subset((pnsubraca), (pnsubraca$COD == cod_ &  funcao != "Trabalhador doméstico"))
                }
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                            "Trabalhador externo;", cod_, ";", denominacao_, ";", 
                            taxas(pnsubcod),";", 
                            rendas(pnsubcod),";",
                            "\n" ),
                     file = arquivo_saida,
                     append = TRUE)
              }
              
            }
          }  
        }
      }
    }  
  }
}


gerar_trabalhadores_cuidadores2 <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará", "Nordeste")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  CODs = c("2342", "2352","5120", "5152" , "5164", "5311", "5322", "5414", "6112", "8322", "8350", "9111", "9129", "9214", "9412", "", "Geral")
  Denominacao         = c("Professores do ensino pré-escolar",
                          "Educadores para necessidades especiais",
                          "Cozinheiros", 
                          "Governantas e mordomos domésticos", 
                          "Cuidadores de animais", 
                          "Cuidadores de crianças", 
                          "Trabalhadores de cuidados pessoais a domicílios",
                          "Guardas de segurança",
                          "Agricultores e trabalhadores qualificados no cultivo de hortas, viveiros e jardins",
                          "Condutores de automóveis, taxis e caminhonetes",
                          "Marinheiros de coberta e afins",
                          "Trabalhadores dos serviços domésticos em geral",
                          "Outros trabalhadores de limpeza",
                          "Trabalhadores elementares da jardinagem e horticultura",
                          "Ajudantes de cozinha",
                          "")
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Situação;",  "COD;", "Denominação;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade (anos)",
               "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      if (local_ == "Nordeste")
      {
        pnsublocal <- subset(pnsub, (pnsub$regiao == local_))
      }
      else
      {
        pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
      }
      
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (i in  1:length(CODs))
              {
                # Domésticos e externos
                cod_ <- CODs[i]
                denominacao_ <- Denominacao[i]
                
                if (CODs[i] == "Geral")
                {
                  pnsubcod <- subset(pnsubraca)
                }
                else if (CODs[i] == "")
                {
                  pnsubcod <- subset((pnsubraca), (   COD == "2342" |
                                                      COD == "2352" |
                                                      COD == "5120" |
                                                      COD == "5152" |
                                                      COD == "5164" | 
                                                      COD == "5311" |
                                                      COD == "5312" |
                                                      COD == "5321" |
                                                      COD == "5322" |
                                                      COD == "5329" |
                                                      COD == "5414" |
                                                      COD == "6112" |   
                                                      COD == "8322" | 
                                                      COD == "8350" |
                                                      COD == "9111" |
                                                      COD == "9121" |
                                                      COD == "9129" |
                                                      COD == "9214" |
                                                      COD == "9412"  )  )
                  cod_ = "Agregado"
                }
                else
                {
                  pnsubcod <- subset((pnsubraca), (pnsubraca$COD == cod_))
                }
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                            ";", cod_, ";", denominacao_, ";", 
                            taxas(pnsubcod),";", 
                            rendas(pnsubcod),";",
                            "\n" ),
                     file = arquivo_saida,
                     append = TRUE)
              }
              for (i in  1:length(CODs))
              {
                # Apenas domésticos
                cod_ <- CODs[i]
                denominacao_ <- Denominacao[i]
                
                if (CODs[i] == "Geral")
                {
                  pnsubcod <- subset((pnsubraca), (funcao == "Trabalhador doméstico"))
                }
                else if (CODs[i] == "")
                {
                  pnsubcod <- subset((pnsubraca), (   COD == "2342" |
                                                      COD == "2352" |
                                                      COD == "5120" |
                                                      COD == "5152" |
                                                      COD == "5164" | 
                                                      COD == "5311" |
                                                      COD == "5312" |
                                                      COD == "5321" |
                                                      COD == "5322" |
                                                      COD == "5329" |
                                                      COD == "5414" |
                                                      COD == "6112" |   
                                                      COD == "8322" | 
                                                      COD == "8350" |
                                                      COD == "9111" |
                                                      COD == "9121" |
                                                      COD == "9129" |
                                                      COD == "9214" |
                                                      COD == "9412"  )  &  funcao == "Trabalhador doméstico" )
                  cod_ = "Agregado"
                }
                else
                {
                  pnsubcod <- subset((pnsubraca), (pnsubraca$COD == cod_ &  funcao == "Trabalhador doméstico"))
                }
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                            "Trabalhador doméstico;", cod_, ";", denominacao_, ";", 
                            taxas(pnsubcod),";", 
                            rendas(pnsubcod),";",
                            "\n" ),
                     file = arquivo_saida,
                     append = TRUE)
              }
              
              for (i in 1:length(CODs))
              {
                # Apenas externos
                cod_ <- CODs[i]
                denominacao_ <- Denominacao[i]
                if (CODs[i] == "Geral")
                {
                  pnsubcod <- subset((pnsubraca), (funcao != "Trabalhador doméstico"))
                }
                else if (CODs[i] == "")
                {
                  pnsubcod <- subset((pnsubraca),   (   COD == "2342" |
                                                         COD == "2352" |
                                                         COD == "5120" |
                                                         COD == "5152" |
                                                         COD == "5164" | 
                                                         COD == "5311" |
                                                         COD == "5312" |
                                                         COD == "5321" |
                                                         COD == "5322" |
                                                         COD == "5329" |
                                                         COD == "5414" |
                                                         COD == "6112" |   
                                                         COD == "8322" | 
                                                         COD == "8350" |
                                                         COD == "9111" |
                                                         COD == "9121" |
                                                         COD == "9129" |
                                                         COD == "9214" |
                                                         COD == "9412"  )   &  funcao != "Trabalhador doméstico")
                  cod_ = "Agregado"
                }
                else
                {
                  pnsubcod <- subset((pnsubraca), (pnsubraca$COD == cod_ &  funcao != "Trabalhador doméstico"))
                }
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                            "Trabalhador externo;", cod_, ";", denominacao_, ";", 
                            taxas(pnsubcod),";", 
                            rendas(pnsubcod),";",
                            "\n" ),
                     file = arquivo_saida,
                     append = TRUE)
              }
              
            }
          }  
        }
      }
    }  
  }
}


gerar_desocupados <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
providencia_tomada = c("",
                         "Entrou em contato com empregador (pessoalmente, por telefone, por email ou pelo portal da empresa, inclusive enviando currículo)",
                         "Colocou ou respondeu anúncio de trabalho em jornal ou revista",
                         "Consultou ou inscreveu-se em agência de emprego privada ou sindicato",
                         "Consultou ou inscreveu-se em agência de emprego municipal, estadual ou no Sistema Nacional de Emprego (SINE)",
                         "Fez ou inscreveu-se em concurso",
                         "Consultou parente, amigo ou colega",
                         "Tomou medida para iniciar o próprio negócio (recursos financeiros, local para instalação, equipamentos, legalização etc.)",
                         "Tomou outra providência, especifique:",
                         "Não tomou providência efetiva",
                         "Não aplicável")

  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Providência tomada;",
               "Amostra;", "Pessoas em idade ativa;", "Tempo sem trabalhar;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade (anos)",
               "\n"),
        file = arquivo_saida,
        append = FALSE )
  

  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14 & pnadc$trabalhando == "Não" & pnadc$tomouprovidencia == "Sim")
    
    for (local_ in todos_locais)
    {
      # Local
      pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (providencia_ in providencia_tomada)
              {
                pnsubprov <- subset(pnsubraca, ( (providencia_ == "") | (pnsubraca$providencia == providencia_) ) )
                cat(paste(pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                          providencia_, ";", taxas_desocupados(pnsubprov), ";", rendas_desocupados(pnsubprov),
                          "\n"),
                    file = arquivo_saida,
                    append = TRUE)
                
              }
            }
          }
        }
      }
    }
  }
}

gerar_fora_da_forca <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  providencia_motivo = c("",
                         "Conseguiu proposta de trabalho para começar após a semana de referência",
                         "Estava aguardando resposta de medida tomada para conseguir trabalho",
                         "Não conseguia trabalho adequado",
                         "Não tinha experiência profissional ou qualificação",
                         "Não conseguia trabalho por ser considerado muito jovem ou muito idoso",
                         "Não havia trabalho na localidade",
                         "Tinha que cuidar dos afazeres domésticos, do(s) filho(s) ou de outro(s) parente(s)",
                         "Estava estudando (curso de qualquer tipo ou por conta própria)",
                         "Por problema de saúde ou gravidez",
                         "Outro motivo, especifique",
                         "Não aplicável")
  
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Providência tomada;",
               "Amostra;", "Pessoas em idade ativa;",  
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade (anos)",
               "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14 & pnadc$trabalhando == "Não" & pnadc$tomouprovidencia == "Não")
    
    # Fora da força

    for (local_ in todos_locais)
    {
      # Local
      pnsubl <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (motivo_ in providencia_motivo)
              {
                pnsubprov <- subset(pnsubraca, ( (motivo_ == "") | (pnsubraca$motivo == motivo_) ))
                cat(paste(pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                          motivo_, ";", taxas_desocupados(pnsubprov), ";", rendas_desocupados(pnsubprov),
                          "\n"),
                    file = arquivo_saida,
                    append = TRUE)
                
              }
            }
          }
        }
      }
    }
  }
}


gerar_subocupados <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  providencia_motivo = c("",
                         "Conseguiu proposta de trabalho para começar após a semana de referência ",
                         "Estava aguardando resposta de medida tomada para conseguir trabalho ",
                         "Não conseguia trabalho adequado",
                         "Não tinha experiência profissional ou qualificação",
                         "Não conseguia trabalho por ser considerado muito jovem ou muito idoso",
                         "Não havia trabalho na localidade",
                         "Tinha que cuidar dos afazeres domésticos, do(s) filho(s) ou de outro(s) parente(s) ",
                         "Estava estudando (curso de qualquer tipo ou por conta própria)",
                         "Por problema de saúde ou gravidez ",
                         "Outro motivo, especifique",
                         "Não aplicável")
  
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Nível de instrução;", "Raça;", 
               "Providência tomada;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade (anos)",
               "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      
      pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (nivel_instrucao_ in todos_niveis)
          {
            # Nível de instrução
            pnsubnivel <- subset(pnsubfaixa,  ( (nivel_instrucao_ == "") | (nivel_instrucao == nivel_instrucao_ ) ) ) 
            
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubnivel,  ( (raca_ == "") | (raca == raca_ ) ) )
              }
              
              for (motivo_ in providencia_motivo)
              {
                pnsubprov <- subset(pnsubraca, ( (motivo_ == "") | (pnsubraca$motivo == motivo_) ))
                cat(paste(pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", nivel_instrucao_, ";", raca_, ";", 
                          motivo_, ";", taxas(pnsubprov), ";", rendas(pnsubprov), ";",
                          "\n"),
                    file = arquivo_saida,
                    append = TRUE)
                
              }
            }
          }
        }
      }
    }
  }
}


gerar_pno_cnae <- function(pnads, arquivo_saida)
{
  options(OutDec = ',')
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "14 a 17", "18 a 24", "25 a 39", "40 a 59", "60 ou mais")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  todos_niveis = c("", "Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto ou equivalente",
                   "Fundamental completo ou equivalente", "Médio incompleto ou equivalente", "Médio completo ou equivalente",
                   "Superior incompleto ou equivalente", "Superior completo")
  
  posicao_na_ocupacao = c("", "Empregado", "Empregador", "Conta própria", "Trabalhador doméstico","Trabalhador familiar auxiliar", "Formal", "Informal")
  situacao_empregado = c("", "Setor privado com carteira", "Setor privado sem carteira",
                         "Setor público com carteira", "Setor público sem carteira", "Militar e servidor estatutário")
  situacao_domestico = c("", "Trabalhador doméstico com carteira", "Trabalhador doméstico sem carteira")
  situacao_empregador = c("", "Com CNPJ", "Sem CNPJ")
  situacao_contapropria = c("", "Com CNPJ", "Sem CNPJ")
  

  todas_atividades = c("",
                       "Agricultura, pecuária, produção florestal, pesca e aquicultura",
                       "Indústria",
                       "Construção",
                       "Comércio, reparação de veículos automotores e motocicletas",
                       "Transporte, armazenagem e correio",
                       "Alojamento e alimentação",
                       "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas",
                       "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                       "Outros serviços",
                       "Serviços domésticos")
  todos_grupamentos = c("",
                        "A: Agricultura, pecuária, produção florestal, pesca e aquicultura",
                        "B: Indústrias extrativas",
                        "C: Indústrias de transformação",
                        "D: Eletricidade e gás",
                        "E: Água, esgoto, atividades de gestão de resíduos e descontaminação",
                        "F: Construção",
                        "G: Comércio, reparação de veículos automotores e motocicletas",
                        "H: Transporte, armazenagem e correio",
                        "I: Alojamento e alimentação",
                        "J: Informação e comunicação",
                        "K: Atividades financeiras, de seguro e serviços relacionados",
                        "L: Atividades imobiliárias",
                        "M: Atividades profissionais, científicas e técnicas",
                        "N: Atividades administrativas e serviços complementares",
                        "O: Administração pública, defesa e seguridade social",
                        "P: Educação",
                        "Q: Saúde humana e serviços sociais",
                        "R: Artes, cultura, esporte e recreação",
                        "S: Outras atividades de serviço",
                        "T: Serviços domésticos",
                        "U: Organismos internacionais e outras instituições extraterritoriais",
                        "V: Atividades maldefinidas"
  )
  
  
    
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", 
               "Principal atividade;", "Grupamento;", "Posição;", "Situação;",
               "Amostra;", "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Renda habitual principal;", "Renda efetiva principal;", "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;", "Horas habituais total;",  "Horas efetivas total;", 
               "Renda Domiciliar efetiva total;", "Renda Domiciliar efetiva per capita;",    
               "Escolaridade",  "\n"),
        file = arquivo_saida,
        append = FALSE )
  
  
  
  for (pnadf in pnads)
  {
    pnadc <- preprocessamento(pnadf)
    
    # Idade mínima
    pnsub <- subset(pnadc,  pnadc$idade >=14)
    
    for (local_ in todos_locais)
    {
      # Local (estado ou Brasil)
      
      pnsublocal <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) |  (pnsub$regiao == local_) | (pnsub$regmetro == local_) | (pnsub$municipio == local_) ) )
      if (local_ == "") local_ <- "Brasil"
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          if (faixa_etaria_ == "15 a 29")
          {
            pnsubfaixa <- subset(pnsubsexo,  (idade >= 15 & idade <=29 ) )
          }
          else
          {
            pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          }
          
            for (raca_ in todas_racas)
            {
              if (raca_ == "Negra")
              {
                pnsubraca <- subset(pnsubfaixa,  ( (raca == "Preta") | (raca == "Parda" ) ) )
              }
              else if (raca_ == "Não Negra")
              {
                pnsubraca <- subset(pnsubfaixa,  ( (raca != "Preta") & (raca != "Parda" ) ) )
              }
              else
              {
                pnsubraca <- subset(pnsubfaixa,  ( (raca_ == "") | (raca == raca_ ) ) )
              }

              
              for (atividade_ in c("",
                                   "Indústria",
                                   "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas",
                                   "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                                   "Outros serviços"))
              {
                grupamento_ <- ""
                pnsuba <- subset(pnsubraca,  ( (atividade_ == "") | (atividade == atividade_ ) ) )  
                
                for (posicao_ in posicao_na_ocupacao)
                {
                  
                  if (posicao_ == "")
                  {
                    situacao_ <- ""
                    pnsubsit <- pnsuba
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";", posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                  
                  else if (posicao_ == "Empregado")
                  {
                    for (situacao_ in situacao_empregado)
                    {
                      
                      if (situacao_ == "Setor privado com carteira")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Empregado no setor privado com carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Setor privado sem carteira")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Empregado no setor privado sem carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Setor público com carteira")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Empregado no setor público com carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Setor público sem carteira")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Empregado no setor público sem carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Militar e servidor estatutário")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Militar e servidor estatutário") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Trabalhador doméstico") |
                                             (funcao == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar") |
                                             (funcao == "Empregado do setor privado") |
                                             (funcao == "Empregado do setor público (inclusive empresas de economia mista)") )
                      }
                      
                      
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Empregador")
                  {
                    for (situacao_ in situacao_empregador)
                    {
                      if (situacao_ == "Com CNPJ")
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Empregador") & (CNPJ == "Sim") )
                      }
                      else if (situacao_ == "Sem CNPJ")
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Empregador") & (CNPJ == "Não") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Empregador") )
                      }
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Conta própria")
                  {
                    for (situacao_ in situacao_empregador)
                    {
                      if (situacao_ == "Com CNPJ")
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Conta própria") & (CNPJ == "Sim") )
                      }
                      else if (situacao_ == "Sem CNPJ")
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Conta própria") & (CNPJ == "Não") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsuba), (funcao == "Conta própria") )
                      }
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Trabalhador doméstico")
                  {
                    for (situacao_ in situacao_domestico)
                    {
                      if (situacao_ == "Trabalhador doméstico com carteira")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Trabalhador doméstico sem carteira")
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsuba), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") | 
                                             (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                      }
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Trabalhador familiar auxiliar")
                  {
                    situacao_ <- ""
                    pnsubsit <- subset((pnsuba), (especificacao == "Trabalhador familiar auxiliar") )
                    
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                    
                  }
                  else if (posicao_ == "Formal")
                  {
                    situacao_ <- ""
                    pnsubsit <- subset((pnsuba), (especificacao == "Empregado no setor privado com carteira de trabalho assinada") |
                                         (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") |
                                         (especificacao == "Empregado no setor público com carteira de trabalho assinada") |
                                         (especificacao == "Empregado no setor público sem carteira de trabalho assinada") |
                                         (especificacao == "Militar e servidor estatutário") |
                                         (especificacao == "Empregador" & CNPJ == "Sim") |
                                         (especificacao == "Conta-própria" & CNPJ == "Sim"))
                    
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                  else if (posicao_ == "Informal")
                  {
                    situacao_ <- ""
                    pnsubsit <- subset((pnsuba), (especificacao == "Empregado no setor privado sem carteira de trabalho assinada") |
                                         (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") |
                                         (especificacao == "Empregador" & CNPJ == "Não") |
                                         (especificacao == "Conta-própria" & CNPJ == "Não") |
                                         (especificacao == "Trabalhador familiar auxiliar") )
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
              }
              
              # Setor econômico da atividade
              #pnsuba <- subset(pnsubp,  ( (atividade_ == "") | (atividade == atividade_ ) ) )  
              for (grupamento_ in todos_grupamentos)
              {
                atividade_ = ""
                if (grupamento_ == "A: Agricultura, pecuária, produção florestal, pesca e aquicultura") atividade_ <- "Agricultura, pecuária, produção florestal, pesca e aquicultura"
                if (grupamento_ == "B: Indústrias extrativas") atividade_ <- "Indústria"
                if (grupamento_ == "C: Indústrias de transformação") atividade_ <- "Indústria"
                if (grupamento_ == "D: Eletricidade e gás") atividade_ <- "Indústria"
                if (grupamento_ == "E: Água, esgoto, atividades de gestão de resíduos e descontaminação") atividade_ <- "Indústria"
                if (grupamento_ == "F: Construção") atividade_ <- "Construção"
                if (grupamento_ == "G: Comércio, reparação de veículos automotores e motocicletas") atividade_ <- "Comércio, reparação de veículos automotores e motocicletas"
                if (grupamento_ == "H: Transporte, armazenagem e correio") atividade_ <- "Transporte, armazenagem e correio"
                if (grupamento_ == "I: Alojamento e alimentação") atividade_ <- "Alojamento e alimentação"
                if (grupamento_ == "J: Informação e comunicação") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas" 
                if (grupamento_ == "K: Atividades financeiras, de seguro e serviços relacionados") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                if (grupamento_ == "L: Atividades imobiliárias") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                if (grupamento_ == "M: Atividades profissionais, científicas e técnicas") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                if (grupamento_ == "N: Atividades administrativas e serviços complementares") atividade_ <- "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas"
                if (grupamento_ == "O: Administração pública, defesa e seguridade social") atividade_ <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
                if (grupamento_ == "P: Educação") atividade_ <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
                if (grupamento_ == "Q: Saúde humana e serviços sociais") atividade_ <- "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais"
                if (grupamento_ == "R: Artes, cultura, esporte e recreação") atividade_ <- "Outros serviços"
                if (grupamento_ == "S: Outras atividades de serviço") atividade_ <- "Outros serviços"
                if (grupamento_ == "T: Serviços domésticos") atividade_ <- "Serviços domésticos"
                if (grupamento_ == "U: Organismos internacionais e outras instituições extraterritoriais") atividade_ <- "Outros serviços"
                if (grupamento_ == "V: Atividades maldefinidas") atividade_ <- "Atividades maldefinidas"
                
                pnsubg <- subset(pnsubraca,  ( (grupamento_ == "") | (grupamento == grupamento_ ) ) )  

                for (posicao_ in posicao_na_ocupacao)
                {
                  
                  if (posicao_ == "")
                  {
                    situacao_ <- ""
                    pnsubsit <- pnsubg
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                  
                  else if (posicao_ == "Empregado")
                  {
                    for (situacao_ in situacao_empregado)
                    {
                      
                      if (situacao_ == "Setor privado com carteira")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Empregado no setor privado com carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Setor privado sem carteira")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Empregado no setor privado sem carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Setor público com carteira")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Empregado no setor público com carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Setor público sem carteira")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Empregado no setor público sem carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Militar e servidor estatutário")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Militar e servidor estatutário") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Trabalhador doméstico") |
                                             (funcao == "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar") |
                                             (funcao == "Empregado do setor privado") |
                                             (funcao == "Empregado do setor público (inclusive empresas de economia mista)") )
                      }
                      
                      
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Empregador")
                  {
                    for (situacao_ in situacao_empregador)
                    {
                      if (situacao_ == "Com CNPJ")
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Empregador") & (CNPJ == "Sim") )
                      }
                      else if (situacao_ == "Sem CNPJ")
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Empregador") & (CNPJ == "Não") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Empregador") )
                      }
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Conta própria")
                  {
                    for (situacao_ in situacao_empregador)
                    {
                      if (situacao_ == "Com CNPJ")
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Conta própria") & (CNPJ == "Sim") )
                      }
                      else if (situacao_ == "Sem CNPJ")
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Conta própria") & (CNPJ == "Não") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsubg), (funcao == "Conta própria") )
                      }
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Trabalhador doméstico")
                  {
                    for (situacao_ in situacao_domestico)
                    {
                      if (situacao_ == "Trabalhador doméstico com carteira")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") )
                      }
                      else if (situacao_ == "Trabalhador doméstico sem carteira")
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                      }
                      else
                      {
                        pnsubsit <- subset((pnsubg), (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") | 
                                             (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") )
                      }
                      cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                  atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                  numeros(pnsubsit),";", 
                                  rendas(pnsubsit),";",
                                  "\n" ),
                           file = arquivo_saida,
                           append = TRUE)
                    }
                  }
                  else if (posicao_ == "Trabalhador familiar auxiliar")
                  {
                    situacao_ <- ""
                    pnsubsit <- subset((pnsubg), (especificacao == "Trabalhador familiar auxiliar") )
                    
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                    
                  }
                  else if (posicao_ == "Formal")
                  {
                    situacao_ <- ""
                    pnsubsit <- subset((pnsubg), (especificacao == "Empregado no setor privado com carteira de trabalho assinada") |
                                         (especificacao == "Trabalhador doméstico com carteira de trabalho assinada") |
                                         (especificacao == "Empregado no setor público com carteira de trabalho assinada") |
                                         (especificacao == "Empregado no setor público sem carteira de trabalho assinada") |
                                         (especificacao == "Militar e servidor estatutário") |
                                         (especificacao == "Empregador" & CNPJ == "Sim") |
                                         (especificacao == "Conta-própria" & CNPJ == "Sim"))
                    
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                  else if (posicao_ == "Informal")
                  {
                    situacao_ <- ""
                    pnsubsit <- subset((pnsubg), (especificacao == "Empregado no setor privado sem carteira de trabalho assinada") |
                                         (especificacao == "Trabalhador doméstico sem carteira de trabalho assinada") |
                                         (especificacao == "Empregador" & CNPJ == "Não") |
                                         (especificacao == "Conta-própria" & CNPJ == "Não") |
                                         (especificacao == "Trabalhador familiar auxiliar") )
                    cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                                atividade_,  ";", grupamento_,  ";",posicao_, ";", situacao_, ";", 
                                numeros(pnsubsit),";", 
                                rendas(pnsubsit),";",
                                "\n" ),
                         file = arquivo_saida,
                         append = TRUE)
                  }
                }
              }
            
          }  
        }
      }
    }  
  }
}

