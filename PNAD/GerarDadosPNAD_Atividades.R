
options(OutDec= ",")


taxas <- function(pnadc)
{
  
  total_lsir        <- sum(pnadc$pesoscalibrados)
  ocupadas_lsir     <- sum(subset(pnadc, (pnadc$ocupadas=="Pessoas ocupadas") & (pnadc$forca=="Pessoas na força de trabalho") )$pesoscalibrados )  
  forca_lsir        <- sum(subset(pnadc,  (pnadc$forca=="Pessoas na força de trabalho") )$pesoscalibrados )
  taxa_lsir         <- 100 * (1 - ocupadas_lsir/forca_lsir)
  nivel_lsir        <- 100*ocupadas_lsir/total_lsir
  participacao_lsir <- 100*forca_lsir/total_lsir
  
  return (paste(
    format(round(total_lsir, 0), nsmall = 0), ";", 
    format(round(ocupadas_lsir, 0), nsmall = 0), ";", 
    format(round(forca_lsir, 0), nsmall = 0), ";", 
    format(round(taxa_lsir, 2), nsmall = 0), ";", 
    format(round(nivel_lsir, 2), nsmall = 2), ";", 
    format(round(participacao_lsir, 2), nsmall = 2)))
}

rendas <- function(pnadc)
{
  
  total_lsir <- nrow(subset(pnadc))
  renda_media_hab_principal <- weighted.mean(pnadc$rendahabprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_media_efe_principal <- weighted.mean(pnadc$rendaefeprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_media_hab_total <- weighted.mean(pnadc$rendahabtotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  renda_media_efe_total <- weighted.mean(pnadc$rendaefetotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
   
  horas_hab_principal <- weighted.mean(pnadc$horashabprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  horas_efe_principal <- weighted.mean(pnadc$horashabprincipal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  horas_hab_total <- weighted.mean(pnadc$horashabtotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  horas_efe_total <- weighted.mean(pnadc$horashabtotal, w = pnadc$pesoscalibrados, na.rm = TRUE)
  
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
                format(round(escolaridade, 2), nsmall = 2)))
  
}


todas_categorias <- function(pnads)
{
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "Jovem", "Junior", "Pleno", "Senior", "Idoso", "15a29")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", 
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda habitual principal;", "Renda efetiva principal;",
               "Renda habitual total;", "Renda efetiva total;", 
               "Horas habituais principal;", "Horas efetivas principal;",
               "Horas habituais total;",  "Horas efetivas total;", 
               "Escolaridade",  "\n"),
        file = "resultados_categorias.csv",
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
          pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (raca_ in todas_racas)
          {
            if (raca_ == "Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca == "Preta") | (raca == "Parda" ) ) )
            }
            else if (raca_ == "Não Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca != "Preta") & (raca != "Parda" ) ) )
            }
            else
            {
              pnsubr <- subset(pnsubf,  ( (raca_ == "") | (raca == raca_ ) ) )
            }
            
            if (local_ == "") local_ <- "Brasil"
            cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                        taxas(pnsubr),";", 
                        rendas(pnsubr),";",
                        "\n" ),
                 file = "resultados_categorias.csv",
                 append = TRUE)
          }
        }
      }
    }  
  }
}


todas_as_atividades <- function(pnads)
{
  
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "Jovem", "Junior", "Pleno", "Senior", "Idoso")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
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
                       "G: Comércio; reparação de veículos automotores e motocicletas",
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
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", 
               "Contribuição previdenciária?;", "Principal atividade;", "Grupamento;", 
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda efetiva total;", "Horas trabalhadas efet total;", "Escolaridade",  "\n"),
        file = "resultados_atividades.csv",
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
          pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (raca_ in todas_racas)
          {
            if (raca_ == "Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca == "Preta") | (raca == "Parda" ) ) )
            }
            else if (raca_ == "Não Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca != "Preta") & (raca != "Parda" ) ) )
            }
            else
            {
              pnsubr <- subset(pnsubf,  ( (raca_ == "") | (raca == raca_ ) ) )
            }
            
            for (contribuicao_ in todas_contribuicoes)
            {
              # Contribuição previdenciária    
              pnsubc <- subset(pnsubr,  ( (contribuicao_ == "") | (contribuicao == contribuicao_ ) ) )
              
              #for (atividade_ in todas_atividades)
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
                  if (grupamento_ == "G: Comércio; reparação de veículos automotores e motocicletas") atividade_ <- "Comércio, reparação de veículos automotores e motocicletas"
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
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
                              contribuicao_, ";", atividade_, ";", grupamento_, ";",
                              taxas(pnsubg),";", 
                              rendas(pnsubg),";",
                              "\n" ),
                       file = "resultados_atividades.csv",
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

todas_as_funcoes <- function(pnads)
{
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "Jovem", "Junior", "Pleno", "Senior", "Idoso", "15a29")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")

  
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
  
  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", 
                "Função;", "Especificação;",
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
               "Taxa de desocupação;", "Nível da ocupação;", "Participação;", 
               "Renda efetiva total;", "Horas trabalhadas efet total;", "Escolaridade",  "\n"),
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
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubsexo <- subset(pnsublocal,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubfaixa <- subset(pnsubsexo,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
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
                  cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
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

todas_as_posicoes <- function(pnads)
{
  
  todos_locais = c("", "Ceará")
  todos_sexos = c("", "Homem", "Mulher")
  todas_idades = c("", "Jovem", "Junior", "Pleno", "Senior", "Idoso")
  todas_racas = c("", "Amarela", "Branca", "Indígena", "Parda", "Preta", "Negra", "Não Negra")
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
  

  cat ( paste ("Fonte;", "Local;", "Sexo;", "Faixa etária;", "Raça;", 
               "Contribuição previdenciária?;", "Posição;", 
               "Pessoas em idade ativa;", "Pessoas ocupadas;", "Força de trabalho;", 
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
      pnsubl <- subset(pnsub,  ( (local_== "") | (pnsub$UFN == local_) ) )
      
      for (sexo_ in todos_sexos)
      {
        # Sexo (homem ou mulher)
        pnsubs <- subset(pnsubl,  ( (sexo_ == "") | (sexo == sexo_ ) ) ) 
        for (faixa_etaria_ in todas_idades)
        {
          # Faixa etária
          pnsubf <- subset(pnsubs,  ( (faixa_etaria_ == "") | (faixa_etaria == faixa_etaria_ ) ) ) 
          
          for (raca_ in todas_racas)
          {
            if (raca_ == "Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca == "Preta") | (raca == "Parda" ) ) )
            }
            else if (raca_ == "Não Negra")
            {
              pnsubr <- subset(pnsubf,  ( (raca != "Preta") & (raca != "Parda" ) ) )
            }
            else
            {
              pnsubr <- subset(pnsubf,  ( (raca_ == "") | (raca == raca_ ) ) )
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
                cat( paste (pnadf, ";", local_, ";", sexo_, ";", faixa_etaria_, ";", raca_, ";", 
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




