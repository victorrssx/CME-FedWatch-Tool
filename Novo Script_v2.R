  
  #############################################
  #############   Fed Meetings   ##############
  #############    29/04/2023    ##############
  #############################################
  
  # Carregando pacotes e explicitando funções auxiliares
  pacman::p_load(tidyverse, janitor, rvest, xml2, QCSimulator, 
                 geometry, lubridate, ggrepel, ggalt)
  pacman::p_loaded()
  
  decimais <- function(x, k) format(round(x, k), nsmall=k, big.mark = ".", decimal.mark = ",",  scientific = FALSE)
  
  # Carregando dados
  link = "https://cmegroup-tools.quikstrike.net/User/Export/FedWatch/AllMeetings.aspx?insid=92792945&qsid=4519fa4a-6db4-4797-9906-dd429defba3c.csv"
  dados <- read.csv(link)
  
  reunioes_df = data.frame(reuniao = grep("History.for.\\s*(.*?)\\s*.Fed.meeting", colnames(dados), value = T),
                           inicio  = grep("History.for.\\s*(.*?)\\s*.Fed.meeting", colnames(dados))) %>% 
                mutate(fim = lead(inicio)-1) %>% .[-nrow(.),] %>%
                pmap(function(reuniao, inicio, fim) {dados[c(1, inicio:fim)]}) %>% 
                `names<-` (map(., function(df) colnames(df)[2])) %>% 
                map(function(df) {
                  bind_rows(df, 
                            map(df[1,], ~ {ifelse(is.na(as.numeric(sub("(?i).*-.*?(\\d+).*", "\\1", .x))), 
                                                     0, # Se for NA, 0
                                                     as.numeric(sub("(?i).*-.*?(\\d+).*", "\\1", .x))) # Caso contrário, número entre parênteses
                            }) %>% 
                              unlist() %>% 
                              as.vector %>% 
                              as.data.frame() %>% 
                              mutate(pontomedio = ifelse(. == 25, 12.5, ((. - lag(.))/2) + lag(.)),
                                     pontomedio = as.character(pontomedio)) %>% 
                              mutate(pontomedio = ifelse(is.na(pontomedio), 0, pontomedio)) %>%
                              .[2] %>% 
                              t() %>% 
                              as.data.frame() %>% 
                              `colnames<-`(names(df)))
                })
  
       