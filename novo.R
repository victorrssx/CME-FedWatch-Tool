  
  #####################################################
  ################                     ################
  ################     Fed Meetings    ################
  ################      01/01/2023     ################
  ################                     ################
  #####################################################
  
  
  pacman::p_load(janitor, rvest, stringr, xml2, purrr, QCSimulator, 
                 geometry, lubridate, dplyr, tidyr, ggplot2, forcats, ggrepel, ggalt, berryFunctions)
  pacman::p_loaded()
  
  decimais <- function(x, k) format(round(x, k), nsmall = k, big.mark = ".", decimal.mark = ",",  scientific = FALSE)

  

  # Importando dados
  fedmeetings = read.csv("https://cmegroup-tools.quikstrike.net/User/Export/FedWatch/AllMeetings.aspx?insid=111319971&qsid=cb4da172-0bc4-4e77-b985-ac7f2dbe8454.csv") %>%  
                clean_names() %>%
                set_names(c('Date', colnames(.[2:length(.)]))) %>% 
                select(where(function(x) any(!is.na(x)))) %>% # remove colunas que contém apenas NAs https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
                mutate(across(everything(), ~replace(., . == "", 0))) # substituir "" por 0
              
  
  fedmeetings_wavg = pmap(tibble(inicio = grep("history", colnames(fedmeetings)),
                                 fim    = ((grep("history", colnames(fedmeetings)) %>% lead()) - 1) %>% ifelse(is.na(.), length((fedmeetings)), .)),
                          
                          function(inicio, fim) {
                          
                          z = (str_extract(fedmeetings[1, inicio:fim], "(?<=\\()(\\d+)(?=\\-)") %>% as.numeric() +
                               str_extract(fedmeetings[1, inicio:fim], "(?<=\\-)(\\d+)(?=\\))") %>% as.numeric()) / 2   
                          date = fedmeetings[-1, 1]
                          meeting = colnames(fedmeetings[1, inicio:fim])[1]
                          
                          ((fedmeetings[-1, inicio:fim] %>%
                              mutate(across(where(is.character), as.numeric)) %>% 
                              as.matrix) %*% z) %>% 
                          as.data.frame %>% 
                          cbind(date, .) %>% 
                          mutate(m = meeting) %>% 
                          `colnames<-`(c("date", "w_avg", "meeting"))
                          
                          }) %>% 
                     set_names(grep("history", colnames(fedmeetings), value = T)) %>% 
                     reduce(rbind) %>% 
                     mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
                     arrange(desc(date))
                       

    
    
    
 
  
  # Testar algo parecido com df %>% rowwise() %>% mutate(TermID = grep(Term,indices))
  clean_names() %>% 
       mutate_if(is.logical, ~replace(., is.na(.), 0) %>% as.character)
  
  c = map2(grep("history", colnames(fedmeetings))[-10], 
           (grep("history", colnames(fedmeetings)) %>% lead())[-10] - 1, 
           function(inicial, final) {
             y = fedmeetings %>% select(x, all_of(inicial):all_of(final))
             y[1,] %>% mutate(ifelse(. == "Date", "0", .))
             })
  
  
 
  