
  #####################################################
  ################                     ################
  ################     Fed Meetings    ################
  ################      01/01/2023     ################
  ################                     ################
  #####################################################


    pacman::p_load(janitor, rvest, stringr, xml2, purrr, QCSimulator, 
                   geometry, lubridate, dplyr, tidyr, ggplot2, forcats, ggrepel, ggalt)
    pacman::p_loaded()
    
    decimais <- function(x, k) format(round(x, k), nsmall=k, big.mark = ".", decimal.mark = ",",  scientific = FALSE)
    #page <- xml2::read_html("https://www.cmegroup.com/trading/interest-rates/countdown-to-fomc.html", encoding = "UTF-8")
    
    #url = "https://www.cmegroup.com/trading/interest-rates/countdown-to-fomc.html"
    #download.file(url, destfile = "scrapedpage.html", quiet = TRUE)
    #page <- read_html("scrapedpage.html")
    
    #page %>%
    #html_nodes("a") %>%       # find all links
    #html_attr("href") %>%     # get the url
    #str_subset("\\.xlsx") %>% # find those that end in xlsx
    #.[[1]]  
    
    dados <- read.csv("https://cmegroup-tools.quikstrike.net/User/Export/FedWatch/AllMeetings.aspx?insid=105833230&qsid=5959fd59-9f31-415e-8486-312697c9d4eb.csv")
    
    x = matrix()
    for (i in 1:length(dados)) {
     x[i] <- ifelse(is.na(as.numeric(sub("(?i).*-.*?(\\d+).*", "\\1", dados[1,i]))), 0, as.numeric(sub("(?i).*-.*?(\\d+).*", "\\1", dados[1,i])))
     print(x)
    }
    x <- as.data.frame(x) %>% 
         mutate(pontomedio = ifelse(x == 25, 12.5, ((x - lag(x))/2) + lag(x)),
                pontomedio = as.character(pontomedio))
    x[1,2] <- 0
    x <- as.data.frame(t(x[2]))
    colnames(x) <- names(dados)
    
    dados <- bind_rows(dados, x)
    row.names(dados) <- NULL
    rm(x, i)
    
    
    
    marcador <- as.data.frame(grep("History", colnames(dados))) %>% setNames(c("linha")) %>%
                rbind(length(dados)) %>% 
                mutate(dif = linha - lag(linha)) 
    
    reunioes_df = list()
    valoresp = data.frame(Data = dados[2:nrow(dados),1])
    for (i in 2:nrow(marcador)-1) {
    reunioes_df[[i]] <- select(dados, marcador[i,1]:(marcador[i+1,1]-1)) %>% 
                        cbind(dados[1], .) %>% 
                        row_to_names(row_number = 1) %>% 
                        .[c(nrow(.), 1:nrow(.)-1),]
    reunioes_df[[i]][2:length(reunioes_df[[i]])] <- sapply(reunioes_df[[i]][2:length(reunioes_df[[i]])], as.numeric) 
    reunioes_df[[i]][is.na(reunioes_df[[i]])] <- 0
    
      for (j in 2:nrow(reunioes_df[[i]])) {
    valoresp[j-1, i+1] <- as.matrix(reunioes_df[[i]][j,-1]) %*% t(as.matrix(reunioes_df[[i]][1,-1]))    
      }
    }
    rm(i,j)
    
    
    names(reunioes_df) <- colnames(dados)[marcador[1:10,1]]
    names(valoresp) <- c("Data", gsub("\\.", "/", str_match(colnames(dados)[marcador[1:10,1]], "History.for.\\s*(.*?)\\s*.Fed.meeting")[,2]))
    valoresp <- valoresp[-nrow(valoresp),] %>% 
                pivot_longer(cols = 2:11, names_to = "meetings", values_to = "valoresp") %>% 
                mutate(Data = as.Date(Data, format = "%m/%d/%Y"), 
                       meetings = factor(meetings),
                       meetings = fct_relevel(meetings, c("15/Jun/2022", "27/Jul/2022", "21/Sep/2022",
                                                          "2/Nov/2022", "14/Dec/2022", "1/Feb/2023",
                                                          "15/Mar/2023", "3/May/2023", "14/Jun/2023",
                                                          "26/Jul/2023")))
    
    graf_fed_1 <- valoresp %>% 
                  ggplot(aes(x = Data, y = valoresp, group = meetings, color = meetings)) +
                  geom_line(size = 1) +
                  labs(title = "Valor Esperado do Juro Americano",
                       subtitle = "Janela móvel de 1 ano, em bps.\nFonte: Elaboração própria a partir de dados do CME Group.",
                       x = "", y = "") +
                  scale_x_date(breaks = seq.Date(ymd("2021-05-01"), ymd("2022-05-01"), by = "month"), date_labels = "%m/%Y") +
                  scale_y_continuous(breaks = seq(0, 350, 25), limits = c(0, 500)) +
                  scale_color_viridis_d(option = "mako", begin = 0.1, end = 0.9, direction = -1) +
                  theme_minimal(base_size = 15) +
                  theme(plot.subtitle = element_text(size = 12, margin = unit(c(-1,0,6,0), "mm"), lineheight = 1.1),
                        axis.text = element_text(colour = "black"),
                        axis.text.x = element_text(angle = 45),
                        axis.title.y = element_text(size = 12),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid = element_line(linetype = "twodash"),
                        plot.caption = element_text(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")),
                        legend.title = element_blank())
    graf_fed_1    
    
    png(file = "./fed1.png", width = 12, height = 7, units = "in", res = 900)
    graf_fed_1
    dev.off()
    
  
    graf_fed_2 <- valoresp %>% 
                  filter((meetings == "14/Dec/2022" | 
                         meetings == "14/Jun/2023") &
                         Data >= "2021-12-01") %>% 
                  ggplot(aes(x = Data, y = valoresp, group = meetings, color = meetings)) +
                  geom_line(size = 1, show.legend = F) +
                  geom_point(size = 1.5, show.legend = F) +
                  labs(title = "Valor Esperado do Juro Americano - Reuniões de 12/2022 e 06/2023",
                       subtitle = "Novembro de 2021 até o momento, em bps.\nFonte: Elaboração própria a partir de dados do CME Group.",
                       x = "", y = "") +
                  scale_x_date(breaks = seq.Date(ymd("2021-05-01"), ymd("2022-05-01"), by = "month"), date_labels = "%m/%Y") +
                  scale_y_continuous(breaks = seq(0, 350, 25), limits = c(0, 350)) +
                  theme_minimal(base_size = 15) +
                  theme(plot.subtitle = element_text(size = 12, margin = unit(c(-1,0,6,0), "mm"), lineheight = 1.1, colour = "#9999aa"),
                        axis.text = element_text(colour = "black"),
                        axis.text.x = element_text(angle = 45),
                        axis.title.y = element_text(size = 12),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid = element_line(linetype = "twodash"),
                        plot.caption = element_text(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")),
                        legend.title = element_blank()) +
                  facet_wrap(~meetings)
    graf_fed_2
    
    png(file = "./fed2.png", width = 12, height = 7, units = "in", res = 900)
    graf_fed_2
    dev.off()
    
    
   fed_3 <- valoresp %>% 
            pivot_wider(names_from = meetings, values_from = valoresp) %>% 
            .[seq(which(.$Data == max(.$Data)) - 5, 
                 which(.$Data == max(.$Data)), 5),] %>% 
            pivot_longer(cols = 2:11, names_to = "meetings", values_to = "valoresp") %>% 
            mutate(Data = format(strptime(as.character(Data), "%Y-%m-%d"), "%d/%m/%Y"),
                   #Data = factor(Data),
                   #Data = fct_relevel(Data, unique(.$Data)),
                   meetings = factor(meetings),
                   meetings = fct_relevel(meetings, c("15/Jun/2022", "27/Jul/2022", "21/Sep/2022",
                                                      "2/Nov/2022", "14/Dec/2022", "1/Feb/2023",
                                                      "15/Mar/2023", "3/May/2023", "14/Jun/2023",
                                                      "26/Jul/2023"))) 
    graf_fed_3 <- ggplot(fed_3, aes(x = meetings, y = valoresp, group = Data, color = Data)) +
                  geom_line(size = 1) +
                  geom_point(size = 3) +
                  scale_color_viridis_d(option = "mako", begin = 0.1, end = 0.9, direction = -1) +
                  geom_text_repel(data = fed_3, aes(label = decimais(valoresp,1)), show.legend = F, force_pull = 2.5, force = 8) +
                  scale_y_continuous(breaks = seq(100, 350, 50), limits = c(100, 350)) +
                  labs(title = "Curva de Valor Esperado do Juro Americano",
                        subtitle = "Em bps. Fonte: Elaboração própria a partir de dados do CME Group.",
                        x = "Reuniões do Fed", y = "Valor Esperado") +
                  theme_minimal(base_size = 15) +
                  theme(plot.subtitle = element_text(size = 12, margin = unit(c(-1,0,6,0), "mm"), lineheight = 1.1, colour = "#9999aa"),
                        axis.text = element_text(colour = "black"),
                        axis.text.y = element_text(size = 15, margin = unit(c(0,0,0,5), "mm")),
                        axis.text.x = element_text(angle = 45, margin = unit(c(0,0,-8,0), "mm")),
                        axis.title = element_text(size = 15),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid = element_line(linetype = "twodash"),
                        #plot.caption = element_text(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")),
                        legend.title = element_blank())
    graf_fed_3    

    png(file = "./fed3.png", width = 12, height = 7, units = "in", res = 900)
    graf_fed_3
    dev.off()

    
    graf_fed_4 <- valoresp %>% 
      mutate(Data = case_when(Data == max(Data) ~ "Hoje",
                              Data == (max(Data)-7) ~ "Semana passada", 
                              T ~ as.character(Data))) %>% 
                                 filter(Data == "Hoje" |
                                        Data == "Semana passada") %>%
             pivot_wider(names_from = Data, values_from = valoresp) %>% 
             mutate(meetings = factor(meetings, levels = meetings),
                    Inicio = 0,
                    Diff = Hoje - `Semana passada`) %>% 
    ggplot(aes(x = Inicio, xend = Diff, y = meetings, group = meetings)) +
      geom_vline(xintercept = 0, colour = "black", size = 0.5) +
      geom_dumbbell(color = "#a3c4dc", 
                    size = 2.75, 
                    colour_xend = "#0e668b") + 
      scale_y_discrete(limits = rev) + 
      labs(x = NULL, 
           y = NULL, 
           title = "Variação do Valor Esperado (t - t-7)", 
           subtitle = "Em bps. Fonte: Elaboração própria a partir de CME Group.") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(size = 22),
            plot.subtitle = element_text(size = 15, lineheight = 1.1, colour = "#9999aa"),
            axis.text = element_text(colour = "black"),
            #axis.text.y = element_text(size = 15, margin = unit(c(0,0,0,5), "mm")),
            #axis.text.x = element_text(angle = 45, margin = unit(c(0,0,-8,0), "mm")),
            axis.title = element_text(size = 15),
            panel.grid.major.x = element_line(size = 0.5, colour = "#9999aa"),
            panel.grid.minor = element_blank(),
            panel.grid = element_line(linetype = "twodash"),
            #plot.caption = element_text(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")),
            legend.title = element_blank())
    graf_fed_4    
    
    png(file = "./fed4.png", width = 12, height = 7, units = "in", res = 900)
    graf_fed_4
    dev.off()
    