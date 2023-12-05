  
  #####################################################
  ################                     ################
  ################     Fed Meetings    ################
  ################      01/01/2023     ################
  ################                     ################
  #####################################################
  
  
  pacman::p_load(tidyverse, rvest, xml2,  
                 janitor, lubridate, ggtext, ggrepel, extrafont, scales, ggalt)
  pacman::p_loaded()
   
  decimais <- function(x, k) format(round(x, k), nsmall = k, big.mark = ".", decimal.mark = ",",  scientific = FALSE)

  

  ## Importando dados
  
  fedmeetings = read.csv("https://cmegroup-tools.quikstrike.net/User/Export/FedWatch/AllMeetings.aspx?insid=111404028&qsid=61f2911a-642f-4c51-9a2d-0118447f3f23.csv") %>%  
                clean_names() %>%
                set_names(c('Date', colnames(.[2:length(.)]))) %>% 
                select(where(function(x) any(!is.na(x)))) %>% # remove colunas que contém apenas NAs https://stackoverflow.com/questions/2643939/remove-columns-from-dateframe-where-all-values-are-na
                mutate(across(everything(), ~replace(., . == "", 0))) # substituir "" por 0
              
  
  fedmeetings_wavg = pmap(tibble(inicio = grep("history", colnames(fedmeetings)),
                                 fim    = ((grep("history", colnames(fedmeetings)) %>% lead()) - 1) %>% ifelse(is.na(.), length((fedmeetings)), .)),
                          
                          function(inicio, fim) {
                          
                          z = (str_extract(fedmeetings[1, inicio:fim], "(?<=\\()(\\d+)(?=\\-)") %>% as.numeric() +
                               str_extract(fedmeetings[1, inicio:fim], "(?<=\\-)(\\d+)(?=\\))") %>% as.numeric()) / 2   
                          date = fedmeetings[-1, 1]
                          meeting = colnames(fedmeetings[1, inicio:fim])[1] %>% 
                                    sub(".*history_for_ *(.*?) *_fed_meeting.*", "\\1", .) %>%
                                    dmy() %>% 
                                    format("%d/%m/%Y")
                          
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
                     filter(w_avg != 0) %>% 
                     mutate(date = as.Date(date, format = "%m/%d/%Y"),
                            meeting = meeting %>% dmy() %>% format("%b/%Y"),
                            meeting = fct_reorder(factor(meeting), as.Date(paste0("01/", meeting), format = "%d/%b/%Y"))) %>% 
                     arrange(across(c(date, meeting))) 
  
  
  
  ## Gráficos
  
  tema_base = theme(plot.title = element_markdown(size = 23, family = "AvantGarde"),
                    plot.subtitle = element_markdown(size = 15, margin = unit(c(-1,0,6,0), "mm"), lineheight = 1.1),
                    axis.text = element_markdown(size = 15, colour = "black"),
                    axis.title.y = element_markdown(size = 12),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid = element_line(linetype = "twodash"),
                    plot.caption = element_markdown(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")),
                    legend.title = element_markdown(size = 15),
                    legend.text = element_markdown(size = 13))
  
  
  fedmeetings_wavg %>%
     {ggplot(., aes(x = date, y = w_avg, group = meeting, color = meeting)) +
         geom_line(size = 1) +
         labs(title = "**Valor Esperado da Taxa de Juros Americana**",
              subtitle = "Janela móvel de 1 ano, em bps. <br>",
              color = "**Reunião**",
              caption = "<br> Fonte: Elaboração própria a partir de dados do CME Group.",
              x = "", y = "") +
         scale_x_date(breaks = "1 months", labels = label_date_short()) +
         scale_y_continuous(n.breaks = 6) +
         scale_color_viridis_d(option = "mako", begin = 0.1, end = 0.9, direction = -1) +
         theme_minimal(base_size = 15) +
         tema_base} %>% 
     ggsave("2.png", ., width = 12, height = 7, units = "in", dpi = 300)
  
    
  fedmeetings_wavg %>% 
     filter((meeting == "jan/2024" | 
             meeting == "mar/2024") &
             date >= "2021-12-01") %>% 
     {ggplot(., aes(x = date, y = w_avg, group = meeting, color = meeting)) +
         geom_line(size = 1, show.legend = F) +
         geom_point(size = 1.5, show.legend = F) +
         labs(title = "**O quê o mercado espera da taxa de juros americana?**",
              subtitle = "Novembro de 2021 até o momento, em bps.<br>",
              caption = "<br> Fonte: Elaboração própria a partir de dados do CME Group.",
              x = "", y = "") +
         scale_x_date(breaks = "1 months", labels = label_date_short(format = c("%y", "%b"))) +
         #scale_y_continuous(breaks = seq(0, 350, 25), limits = c(0, 350)) +
         theme_minimal(base_size = 15) +
         tema_base +
         theme(panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid = element_line(linetype = "twodash")) +
         facet_wrap(~meeting)} %>% 
     ggsave("2.png", ., width = 12, height = 7, units = "in", dpi = 300)
  
  
  
  fedmeetings_wavg %>% 
     pivot_wider(names_from = meeting, values_from = w_avg) %>% 
     .[seq(which(.$date == max(.$date)) - 5, 
           which(.$date == max(.$date)), 
           5),] %>% 
     pivot_longer(cols = 2:10, names_to = "meeting", values_to = "w_avg") %>% 
     mutate(date = factor(date)) %>% 
     {ggplot(., aes(x = meeting, y = w_avg, group = date, color = date)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         scale_color_viridis_d(option = "mako", begin = 0.1, end = 0.9, direction = -1) +
         geom_text_repel(aes(label = decimais(w_avg,1)), show.legend = F, force_pull = 2.5, force = 8) +
         #scale_y_continuous(breaks = seq(100, 350, 50), limits = c(100, 350)) +
         labs(title = "Curva de Valor Esperado do Juro Americano",
              subtitle = "Em bps. Fonte: Elaboração própria a partir de dados do CME Group.",
              x = "Reuniões do Fed", y = "Valor Esperado") +
         theme_minimal(base_size = 15) +
         tema_base +
         theme(#axis.text.y = element_text(size = 15, margin = unit(c(0,0,0,5), "mm")),
               #axis.text.x = element_text(angle = 45, margin = unit(c(0,0,-8,0), "mm")),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid = element_line(linetype = "twodash"),
               #plot.caption = element_text(hjust = -0.06, margin = unit(c(-5,0,0,0), "mm")),
               legend.title = element_blank())} %>% 
     ggsave("2.png", ., width = 12, height = 7, units = "in", dpi = 300)

  
  
 fedmeetings_wavg %>% 
    mutate(date = case_when(date == max(date) ~ "Hoje",
                            date == (max(date)-7) ~ "Semana passada", 
                            T ~ as.character(date))) %>% 
    filter(date == "Hoje" |
             date == "Semana passada") %>%
    pivot_wider(names_from = date, values_from = w_avg) %>% 
    mutate(meetings = factor(meeting, levels = meeting),
           Inicio = 0,
           Diff = Hoje - `Semana passada`) %>% 
    {ggplot(., aes(x = Inicio, xend = Diff, y = meeting, group = meeting)) +
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
          legend.title = element_blank())} %>% 
 ggsave("2.png", ., width = 12, height = 7, units = "in", dpi = 300)
  
  
  
  
 
  