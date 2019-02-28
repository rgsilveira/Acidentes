library("tidyverse")

ggpltget_tran <- function(ano) {
  library(readxl)
  arquivo <- paste0("C:/Users/Samsung2/Google Drive/Acidentes/datatran", ano, ".xlsx")
  read_xlsx(arquivo,
            col_types = c("numeric", "date", "text", 
                          "skip", "text", "text", "numeric", 
                          "text", "text", "text", "text", "text", 
                          "text", "text", "text", "text", "text", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric"),
            na = c("", "Ignorado", "Ignorada", "Nao Informado"))
}

tran <- rbind(tran2007, tran2008, tran2009, tran2010, tran2011, tran2012, tran2013, tran2014, tran2015, tran2016, tran2017)

tran <- as.tibble(tran)

tran_data <- tran[complete.cases(tran),]

mortos_2007_2017 <- tran %>% 
  group_by(ano) %>%
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(ano)

mortes <- mortes %>% 
  full_join(mortos2017, by = "uf")

colnames(mortes) <- c('uf', '2007', '2008', '2009', '2010', '2011', '2012','2013', '2014', '2015', '2016', '2017')

ggplot(data = mortes) +
  geom_col(mapping = aes(x = uf, y = mortes, fill = uf))

causa_acidente_levels <- unique(tran$causa_acidente)

tran$causa_acidente <- factor(tran$causa_acidente, levels = causa_acidente_levels)

library("OneR")
modelo <- OneR(classificacao_acidente ~.,data = tran, verbose = TRUE)
summary(modelo)

tran_com_vitimas <- tran$mortos > 0 | tran$feridos_leves > 0 | tran$feridos_graves > 0
tran_com_mortes <- tran$mortos > 0
tran_vitimas <- tran[tran_com_vitimas, ]
tran_mortes <- tran[tran_com_mortes, ]

mortes_por_causa <- tran %>% 
  group_by(causa_acidente) %>% na.omit() %>% 
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))

mortes_por_tipo <- tran %>% 
  group_by(tipo_acidente) %>% na.omit() %>% 
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))

x <- tran %>% group_by(causa_acidente, tipo_acidente) %>% 
     count(mortos > 0 | feridos_graves > 0)
x <- x[x$`mortos > 0 | feridos_graves > 0`,]
causa_tipo <- x %>% filter(causa_acidente, tipo_acidente, n)

vitimas <- tran %>% 
           group_by(ano) %>% 
           summarize(vitimas = sum(mortos>0) + sum(feridos)) %>% 
           arrange(ano)

ggplot(data = vitimas, mapping = aes(x = ano, y = vitimas), ylab= "mortos e feridos graves", xlab = "ano") +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ poly(x, 4))



