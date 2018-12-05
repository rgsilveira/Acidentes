library("tidyverse")

filter(tran2007, is.na(br))

mortos2007 <- tran2007 %>% 
  group_by(uf) %>%
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))
  
mortos2008 <- tran2008 %>% 
  group_by(uf) %>%
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))

mortos2009 <- tran2009 %>% 
  group_by(uf) %>%
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))

mortos <- mortos2007 %>% 
  inner_join(mortos2008, by = "uf")

colnames(mortos)[c(2,3)] <- c('2007', '2008')

mortos <- mortos %>% 
  inner_join(mortos2009, by = "uf")

colnames(mortos)[4] <- '2009'

ggplot(data = mortos2007) +
  geom_col(mapping = aes(x = uf, y = mortes, fill = uf))

mortos2010 <- tran2010 %>% 
  group_by(uf) %>%
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))

mortos <- mortos %>%
  inner_join(mortos2010, by = "uf")

colnames(mortos)[5] <- '2010'

mortos2011 <- tran2011 %>% 
  group_by(uf) %>%
  summarize(mortes = sum(mortos > 0)) %>% 
  arrange(desc(mortes))

mortos <- mortos %>% 
  inner_join(mortos2011, by = "uf")

colnames(mortos)[6] <- '2011'
