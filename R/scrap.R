

library(readxl)

library(tidyverse)

library(here)

library(gtsummary)

library(ggthemes)

day1 <- read_excel(here('data', 'day1.xlsx'))|>
  distinct(name, .keep_all = T)

day2 <- read_excel(here('data', 'day2.xlsx'))|>
  distinct(name, .keep_all = T)

day3 <- read_excel(here('data', 'day3.xlsx'))|>
  distinct(name, .keep_all = T)

day4 <- read_excel(here('data', 'day4.xlsx'))|>
  distinct(name, .keep_all = T)


registration <- read_excel("data/registration.xlsx", 
                           sheet = "true_data")|>janitor::clean_names()










# Number of participants registered 

day12 <- c(day1$name, setdiff(day2$name, day1$name))

day34 <- c(day3$name, setdiff(day4$name, day3$name))


tot <- c(day12, setdiff(day34, day12))


unique(c(day1$name, day2$name, day3$name, day4$name))
# Analyse de la satisfaction 



# Satisfaction par session


p <- day4 |> pivot_longer(cols = c(vacc_expect:ebola_recom), names_to = 'session', values_to = 'evaluation')|> 
  separate(session, into = c('subject', 'aspect'), sep = '_')|> 
  group_by(subject, aspect)|>
  summarise(mean_score = mean(evaluation, na.rm = T))|>
  mutate(session = case_when(
    str_detect(subject, 'addis')~ 'Déclaration\nAddis Abeba',
    str_detect(subject, 'anniv')~ '50 ans du PEV',
    str_detect(subject, 'bcu')~ 'Big Catch-up',
    str_detect(subject, 'digital')~ 'Santé digitale',
    str_detect(subject, 'ebola')~ 'Vaccination Ébola',
    str_detect(subject, 'gender')~ 'Équité et Genre',
    str_detect(subject, 'measles')~ 'Surveillance de la rougeole',
    str_detect(subject, 'mpox')~ 'Vaccination Mpox et Polio',
    str_detect(subject, 'newvacc')~ 'Introduction\nNouveau Vaccins',
    str_detect(subject, 'pol')~ 'Politique et Financement\nVaccination',
    str_detect(subject, 'sideone')~ 'Session Parallèle I',
    str_detect(subject, 'sidetwo')~ 'Session Parallèle II',
    str_detect(subject, 'vacc')~ 'Situation de la vaccination'),
    aspect2 = case_when(
      str_detect(aspect, 'time')~ 'Temps Alloué',
      str_detect(aspect, 'recom')~ 'Recommandation',
      str_detect(aspect, 'expect')~ 'Attentes',
      str_detect(aspect, 'hauteur')~ 'Animation',
      
    ))|>
  ggplot(aes(y = aspect2, x = mean_score))+
  geom_segment(aes(yend = aspect2, x = 0, xend = mean_score), linewidth = 2, color = alpha('grey50', 0.2))+
  geom_segment(aes(yend = aspect2, x = 0, xend = mean_score), linewidth = 0.7, color = alpha('grey35', 0.6))+
  geom_point(pch = 21, fill = NA, color = "#FFFFFF", size = 10, stroke = 1.5)+
  geom_point( size = 10, pch = 21, fill = 'purple', color = 'transparent')+
  labs(x = 'Niveau de satisfaction', y = '')+
  facet_wrap(vars(session), scales = 'free_x')+
  ggthemes::theme_fivethirtyeight(base_family = 'IBMPlexSans')+
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(face = 'bold', size = 12),
        panel.grid.major.x = element_line(color = 'grey80', linetype = 2),
        plot.background = element_rect(fill = "#FFFFFF"), panel.background = element_rect(fill = "#FFFFFF"))

 
ggsave("plot1.png", p, device = "png", dpi = 700, height = 5, width = 8)
# Satisfaction organisation
library(scales)
colnames(day4)

p <- day4 |> select(-contains('hauteur'), -contains('time'), -contains('recom'), -contains('expect'))|>
  select(2:8)|>
  pivot_longer(cols = everything(),names_to = 'rubrique', values_to = 'score')|>
  group_by(rubrique)|>
  summarise(mean_score = mean(score))|>
  mutate(
    rubrique = case_when(
      str_detect(rubrique, 'chambre')~"Chambre d'hotel",
      str_detect(rubrique, 'lieu')~"Lieu de la réunion",
      str_detect(rubrique, 'note')~"Note d'information",
      str_detect(rubrique, 'pause')~"Pause café",
      str_detect(rubrique, 'satis')~"Satisfaction Générale",
      str_detect(rubrique, 'trad')~"Service de traduction",
      str_detect(rubrique, 'visa')~"Obtention de visa",
    )
  )|>
  ggplot(aes(y = reorder(rubrique, mean_score), x = mean_score))+
  geom_col(width = 0.5, fill = '#6ab9eb', color = 'transparent')+
  geom_text(aes(label = paste0(number(mean_score, accuracy = 0.1),'  ')), hjust = 1, family = 'Spline Sans', color = 'white', size = 4, fontface = 'bold')+
  labs(x = 'Niveau de satisfaction', y = '')+
  ggthemes::theme_fivethirtyeight(base_size = 12, base_family = 'IBMPlexSans')+
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(face = 'bold', size = 12),
        panel.grid.major.x = element_line(color = 'grey83', linetype = 2))




t <- day4 |> select(next_countries)|> separate(col = proposal, into = c(letters[1:3]),sep = ';')|>
  pivot_longer(a:c, names_to = 'proposal', values_to = 'proposition')|>
  mutate(proposition = trimws(proposition, 'left'))|>
  distinct(name, proposition)|>count(proposition)|>
  filter(!str_detect(proposition, 'Aucun|Ras'), !is.na(proposition), row_number() != 1)|>
  mutate(vote = number(n/sum(n), accuracy = 0.01, scale = 100))|>
  arrange(desc(n))|>
  flextable::flextable()


t <- day4 |> select(next_countries)|>
  count(next_countries)|>
  mutate(percent = number(100 * n/sum(n), accuracy = 1.1, suffix = "%"))|>
  arrange(desc(n))|>
  flextable::flextable()


flextable::save_as_docx(t, path = 't.docx')




day4 |> select(c(vacc_expect:ebola_recom))|>
  mutate(across(everything(), as.double))|>
  tbl_summary(statistic = list( all_continuous() ~ "{mean} ({sd})"), type = list(everything()~'continuous'))



# Nombre de participanys 

nrow(registration)-1

# par organisation 

t <- registration |> mutate(organisation = 
                         case_when(
                           
                           str_detect(organisation, "Ministère de la Santé|MSP|	
MSM|MPS|PNSR|Cabinet MSPHPS|MSM|INSP")~ "Ministère en charge de la santé",
                           str_detect(organisation, "G[Aa][Vv][Ii]")~"GAVI",
                           str_detect(organisation, "[Uu][Nn][Ii][Cc][Ee][Ff]")~"Unicef",
                           str_detect(organisation, "JSI")~"JSI",
                           str_detect(organisation, "CDC")~"CDC-Atlanta",
                           str_detect(organisation, "GTCV|Programme Elargi de Vaccination|PEV")~"Programme Elargi de Vaccination",
                           is.na(organisation)|str_detect(organisation, "CD Technique|ASS nationale|CD Technique|CS Surv")~"Autres",
                           str_detect(organisation, "Biomedical")~"Haier Biomedical",
                           TRUE ~ organisation
                         ),
                       pays = ifelse(is.na(pays),"Non Spécifié", pays))|>
  count(organisation, pays)|> pivot_wider(names_from = organisation, values_from = n, values_fill = 0)|>ungroup()|>
  rowwise(pays)|>
  mutate(Total = sum(c_across(where(is.numeric))))|>
  janitor::adorn_totals(where = "row")|>
  flextable::flextable()

flextable::save_as_docx(t, path = 't.docx')








