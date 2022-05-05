# responses from the MEDS Class of 2022 Mural Inspiration survey: https://forms.gle/XESirQia4UapPPbZ9
# specifically using responses to the following 2 questions:
  # What 5 words would you use to describe the MEDS Class of 2022 Cohort?
  # What 5 words describe the MEDS Program? 

#..........................load packages.........................
library(tidyverse)
library(wordcloud)
library(RColorBrewer)

#............................load data...........................
data <- read_csv(here::here("responses.csv"))

#..........................wrangle data..........................
cohort <- data %>% select(five_words_cohort) %>% 
  mutate(five_words_cohort = strsplit(as.character(five_words_cohort), ", ")) %>% 
  unnest(five_words_cohort) %>% 
  drop_na() %>% 
  mutate(five_words_cohort = str_to_lower(five_words_cohort)) %>% 
  select(word = five_words_cohort) #%>% 
  #count(word, sort = TRUE)

program <- data %>% select(five_words_program) %>% 
  mutate(five_words_program = strsplit(as.character(five_words_program), ", ")) %>% 
  unnest(five_words_program) %>% 
  drop_na() %>% 
  mutate(five_words_program = str_to_lower(five_words_program),
         five_words_program_new = gsub(pattern = ",", x = five_words_program, replacement = "")) %>% 
  select(word = five_words_program_new) #%>% 
  #count(word, sort = TRUE) 
  
# combine into same df
all <- rbind(cohort, program) %>% 
  count(word, sort = TRUE)


#........................create wordcloud........................
# set.seed(100)
# wordcloud(words = cohort$word, freq = cohort$n,
#           min.freq = 1, max.words = 50, 
#           random.order = FALSE, rot.per = 0.35)

# set.seed(100)
# wordcloud(program$word, freq = program$n,
#           min.freq = 1, max.words = 50, 
#           random.order = FALSE, rot.per = 0.35)

jpeg('meds2022.jpeg', width = 500, height = 500, quality = 100)
set.seed(100)
meds2022<- wordcloud(all$word, freq = all$n,
                     min.freq = 1, max.words = 85, 
                     random.order = FALSE, rot.per = 0.35,
                     scale=c(4,.5), colors=brewer.pal(8, "Dark2"))
dev.off()




 