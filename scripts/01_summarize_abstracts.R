library(tidyverse)
library(tidytext)
library(stopwords)

nih <- read_csv("../data/processed/nhlbi_data_5000_current_projects.csv")
data("stop_words")

abstracts <- nih %>%
  select(`Project Title`, `Project Abstract`) %>%
  unnest_tokens(word, `Project Abstract`) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("project", "summary", "abstract", "rationale", "1", "2", "3", "aims", "research", "studies", "specific", "study", "data", "proposal", "proposed", "goal", "aim")))

abstracts %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
