library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)

extraneous <- c("Scary Goldings", "Desert Island Discs", "un:CUT - The Makers' Podcast", "What I Believe", "Feel Better, Live More with Dr Rangan Chatterjee", "What Makes Us Human with Jeremy Vine", "Justin Hurwitz", "La La Land Cast", "Ryan Gosling", "Emma Stone", "Herb Alpert", "Herb Alpert & The Tijuana Brass")
lalaland <- c("Justin Hurwitz", "La La Land Cast", "Ryan Gosling")
herbAlpert <- c("Herb Alpert & The Tijuana Brass", "Herb Alpert")

raw_data <- fromJSON("data.json", flatten=TRUE)

lalalandTotal <- raw_data %>% filter(artistName %in% lalaland) %>%
  summarise(sum(msPlayed)) %>% 
  as.numeric()

herbAlpertTotal <- raw_data %>% filter(artistName %in% herbAlpert) %>%
  summarise(sum(msPlayed)) %>%
  as.numeric()

processedData <- raw_data %>% 
  group_by(artistName) %>% 
  summarise(msPlayed = sum(msPlayed)) %>%
  bind_rows(data.frame(artistName = "La La Land Soundtrack", msPlayed = lalalandTotal)) %>%
  bind_rows(data.frame(artistName = "Herb Alpert & The Tijuana Brass Band", msPlayed = herbAlpertTotal)) %>%
  mutate(minsPlayed = msPlayed / 60000) %>%
  filter(!(artistName %in% extraneous)) %>%
  filter(minsPlayed > 15) %>%
  arrange(desc(minsPlayed)) 

processedDataTibble <- as_tibble(processedData)
processedDataJSON <- toJSON(processedDataTibble)
write(processedDataJSON, file="processedData.json")

  processedDataTibble %>% mutate(name = fct_reorder(artistName, minsPlayed)) %>%
ggplot(aes(x=name, y=minsPlayed, fill=minsPlayed)) + 
  geom_col(width=0.8) + 
  coord_flip() +
  theme_classic() +
  scale_fill_distiller(palette="Blues", direction=1) +
  xlab("Artist") +
  ylab("Minutes played")


