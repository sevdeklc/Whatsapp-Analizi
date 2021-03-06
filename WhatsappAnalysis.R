install.packages("dplyr")
install.packages("rwhatsapp")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggimage")
install.packages("tidytext")
install.packages("stopwords")


history <- system.file("extdata", "sample.txt", package = "rwhatsapp")


#Gruptaki konu�malar
library("rwhatsapp")
chat <- rwa_read("C:/Users/asus/whatsapp_chat.txt", tz = NULL, format = NULL, verbose = TRUE, encoding = "UTF-8")
chat



#Aylara g�re g�nl�k mesajla�ma grafi�i
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
library("magrittr")
library("tidyverse")
chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")



#Gruptakilerin mesaj yazma s�kl���n�n grafi�i
chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")



#Kim hangi emojileri ne s�kl�kla kullanm��?
library("ggplot2")
library("lubridate")
library("magrittr")
library("tidyverse")
library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")



#Baz� i�letim sistemlerinde, varsay�lan yaz� tipi ggplot2emojileri desteklemez.
#Twitter'daki emoji g�rsellerini kullanmak istersek:
library("ggimage")
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



#Kimler en �ok hangi kelimeleri kullanm��?
library("stopwords")
to_remove <- c(stopwords(language = "de"),
               "https",
               "www.udemy",
               "topic",
               "free",
               "ve",
               "ya",
               "mi",
               "ne",
               "bi",
               "edilmedi",
               "dahil",
               "g�r�nt�",
               "bir",
               "de",
               "o",
               "bu",
               "sat�r",
               "www.udemy.com",
               "i�in",
               "m�",
               "her",
               "simgesini",
               "olu�turdunuz",
               "grubunu",
               "grubun",
               "de�i�tirdiniz",
               "babu�lar",
               "u�tan",
               "uca",
               "�ifreleme",
               "mesajlar",
               "korunmaktad�r",
               "ile",
               "gruba",
               "g�nderdi�iniz",
               "art�k",
               "sey",
               "�ok",
               "yok",
               "var",
               "onu",
               "1",
               "3",
               "2",
               "ama",
               "ben",
               "son",
               "sen",
               "kadar",
               "tek")
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")



#Melek Dedik ki�isinin di�er ki�ilerden farkl� olarak en �ok kulland��� kelimeler
o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Melek Dedik") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  filter(author == "Melek Dedik") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Melek Dedik")







