library(tidyverse)
library(janitor)
library(ggpattern)
library(ggimage)
library(showtext)

df <- read_csv("posts/Spotify-Total/helper/datasets/top_artists_indo.csv")%>%
  head(10) %>%
  mutate(pembulatan = round(followers / 1000000, 2)) %>%
  mutate(image_name = tolower(str_replace_all(artist," ","_")),
         image_path = paste0("posts/Spotify-Total/helper/images/circle-labels/",image_name,".png"),
         wordcloud = paste0("posts/Spotify-Total/helper/images/wordcloud/",image_name,".png"))

pal_bar = '#1dd05d'
pal_text <- "white"
pal_subtext <- "#DFDFDF"
pal_grid <- "grey30"
pal_bg<-'#191919'

tulus <- "posts/Spotify-Total/helper/images/wordcloud/tulus.png"
mahalini <- "posts/Spotify-Total/helper/images/wordcloud/mahalini.png"
sheila_on_7 <- "posts/Spotify-Total/helper/images/wordcloud/sheila_on_7.png"
virgoun <- "posts/Spotify-Total/helper/images/wordcloud/virgoun.png"
rizky_febian <- "posts/Spotify-Total/helper/images/wordcloud/rizky_febian.png"
andmesh <- "posts/Spotify-Total/helper/images/wordcloud/andmesh.png"
fiersa_besari <- "posts/Spotify-Total/helper/images/wordcloud/fiersa_besari.png"
judika <- "posts/Spotify-Total/helper/images/wordcloud/judika.png"
rossa <- "posts/Spotify-Total/helper/images/wordcloud/rossa.png"
pamungkas <- "posts/Spotify-Total/helper/images/wordcloud/pamungkas.png"

ggplot(df, aes(reorder(artist, -pembulatan), pembulatan, pattern_filename = as.factor(pembulatan))) +
  geom_col_pattern(pattern = "image",
                   color = "black",
                   alpha = 0.8,
                   pattern_type = "expand") +
  #scale_pattern_filename_manual(values = c(`11.94` = tulus, `6.38` = mahalini, `6.38` = sheila_on_7, `6.02` = virgoun, `5.91` = rizky_febian, `5.80` = andmesh, `5.29` = fiersa_besari, `5.03` = judika, `4.82` = rossa, `4.72` = pamungkas)) +
  scale_pattern_filename_manual(values = df%>%select(pembulatan, wordcloud)) +
  #geom_col() +
  geom_text(mapping = aes(label=paste0(pembulatan," Jt"), x=artist, y=pembulatan+0.5), color="white", fontface="bold") +
  geom_image(mapping = aes(image=image_path, x=artist, y=-1), size=0.1) +
  #geom_image(aes(image=wordcloud, y=pembulatan), size=.2) +
  labs(y="", x="Jumlah Tayangan (Juta)",
       title = "Zaman Keemasan Musik Indonesia dalam Era Streaming",
       subtitle = "Lagu Teratas Indonesia yang diambil berdasarkan jumlah tayangan di Spotify. \nData diambil pada Juni 2023\n",
       caption = "Dibuat oleh @alifdwt | Data Kworb.net") +
  theme_void()+
  theme(text=element_text(color="white"), 
        plot.background = element_rect(fill=pal_bg),
        plot.caption =element_text(size=7),
        plot.title = element_text(hjust=0.5, color="white", face="bold", size=18),
        plot.subtitle = element_text(hjust=0.5, color="white", size=13),
        plot.margin = margin(t=15, b=10, r=10, l=10))