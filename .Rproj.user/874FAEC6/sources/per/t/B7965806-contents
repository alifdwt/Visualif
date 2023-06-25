# Source = https://github.com/tashapiro/horror-movies

library(tidyverse)
library(ggimage)
library(ggtext)
library(showtext)

sysfonts::font_add_google("Roboto", "Roboto")
sysfonts::font_add_google("Anton", "anton")
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

top10d <- head(read.csv("Spotify-Top-Tracks-Indo/datasets/df_total.csv"), 10) %>%
  select(Title, Artist, Total)
top10 <- merge(top10d, read.csv("Spotify-Top-Tracks-Indo/datasets/artists_total.csv")%>%select(artist, image), by.x = "Artist", by.y = "artist", all.x = TRUE)%>%
  arrange(desc(Total))

top10$Pembulatan <- as.numeric(substring(top10$Total, 1, 3))

pal_bar = '#1dd05d'
pal_text <- "white"
pal_subtext <- "#DFDFDF"
pal_grid <- "grey30"
pal_bg<-'#191919'

top10_plot <- ggplot(data = top10, mapping = aes(y=reorder(Title, Pembulatan), x=Pembulatan)) +
  geom_col(fill=pal_bar, width = 0.4) +
  geom_text(mapping = aes(x=Pembulatan+5, label=round(Pembulatan,0)),
            color = "white", size = 3.5) +
  geom_text(mapping = aes(x=0, y=Title, label=paste0(Title, " - ", Artist)), color = "white", vjust = -2.2, hjust = 0) +
  geom_image(mapping = aes(x=-15, image=image), size = 0.06) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-30,220),
                     breaks = c(0, 100, 200)) +
  labs(y="", x="Jumlah Tayangan (Juta)",
       title = "Zaman Keemasan Musik Indonesia dalam Era Streaming",
       subtitle = "Lagu Teratas Indonesia yang diambil berdasarkan jumlah tayangan di Spotify. \nData diambil pada Juni 2023\n",
       caption = "Dibuat oleh @alifdwt | Data Kworb.net") +
  theme(
    #change background color
    plot.background =element_rect(fill=pal_bg, color=pal_bg),
    panel.background =element_rect(fill=pal_bg, color=pal_bg),
    #modify text font
    text = element_text(color=pal_text, family="Roboto"),
    axis.text = element_text(color=pal_text),
    axis.title.x = element_text(margin=margin(t=10)),
    axis.text.y = element_blank(),
    plot.subtitle = element_text(color=pal_subtext, size=12),
    plot.caption = element_text(color=pal_subtext, size=10),
    plot.title=element_text(family="anton", size=30, color=pal_text),
    #adjust lines for ticks and grid
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x= element_line(color=pal_grid, size=0.2),
    #add padding around plot
    plot.margin = margin(t=20, b=20, l=20, r=20)
  )

ggsave("Spotify-Top-Tracks-Indo/spotify-top-tracks.png", units="in", width=10, height=10)