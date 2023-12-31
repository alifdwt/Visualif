library(tidyverse)
library(geomtextpath)
library(ggtext)
library(htmltools)
library(sysfonts)
library(showtext)

# Mengimpor font
sysfonts::font_add_google("Barlow","Barlow")
sysfonts::font_add_google("Roboto","Roboto")
sysfonts::font_add('fb', 'C:/Users/aputr/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

# data preprocessing
audio_mood <- read.csv("Spotify-Indo-Mood/datasets/audio_mood.csv", row.names = 1)
audio_features <- read.csv("Spotify-Indo-Mood/datasets/audio_features.csv")
colnames(audio_features)[1] <- "numCharts"
df_audio <- merge(audio_mood, audio_features%>%select(id, release_date),
                  by.x = "uri", by.y = "id")
df_audio$tahun <- substring(df_audio$release_date, 1, 4)

df_plot <- df_audio %>%
  select(tahun, mood) %>%
  group_by(tahun, mood) %>%
  summarize(jumlah = n()) %>%
  filter(tahun >= 2014 & tahun <= 2023)

# Title
title = "<span style='font-family:Barlow;color:white;'><span style='font-family:fb;color:#1DB954;font-size:35pt;'>&#xf1bc;</span> **Spotify <span style='color:#f70000;'>Indonesia**</span>: Suasana <span style='color:#fc5c65;'>Hati.</span></span>"

# Subtitle
subtitle = "<span>Grafik ini memvisualisasikan suasana hati dari **+4000** lagu teratas Indonesia di Spotify berdasarkan tahun rilisnya. Spotify menggunakan teknologi pemrosesan sinyal digital dan algoritma untuk menganalisis berbagai aspek dari rekaman audio, lalu menghasilkan serangkaian fitur yang menggambarkan sifat-sifat musik tersebut.</span>"

# Caption
caption = paste0("<span style='font-family:Barlow;'>Source: **Spotify API / 20 Juni 2023**</span><br>",
       "<span style='font-family:fb;'>&#xf099;</span>",
       "<span style='font-family:Barlow;color:black;'>.</span>",
       "<span style='font-family:Barlow;'>@alifdwt</span>",
       "<span style='font-family:Barlow;color:black;'>... </span>",
       "<span style='font-family:fb;'>&#xf09b;</span>",
       "<span style='font-family:Barlow;color:black;'>.</span>",
       "<span style='font-family:Barlow;'>alifdwt</span>",
       "<span style='font-family:Barlow;color:black;'>... </span>",
       "<span style='font-family:fb;'>&#xf09b;</span>",
       "<span style='font-family:Barlow;color:black;'>.</span>",
       "<span style='font-family:Barlow;'>tashapiro</span>"
)

# Membuat plot
ggplot(df_plot, aes(x=tahun, y=jumlah, group = mood, color=mood)) +
  geomtextpath::geom_textline(mapping = aes(label=mood),
                              family="Roboto",
                              linewidth=1, hjust=1, vjust=.5, fontface="bold") +
  scale_colour_manual(values = c("#4ebeef","#fed361","#ff4244","#1d6d86")) +
  labs(y = "Jumlah Rilis",
       x = "",
       title = title,
       subtitle = subtitle,
       caption = caption) +
  theme(text=element_text(family="Roboto", color = "white"), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_blank(),
        plot.margin  = ggplot2::margin(t=20, r=40, l=15, b=10),
        axis.text = element_text(color = "white"),
        axis.title.y = element_text(margin=margin(r=10)),
        plot.title = ggtext::element_textbox_simple(face="bold", size=26, margin=margin(b=7)),
        plot.caption = ggtext::element_textbox_simple(color="#D7DDDD"),
        plot.subtitle = ggtext::element_textbox_simple(color="#D7DDDD", family="Roboto", margin=margin(b=10)),
        legend.position="none",
        panel.grid.major.y = element_line(color="grey30", size=.2),
        axis.ticks = element_blank())

#save
ggsave("Spotify-Indo-Mood/spotify-indo-mood.png", units="in", width=9, height=6)