library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(gggibbous)
library(forcats)
library(ggpath)

# Memuat data
lyrics_mood <- read.csv("Sentimen-Artis-Indo/datasets/lyrics_mood.csv", row.names = 1) %>%select(-c(numCharts, id, language, lyrics_translated, track, lyrics, vader_neg, vader_neu, vader_pos, vader_compound))
top_artists_indo <- read.csv("Sentimen-Artis-Indo/datasets/top_artists_indo.csv", row.names = 1) %>% head(16)

# Memuat font dan warna
txt <- "white"
bg <- "#191919"
accent <- "#22A699"

pal <- c(Positive = "#22A699", Neutral =  "#F2BE22", Negative = "#F24C3D")

sysfonts::font_add_google("Barlow","Barlow")
sysfonts::font_add_google("Roboto","Roboto")
sysfonts::font_add('fb', 'C:/Users/aputr/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

# Data Wrangling
df <- merge(lyrics_mood, top_artists_indo%>%select(artist), by = 'artist')

df = df %>%
  rename("Negative" = "roberta_neg",
         "Neutral" = "roberta_neu",
         "Positive" = "roberta_pos") %>%
  group_by(artist) %>%
  mutate(
    Negative = mean(Negative),
    Neutral = mean(Neutral),
    Positive = mean(Positive)
  ) %>%
  distinct() %>%
  mutate(
    path = paste0(tolower(str_replace_all(artist," ","_")),".png"),
    image = glue("Spotify-Top-Artis-Indo/images/circles/{path}")
  ) %>%
  pivot_longer(cols = c(Negative, Neutral, Positive), names_to = "label", values_to = "value") %>%
  mutate(
    pct = paste0(round(value, 2)*100, "%"),
    p = value/sum(value),
    cm = cumsum(p),
    y_text = ifelse(is.na(lag(cm)), 0, lag(cm)) + p/2,
    label = factor(label, level = c("Positive", "Neutral", "Negative"))
  )

caption <- paste0("<span>Sumber Lirik: **Musixmatch** |  </span>",
                             "<span style='font-family:fb;'>&#xf099;</span>",
                             "<span style='color:#191919;'>.</span>",
                             "<span>@alifdwt</span>",
                             "<span style='color:#191919;'>..</span>",
                             "<span style='font-family:fb;'>&#xf09b;</span>",
                             "<span style='color:#191919;'>.</span>",
                             "<span>alifdwt</span>")

# Plot
df %>%
  ggplot() +
  geom_col(aes(3, value, fill = label, colour = label), width = 1, alpha = 0.9) +
  geom_from_path(aes(0, 0, path = image), distinct(df, artist, image), width = 0.3) +
  geom_text(aes(4.4, y_text, label = pct), colour = txt) +
  geom_text(aes(5, 1, label = artist), colour = txt) +
  facet_wrap(~artist) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  xlim(0, 5) +
  coord_polar("y", clip = "off") +
  labs(
    title = "Sentimen Artis Indonesia",
    subtitle = "Analisis sentimen dari lagu-lagu artis teratas Indonesia",
    fill = "Sentimen",
    color = "Sentimen",
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto", size = 18, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 35, family = "Barlow", face = "bold", margin = margin(b = 10), hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 20, r = 30, l = 30),
    # strip.text = element_text(margin = margin(t = 5, b = 5)),
    strip.text= element_blank(),
    legend.position = "bottom"
  )

ggsave("Sentimen-Artis-Indo/sentimen-artis-indo.png", width = 12, height = 12, units = "in")