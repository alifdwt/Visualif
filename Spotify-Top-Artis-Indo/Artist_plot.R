library(tidyverse)
library(ggimage)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)

#set up fonts
sysfonts::font_add_google("Chivo","Chivo")
sysfonts::font_add("Gotham", regular = "C:/Users/aputr/AppData/Local/Microsoft/Windows/Fonts/Gotham-Light.otf", bold="C:/Users/aputr/AppData/Local/Microsoft/Windows/Fonts/Gotham-Bold.otf")
sysfonts::font_add('Font Awesome 6 Brands', 'C:/Users/aputr/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
showtext_opts(dpi=300)

df_artists <- read.csv("posts/Spotify-Total/helper/datasets/top_artists_indo.csv")

#create subset
df_sub = df_artists%>%
  arrange(desc(popularity))%>%
  head(100)%>%
  mutate(path = paste0(tolower(str_replace_all(artist," ","_")),".png"))

#import custom social caption function
source("posts/Spotify-Total/helper/social-caption.R")

#plot title, subtitle, and caption for ggtext (HTML/CSS)
title = "<span style='font-family:Gotham;color:white;font-size:35pt;'><span style='font-family:\"Font Awesome 6 Brands\";color:#1DB954;font-size:35pt;'>&#xf1bc;</span> **Artis <span style='color:#f70000;'>Indonesia</span> Ter<span style='color:#F7D22F;'>Pop</span>uler di <span style='color:#1DB954;'>Spotify</span>**</span>"
subtitle = "<p>Popularitas diambil dari Spotify: skala 0 (paling tak populer) sampai 100 (paling populer).</p>"
caption = paste0("<span>**Sumber: {Spotify API} / 20 Juni 2023 | Inspirasi: Tanya Shapiro**<span><br>",
                 social_caption(icon_color ="#1DB954", bg_color="black", font_color = "#D7DDDD",
                                font_family="sans",
                                linkedin="alifdwt"))

#create positional arguments (similar to beeswarm)
df_plot = df_sub%>%
  select(artist, popularity, path)%>%
  group_by(popularity)%>%
  mutate(group_count=n(),
         row = row_number()-1,
         type = case_when(group_count %% 2 ==0 ~ "even", TRUE ~ "odd"),
         spacer = case_when(popularity>73 ~ 1.50, 
                            popularity>=70 ~ 1.25,
                            popularity>=65 ~ 1.1,
                            group_count<13 ~ 0.9, 
                            TRUE ~ 0.5),
         max =0-((group_count/2)-0.5)*spacer,
         pos = max + spacer*row)%>%
  mutate(image =glue("posts/Spotify-Total/helper/images/circle-labels/{path}"))

#plot - use geom image to plot images
ggplot()+
  #change up size for images in different popularity ranges
  #size cannot be mapped to aes, set manually outside of aes
  geom_image(data=df_plot|>filter(popularity==75),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0.1, height=0.15),
             size = 0.1,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(popularity>=70 & popularity<75),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0.1, height=0.15),
             size = 0.08,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(popularity>=65 & popularity<70),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0.1, height=0.15),
             size = 0.065,
             asp=9.5/6)+
  geom_image(data=df_plot|>filter(popularity<65),
             aes(y = popularity, x = pos, image=image),
             position = position_jitter(width=0, height=0.12),
             size = 0.05,
             asp=9.5/6)+
  coord_flip()+
  scale_x_continuous(limits=c(-5.8,5.8), expand=c(0,0))+
  labs(title = title,
       subtitle = subtitle,
       caption = caption)+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill="black", color="black"),
        plot.title = element_textbox_simple(),
        plot.subtitle  = element_textbox_simple(color="#D7DDDD", size=15, margin=margin(t=8, b=10)),
        plot.caption = element_textbox_simple(color="#D7DDDD", margin=margin(t=15), size=12),
        plot.margin = margin(t=20, r=20, l=20, b=10),
        text = element_text(color="white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="#178F41",linewidth=0.25),
        axis.text.y=element_blank(),
        axis.text.x=element_text(family="Chivo", color="white", size=12),
        axis.ticks = element_blank(),
        axis.title = element_blank())

factor = 19/14
ggsave("posts/Spotify-Total/helper/images/plots/indos-popular-artists.png", height=14/factor, width=14, unit="in")