# 애니메이션 차트를 생성 후 gif로 렌더링 함
# author : KYG

install.packages("ggplot2"); install.packages("gganimate"); install.packages("tidyverse"); install.packages("janitor"); install.packages("scales"); install.packages("readxl"); install.packages("magrittr"); install.packages("dplyr"); install.packages("png"); install.packages("devtools"); install.packages("png")
library(ggplot2) ; library (gganimate); library(tidyverse); library(janitor); library(scales); library(readxl); library(magrittr); library(dplyr)

#상대 경로를 사용하여 파일을 읽어옴.
scr_tidy <- read.csv("SEC.csv")
head(scr_tidy)


#상위랭크 10개 필터링
scr_formatted <- scr_tidy %>% 
  group_by(year) %>% 
  mutate(rank=rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ", round(value/1e0))) %>% 
  group_by(Country.Name) %>% 
  filter(rank <= 10) %>% 
  ungroup()

#데이터 확인
scr_formatted

#차트
staticplot = ggplot(scr_formatted, aes(rank, group = Country.Name, 
                                       fill = as.factor(Country.Name), color = as.factor(Country.Name))) + 
  geom_tile(aes(y = value/2, 
                height = value, 
                width = 0.9), alpha = 0.8 , color = NA)+
  geom_text(aes(y=0, label = paste(Country.Name, " ")), vjust = 0.2 , hjust = 1) +
  geom_text(aes(y=value, label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=0.1, color="grey" ),
        panel.grid.minor.x = element_line( size=0.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

# 애니메이션 차트 제목, 소제목, 캡션
anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Children Sexual Criminal per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "CSC Millions USD | Data Source: UNDP")

# gif 프레임, fps, size, 파일이름 지정
animate(anim, 200, fps = 20,  width = 800, height = 600, 
        renderer = gifski_renderer("gganim2.gif"))
