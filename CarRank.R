# 애니메이션 차트를 생성 후 gif로 렌더링 함
# author : KYG


install.packages("ggplot2"); install.packages("gganimate"); install.packages("tidyverse"); install.packages("janitor"); install.packages("scales"); install.packages("readxl"); install.packages("magrittr"); install.packages("dplyr"); install.packages("png"); install.packages("devtools"); install.packages("showtext")
library(ggplot2) ; library (gganimate); library(tidyverse); library(janitor); library(scales); library(readxl); library(magrittr); library(dplyr); library(showtext)


#상대 경로를 사용하여 파일을 읽어옴.
car2 <- read.csv("car2.csv")
str(car2)
summary(car2)


#폰트설정
{fig.showtext=TRUE}
font_add('jeju',file.choose())
showtext_auto()


#데이터 필터링
card <- car2 %>%
  group_by(date1) %>%
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e0))) %>%
  group_by(carname) %>% 
  filter(rank <=10) %>%
  ungroup()

#차트 생성

my_theme <- theme_classic(base_family = "jeju") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))

plott <- ggplot(data = card)+
  aes (group = carname, fill = brand)+
  theme(base_family = "jeju")+
  aes (xmin = 0,
       xmax = value/1)+
  aes(ymin = rank - .45,
      ymax = rank + .45)+
  scale_y_reverse()+
  scale_x_continuous(
    limits = c(-2000, 17000),
    breaks = c(0,5000,10000,15000),
    labels = c(0,5000,10000,15000)
  )+
  labs(fill="")+
  geom_rect(alpha = .7)+
  labs(x = "자동차 판매 대수")+
  theme(base_family = "jeju")+
  aes(label = carname, y = rank)+
  labs (y="")+
  geom_text(x = 16000 , y = -10,
            family = "jeju",
            aes(label = as.character(date1)),
            size = 20, col = "grey18")+
  geom_text(col = "gray13",
            hjust = "right",
            x = -50)+
  scale_fill_manual(values = c("#FFCC00","#666699","#000099","#FF0000","#3366CC","#009966"))+
  my_theme


windows()
plott


#애니메이션 렌더링링

anim = plott + transition_states(date1, transition_length = 4, state_length = 1)

animate(anim, 300, fps = 8, width = 800, height = 600,
        renderer = gifski_renderer("gganimtest.gif"))
