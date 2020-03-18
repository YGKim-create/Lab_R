{fig.showtext=TRUE}
install.packages(c("devtools","dplyr","ggplot2","readr","gganimate","tidyverse","showtext","readxl","showtext"))
library(devtools)
library(showtext)
library(dplyr)
library(ggplot2)
library(readr)
library(gganimate)
library(tidyverse)
library(readxl)
library(scales)

#옵션설정
options("scipen" = 10)
#상대 경로를 통해 file을 불러옴
chi <- read_excel("ch.xlsx")
str(chi)

# year값을 integer 타입으로 변경
chi$연도 <- as.integer(chi$시점)
str(chi)

#폰트설정
font_add('SCD',file.choose())
showtext_auto()

#변수 명 수정
chi <- rename(chi, 연도 = year)

str(chi) #수정된 변수 명 확인
names(chi) <- c("연도","연령별","환자수")
#plot 생성 (연도에 따른 총급여비용 변동추이)

p <- ggplot(chi, aes(x = 연도, y = 환자수, fill = 연령별)) +
  labs(title =  "연도별 치매 환자 총 관리 비용 변동추이", 
       caption = "Data Source From : KOSIS")+
  scale_y_continuous(labels = scales::comma)+
  geom_col() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text (family = "SCD", color = "#52290a", size = 15, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE,
    axis.text.y = element_text(angle=90, hjust=1),
    legend.position = "bottom",
    legend.key.width = unit(3,'cm'))
#    legend.position = "none")

p #확인


#animated chart
p2 <- p + transition_states(환자수, wrap = FALSE) + 
  shadow_mark()+
  enter_grow()+
  enter_fade()+
  exit_recolor(color = "#FEEFDC", fill = color)


#plot확인
p2 


#gif 렌더링
animate(p2, width = 800, height = 400, render = gifski_renderer("first.gif"))