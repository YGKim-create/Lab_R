{fig.showtext=TRUE}

install.packages(c("devtools","dplyr","ggplot2","readr","gganimate","tidyverse","showtext","readxl","showtext"))
library(ggplot2)
library(devtools)
library(dplyr)
library(readr)
library(gganimate)
library(tidyverse)
library(readxl)
library(showtext)

#project file에서 전처리 된 엑셀 데이터를 불러온다. 
sig <- read_excel("sig2.xlsx")
str(sig)
sig$year2 <- as.integer(sig$year)
str(sig)

#폰트설정
font_add('ilopge',file.choose())
showtext_auto()

#png plot 생성
sigplot <- ggplot(sig) +
  aes(x = st, y = sur, colour = Country, size = GDPp, frame=year2) +
  geom_point(show.legend = TRUE, alpha = 0.7)+
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_y_log10()+
  labs(x = "소득불평등 정도" , y = "자살률")+
  theme_dark(base_family = "ilopge")


#legend 설정
sigplot <- sigplot+guides(size = FALSE)
sigplot <- sigplot+theme(legend.spacing = unit(0.4,"cm"), legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.5,"cm"),legend.justification = c(0.9,0.1) , legend.position = c(0.9,0.1))
sigplot <-  sigplot+theme(legend.position = "left", legend.box = "horizontal", legend.background = element_rect(fill="#000D11"), legend.text = element_text(color = "#ffffff"), legend.title = element_text(color="#ffffff"))


#윈도우 창 확인
windows()


#animated chart 생성
plot <-  sigplot + transition_time(year2)+
  labs(title =  "소득불평등에 따른 자살률 변동 추이", 
       subtitle = "Year : {frame_time}", 
       caption = "Data Source From : KOSIS")+
  view_follow(fixed_y = TRUE)+
  shadow_mark(alpha = 0.3, size = 0.5)+
  theme(plot.title = element_text(family = "ilopge", color = "gray", size = 15, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "#001419",colour = "#001419"), 
        panel.grid.major = element_line(linetype = "solid",colour ="gray"),panel.grid.minor = element_line(linetype = "solid",colour ="gray"),
        axis.title.x = element_text(color="gray") , axis.title.y = element_text(color="gray"), 
        plot.subtitle = element_text(color="gray"), 
        plot.caption = element_text(color="gray"),
        plot.background = element_rect(fill="#001419"))

#aminated plot chart 확인
plot

#gif로 렌더링 
animate(plot, fps = 13, width = 650, height = 400,dpi = 300, render = gifski_renderer("final test.gif"))