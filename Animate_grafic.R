require(dplyr)     # Manipula??o de dados
require(ggplot2)   # Cria??o de gr?ficos profissionais
require(gganimate) # Para usar TRANSITION_REVEAL() para anima??o (gif)
require(ggimage)   # Para usar imagem na anima??o
require(ggthemes)  # Mais temas do pacote ggplot
require(ggside)    # Para painel com 3 gr?ficos juntos


covid_ce <- read.csv2("C:/Users/Paulo Barbosa/Downloads/Compressed/HIST_PAINEL_COVIDBR_2020_Parte1_02out2021.csv",
                      colClasses = c(data='Date')) %>%  
  filter(estado == "CE" & municipio =="Paraipaba" |
           municipio=="Trairi" |
           municipio=="Paracuru") %>% 
  select(municipio,casosAcumulado,Recuperadosnovos, data)


covid_ce2 <- read.csv2("C:/Users/Paulo Barbosa/Downloads/Compressed/HIST_PAINEL_COVIDBR_2020_Parte2_02out2021.csv",
                       colClasses = c(data='Date')) %>%  
  filter(estado == "CE" & municipio =="Paraipaba" |
           municipio=="Trairi" |
           municipio=="Paracuru") %>% 
  select(municipio,casosAcumulado,Recuperadosnovos, data)


base <- full_join(covid_ce,covid_ce2)

#..............Anima??o............#
base$municipio %>%  table()

minha.anim<-
  base %>% 
  ggplot(aes(x=data, y=casosAcumulado, col=municipio))+
  geom_line()+
  geom_image(image="C:/Users/Paulo Barbosa/Pictures/Saved Pictures/corona.png",size=0.08)+
  labs(x=NULL,
       y="Casos acumulados",
       title = " Casos de Covid-19",
       subtitle = "Compara??o",
       col ="Municipio")+
  transition_reveal(along = data)

# ..........Dimensionando a imagem..........
# heiht: altura da imagem
# Width: largura da imagem
# duration: dura??o
# Res: resolu??o
nova_animacao <- minha.anim %>%  animate(height=422,width=622,duration=5, res=150)

# Salvando imagem no diret?rio especificado
save_animation(animation=nova_animacao, file="C:/Users/Paulo Barbosa/Pictures/Saved Pictures/Nova_animacao.gif")

require(plotly)
ggplotly(base[base$municipio=="Paracuru",] %>% ggplot()+geom_line(aes(y=casosAcumulado, x=data)))
#..............................................................#

theme_set(theme_gray())

# Gr?fico de Barras

covid_ce %>% 
  ggplot()+
  geom_col(aes(x=municipio ,y=casosAcumulado, fill=municipio),
           show.legend = FALSE)+
  coord_flip()+
  labs(x = NULL,
       y = NULL,
       fill= "Munic?pios")

# Apresenta??o gr?fica tripla usando pacote "ggside"
dados <- iris

# Pacote ggside
dados %>% 
  ggplot(aes(x=Sepal.Length, y=Petal.Width, color=Species))+
  geom_point(show.legend = T)+
  geom_ysidebar(fill = "red")+
  geom_xsideboxplot(aes(y=Petal.Width), orientation = "x")+
  scale_xsidey_discrete()+
  scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +
  theme(ggside.panel.scale = .45)


