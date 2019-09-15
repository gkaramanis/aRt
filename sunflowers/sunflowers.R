library(ggplot2)
library(here)

a=pi*(5-sqrt(7))
n=500
ggplot(data.frame(r=sqrt(1:n),t=(1:n)*a),
       aes(x=r*cos(t),y=r*sin(t)))+
  geom_point(aes(x=0,y=0),
             size=220,
             colour="violetred")+
  geom_point(aes(size=(n-r)),
             shape=21,fill="gold",
             colour="gray90")+
  theme_void()+theme(legend.position="none")
  
ggsave(here("sunflowers", "sunflower.png"))