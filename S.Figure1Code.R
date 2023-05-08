d<-read.csv("/Users/olaozernov-palchik/Dropbox (MIT)/GitHub/procedural-learning/FINAL_learning_df_050123.csv")

ggplot(d, aes(x = slopeProp_On, y = WR, by=Subgroup)) +
  stat_smooth (
    method = "glm",
    formula = y ~ x,
    colour = "black",
    size = 1) +
  geom_point(aes(size = 3,shape = Subgroup)) +
  labs(x = "Rotary Pursuit Proportion On Slope ", y = "Decoding Skills") +
  theme(axis.title = element_text(family = "Trebuchet MS", size = 20))+ 
  theme(panel.background = element_blank(),legend.position = "none") + 
  theme(axis.text = element_text(size = 20))