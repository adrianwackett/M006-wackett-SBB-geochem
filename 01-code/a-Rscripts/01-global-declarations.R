#### Declare cowplot custom theme ####
# set custom theme
custom_theme <- cowplot::theme_cowplot()
custom_theme <- 
  custom_theme + 
  theme_bw() + 
  theme(strip.background = element_blank(), panel.border = element_rect(colour = "black"), strip.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5), colour = 1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.key = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  panel.background = element_rect(colour = "black", size=1, fill=NA))
