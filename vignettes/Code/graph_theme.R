library(ggplot2)
library(grid)
# define consistent ggplot theme to apply to all figures
theme_ms <- function(base_size=10, base_family="Times") {
  library(grid)
  (theme_bw(base_size = base_size, base_family = base_family)+
      theme(text=element_text(color="black"),
            axis.title=element_text(face="bold", size = rel(1.3)),
            axis.text=element_text(size = rel(1), color = "black"),
            legend.title=element_text(face="bold"),
            legend.text=element_text(face="bold"),
            legend.background=element_rect(fill="transparent"),
            legend.key.size = unit(0.8, 'lines'),
            panel.border=element_rect(color="black",size=1),
            panel.grid=element_blank()
      ))
}

theme_pub <- function(){
  theme_bw(base_family = "LM Roman 10") + theme(legend.position = "bottom",
                     legend.title = element_blank(),
                     panel.background = element_rect(colour = NA),
                     plot.background = element_rect(colour = NA),
                     panel.border = element_rect(colour = NA),
                     axis.title = element_text(size = rel(1)),
                     axis.title.y = element_text(angle=90,vjust =2),
                     axis.title.x = element_text(vjust = -0.2),
                     axis.text = element_text(),
                     axis.line = element_line(colour="black"),
                     axis.ticks = element_line(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0")
  )
}


theme_set(theme_pub())
