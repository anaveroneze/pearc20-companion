#!/usr/bin/Rscript
library(ggplot2)
library(dplyr)
library(wesanderson)
require(gridExtra)
options(crayon.enabled=FALSE)

#Rename and reorder cases
x <- c('ligurian', 'chuetsuoki', 'sishuan')
case.labs <- c('Ligurian', 'Chuetsu-Oki','Sishuan')

#Set individual breaks for the y-axis of each case
breaks_cases <- function(x) {
	if(max(x) > 2)
        	  seq(0, 15, 2.5)
}

df  <-  read.csv("./../data/all_traces.csv")

df %>%
	filter(Type == "Application") %>%
	filter(Operation != "seisMoment") %>%
	mutate(Case = factor(Case, levels = x, labels = case.labs)) %>%
       	group_by(Case, Rank, Iteration) %>%
        summarize(Load = sum(Duration)) %>%
       	ggplot(aes(x = Iteration, y = Load, color=as.factor(Rank))) +
       	geom_point(alpha=0.5, size=1) +
        scale_color_manual(values = wes_palette(64, name = "Zissou1", type = "continuous"), name = "") +
        scale_x_continuous("Iteration", limits=c(0,NA), expand = c(0, 0)) +
        scale_y_continuous("Duration (s)", limits=c(0,NA), expand = c(0, 0)) +
        theme_bw(base_size=22) +
        theme(plot.margin = unit(c(0,0.5,0,0.5), "cm"),
              panel.grid = element_blank(),
              text = element_text(size=22),
              legend.position = "none") +
        labs(color = "Rank") +
        facet_wrap(~Case, scales="free")

ggsave('./../img/PEARC_8x8_load_per_iteration_facetwrap.png', device="png", width=300, height=100, units="mm")
