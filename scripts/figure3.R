#!/usr/bin/Rscript
library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)
require(gridExtra)
source("./imb_func.R")

df  <-  read.csv(file = "./../data/all_traces.csv")

df %>%
    filter(Case == "ligurian") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Load = sum(Duration)) %>%
    print -> df.ligurian.temp0;

df %>%
    filter(Case == "ligurian") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Iter.Duration = max(End) - min(Start)) %>%
    print -> df.ligurian.temp1;

# Chuetsuoki data
df %>%
    filter(Case == "chuetsuoki") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Load = sum(Duration)) %>%
    print -> df.chuetsuoki.temp0;

df %>%
    filter(Case == "chuetsuoki") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Iter.Duration = max(End) - min(Start)) %>%
    print -> df.chuetsuoki.temp1;

# Sishuan data
df %>%
    filter(Case == "sishuan") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Load = sum(Duration)) %>%
    print -> df.sishuan.temp0;

df %>%
    filter(Case == "sishuan") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Iter.Duration = max(End) - min(Start)) %>%
    print -> df.sishuan.temp1;

x <- c('ligurian', 'chuetsuoki', 'sishuan')
case.labs <- c('Ligurian', 'Chuetsu-Oki','Sishuan')

my_theme <- function() {
    theme_bw(base_size=22) +
        theme(plot.margin = unit(c(0,1,0,0), "cm"),
              legend.spacing = unit(1.5, "mm"),
              panel.grid = element_blank(),
              plot.title=element_text(hjust=0, size = 22, face = "bold"),
              legend.position = "none",
              legend.justification = "left",
              legend.box.spacing = unit(0.5, "cm"),
              legend.box.margin = margin(0,0,0,0),
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 22),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank())
}

df.ligurian.temp0 %>%
    left_join(df.ligurian.temp1) %>%
    group_by(Iteration) %>%
    summarize(maxDur = max(Load),
              N = n(),
              PI = metric_percent_imbalance(Load, unique(maxDur)),
              IP = metric_imbalance_percentage(Load),
              IT = metric_imbalance_time(Load, unique(maxDur)),
              STD = metric_imbalance_std(Load, unique(maxDur)),
              Norm = metric_imbalance_norm(Load, unique(maxDur))) %>%
    select(Iteration, "Percent Imbalance" = PI,  "Imbalance Percentage" = IP, "Imbalance Time" = IT, "Standard Deviation" = STD, "Norm" = Norm) %>%
    pivot_longer(-Iteration, names_to = "metric", values_to = "value") -> df.plot1;

df.chuetsuoki.temp0 %>%
    left_join(df.chuetsuoki.temp1) %>%
    group_by(Iteration) %>%
    summarize(maxDur = max(Load),
              N = n(),
              PI = metric_percent_imbalance(Load, unique(maxDur)),
              IP = metric_imbalance_percentage(Load),
              IT = metric_imbalance_time(Load, unique(maxDur)),
              STD = metric_imbalance_std(Load, unique(maxDur)),
              Norm = metric_imbalance_norm(Load, unique(maxDur))) %>%
    select(Iteration, "Percent Imbalance" = PI,  "Imbalance Percentage" = IP, "Imbalance Time" = IT, "Standard Deviation" = STD, "Norm" = Norm) %>%
    pivot_longer(-Iteration, names_to = "metric", values_to = "value") -> df.plot2;

df.sishuan.temp0 %>%
    left_join(df.sishuan.temp1) %>%
    group_by(Iteration) %>%
    summarize(maxDur = max(Load),
              N = n(),
              PI = metric_percent_imbalance(Load, unique(maxDur)),
              IP = metric_imbalance_percentage(Load),
              IT = metric_imbalance_time(Load, unique(maxDur)),
              STD = metric_imbalance_std(Load, unique(maxDur)),
              Norm = metric_imbalance_norm(Load, unique(maxDur))) %>%
    select(Iteration, "Percent Imbalance" = PI,  "Imbalance Percentage" = IP, "Imbalance Time" = IT, "Standard Deviation" = STD, "Norm" = Norm) %>%
    pivot_longer(-Iteration, names_to = "metric", values_to = "value") -> df.plot3;

df.plot1 %>%
    mutate(Case = "ligurian") %>%
    bind_rows(df.plot2 %>%
              mutate(Case = "chuetsuoki")) %>%
    bind_rows(df.plot3 %>%
              mutate(Case = "sishuan")) %>%
    print -> df.cases;

df.cases %>%
    mutate(Case = factor(Case, levels = x, labels = case.labs)) %>%
    ggplot(aes(x=Iteration, y=value, colour=metric)) + 
    geom_line(alpha=0.5, size=0.2, show.legend = FALSE) + 
    geom_point() + guides(colour = guide_legend(override.aes = list(size=4))) + 
    my_theme() + theme(legend.position = "top", legend.justification="left") + 
    scale_x_continuous(limits=c(0, NA), breaks=seq(0, 1000, 100), expand = c(0, 0)) +
    scale_color_manual(values = wes_palette(5, name = "Darjeeling1", type = "continuous"), name = "") +
    scale_y_continuous("Imbalance Metrics", limits=c(0, 1), expand = c(0, 0)) + 
    facet_wrap(~Case, scales="free", ncol=1)

ggsave('./../img/PEARC_8x8_max_per_iteration_facetwrap.png', device="png", width=400, height=200, units="mm")
