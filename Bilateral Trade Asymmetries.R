library(tidyverse)
library(data.table)
library(reshape2)
library(readxl)
library(treemap)
library(treemapify)
library(RColorBrewer)
library(gridExtra)


## BILATERAL TRADE ASYMMETRIES FOR ASIA PACIFIC ##

# A   Preliminaries ----

setwd("C:/Users/Janine De Vera/Documents/ADB/Others/ISI 2021/")
pathdata =("Comtrade 2015-2019/")
pathcharts = ("Charts/")

countries = c("AUS", "BRN", "CHN", "IND", "IDN", "JPN", "MYS", "NZL", 
              "PHL", "SGP", "KOR", "THA", "TUR", "VNM", "HKG")

charts.theme <- theme(axis.title.y.left = element_text(size = 12, margin = margin(r = 15)),
                      axis.title.y.right = element_text(size = 12, margin = margin(l = 15)),
                      axis.title.x = element_text(size = 12, margin = margin(t = 15)),
                      axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
                      axis.text.y = element_text(size = 12),
                      axis.ticks = element_blank(),
                      axis.line.x = element_line("black", size = 0.5), 
                      axis.line.y = element_line("black", size = 0.5),
                      panel.background = element_rect(fill = "white", color = "white"),
                      panel.grid.major = element_line(color = "white"),
                      panel.grid.minor = element_line(color = "white"),
                      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(b = 15)),
                      legend.position = "bottom",
                      legend.box = "vertical",
                      legend.box.margin = margin(b = 15),
                      legend.margin = margin(r = 10),
                      legend.background = element_rect(fill = "transparent"),
                      legend.spacing.x = unit(0.4, "cm"),
                      legend.key = element_blank(),
                      legend.title = element_blank(),
                      legend.text = element_text(size = 12),
                      plot.caption = element_text(size = 8, hjust = 0))

# B   Data ----

filelist = list.files(path=pathdata, pattern = ".csv")
data.raw = ldply(filelist, read.csv, header=TRUE)

export = data.raw %>% filter(Trade.Flow == "Export", 
                             Partner.ISO %in% countries) %>% 
                      select(Year, Reporter, Reporter.ISO, Partner, Partner.ISO, Trade.Value..US..)

import = data.raw %>% filter(Trade.Flow == "Import",
                             Partner.ISO %in% countries) %>% 
                      select(Year, Reporter, Reporter.ISO, Partner, Partner.ISO, Trade.Value..US..)

data1 = merge(import, export, 
           by.x = c("Year", "Partner.ISO", "Reporter.ISO"), 
           by.y = c("Year", "Reporter.ISO", "Partner.ISO")) 

colnames(data1)[c(6,9)] <- c("Import", "Export")

data1 = data1 %>% mutate(Import = Import/1000000,
                         Export = Export/1000000,
                         DIF = (Import-Export)/Import,
                         Disc = abs(Import - Export))

countries = read_excel("Countries.xlsx", sheet = 2)

write.csv(data1, "Bilateral Trade 2005-2019.csv")

# C   Section 3 ----

  #   C1 Line Chart ----
data2.annual = data1 %>% select(Year, Export, Disc) %>% 
                         aggregate(. ~ Year, ., sum) %>% 
                         mutate(Disc = Disc / 1000,
                                Export = Export / 1000,
                                Percent = Disc/Export)

y1 <- min(data2.annual$Disc)
y2 <- max(data2.annual$Disc)
x1 <- min(data2.annual$Percent) * 100 - 10
x2 <- max(data2.annual$Percent) * 100 + 10
b <- (y2 - y1) / (x2 - x1)
a <- y1 - b * x1


C1 <- data2.annual %>% 
      mutate(perc.scaled = a + b * Percent * 100) %>% 
      ggplot(aes(x = Year, y = Disc)) +
      geom_col(data = data2.annual,
               aes(x = Year, y = Disc), fill = "#007db7") +
      #geom_point(aes(x = Year, y = perc.scaled), color = "#8dc63f", size = 4) +
      geom_line(aes(x = Year, y = perc.scaled), color = "#8dc63f", size = 2) +
      #geom_label(aes(x = Year, y = perc.scaled, label = round(Percent * 100, 2)), 
                 #fill = "#8dc63f", label.size = NA, size = 3, fontface = "bold", color = "white", vjust = 1.5) +
  charts.theme +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        #panel.grid.major.x = element_line(color = "#dedede"),
        panel.grid.major.y = element_line(color = "#dedede"),
        axis.line.x = element_line("transparent", size = 0.5), 
        axis.line.y = element_line("transparent", size = 0.5),
        panel.border = element_rect(color = "#a3a3a3", fill = "transparent")) +
  guides(fill = guide_legend(reverse=TRUE, nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = c(2005:2019)) +
  scale_y_continuous(name = "Trade Discrepancy (Billion USD)", 
                     sec.axis = sec_axis(~(.-a)/b, name = "Trade Discrepancy (Percent of Exports)")
                     ) +
  coord_cartesian(ylim = c(100, 1200)) +
  labs(title = "Figure 1. Total Trade Asymmetries" ,
       subtitle = "Selected Asia Pacific Economies, 2005-2019",
       caption = "Source: UN Comtrade Database.") +
  xlab("") +
  guides(fill=guide_legend(title=""))
C1

ggsave(filename="3.1.png", plot=C1, device="png", path=pathcharts, 
       width = 12, height = 9)


  #   C2 Tree Map ----

data3.export = data1 %>% select(Year, Reporter.y, Export, Disc) %>% 
                         aggregate(. ~ Year + Reporter.y, ., sum) %>% 
                         filter(Year == 2019) %>% 
                         mutate(Disc.Total = sum(Disc),
                                Share = Disc / Disc.Total,
                                Rank = rank(desc(Share))) %>% 
                         merge(., countries, by.x = "Reporter.y", by.y = "Country")

others = as.data.frame(c("Others", 2019, 0, 0 , 0, data3.export %>% filter(Rank > 9) %>% summarise(sum(Share)), 
                         10, "Others", "OTH"))
colnames(others) = colnames(data3.export)

data3 = data3.export %>% filter(Rank <= 9) %>% 
        rbind(., others) %>% 
        mutate(Reporter.y = replace(Reporter.y, Reporter.y == "China, Hong Kong SAR", "Hong Kong, China"),
               Reporter.y = replace(Reporter.y, Reporter.y == "China", "PRC"))

C2 <- ggplot(data3 %>% arrange(Share), 
             aes(area = Share, 
                 fill = Reporter.y,
                 label = paste0(Reporter.y, " (", round(Share*100), "%", ")"), 
                 subgroup = Region)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_subgroup_text(
    place = "top",
    colour = "white",
    fontface = "italic",
    alpha = 0.7,
    size = 11
  ) +
  geom_treemap_text(
    colour = "white", 
    place = "middle", 
    fontface = "bold",
    size = 10,
    reflow = T) + 
  scale_fill_manual(values = c("#8dc63f", "#007db7", "#41bee8", "#e9712b", "#c8da2b",
                               "#00a1cb", "#68c5ea", "#e9532b", "#f57f29", "#fdb515" )) + 
  labs(title = "Figure 2. Share in Asymmetries" ,
       subtitle = "Selected Asia Pacific Economies, 2019",
       caption = "Source: UN Comtrade Database. Authors' estimates.") +
  charts.theme +
  theme(legend.position = "none")
C2

ggsave(filename="3.2.png", plot=C2, device="png", path=pathcharts, 
       width = 12, height = 9)


  #   C3 Box Plot (Export) ----

data4 = merge(data1 %>% filter(Year > 2009), countries, by.x = "Partner.ISO", by.y = "ISO")
data4$Region = factor(data4$Region, levels = c("East and North East Asia", "Southeast Asia", "South and South West Asia",
                                               "North and Central Asia", "Pacific"))

C3 <- ggplot(data4, aes(x=fct_reorder(ADB, as.numeric(Region)), y=DIF * 100)) +
  geom_boxplot(aes(fill = Region), color="black", outlier.alpha = 0)+
  charts.theme +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        axis.line.x = element_line("transparent", size = 0.5), 
        axis.line.y = element_line("transparent", size = 0.5),
        panel.border = element_rect(color = "#a3a3a3", fill = "transparent"),
        legend.text = element_text(size = 16)) +
  xlab("") +
  ylab("DIF") + 
  scale_fill_manual(values = c("#007db7", "#e9532b", "#fdb515", "#8dc63f", "#c8da2b")) +
  scale_y_continuous(limits = c(-100,100), breaks = c(-100, -80, -60, -40, -20, 
                                                     0, 20, 40, 60, 80, 100)) 
  labs(title = "Figure 3. Distribution of Discrepancy Indices",
       subtitle = "Selected Asian Economies (as exporters), 2010-2019",
       caption = "       HKG = Hong Kong, China; JPN = Japan; KOR = Republic of Korea; PRC = People's Republic of China\ 
       BRN = Brunei Darussalam, IDN = Indonesia, MYS = Malaysia, PHL = Philippines, SGP = Singapore, \
       THA = Thailand, VNM = Viet Nam, IND = India, TUR = Turkey, AUS = Australia, NZL = New Zealand\
       Source: UN Comtrade Database. Authors' estimates.") 
C3

ggsave(filename="Fig 3 (no label).png", plot=C3, device="png", path=pathcharts, 
       width = 12, height = 9)

  #   C4 Box Plot (Import) ----

data5 = merge(data1 %>% filter(Year > 2009), countries, by.x = "Reporter.ISO", by.y = "ISO")
data5$Region = factor(data5$Region, levels = c("East and North East Asia", "Southeast Asia", "South and South West Asia",
                                               "North and Central Asia", "Pacific"))


C4 <- ggplot(data5, aes(x=fct_reorder(ADB, as.numeric(Region)), y=DIF * 100)) +
  geom_boxplot(aes(fill = Region), color="black", outlier.alpha = 0)+
  charts.theme +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        axis.line.x = element_line("transparent", size = 0.5), 
        axis.line.y = element_line("transparent", size = 0.5),
        panel.border = element_rect(color = "#a3a3a3", fill = "transparent"),
        legend.text = element_text(size = 16)) +
  xlab("") +
  ylab("DIF") + 
  scale_fill_manual(values = c("#007db7", "#e9532b", "#fdb515", "#8dc63f", "#c8da2b")) +
  scale_y_continuous(limits = c(-100,100), breaks = c(-100, -80, -60, -40, -20, 
                                                      0, 20, 40, 60, 80, 100)) 
  labs(title = "Figure 4. Distribution of Discrepancy Indices",
       subtitle = "Selected Asian Economies (as importers), 2010-2019",
       caption = "       HKG = Hong Kong, China; JPN = Japan; KOR = Republic of Korea; PRC = People's Republic of China\ 
       BRN = Brunei Darussalam, IDN = Indonesia, MYS = Malaysia, PHL = Philippines, SGP = Singapore, \
       THA = Thailand, VNM = Viet Nam, IND = India, TUR = Turkey, AUS = Australia, NZL = New Zealand\
       Source: UN Comtrade Database. Authors' estimates.") 
C4

ggsave(filename="Fig 4 (no label).png", plot=C4, device="png", path=pathcharts, 
       width = 12, height = 9)


  #   C5 Heat Map ----

data6 = merge(data1, countries, by.x = "Partner.ISO", by.y = "ISO") %>% 
        merge(., countries, by.x = "Reporter.ISO", by.y = "ISO")

colnames(data6)[c(14, 17)] <- c("Exporter", "Importer")

data7 = data6 %>% filter(Year == 2019) %>%
                  mutate(DIF2 = abs(DIF) * 100, 
                         t = ifelse(abs(DIF2) > 20, 1, 0),
                         DIF3 = ifelse(DIF2 > 100, 100, DIF2)) %>% 
                  group_by(Exporter) %>% 
                  mutate(count = sum(t)) %>% 
                  ungroup() %>% 
                  mutate(rank = rank((count), ties.method = "min"))
                  
data7$Importer = factor(data7$Importer, levels = c("SIN", "HKG", "TUR", "PHI", "VIE", "IND", "BRU", "AUS", 
                                                   "PRC", "INO", "MAL", "JPN", "THA", "NZL", "KOR"))
                          
#levels = c("KOR", "NZL", "THA", "JPN", "MAL", "INO", "PRC", "AUS", "BRU", "IND", "VIE", "PHI", "TUR", "HKG", "SIN")
#levels = c("SIN", "HKG", "TUR", "PHI", "VIE", "IND", "BRU", "AUS", "PRC", "INO", "MAL", "JPN", "THA", "NZL", "KOR")

data8 = data6 %>% filter(Year == 2010) %>%
        mutate(DIF2 = abs(DIF) * 100, 
               t = ifelse(abs(DIF2) > 20, 1, 0),
               DIF3 = ifelse(DIF2 > 100, 100, DIF2)) %>% 
        group_by(Exporter) %>% 
        mutate(count = sum(t)) %>% 
        ungroup() %>% 
        mutate(rank = rank((count), ties.method = "min"))

data8$Importer = factor(data8$Importer, levels = c("SIN", "HKG", "TUR", "PHI", "VIE", "IND", "BRU", "AUS", 
                                                   "PRC", "INO", "MAL", "JPN", "THA", "NZL", "KOR"))

data8$Exporter = factor(data8$Exporter, levels = c("SIN", "HKG", "TUR", "PHI", "VIE", "IND", "BRU", "AUS", 
                                                   "PRC", "INO", "MAL", "JPN", "THA", "NZL", "KOR"))


length(unique(data6$DIF))
colors <- colorRampPalette(c("#e9532b", "#f57f29", "#fdb515", "#8dc63f", "#f2e600", "#63ccec", "#007db7"))(338)
colors <- colorRampPalette(c("#e9532b", "#f2e600"))(210)

      # 2019 #
C5 <- ggplot(data = data7 %>% filter(), mapping = aes(x = Importer, 
                                         y = fct_reorder(Exporter, rank))) + 
  geom_tile(aes(fill = DIF3, alpha = as.character(t))) +
  scale_fill_gradient(low = "#f2e600", high = "#e9532b",
                      breaks = c(0, 20, 40, 60, 80, 100),
                      labels = c("< 1%", "   20%", "   40%", "   60%", "   80%", "> 100%")) +
  guides(alpha = FALSE, fill = guide_legend(reverse = TRUE, override.aes = list(size = 10))) +
  scale_alpha_manual(values = c(0.15, 1)) +
  charts.theme +
  theme(axis.line.x = element_line("transparent", size = 0.5), 
        axis.line.y = element_line("transparent", size = 0.5),
        #panel.border = element_rect(color = "black", fill = "transparent"),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        legend.key.size = unit(0, "cm"))  + 
  scale_x_discrete(position = "top") +
  xlab(label = "") +
  ylab(label = "") +
  labs(title="Figure 2. Bilateral Discrepancy Indices", 
       subtitle="Selected Asian Economies, 2019")
C5

ggsave(filename="Fig 2 (ver1).png", plot=C5, device="png", path=pathcharts, 
       width = 12, height = 9)


      # 2010 #
C6 <- ggplot(data = data8 %>% filter(), mapping = aes(x = Importer, 
                                                      y = fct_reorder(Exporter, desc(Exporter)))) +
  geom_tile(aes(fill = DIF3, alpha = as.character(t))) +
  scale_fill_gradient(low = "#f2e600", high = "#e9532b",
                      breaks = c(0, 20, 40, 60, 80, 100),
                      labels = c("< 1%", "   20%", "   40%", "   60%", "   80%", "> 100%")) +
  guides(alpha = FALSE, fill = guide_legend(reverse = TRUE, override.aes = list(size = 10))) +
  scale_alpha_manual(values = c(0.15, 1)) +
  charts.theme +
  theme(axis.line.x = element_line("transparent", size = 0.5), 
        axis.line.y = element_line("transparent", size = 0.5),
        #panel.border = element_rect(color = "black", fill = "transparent"),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        legend.direction = "vertical",
        legend.position = "right",
        legend.key.size = unit(0, "cm"))  + 
  scale_x_discrete(position = "top") +
  xlab(label = "") +
  ylab(label = "")
  labs(title="Figure 1. Bilateral Discrepancy Indices", 
       subtitle="Selected Asian Economies, 2010")
C6

ggsave(filename="Fig 1 no label.png", plot=C6, device="png", path=pathcharts, 
       width = 12, height = 9)

