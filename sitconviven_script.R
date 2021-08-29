
library(tidyverse)
library(haven)
library(countrycode)
library(ggflags)
library(ggpubr)
library(ragg) # For the device for save the plot
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(scales)
library(ggtext)

# EVOLUCIÓN PAISES EN EL TIEMPO -------------

library(ggbump)
library(ggrepel)

data <- readxl::read_xls("ilc_lvps08.xls")
data$`GEO/TIME`



data <- data %>% 
  mutate(`2019` = coalesce(`2019`, `2018`)) %>% 
  gather(key = "year", value = "values", 4:18) %>% 
  rename(country = `GEO/TIME`) %>% 
  mutate(country = recode(country, 
                          "Germany (until 1990 former territory of the FRG)" = "Germany")) %>% 
  filter(country %in% c("United Kingdom",
                        "Belgium",
                        "Germany",
                        "Estonia",
                        "Ireland",
                        "Montenegro",
                        "Sweden",
                        "Switzerland",
                        "Finland",
                        "Slovenia",
                        "Denmark",
                        "Slovakia",
                        "Netherlands",
                        "Poland",
                        "Norway",
                        "France",
                        "Croatia",
                        "Spain",
                        "Iceland",
                        "Serbia",
                        "Austria",
                        "Italy",
                        "Lithuania",
                        "Portugal",
                        "Hungary",
                        "Latvia",
                        "Cyprus",
                        "Czechia")) %>% 
  select(-contains("2")) 

data$country <- countrycode(data$country, "country.name", "iso2c")

data <- data %>%
  mutate(country = str_to_lower(country)) 


data$year <- as.numeric(data$year)



data$year <- as.numeric(data$year)

data %>% 
  filter(year == 2019) %>% 
  summarise(mean(values)) 

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "bisque1"),
                  panel.background = element_rect(fill = "bisque1"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "none", 
                  axis.text.x = element_text(vjust = 0.5, hjust=1, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

p1 <- ggplot(data, aes(x = as.numeric(year), y = values, color = country, na.rm = T)) +
  geom_bump(size = 1.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 29.4) +
  annotate("text", x = 2012, y = 27.9, label = "Mean 2019 29.4", color = "darkred") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(2004, 2019, by = 1)) +
  #geom_text_repel(data = data %>% filter(year == min(year)),
                  #aes(x = year - 0.1, label = country),
                  #size = 3, hjust = 0) +
  #geom_text_repel(data = data %>% filter(year == max(year)),
                  #aes(x = year + 0.1, label = country),
                  #size = 3, hjust = 1) +
  geom_flag(data = data,
            aes(country = country),
            size = 8) +
  my_theme +
  labs(x = "",
       y = "",
       title = "Evolución de la población entre 25 y 34\naños que vive en casa de sus padres",
       subtitle = "Solo países seleccionados que coinciden con la ESS round 9",
       caption = "Fuente: Eurostat")

ggsave("evol.png", width = 17.5, height = 12, device = agg_png, dpi = 500)


  
# Nº DE HIJOS POR MUJER ------------------

children_per_woman <- readxl::read_xlsx("tfr-by-gapminder.xlsx",
                                        sheet = "countries_and_territories")

children_per_woman <- children_per_woman %>% 
  select(geo.name, `2019`) %>% 
  filter(geo.name %in% c("United Kingdom",
                         "Belgium",
                         "Germany",
                         "Estonia",
                         "Ireland",
                         "Montenegro",
                         "Sweden",
                         "Switzerland",
                         "Finland",
                         "Slovenia",
                         "Denmark",
                         "Slovak Republic",
                         "Netherlands",
                         "Poland",
                         "Norway",
                         "France",
                         "Croatia",
                         "Spain",
                         "Iceland",
                         "Serbia",
                         "Austria",
                         "Italy",
                         "Lithuania",
                         "Portugal",
                         "Hungary",
                         "Latvia",
                         "Cyprus",
                         "Czech Republic")) %>% 
  rename(country = geo.name) %>%
  rename(rate = `2019`) %>% 
  mutate(country = recode(country, 
                          "Czech Republic" = "Czechia",
                          "Slovak Republic" = "Slovakia"))

children_per_woman$country <- countrycode(children_per_woman$country, "country.name", "iso2c")

children_per_woman <- children_per_woman %>%
  mutate(country = str_to_lower(country)) 



data2019 <- data %>% 
  filter(year == 2019)


data2019 <- data2019 %>% 
  left_join(children_per_woman, by = "country") 



library(ggpubr)
cor.test(data2019$rate, data2019$values)

# lm-----------

ggplot(data2019, aes(x = rate, y = values)) +
  geom_smooth(method = "lm", se = F, color = "green") + # decidir si loees o lm
  geom_point(size = 2.1, alpha = 1, color = "red") +
  geom_flag(data = data2019,
            aes(country = country),
            size = 8) +
  scale_y_continuous(limits = c(0,70)) +
  scale_x_continuous(limits = c(1.2,2)) +
  geom_text_repel(aes(label = country), color = "black", size = 5) +
  labs(x = "Hijos por mujer", 
       y = "% Personas que viven en casa de sus padres") +
  labs(title = "Relación entre jóvenes de 25-34 años\nque viven en casa de sus padres y\n nº de hijos por mujer",
       subtitle = "año 2019",
       caption = "Fuente: Eurostat y gapminder.org") 


res <- lm(values ~ rate, data2019)
summary(res)


# loess ---------
my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "none", 
                  axis.text.x = element_text(vjust = 0.5, hjust=1, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

res <- loess(values ~ rate, data2019)
summary(res)

p2 <- ggplot(data2019, aes(x = rate, y = values)) +
  geom_smooth(method = "lm", se = F, color = "green") +
  geom_smooth(method = "loess", se = F, color = "red", span = 0.25) +
  geom_point(size = 2.1, alpha = 1, color = "red") +
  geom_flag(data = data2019,
            aes(country = country),
            size = 8) +
  scale_y_continuous(limits = c(0,70), breaks = seq(0,70, by = 10), labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(limits = c(1,2), breaks = seq(1,2, by = 0.1)) +
  #geom_text_repel(aes(label = country), color = "black", size = 5) +
  labs(x = "Hijos por mujer", 
       y = "Vive en casa de sus padres") +
  labs(title = "Relación de población entre 25 y 34 años que vive\n en casa de sus padres y nº de hijos por mujer",
       subtitle = "Año 2019 ",
       caption = "Fuente: Eurostat y gapminder.org")  +
  my_theme


ggsave("rel.png", width = 15.5, height = 12, device = agg_png, dpi = 500)



 

# k means -------------------
library(factoextra)
library(FactoMineR)
library(ggrepel)

data2019$country <- countrycode(data2019$country,  "iso2c", "country.name",)


data2019 <- data2019 %>% 
  remove_rownames %>% 
  column_to_rownames(var="country") %>% 
  select(values, rate)

data2019 = as.data.frame(scale(data2019))

res.pca <- PCA(data2019,  graph = TRUE)


fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_nbclust(data2019, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data2019, kmeans, method = "gap_stat")
fviz_nbclust(data2019, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(32145151)
km <- kmeans(data2019,4)
km

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "none", 
                  axis.text.x = element_text(vjust = 0.5, hjust=1, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

p3 <- fviz_cluster(km, data = data2019, repel = T, addEllipses = T, 
             ggtheme = my_theme) +
  labs(title = "K-means de población entre 25 y 34 años que vive\n en casa de sus padres y nº de hijos por mujer",
       subtitle = "Año 2019",
       caption = "Fuente: Eurostat y gapminder.org",
       x = "Nº de hijos por mujer",
       y = "Vive en casa de sus padres") +
  scale_color_manual(values = c("darkgreen", "black", "blue", "darkred")) +
  scale_fill_hue(l=10)

ggsave("kmeans.png", width = 15.5, height = 12, device = agg_png, dpi = 500)

  

# unir plot 1, 2 y 3 -----------
library(patchwork)

p123 <- (p1 | p2 / p3)

p123 <- p123 + plot_annotation(
  title = '',
  subtitle = '',
  caption = '@dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

p123 <- p123 + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'white', color = "white"),
                                       plot.title = element_text(color = "black", hjust = 0.5),
                                       plot.subtitle = element_text(color = "black", hjust = 0.5),
                                       #panel.background = element_rect(fill = "black", color = "black"),
                                       plot.caption = element_text(color = "black")))

p123 <- p123 + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

p123 <- p123 + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                       plot.caption = element_text(size = 9.5, hjust = 1)))

ggsave("p123.png", width = 17.5, height = 12, device = agg_png, dpi = 500)



# european social survey -----------------

df <- read_sav("ESS9e03_1.sav")

df$cntry


df <- df %>% 
  mutate(lvpntyr =
           case_when(lvpntyr  == 0 ~ 1,
                     lvpntyr  >= 1 ~ 0))

df$cntry

df <- df %>% 
  drop_na(happy, agea, lvpntyr, cntry) %>% 
  select(happy, agea, lvpntyr, cntry) %>% 
  filter(happy <11, agea %in% c(25:34))
df




# ess boxplot ---------------

library(car)
leveneTest(df$happy ~ as.factor(df$lvpntyr), data = df, center = "median") # varianza no igual pues <0.05

anova <- aov(df$happy ~ df$lvpntyr)
summary(anova)

res <- t.test(df$happy ~ df$lvpntyr, var.equal = F)
res  # add en el plot

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "none", 
                  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

p4 <- df %>% 
  ggplot(aes(x = as.factor(lvpntyr), y = happy, fill = as.factor(lvpntyr))) +
  geom_violin() +
  scale_fill_manual(values=c("blue", "red")) +
  stat_summary(fun.y=mean, geom="point", color="purple", shape = 17, size = 7) +
  scale_y_continuous(name="Sentimiento de felicidad", limits=c(0, 10), breaks = seq(0, 10, by = 1))+
  scale_x_discrete(name = "¿Vive con sus padres?",  labels=c("0" = "No", 
                                                                "1" = "Sí")) +
annotate(geom = "text",x = 1.5,y = 3.5,
             label = "Welch Two Sample t-test
t = 8.885, df = 1235.4, p-value < 2.2e-16
95 percent confidence interval:
 0.44 0.69
sample estimates:
mean in group 0 mean in group 1 
       7.824951        7.260085 ", fontface="bold", family="Bahnschrift", size = 3.4) +
  labs(title = "Diferencia global según situación de convivencia",
       subtitle = "",
       caption = "") +
  my_theme

ggsave("geomviolin.png", width = 15.5, height = 12, device = agg_png, dpi = 500)



#ggplot(df,aes(x = happy)) + 
  #geom_histogram(aes(y = ..density..), binwidth = 1) +
  #facet_grid(.~ lvpntyr) +
  #theme_bw() + 
  #theme(legend.position = "none") +
  #scale_x_continuous(breaks = seq(0,10, by = 1))




# ess reglog -----------------

model <- glm(lvpntyr ~ happy,
             data = df,
             family = "binomial"(link="logit"))
summary(model)



pred <- predict(model, type = "response")
pred_df <- data.frame(happy = df$happy, pred)

library(hexbin)
library(ggside)

df$happy <- as.numeric(df$happy) 

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "right", 
                  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

p5 <- ggplot(df, aes(x = happy, y = lvpntyr)) +
  #stat_sum() + 
  #geom_rug() +
  geom_bin2d(bins = 10) +
  scale_fill_gradient(low = "gray95", high = "red") +
  #geom_jitter(height = 0.02, alpha=0.5, color="blue") +
  #geom_histogram(aes(y = ..density.., colour = lvpntyr)) +
  #geom_hex(bins = 0.5) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = T, fullrange=TRUE) +
  geom_point(data = pred_df, aes(x = happy, y = pred), colour = "red") +
  geom_hline(data = data.frame(c(0.25, 0.50, 0.75)),
             aes(yintercept = c(0.25, 0.50, 0.75)),
             colour = "darkgrey", linetype = "dashed") +
  #geom_xsidehistogram(aes(y = after_stat(count)), bins = 50) +
  #geom_ysidehistogram(aes(x = after_stat(count)), bins = 50) +
  #geom_xsidedensity(aes(y = after_stat(count))) +
  #geom_ysidedensity(aes(x = after_stat(count))) +
  labs(y = "P(Vivir con sus padres)",
       x = "Sentimiento de felicidad",
       title = "Situación de convivencia ~ Sentimiento de felicidad") +
  scale_x_continuous(breaks = seq(0,10, by = 1), limits = c(-1,10)) +
  my_theme

ggsave("prob.png", width = 15.5, height = 12, device = agg_png, dpi = 500)



# DIFERENCIAS DENTRO DE CADA PAIS -----------

df %>% 
  group_by(cntry) %>% 
  count(lvpntyr)


df_group <- df %>% 
  group_by(lvpntyr, cntry) %>% 
  summarise(mean_ = mean(happy)) %>% 
  spread(key = lvpntyr, value = mean_) %>% 
  mutate(difference_ = `1` - `0`)
df_group

df_group %>% 
  arrange(difference_) %>% 
  print(n = 29)



# dumbbell -------

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 22, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "none", 
                  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 

library(ggalt)   

df_group$cntry <- countrycode(df_group$cntry,  "iso2c", "country.name",)


p6 <- df_group %>% 
ggplot() +
  geom_segment(aes(y=reorder(cntry, -difference_), yend=cntry, x=`1`, xend= `0`), color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=cntry, x=`1`, xend=`0`),
                size=1.5, color="#b2b2b2", size_x=4, size_xend = 4, colour_x = "red", colour_xend = "blue") +
  geom_text(data=filter(df_group, cntry=="Iceland"),
            aes(x=`1`, y=cntry, label="Sí", size = 2),
            color= "red", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_text(data=filter(df_group, cntry=="Iceland"),
            aes(x=`0`, y=cntry, label="No", size = 2),
            color= "blue", size=4, vjust=1.5, fontface="bold", family="Bahnschrift") +
  geom_rect(aes(xmin=9.5, xmax=10, ymin=-Inf, ymax=Inf), fill="gray95") +
  geom_text(aes(label=round(difference_, 1), y=cntry, x=9.75), fontface="bold", size=4.5, family="Bahnschrift",
             color = "purple") +
  geom_text(data=filter(df_group, cntry=="Iceland"), 
            aes(x=9.75, y=cntry, label=""),
            color="purple", size=3.1, vjust=-2, fontface="bold", family="Bahnschrift") +
  scale_x_continuous(breaks = seq(4.5,10, by = 0.5), limits=c(4.5, 10)) +
  #scale_y_discrete(expand=c(0.05,0)) +
  labs(title = "Diferencia dentro de cada país\n según situación de convivencia",
       subtitle = "",
       caption = "",
       x = "Sentimiento de felicidad",
       y = "") +
  my_theme   

ggsave("dumbell.png", width = 17.5, height = 12, device = agg_png, dpi = 500)



# unir plots 4, 5 y 6 ---------------


library(patchwork)

p456 <- (p6 | p4 / p5)

p456 <- p456 + plot_annotation(
  title = "Sentimiento de felicidad del grupo que vive\ncon sus padres vs no vive con sus padres",
  subtitle = 'Jóvenes entre 25 y 34 años',
  caption = 'Fuente: European Social Survey (ESS round 9) | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

p456 <- p456 + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'white', color = "white"),
                                             plot.title = element_text(color = "black", hjust = 0.5),
                                             plot.subtitle = element_text(color = "black", hjust = 0.5),
                                             #panel.background = element_rect(fill = "black", color = "black"),
                                             plot.caption = element_text(color = "black")))

p456 <- p456 + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

p456 <- p456 + plot_annotation(theme = theme(plot.title = element_text(size = 28, hjust = 0.5),
                                             plot.subtitle = element_text(size = 24, hjust = 0.5),
                                             plot.caption = element_text(size = 9.5, hjust = 1)))

ggsave("p456.png", width = 17.5, height = 12, device = agg_png, dpi = 500)
