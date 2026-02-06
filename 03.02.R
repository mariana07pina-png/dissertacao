# Mariana Pina

remove(list=ls())


library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(gridExtra)
library(cluster)
library(factoextra)
library(NbClust)
library(ggcorrplot)
library(patchwork)
library(readxl)


# Tabela Completa - Todas as Variáveis

tf <- read_excel("D:/Documents/UESB - Mestrado/3.Desenvolvimento do Projeto/Projeto Modelagem/Trabalho final/dados.finais.xlsx",
                 sheet = "Plan1")

view(tf)


Matriz.dat <- round(cor(tf [,3:21], method = "spearman"), 2)
View(Matriz.dat)

corrplot(Matriz.dat, type="lower",
         method="color", 
         order = "hclust",
         tl.cex=0.7,
         tl.col = "Black",
         tl.srt = 0.45,
         addCoef.col = "Black",
         number.cex = 0.5,
         diag = FALSE)


install.packages("Hmisc")   # se necessário
library(Hmisc)
rc <- rcorr(as.matrix(tf[, 2:20]), type = "spearman")
Matriz <- rc$r        # correlações
p.tf   <- rc$P        # p-values

corrplot(Matriz, type = "lower", method = "color", order = "hclust",
         tl.cex = 0.7, tl.col = "black", addCoef.col = "black",
         number.cex = 0.5, diag = FALSE,
         p.mat = p.tf, sig.level = 0.05, insig = "blank")

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function (tf, ...) {
  tf <- as.matrix(tf)
  n <- ncol(tf)
  p.tf<- matrix(NA, n, n)
  diag(p.tf) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(tf[, i], tf[, j], ...)
      p.tf[i, j] <- p.tf[j, i] <- tmp$p.value
    }
  }
  colnames(p.tf) <- rownames(p.tf) <- colnames(tf)
  p.tf
}
# matrix of the p-value of the correlation
p.tf <- cor.mtest(tf)
head(p.tf [,2:19])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Matriz, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

########################################################


# Gráfico de correlação completo
panel.cor <- function(x, y, digits=2, method = "spearman") {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  # Spearman
  s <- cor.test(x, y, method = "spearman")
  
  # Valor da correlação Spearman
  corr <- unname(s$estimate)
  # Valor do p-valor
  pval <- s$p.value
  
  # Ajuste de exibição do p-valor
  p_txt <- if (pval <= 0.009) {
    "p<0,01"
  } else {
    sprintf("p=%.2f", pval)
  }
  
  # Texto formatado
  txt <- sprintf("c=%.2f; %s", corr, p_txt)
  text(0.5, 0.5, txt, cex=0.85)
}
# Faça o pairs plot (base R)
pairs(tf[,2:19],
      lower.panel = panel.cor,             # Insere painel de correlação abaixo da diagonal
      upper.panel = function(x, y, ...) {
        points(x, y, pch=1)
        abline(lm(y ~ x), col="red", lty=2)
      },
      diag.panel = function(x) {
        par(new = TRUE)
        hist(x, col="orange", border="white", main="", axes=FALSE)
      }
)



# Variáveis selecionadas para análise


options(scipen = 999)
set.seed(123)

library("ggpubr")
library("ggpmisc")


g1 <- tf %>%
  ggplot(aes(x = Production, y = TE)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8, fill = "lightblue") +        # linha de regressão + banda de erro
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x,                 # equação da reta
                        label.x.npc = 0.7, label.y.npc = 0.25, size = 3) +
  stat_cor(aes(label = ..rr.label..), method = "pearson",                           # R² (ou r)
           label.x.npc = 0.7, label.y.npc = 0.15, size = 3) +
  theme_minimal() +                                                   # plano de fundo limpo
  theme(panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"))                            # personaliza o plano de fundo

print(g1)

g2 <- tf %>%
  ggplot(aes(x = Production, y = GL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8, fill = "lightblue") +        # linha de regressão + banda de erro
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x,                 # equação da reta
                        label.x.npc = 0.7, label.y.npc = 0.25, size = 3) +
  stat_cor(aes(label = ..rr.label..), method = "pearson",                           # R² (ou r)
           label.x.npc = 0.7, label.y.npc = 0.15, size = 3) +
  theme_minimal() +                                                   # plano de fundo limpo
  theme(panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"))

g3 <- tf %>%
  ggplot(aes(x = Production, y = Herd)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8, fill = "lightblue") +        # linha de regressão + banda de erro
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x,                 # equação da reta
                        label.x.npc = 0.7, label.y.npc = 0.25, size = 3) +
  stat_cor(aes(label = ..rr.label..), method = "pearson",                           # R² (ou r)
           label.x.npc = 0.7, label.y.npc = 0.15, size = 3) +
  theme_minimal() +                                                   # plano de fundo limpo
  theme(panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"))

g4 <- tf %>%
  ggplot(aes(x = TE, y = GL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8, fill = "lightblue") +        # linha de regressão + banda de erro
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x,                 # equação da reta
                        label.x.npc = 0.8, label.y.npc = 0.25, size = 3) +
  stat_cor(aes(label = ..rr.label..), method = "pearson",                           # R² (ou r)
           label.x.npc = 0.8, label.y.npc = 0.15, size = 3) +
  theme_minimal() +                                                   # plano de fundo limpo
  theme(panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"))

g5 <- tf %>%
  ggplot(aes(x = TE, y = Herd)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 0.8, fill = "lightblue") +        # linha de regressão + banda de erro
  stat_regline_equation(aes(label = ..eq.label..), formula = y ~ x,                 # equação da reta
                        label.x.npc = 0.8, label.y.npc = 0.25, size = 3) +
  stat_cor(aes(label = ..rr.label..), method = "pearson",                           # R² (ou r)
           label.x.npc = 0.8, label.y.npc = 0.15, size = 3) +
  theme_minimal() +                                                   # plano de fundo limpo
  theme(panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"))



# Junta tudo em um único painel
(g1 | g2 | g3) /
  (g4 | g5)


g7 <- dc.norm %>%
  ggplot(aes(x = TE, y = PropMix_TE)) +
  geom_point()

g8 <- dc.norm %>%
  ggplot(aes(x = TE, y = FR)) +
  geom_point()

g9 <- dc.norm %>%
  ggplot(aes(x = AFkg, y = GL)) +
  geom_point()


(g7 | g8 | g9)


pairs(dat[,3:15], upper.panel = NULL )




#####################################################
# Iniciando Clusterização com método k-means


dat9 <- read_excel("D:/Documents/UESB - Mestrado/3.Desenvolvimento do Projeto/Projeto Modelagem/Trabalho final/dados.finais.xlsx",
                   sheet = "corr.09")

set.seed(123)


dat.num9 <- dat9[,2:15]
row.names(dat.num9) <- tf$Countries

dc.norm9 <- as.data.frame(scale(dat.num9))



k_otimo.dat <- 15
kmeans_Final.dat9 <- kmeans(dc.norm9,
                           centers = k_otimo.dat,
                           nstart = 50)

grafico_cluster_dat <- fviz_cluster(kmeans_Final.dat,
                                    data = dc.norm9,
                                    geom = "point",
                                    ellipse.type = "convex",
                                    palette = "jco",
                                    ggtheme = theme_minimal(),
                                    main = paste("Gráfico de Clusterização (k=", k_otimo.dat,")"))

kmeans_Final.dat9$withinss

dat9$cluster <- kmeans_Final.dat9$cluster  

print(grafico_cluster_dat)

sil_kmeans <- silhouette(kmeans_Final.dat$cluster, DC)
mean(sil_kmeans[, 3])  # Média do silhouette para esse k


perfis_cluster_dat9 <- dat9 %>%
  mutate(cluster.dat9 = as.factor(kmeans_Final.dat$cluster)) # Adiciona a coluna com o nº do cluster

View(perfis_cluster_dat)

tabela_perfis.dat9 <- perfis_cluster_dat9 %>%
  group_by(cluster.dat9) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) # Calcula a média de todas as colunas para cada grupo

View(tabela_perfis.dat)

cat("\n--- Tabela de Perfis dos Clusters (Médias) ---\n")

writexl::write_xlsx(tabela_perfis.dat9, "dat9tab.xlsx")

writexl::write_xlsx(perfis_cluster_dat9, "dat9.2.xlsx")



library(dplyr) 

set.seed(123)

n_runs <- 50
cluster_matrix <- matrix(NA, nrow = nrow(dc.norm9), ncol = n_runs)

for (i in 1:n_runs) {
  km_tmp <- kmeans(dc.norm9, centers = 15, nstart = 1)
  cluster_matrix[, i] <- km_tmp$cluster
}
stability_df <- as.data.frame(cluster_matrix) %>%
  mutate(country = rownames(dc.norm9)) %>%
  rowwise() %>%
  mutate(
    n_clusters = n_distinct(c_across(V1:V50))
  ) %>%
  ungroup()

table(stability_df$n_clusters)


library(cluster)
library(fpc)

cb_km <- clusterboot(
  dc.norm9,
  B = 200,
  bootmethod = "boot",
  clustermethod = kmeansCBI,
  k = k_otimo.dat
)

jaccard_km <- cb_km$bootmean


jaccards <- cb_km(
  Metodo = "K-means",
  Jaccard_Medio = mean(jaccard_km)
)

jaccards




####################



set.seed(123) 


dc.9 <- dc.norm9 %>% mutate(cluster = kmeans_Final.dat9$cluster)

# 2. Tabelas descritivas automatizadas por cluster
# Função personalizada para resumo descritivo
resumo_cluster <- function(dc.k15, cluster_col = "cluster") {
  df %>%
    group_by(!!sym(cluster_col)) %>%
    summarise(across(where(is.numeric), list(
      média   = ~mean(.x, na.rm = TRUE),
      mediana = ~median(.x, na.rm = TRUE),
      dp      = ~sd(.x, na.rm = TRUE),
      min     = ~min(.x, na.rm = TRUE),
      max     = ~max(.x, na.rm = TRUE),
      Q1      = ~quantile(.x, 0.25, na.rm = TRUE),
      Q3      = ~quantile(.x, 0.75, na.rm = TRUE)
    ))) %>%
    ungroup()
}

# Tabelas para os dois agrupamentos

tabela_k15 <- resumo_cluster(dc.k15, "cluster")


# Transforma os dados para formato longo

dc.k15_long9 <- dc.9 %>% pivot_longer(cols = -cluster, names_to = "variavel", values_to = "valor")

view(dc.k15)
# Boxplot dos clusters para todas as variáveis (painel facetado)



dc.9 %>%
  ggplot(aes(x = as.factor(cluster), y = Production)) +
  geom_boxplot(fill = '#E1E1E1',
               outlier.shape = 1) +
  geom_jitter(size = 1.0, width = 0.2) +
  labs(x = "Cluster (k=15)",
       y = "Production / normalized") +
  theme_minimal()


dc.9 %>%
  ggplot(aes(x = as.factor(cluster), y = EI)) +
  geom_boxplot(fill = '#E1E1E1',
               outlier.shape = 1) +
  labs(x = "Cluster (k=15)",
       y = "Total Emission of CH4 enteric / normalized") +
  theme_minimal()


dc.9 %>%
  ggplot(aes(x = as.factor(cluster), y = GL)) +
  geom_boxplot(fill = '#E1E1E1',
               outlier.shape = 1) +
  labs(x = "Cluster (k=15)",
       y = "Emission Intensity / normalized") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18, 
               size = 2.8, 
               color = "red") +
  theme_minimal()




library(ggpubr)
library(RColorBrewer)

p <- ggboxplot(dc.k15_long9, x = "cluster", y = "valor",
               color = "cluster", palette =c("set1"),
               shape = "cluster")
p

p + stat_compare_means(label.y = 50) # Add pairwise comparisons p-value
stat_compare_means(label.y = 50) 


# 5. Exporta as tabelas resumo (se quiser salvar para Excel ou CSV)

writexl::write_xlsx(tabela_k15, "resumo.k15.xlsx")




library(pheatmap)
library(dplyr)
library(fmsb)
library(scales)


# == 1. HEATMAP ==

# Média das variáveis por cluster

mat_medias_9 <- dc.9 %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  as.data.frame()

rownames(mat_medias_9) <- mat_medias_9$cluster
mat_medias_9$cluster <- NULL


# Plot heatmap
pheatmap(mat_medias_9,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         scale = "column",
         color = colorRampPalette(c("blue", "white", "red"))(100)
         )




# == 2. RADAR CHART ==

# Função utilitária: prepara os dados para radar chart de um cluster específico
prep_radar <- function(medias_por_cluster, clus_num) {
  # médias do cluster selecionado
  vec <- as.numeric(medias_por_cluster[clus_num, ])
  # define máximo e mínimo para cada variável no radar chart (todas médias dos clusters)
  maximos <- apply(medias_por_cluster, 2, max)
  minimos <- apply(medias_por_cluster, 2, min)
  rchart <- rbind(maximos, minimos, vec)
  return(rchart)
}

# Radar chart para todos os clusters

for(i in 1:nrow(mat_medias_k10)) {
  radarchart(as.data.frame(prep_radar(mat_medias_k10, i)),
             axistype = 1,
             pcol = "red",
             pfcol = scales::alpha("red", 0.25),
             plwd = 2,
             cglcol = "grey",
             title = paste("Radar Chart - Cluster", rownames(mat_medias_k10)[i], "(k=10)"))
}


for(i in 1:nrow(mat_medias_k13)) {
  radarchart(as.data.frame(prep_radar(mat_medias_k13, i)),
             axistype = 1,
             pcol = "red",
             pfcol = scales::alpha("red", 0.25),
             plwd = 2,
             cglcol = "grey",
             title = paste("Radar Chart - Cluster", rownames(mat_medias_k13)[i], "(k=13)"))
}

for(i in 1:nrow(mat_medias_k15)) {
  radarchart(as.data.frame(prep_radar(mat_medias_k15, i)),
             axistype = 1,
             pcol = "red",
             pfcol = scales::alpha("red", 0.25),
             plwd = 2,
             cglcol = "grey",
             title = paste("Radar Chart - Cluster", rownames(mat_medias_k15)[i], "(k=15)"))
}


for(i in 1:nrow(mat_medias_k18)) {
  radarchart(as.data.frame(prep_radar(mat_medias_k18, i)),
             axistype = 1,
             pcol = "blue",
             pfcol = scales::alpha("blue", 0.25),
             plwd = 2,
             cglcol = "grey",
             title = paste("Radar Chart - Cluster", rownames(mat_medias_k18)[i], "(k=18)"))
}


# Supondo que mat_medias_k15 é seu data frame de médias por cluster (linhas=clusters, colunas=variáveis)
# Adiciona linha de máximo e mínimo (necessário para radarchart)
maximos <- apply(mat_medias_k15, 2, max)
minimos <- apply(mat_medias_k15, 2, min)
radar_data_k15 <- rbind(maximos, minimos, mat_medias_k15)  # ordem exigida pelo pacote

# Prepara paleta de cores (exemplo com até 15 clusters)
cores <- scales::hue_pal()(nrow(mat_medias_k15)) 

# Gráfico radar único para todos os clusters de k=15
radarchart(
  as.data.frame(radar_data_k15),
  axistype = 1,
  pcol = cores,
  pfcol = scales::alpha(cores, 0.3),   # Transparência nas áreas
  plwd = 2,
  cglcol = "grey",
  title = "Radar Chart - Todos os Clusters (k=15)",
  legend = rownames(mat_medias_k15)    # Adiciona legenda com nomes dos clusters
)

# k = 13
maximos <- apply(mat_medias_k13, 2, max)
minimos <- apply(mat_medias_k13, 2, min)
radar_data_k13 <- rbind(maximos, minimos, mat_medias_k13)  # ordem exigida pelo pacote

# Prepara paleta de cores (exemplo com até 15 clusters)
cores <- scales::hue_pal()(nrow(mat_medias_k13)) 

# Gráfico radar único para todos os clusters de k=15
radarchart(
  as.data.frame(radar_data_k13),
  axistype = 1,
  pcol = cores,
  pfcol = scales::alpha(cores, 0.3),   # Transparência nas áreas
  plwd = 2,
  cglcol = "grey",
  title = "Radar Chart - Todos os Clusters (k=13)",
  legend = rownames(mat_medias_k13)    # Adiciona legenda com nomes dos clusters
)

# k = 15
maximos <- apply(mat_medias_k15, 2, max)
minimos <- apply(mat_medias_k15, 2, min)
radar_data_k15 <- rbind(maximos, minimos, mat_medias_k15)  # ordem exigida pelo pacote


cores <- scales::hue_pal()(nrow(mat_medias_k15)) 

# Gráfico radar único para todos os clusters de k=15
radarchart(
  as.data.frame(radar_data_k15),
  axistype = 1,
  pcol = cores,
  pfcol = scales::alpha(cores, 0.3),   # Transparência nas áreas
  plwd = 2,
  cglcol = "grey",
  title = "Radar Chart - Todos os Clusters (k=15)",
  legend = rownames(mat_medias_k15)    # Adiciona legenda com nomes dos clusters
)

# k = 18
maximos <- apply(mat_medias_k18, 2, max)
minimos <- apply(mat_medias_k18, 2, min)
radar_data_k18 <- rbind(maximos, minimos, mat_medias_k18)  # ordem exigida pelo pacote


cores <- scales::hue_pal()(nrow(mat_medias_k18)) 

# Gráfico radar único para todos os clusters de k=15
radarchart(
  as.data.frame(radar_data_k18),
  axistype = 1,
  pcol = cores,
  pfcol = scales::alpha(cores, 0.3),   # Transparência nas áreas
  plwd = 2,
  cglcol = "grey",
  title = "Radar Chart - Todos os Clusters (k=18)",
  legend = rownames(mat_medias_k18)    # Adiciona legenda com nomes dos clusters
)



library(fmsb)
library(dplyr)

# Dados necessários: cada linha um cluster, cada coluna uma variável
radar_data <- dat %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  as.data.frame()

# Para fmsb, precisa inserir linha para valores máximo e mínimo de cada variável (ajuda no formato)
maximos <- apply(radar_data[-1], 2, max)
minimos <- apply(radar_data[-1], 2, min)
radar_plot_data <- rbind(maximos, minimos, radar_data[-1]) # Remove coluna cluster

prep_radar <- function(medias_por_cluster, clus_num) {
  # médias do cluster selecionado
  vec <- as.numeric(medias_por_cluster[clus_num, ])
  # define máximo e mínimo para cada variável no radar chart (todas médias dos clusters)
  maximos <- apply(medias_por_cluster, 2, max)
  minimos <- apply(medias_por_cluster, 2, min)
  rchart <- rbind(maximos, minimos, vec)
  return(rchart)
}

radarchart(as.data.frame(prep_radar(mat_medias_k15, i)), 
           axistype = 1,
           pcol = "red",
           pfcol = scales::alpha("red", 0.4),
           plwd = 2,
           cglcol = "grey",
           title = paste("Radar Chart - Cluster", radar_data$cluster[1]))

for(i in 1:nrow(mat_medias_k15)) {
  radarchart(
    as.data.frame(prep_radar(mat_medias_k15, i)),
    axistype = 1,
    pcol = "red",
    pfcol = scales::alpha("red", 0.25),
    plwd = 2,
    cglcol = "grey",
    title = paste("Radar Chart - Cluster", rownames(mat_medias_k15)[i], "(k=15)")
  )
}

