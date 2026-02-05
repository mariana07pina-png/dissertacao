#####################################################
# Iniciando Clusterização com método k-means


dat8 <- read_excel("D:/Documents/UESB - Mestrado/3.Desenvolvimento do Projeto/Projeto Modelagem/Trabalho final/dados.finais.xlsx",
                   sheet = "corr.08")

set.seed(123)


dat.num8 <- dat8[,2:12]
row.names(dat.num8) <- tf$Countries

dc.norm8 <- as.data.frame(scale(dat.num8))



k_otimo.dat <- 15
kmeans_Final.dat8 <- kmeans(dc.norm8,
                            centers = k_otimo.dat,
                            nstart = 50)

grafico_cluster_dat <- fviz_cluster(kmeans_Final.dat,
                                    data = dc.norm8,
                                    geom = "point",
                                    ellipse.type = "convex",
                                    palette = "jco",
                                    ggtheme = theme_minimal(),
                                    main = paste("Gráfico de Clusterização (k=", k_otimo.dat,")"))

kmeans_Final.dat8$withinss

dat8$cluster <- kmeans_Final.dat8$cluster  

print(grafico_cluster_dat)

sil_kmeans <- silhouette(kmeans_Final.dat$cluster, DC)
mean(sil_kmeans[, 3])  # Média do silhouette para esse k


perfis_cluster_dat8 <- dat8 %>%
  mutate(cluster.dat8 = as.factor(kmeans_Final.dat8$cluster)) # Adiciona a coluna com o nº do cluster

View(perfis_cluster_dat)

tabela_perfis.dat8 <- perfis_cluster_dat8 %>%
  group_by(cluster.dat8) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) # Calcula a média de todas as colunas para cada grupo

View(tabela_perfis.dat)

cat("\n--- Tabela de Perfis dos Clusters (Médias) ---\n")

writexl::write_xlsx(tabela_perfis.dat8, "dattab8.1.xlsx")

writexl::write_xlsx(perfis_cluster_dat8, "dat8.1.xlsx")


library(dplyr) 

set.seed(123)

n_runs <- 50
cluster_matrix8 <- matrix(NA, nrow = nrow(dc.norm8), ncol = n_runs)

for (i in 1:n_runs) {
  km_tmp <- kmeans(dc.norm8, centers = 15, nstart = 1)
  cluster_matrix8[, i] <- km_tmp$cluster
}
stability_df8 <- as.data.frame(cluster_matrix8) %>%
  mutate(country = rownames(dc.norm8)) %>%
  rowwise() %>%
  mutate(
    n_clusters8 = n_distinct(c_across(V1:V50))
  ) %>%
  ungroup()

table(stability_df8$n_clusters8)
