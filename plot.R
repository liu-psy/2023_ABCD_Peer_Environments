# Theme: visualization results
library(brainconn)
library(circlize)
library(ComplexHeatmap)
library(ggseg)
library(ggradar)
library(ggwordcloud)
library(patchwork)
library(reshape2)
library(RColorBrewer)
library(tidyverse)

# load results from analysis.R
setwd("H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments")
load("20230209_results.RData")

# peer environments
types <- c("PFI", "DFI", "PII")

# new behavior names
behavior_names <- c(
  "Neurocognition - Picture Vocabulary",
  "Neurocognition - Picture Sequence Memory",
  "Neurocognition - Oral reading recognition",
  "CBCL - Anxious/Depressed problems",
  "CBCL - Withdrawn/Depressed problems",
  "CBCL - Somatic complaint problems",
  "CBCL - Social problems",
  "CBCL - Thought problems",
  "CBCL - Attention problems",
  "CBCL - Rule-Breaking problems",
  "CBCL - Aggressive problems",
  "CBCL - Internalizing problems",
  "CBCL - Externalizing problems",
  "CBCL - Total problems",
  "CBCL - Depression",
  "CBCL - Anxiety",
  "CBCL - Somatic",
  "CBCL - ADHD",
  "CBCL - Oppositional defiant problems",
  "CBCL - Conduct problems",
  "CBCL - Sluggish cognitive tempo",
  "CBCL - OCD",
  "CBCL - Stress problems",
  "Subsyndromal Mania (Parent) - Sum score",
  "Trauma Events (Parent) - Total events",
  "Trauma Events (Parent) - Good events",
  "Trauma Events (Parent) - Bad events",
  "Trauma Events (Parent) - Good affections",
  "Trauma Events (Parent) - Bad affections",
  "Trauma Events (Parent) - Mean affections",
  "Trauma Events (Parent) - Total affections",
  "Trauma Events - Total events",
  "Trauma Events - Good events",
  "Trauma Events - Bad events",
  "Trauma Events - Good affections",
  "Trauma Events - Bad affections",
  "Trauma Events - Mean affections",
  "Prodromal Psychosis - Total score",
  "Prodromal Psychosis - Distress score",
  "Impulsivity - Negative urgency",
  "Impulsivity - Lack of planning",
  "Impulsivity - Sensation seeking",
  "Impulsivity - Positive urgency",
  "Impulsivity - Lack of perseverance",
  "Inhibition and Reward-seeking - BIS sum score",
  "Inhibition and Reward-seeking - BAS reward responsiveness",
  "Inhibition and Reward-seeking - BAS drive",
  "Inhibition and Reward-seeking - BAS fun seeking",
  "Inhibition and Reward-seeking - BIS sum score (modified)",
  "Inhibition and Reward-seeking - BAS reward responsiveness (modified)",
  "Inhibition and Reward-seeking - BAS drive (modified)",
  "Adverse Peer Experiences - Relational Victimization",
  "Adverse Peer Experiences - Reputational Aggression",
  "Adverse Peer Experiences - Reputational Victimization",
  "Adverse Peer Experiences - Overt Aggression",
  "Adverse Peer Experiences - Overt Victimization",
  "Adverse Peer Experiences - Relational Aggression"
)

# Figure 2A --------------------------------------------------------------------
mat_aossciations <- function(x) {
  behavior_mat <- data.frame(
  "t" = unlist(x[["behavior"]][1, ]),
  "p" = unlist(x[["behavior"]][2, ])
  )
  behavior_mat$group <- c(
    rep("Neurocognition", 3),
    rep("CBCL", 20),
    "Subsyndromal Mania (Parent)",
    rep("Trauma Events (Parent)", 7),
    rep("Trauma Events", 7),
    rep("Prodromal Psychosis", 2),
    rep("Impulsivity", 5),
    rep("Inhibition and Reward-seeking", 6),
    rep("Adverse Peer Experiences", 6)
  )
  behavior_mat$group <- factor(
    behavior_mat$group,
    levels = c("CBCL",  "Trauma Events", "Trauma Events (Parent)",
      "Adverse Peer Experiences", "Prodromal Psychosis",
      "Subsyndromal Mania (Parent)", "Inhibition and Reward-seeking",
      "Impulsivity", "Neurocognition")
  )

  behavior_mat$pfdr <- p.adjust(behavior_mat$p, method = "fdr")
  behavior_mat$sig <- ifelse(behavior_mat$pfdr < 0.001, "***", ifelse(
    behavior_mat$pfdr < 0.01, "**", ifelse(
      behavior_mat$pfdr < 0.05, "*", ""
      )
    )
  )

  behavior_mat <- arrange(behavior_mat, group)
  return(behavior_mat)
}

behavior_PFI <- mat_aossciations(PFI)
behavior_DFI <- mat_aossciations(DFI)
behavior_PII <- mat_aossciations(PII)
behavior_friends <- rbind(behavior_PFI, behavior_DFI, behavior_PII)

behavior_friends$seq <- rep(1:57, time = 3)
behavior_friends$type <- rep(types, each = nrow(behavior_PFI))
behavior_friends$type <- factor(behavior_friends$type, levels = types)

plot_function <- function(data, friends) {
  df <- filter(data, type == friends)
  ggplot(df, aes(group, t, color = group)) +
  geom_point(size = 8, position = position_jitterdodge(),
    shape = 17, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(-25, 20, 5)) +
  scale_colour_viridis_d(option = "viridis", begin = 0.2, end = 1) +
  labs(x = NULL, y = "t value") +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_hline(yintercept = 2.20, linewidth = 1, linetype = 2) +
  geom_hline(yintercept = -2.20, linewidth = 1, linetype = 2) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 35, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 40, face = "bold"),
    axis.title.x = element_text(size = 40, face = "bold"),
    plot.tag = element_text(size = 35, face = "bold"),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.position = "bottom",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    strip.text =  element_blank(),
    panel.spacing = unit(1.5, "lines")
  )  +
  guides(color = guide_legend(nrow = 3)) +
  coord_flip()
}

# PFI
plot_function(behavior_friends, "PFI")
ggsave("plot/Figure2A_PFI.svg", width = 16, height = 6.5, bg = "transparent")

# DFI
plot_function(behavior_friends, "DFI")
ggsave("plot/Figure2A_DFI.svg", width = 16, height = 6.5, bg = "transparent")

# PII
plot_function(behavior_friends, "PII")
ggsave("plot/Figure2A_PII.svg", width = 16, height = 6.5, bg = "transparent")

# Fiugre 2a
fdr_text <- data.frame(group = "Neurocognition", t = 12, type = "PFI")
fdr_text$type <- factor(fdr_text$type, levels = types)

ggplot(behavior_friends, aes(group, t, color = group)) +
  geom_point(alpha = 0.8, size = 7, position = position_jitterdodge(),
    shape = 17, show.legend = FALSE) +
  geom_text(data = fdr_text, label = paste0("- - - ", expression(italic(p)[fdr]), "< 0.05"),
    size = 11, color = "black", show.legend = FALSE, position = "identity",
    fontface = "bold", parse = TRUE) +
  scale_y_continuous(breaks = seq(-25, 20, 5)) +
  scale_colour_viridis_d(option = "viridis", begin = 0.2, end = 1) +
  labs(x = NULL, y = "t value") +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_hline(yintercept = 2.20, linewidth = 1, linetype = 2) +
  geom_hline(yintercept = -2.20, linewidth = 1, linetype = 2) +
  facet_wrap(~type, nrow = 3) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 30, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 35, face = "bold"),
    axis.title.x = element_text(size = 30, face = "bold"),
    plot.tag = element_text(size = 35, face = "bold"),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.position = "bottom",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_blank(),
    legend.box.background = element_blank(),
    strip.background = element_blank(),
    strip.text =  element_blank(),
    panel.spacing = unit(1.5, "lines")
  )  +
  guides(color = guide_legend(nrow = 3)) +
  coord_flip()
ggsave("plot/Figiure2a.svg", width = 15, height = 15)

# Figure 2B --------------------------------------------------------------------
# atlas: Desikan-Killia"y atlas (aparc)
data(dk)
dk$data$region

extract_region <- function(data, modality) {
  ROI <- str_split(colnames(data[[modality]]), pattern = "_cdk_", simplify = TRUE)[, 2]
  hemi <- str_extract(ROI, "..$")
  regions <- str_split(ROI, hemi, simplify = TRUE)[, 1]

  brain_region <- tibble(
    "hemi" = ifelse(hemi == "lh", hemi <- "left", hemi <- "right"),
    "region" = regions,
    "t" = unlist(data[[modality]][1, ], use.names = FALSE)
  )
  return(brain_region)
}

# PFI
PFI_vol <- extract_region(PFI1, "vol_cortical")
PFI_vol$region <- c("inferior parietal", "paracentral", "precentral",
  "rostral anterior cingulate", "superior frontal", "postcentral", "insula")
p2_1 <- PFI_vol %>%
  ggplot() +
  geom_brain(atlas = dk, aes(fill = t), size = 1, position = position_brain(side ~ hemi), show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-4.5, 4.5), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  labs(title = "PFI", tag = "b") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold")
  )

# DFI
DFI_vol <- extract_region(DFI1, "vol_cortical")
DFI_vol$region <- c("fusiform", "lateral occipital",
  "lateral orbitofrontal", "superior parietal", "inferior temporal",
  "lateral occipital", "middle temporal", "posterior cingulate","precentral",
  "precuneus", "superior parietal")

p2_2  <- DFI_vol %>%
  ggplot() +
  geom_brain(atlas = dk, aes(fill = t), position = position_brain(side ~ hemi), size = 1) +
  scale_fill_gradient2(limits = c(-4.5, 4.5), breaks = c(-4.5, -2.3, 2.3, 4.5), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  labs(title = "DFI") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 24, face = "bold"),
    legend.key.height = unit(1.2, 'cm'),
    legend.key.width = unit(2.5, 'cm'),
  ) +
  guides(
    fill = guide_colorbar(
      title = "t value",
      title.position = "top",
      label.position = "bottom",
      ticks = FALSE
    )
  )
p2_2

# PII
PII_vol <- extract_region(PII1, "vol_cortical")
PII_vol$region <- c("fusiform", "inferior parietal", "lateral occipital",
  "lateral orbitofrontal", "middle temporal",  "paracentral", "postcentral",
  "precentral", "precuneus", "rostral anterior cingulate", "superior frontal",
  "insula", "bankssts", "cuneus", "fusiform", "inferior parietal",
  "middle temporal", "paracentral", "postcentral", "posterior cingulate",
  "precentral", "superior frontal", "superior parietal","supramarginal", "insula")
p2_3 <- PII_vol %>%
  ggplot() +
  geom_brain(atlas = dk, aes(fill = t), size = 1, position = position_brain(side ~ hemi), show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-4.5, 4.5), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  labs(title = "PII") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold")
  )
p2_3

p2 <- p2_1 + p2_2 + p2_3 +
  plot_layout(nrow = 1)
p2

# Figure 3 RSFCs ---------------------------------------------------------------
# cortical RSFCs
network_edge <- function(data) {
  network <- c("ad", "cgc", "ca", "dt", "dla", "fo", "rspltp", "smh", "smm", "sa",
    "vta", "vs")
  network_label <- c("AN", "CON", "CPN", "DMN", "DAN", "FPN", "RTN", "SHN",
    "SMN", "SN", "VAN", "VN")

  network_df <- data.frame(
    "from" = network,
    "to" = network
  )

  network_from <- str_split(colnames(data), "_ngd_", simplify = TRUE)[, 2]
  network_to <- str_split(colnames(data), "_ngd_", simplify = TRUE)[, 3]
  edges <- data.frame("from" = network_from, "to" = network_to)
  network_df <- rbind(edges, network_df)
  network_df$from <- factor(network_df$from, levels = network, labels = network_label)
  network_df$to <- factor(network_df$to, levels = network, labels = network_label)
  network_df$t_value <- c(unlist(data[1, ]), rep(NA, length(network_label)))

  return(network_df)
}
PFI_edges <- network_edge(PFI1$networks)
DFI_edges <- network_edge(DFI1$networks)
PII_edges <- network_edge(PII1$networks)

# sub-cortical ROIs
# 1. cerebellum_cortex(crcx)                 2. thalamus_proper(thp)
# 3. caudate(cde)                            4. putamen(pt)
# 5. pallidum(pl)                            6. brain-stem(bs)
# 7. hippocampus(hp)                         8. amygdala(ag)
# 9. accumbens-area(aa)                      10.ventraldc(vtdc)
# networks
# 1. auditory network(au)                    2. cingulo-opercular network(cerc)
# 3. cingulo-parietal network(copa)          4. default network(df)
# 5. dorsal attention network(dsa)           6. fronto-parietal network(fopa)
# 7. retrosplenial temporal network(rst)     8. sensorimotor hand network(smh)
# 9. sensorimotor mouth network(smm)         10.salience network(sa)
# 11.ventral attention network(vta)          12.visual network(vs)
cortico_subcortical <- function(data) {
  network_subcortical <- c("au", "cerc", "copa", "df", "dsa", "fopa", "rst",
    "smh", "smm", "sa", "vta", "vs", "crcx", "thp", "cde", "pt", "pl", "hp",
    "ag", "aa", "vtdc", "bs")

  network_subcortical_label <- c("AN", "CON", "CPN", "DMN", "DAN", "FPN", "RTN",
    "SHN", "SMN", "SN", "VAN", "VN", "Crcx",  "Tha", "Cde",
    "Pt", "Pl", "Hip", "Amg", "NAc", "Vtdc", "BS")

   template <- c("au", "cerc", "copa", "df", "dsa", "fopa", "rst",
    "smh", "smm", "sa", "vta", "vs", "crcx", "thp", "cde", "pt", "pl", "hp",
    "ag", "aa", "vtdc", "bs")

  network_df <- data.frame(
    "from" = template,
    "to" = template
  )

  network_from <- str_split(colnames(data), "_scs_", simplify = TRUE)
  network_from <- str_split(network_from[, 1], "_ngd_", simplify = TRUE)[, 2]
  network_to <- str_split(colnames(data), "_scs_", simplify = TRUE)[, 2]
  edges <- data.frame("from" = network_from, "to" = network_to)
  network_df <- rbind(edges, network_df)
  network_df$from <- factor(network_df$from, levels = network_subcortical, labels = network_subcortical_label)
  network_df$to <- factor(network_df$to, levels = network_subcortical, labels = network_subcortical_label)
  network_df$t_value <- c(unlist(data[1, ]), rep(NA, length(template)))

  return(network_df)
}

# FDR
cs_DFI <- cortico_subcortical(DFI1$network_sub)
cs_DFI$positive <- ifelse(cs_DFI$t_value > 0, cs_DFI$t_value, NA)
cs_DFI$negative <- ifelse(cs_DFI$t_value < 0, cs_DFI$t_value, NA)

# Bonferroni
DFI2 <- lapply(DFI, p_adjust, "bonferroni")
cs_DFI <- cortico_subcortical(DFI2$network_sub)
cs_DFI$positive <- ifelse(cs_DFI$t_value > 0, cs_DFI$t_value, NA)
cs_DFI$negative <- ifelse(cs_DFI$t_value < 0, cs_DFI$t_value, NA)

col_fun <- colorRamp2(
  c(min(cs_DFI$t_value, na.rm = TRUE), 0, max(DFI_edges$t_value, na.rm = TRUE)),
  c("#377EB8", "white", "#E41A1C")
)

lays <- layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
layout.show(lays)

# PFI
par(cex = 4)
circos.par(gap.after = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), start.degree = 44)
chordDiagram(
  PFI_edges[, c(1,2)],
  grid.col = "grey",
  annotationTrack = c("name", "grid"),
  annotationTrackHeight = c(0.01, 0.02),
  transparency = 0.7,
  col = col_fun(PFI_edges$t_value),
  order = c("AN", "SMN", "SHN", "VN", "DMN", "CON", "DAN", "CPN", "VAN", "FPN",
            "RTN", "SN"),
  scale = TRUE
)
title(main = "Prosocial friend index (PFI)", cex.main = 1, line = -0.5)
circos.clear()

# DFI
circos.par(gap.after = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), start.degree = 44)
chordDiagram(
  DFI_edges[, c(1,2)],
  grid.col = "grey",
  annotationTrack = c("name", "grid"),
  annotationTrackHeight = c(0.01, 0.02),
  transparency = 0.7,
  col = col_fun(DFI_edges$t_value),
  order = c("AN", "SMN", "SHN", "VN", "DMN", "CON", "DAN", "CPN", "VAN", "FPN",
            "RTN", "SN"),
  scale = TRUE
)
title(main = "Delinquent friend index (DFI)", cex.main = 1, line = -0.5)
circos.clear()

# PII
circos.par(gap.after = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), start.degree = 44)
chordDiagram(
  PII_edges[, c(1,2)],
  grid.col = "grey",
  annotationTrack = c("name", "grid"),
  annotationTrackHeight = c(0.01, 0.02),
  transparency = 0.7,
  col = col_fun(PII_edges$t_value),
  order = c("AN", "SMN", "SHN", "VN", "DMN", "CON", "DAN", "CPN", "VAN", "FPN",
            "RTN", "SN"),
  scale = TRUE
)
title(main = "Positive influence index (PII)", cex.main = 1, line = -0.5)
circos.clear()

# DFI - cortico-subcortical RSFC
circos.par(gap.after = c(rep(2, 11), 30, rep(2, 9), 30), start.degree = 263)
chordDiagram(
  cs_DFI[, c(1, 2)],
  grid.col = "grey",
  annotationTrack = c("name", "grid"),
  annotationTrackHeight = c(0.01, 0.02),
  transparency = 0.6,
  col = col_fun(cs_DFI$positive),
  order = c("CON", "CPN", "DMN",  "RTN", "VAN", "SN", "DAN", "FPN", "SHN",
    "SMN", "AN", "VN", "Crcx",  "Tha", "Hip", "Amg", "Pl", "Pt", "Cde", "NAc",
    "Vtdc", "BS"),
  scale = TRUE
)
title("\n DFI \n (positive associations)", cex.main = 1)
circos.clear()

circos.par(gap.after = c(rep(2, 11), 30, rep(2, 9), 30), start.degree = 263)
chordDiagram(
  cs_DFI[, c(1, 2)],
  grid.col = "grey",
  annotationTrack = c("name", "grid"),
  annotationTrackHeight = c(0.01, 0.02),
  transparency = 0.6,
  col = col_fun(cs_DFI$negative),
  order = c("CON", "CPN", "DMN",  "RTN", "VAN", "SN", "DAN", "FPN", "SHN",
    "SMN", "AN", "VN", "Crcx",  "Tha", "Hip", "Amg", "Pl", "Pt", "Cde", "NAc",
    "Vtdc", "BS"),
  scale = TRUE
)
title("\n DFI \n (negative aossciations)", cex.main = 1)
circos.clear()

# add legend (run 2 times)
lgd_links <- Legend(
  at = c(-5.5, 0, 5.5),
  col_fun = col_fun,
  grid_height  = unit(20, "mm"),
  labels_gp = gpar(cex = 5, fontface = "bold"),
  legend_gp = gpar(alpha = 0.2, cex = 2),
  direction = "horizontal",
  title = "t value",
  legend_width = unit(4.5, "in"), #
  title_position = "topcenter",
  title_gp = gpar(fontsize = 50, fontface = "bold"),
  title_gap = unit(5, "mm"),
  border = NA) %>%
draw(x = unit(820, "mm"), y = unit(60, "mm"))
# save as svg 3500 2800

# Figure 4 Neurotransmitters ---------------------------------------------------
Neurotransmitters <- read.csv("pet/Resh.csv") %>%
  rename("Spearman" = Mean.Fisher.s.z..Spearman.rho., "PET" = PET.Map) %>%
  select(File, PET, Spearman, p_exact)
Neurotransmitters$PET <- str_replace(Neurotransmitters$PET, "SERT", "5HTT")
Neurotransmitters$PET <- str_replace(Neurotransmitters$PET, "FDOPA", "F-DOPA")
Neurotransmitters$File <- str_split(Neurotransmitters$File, "/", simplify = TRUE)[, 6]
Neurotransmitters$File <- factor(Neurotransmitters$File, levels = unique(Neurotransmitters$File))

# FDR corrections
p_fdr <- Neurotransmitters %>%
  group_by(File) %>%
  summarise_at(vars(p_exact), p.adjust, method = "fdr")
Neurotransmitters$p_fdr <- p_fdr$p_exact
Neurotransmitters$significants <- ifelse(Neurotransmitters$p_fdr < 0.01, "**",
  ifelse(Neurotransmitters$p_fdr < 0.05, "* ", " "))
Neurotransmitters <- Neurotransmitters %>%
  mutate("PET" = paste0(Neurotransmitters$PET, Neurotransmitters$significants)) %>%
  select(File, PET, Spearman)

Neurotransmitters_vol <- Neurotransmitters[str_detect(Neurotransmitters$File, "vol"), ] %>%
  group_by(File)
Neurotransmitters_vol$File <- str_replace(Neurotransmitters_vol$File, "vol_PFI.nii", "PFI") %>%
  str_replace("vol_DFI.nii", "DFI") %>%
  str_replace("vol_PII.nii", "PII")

vol_PFI <- filter(Neurotransmitters_vol, File == "PFI") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p1 <- ggradar(vol_PFI, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5, axis.label.offset = 1.18,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#7FC97F", fill = TRUE, fill.alpha = 0.4) +
  labs(tag = "Prosocial friend index \n (PFI)") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "top"
  )

# DFI (volume)
vol_DFI <- filter(Neurotransmitters_vol, File == "DFI") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p2 <- ggradar(vol_DFI, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5, axis.label.offset = 1.18,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#9ECAE1", fill = TRUE, fill.alpha = 0.4) +
  labs(tag = "Delinquent friend index \n (DFI)") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "top"
  )

# PII (volume)
vol_PII <- filter(Neurotransmitters_vol, File == "PII") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p3 <- ggradar(vol_PII, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5, axis.label.offset = 1.18,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#EF6548", fill = TRUE, fill.alpha = 0.4) +
  labs(tag = "Positive influence index \n (PII)") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "top"
  )
a <-
p1 + p2 + p3 +
  plot_layout(nrow = 1)
ggsave("plot/Figure4_Neurotrasmitter_vol.svg", width = 18, height = 6)

# Figure 5 Mediation analysis --------------------------------------------------
plot_mat <- function(data, index, group) {
  df <- str_split(rownames(data), index, simplify = TRUE)[, 2] %>%
    str_split(" - ", simplify = TRUE) %>%
    as.data.frame()
  df$p <- data$PathAB_p
  df$peers <- group

  df$P_group <- ifelse(df$p < 0.0005, "group1", ifelse(
    df$p < 0.001, "group2", ifelse(
      df$p < 0.005, "group3", ifelse(
        df$p < 0.01, "group4", "group5"
  ))))
  df$P_group <- as.character(df$P_group) %>%
    factor(levels = paste0("group", 5:1))
  return(df)
}

modify_behavior_name <- function(data) {
  for(i in 1:nrow(data)) {
    data$V2[i] <- behavior_names[which(data$V2[i] == behavior)]
  }
  data$inventory <- str_split(data$V2, " - ", simplify = TRUE)[, 1]
  data$V2 <- str_split(data$V2, " - ", simplify = TRUE)[, 2]
  data$inventory <- factor(
    data$inventory,
    levels = c("Neurocognition", "Impulsivity", "Inhibition and Reward-seeking",
      "Subsyndromal Mania (Parent)", "Prodromal Psychosis",
      "Adverse Peer Experiences", "Trauma Events (Parent)",
      "Trauma Events", "CBCL")
  )
  data <- arrange(data, desc(inventory))
  return(data)
}

# Figure 5c
# volume - PFI
PFI_volume_plot <- plot_mat(PFI_volume, "PFI_", "PFI")
PFI_volume_all_plot <- plot_mat(PFI_volume_all, "PFI_", "PFI")

# volume - DFI
DFI_volume_plot <- plot_mat(DFI_volume, "DFI_", "DFI")
DFI_volume_all_plot <- plot_mat(DFI_volume_all, "DFI_", "DFI")

mediation_volume_plot <- rbind(PFI_volume_plot, DFI_volume_plot,
  PFI_volume_all_plot, DFI_volume_all_plot)

# RSFC - PFI
PFI_fc_plot <- plot_mat(PFI_fc, "PFI_", "PFI")

# RSFC - DFI
DFI_fc_plot <- plot_mat(DFI_fc, "DFI_", "DFI")

mediation_fc_plot <- rbind(PFI_fc_plot, DFI_fc_plot)
mediation_fc_plot$V1 <- str_replace(mediation_fc_plot$V1, "_", "-") %>%
  str_replace("dt", "dmn") %>%
  str_replace("dt", "dmn") %>%
  str_replace("cgc", "con") %>%
  str_replace("ca", "cpn") %>%
  str_replace("dla", "dan") %>%
  str_replace("vs", "VN") %>%
  str_replace("smm", "SMN") %>%
  str_replace("smh", "SHN") %>%
  toupper()
mediation_fc_plot <- arrange(mediation_fc_plot, V1)

# combine volume and RSFC
mediation_plot <- rbind(mediation_fc_plot, mediation_volume_plot) %>%
  modify_behavior_name()

mediation_plot$V1 <- factor(
  mediation_plot$V1,
  levels = c(unique(mediation_fc_plot$V1), "ifpllh", "paracnlh",
    "precnlh", "rracatelh", "sufrlh", "fusiformlh", "lobfrlh", "supllh",
    "postcnrh", "insularh", "iftmrh", "loccrh", "mdtmrh", "ptcaterh",
    "precnrh", "pcrh", "suplrh", "volume"),
  labels = c(unique(mediation_fc_plot$V1), "Inferior Parietal (L)",
    "Paracentral (L)", "Precentral (L)", "Rostral Anterior Cingulate (L)",
    "Superior Frontal (L)", "Fusiformlh (L)", "Lateral Orbitofrontal (L)",
    "Superior Parietal (L)", "Postcentral (R)", "Insula (R)",
    "Inferior Temporal (R)", "Lateral Occipital (R)", "Middle Temporal (R)",
    "Posterior Cingulate (R)", "Precentral (R)", "Precuneus (R)",
    "Superior Parietal (R)", "Total Volume")
  )
mediation_plot$V2 <- factor(
  mediation_plot$V2,
  levels = unique(arrange(mediation_plot, desc(inventory), desc(V2))$V2)
)

ggplot(mediation_plot, aes(V1, V2, size = P_group, color = peers, shape = peers)) +
  geom_point(alpha = 0.7) +
  geom_hline(aes(yintercept = V2), color = "grey") +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  scale_color_manual(name = "Mediator", values = c("#2166AC", "#7FC97F")) +
  scale_size_manual(
    name = expression(bold(bolditalic("p")[fdr])),
    values = 5:8,
    labels = c("< 0.05", "< 0.01", "< 0.005", "< 0.001")) +
  guides(
    size = guide_legend(title.hjust = 0.5, title.position = "top",
      title.theme = element_text(size = 25)),
    color = guide_legend(title.hjust = 0.5, title.position = "top",
      override.aes = list(size = 10),
      title.theme = element_text(size = 25, face = "bold")),
    shape = guide_legend("Mediator")) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(-0.1, "cm"),
    axis.text.x = element_text(size = 20, angle = 30, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 17, face = "bold"),
    legend.spacing = unit(5, "lines"),
    legend.title = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 25, face = "bold"),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.position = "top",
    legend.box.background = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent')
  )
ggsave("plot/Figure5c.svg", width = 18, height = 10, bg = "transparent")

# Figure 6 Longitudinal cloud word ---------------------------------------------
# PFI
df1 <- data.frame(
  "vars" = c("Internalizing", "Depression", "Withdrawal depression",
    "Somatic complaints", "Sluggish cognitive tempo",
    "Prodromal psychosis (distress)", "Prodromal psychosis (total)"),
  "Beta" = c(-0.046, -0.070, -0.071, -0.039, -0.057, -0.065, -0.062)
)
df1 <- df1[order(df1$Beta), ]

ggplot(df1, aes(label = vars, size = Beta, color = Beta)) +
  geom_text_wordcloud(shape = "circle", show.legend = TRUE) +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = "#A6DBA0", high = "#00441B") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 30, face = "bold"),
    legend.title = element_text(size = 30, face = "bold"),
    legend.key.height = unit(1.5, 'cm'),
    legend.key.width = unit(2.5, 'cm')
  ) +
  guides(
    size = FALSE,
    color = guide_colorbar(ticks = FALSE),
    fill = guide_colorbar(
      title = "t value",
      title.position = "top",
      label.position = "bottom",
      ticks = FALSE
    )
  )

ggsave("plot/Longitudinal_PFI.svg", width = 15, height = 7, bg = "transparent")

# DFI
df2 <- data.frame(
  "vars" = c("ADHD", "Aggressive", "Conduct", "Externalizing",
    "Oppositional Defiant", "Rulebreak", "Social",
    "Prodromal psychosis (distress)", "Prodromal psychosis (total)",
    "Total bad life events", "Total bad affections", "Total good affections",
    "Overt aggression", "Overt victimization", "Relational aggression",
    "Reputation aggression", "Reputation victimization"),
  "Beta" = c(0.036, 0.039, 0.069, 0.048, 0.042, 0.075, 0.030, 0.049, 0.060,
    0.040, 0.019, -0.012, 0.086, 0.083, 0.066, 0.095, 0.060)
)
df2 <- df2[order(df2$Beta, decreasing = TRUE), ]

ggplot(df2, aes(label = vars, size = Beta, color = Beta)) +
  geom_text_wordcloud(shape = "circle", show.legend = TRUE) +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "#4286f4", high = "#373B44",
    breaks = c(0, 0.025, 0.050, 0.075, 0.10),
    labels = c("0", "0.025", "0.050", "0.075", "0.10")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 30, face = "bold"),
    legend.title = element_text(size = 30, face = "bold"),
    legend.key.height = unit(1.5, 'cm'),
    legend.key.width = unit(2.5, 'cm')
  ) +
  guides(
    size = FALSE,
    color = guide_colorbar(ticks = FALSE),
    fill = guide_colorbar(
      title = "t value",
      title.position = "top",
      label.position = "bottom",
      ticks = FALSE
    )
  )
ggsave("plot/Longitudinal_DFI.svg", width = 15, height = 7, bg = "transparent")

# Supplementary Figure 2 -------------------------------------------------------
# density of peer environments
friends <- select(abcd_year2_behavior, pbp_ss_prosocial_peers, pbp_ss_rule_break) %>%
  rename("PFI" = pbp_ss_prosocial_peers, "DFI" = pbp_ss_rule_break) %>%
  melt() %>%
  table %>%
  data.frame()

friends$Freq <- ifelse(friends$variable == "DFI", friends$Freq * -1, friends$Freq)

ggplot(friends, aes(x = value, y = Freq, fill = variable)) +
  geom_histogram(stat = "identity") +
  labs(x = "Scores", y = "Counts") +
  scale_fill_manual(name = "Peer\nEnvironments", values = c("#7FC97F", "#2166AC")) +
  scale_x_discrete(breaks = seq(2, 15, 2)) +
  scale_y_continuous(breaks = seq(-4000, 1000, 1000), labels = c("4k", "3k", "2k", "1k", 0, "1k")) +
  coord_flip() +
  theme(
    axis.text.x  = element_text(size = 60, face = "bold"),
    axis.text.y  = element_text(size = 60, face = "bold"),
    axis.title = element_text(size = 100, face = "bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 80, face = "bold"),
    legend.position = c(0.4, 0.6),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent", colour = NA),
    axis.line = element_line(colour = "black")
  )
ggsave(filename = "plot/sFigure2.svg", width = 30, height = 15, bg = "transparent")

# Supplementary Figure 3 -------------------------------------------------------
plot_brain <- function(data) {
  ggplot(data) +
  geom_brain(atlas = dk, aes(fill = t), size = 1, position = position_brain(. ~ side + hemi),
    show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-4.5, 4.5), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold")
  )
}

# DFI (thick)
DFI_thick <- extract_region(DFI1, "thick")
DFI_thick$region <- c("fusiform", "lateral occipital", "lateral orbitofrontal",
  "lingual", "parahippocampal", "paracentral", "precuneus", "superior parietal",
  "temporal pole", "cuneus", "fusiform", "lateral occipital", "parahippocampal",
  "paracentral")
s2_1 <- plot_brain(DFI_thick) + labs(title = "DFI (Thickness)", tag = "a")
s2_1

# PII (thick)
PII_thick <- extract_region(PII1, "thick")
PII_thick$region <- c("inferior parietal", "precuneus", "superiorparietal")
s2_2 <- plot_brain(DFI_thick) + labs(title = "PII (Thickness)", tag = "b")
s2_2

# PII (area)
PII_area <- extract_region(PII1, "area")
PII_area$region <- c("inferior parietal", "precentral", "insula", "postcentral",
  "posterior cingulate", "frontal pole")

s2_3 <- ggplot(PII_area) +
  geom_brain(atlas = dk, aes(fill = t), size = 1, position = position_brain(. ~ side + hemi)) +
  scale_fill_gradient2(limits = c(-4.5, 4.5), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  theme_void() +
  labs(title = "PII (Area)", tag = "c") +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 24, face = "bold"),
    legend.key.height = unit(1.2, 'cm'),
    legend.key.width = unit(2.5, 'cm'),
  ) +
  guides(
    fill = guide_colorbar(
      title = "t value",
      title.position = "top",
      title.hjust	= 0.5,
      label.position = "bottom",
      ticks = FALSE
    )
  )
s2_3

s2_1 + s2_2 + s2_3 +
  plot_layout(nrow = 3)
ggsave("plot/sFigure3.svg", height = 12, width = 16)

# Supplementary Figure 4 -------------------------------------------------------
# area
Neurotransmitters_area <- Neurotransmitters[str_detect(Neurotransmitters$File, "area"), ] %>%
  group_by(File)
Neurotransmitters_area$File <- str_replace(Neurotransmitters_area$File, "area_PFI.nii", "PFI") %>%
  str_replace("area_DFI.nii", "DFI") %>%
  str_replace("area_PII.nii", "PII")

# PFI (area)
area_PFI <- filter(Neurotransmitters_area, File == "PFI") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p4 <- ggradar(area_PFI, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#7FC97F", fill = TRUE, fill.alpha = 0.4) +
  labs(title = "a") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "bottom"
  )

# DFI (area)
area_DFI <- filter(Neurotransmitters_area, File == "DFI") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p5 <- ggradar(area_DFI, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#9ECAE1", fill = TRUE, fill.alpha = 0.4) +
  labs(title = "b") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "bottom"
  )

# PII (area)
area_PII <- filter(Neurotransmitters_area, File == "PII") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p6 <- ggradar(area_PII, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#EF6548", fill = TRUE, fill.alpha = 0.4) +
  labs(title = "c") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "bottom"
  )

# thick
Neurotransmitters_thick <- Neurotransmitters[str_detect(Neurotransmitters$File, "thick"), ] %>%
  group_by(File)
Neurotransmitters_thick$File <- str_replace(Neurotransmitters_thick$File, "thick_PFI.nii", "PFI") %>%
  str_replace("thick_DFI.nii", "DFI") %>%
  str_replace("thick_PII.nii", "PII")

# PFI (thick)
thick_PFI <- filter(Neurotransmitters_thick, File == "PFI") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p7 <- ggradar(thick_PFI, values.radar = c("-0.5", "0", "0.5"),
  grid.min = -0.5, grid.mid = 0, grid.max = 0.5, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#7FC97F", fill = TRUE, fill.alpha = 0.4) +
  labs(title = "d", tag = "PFI") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "bottom"
  )

# DFI (thick)
thick_DFI <- filter(Neurotransmitters_thick, File == "DFI") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p8 <- ggradar(thick_DFI, values.radar = c("-0.6", "0", "0.6"),
  grid.min = -0.6, grid.mid = 0, grid.max = 0.6, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#9ECAE1", fill = TRUE, fill.alpha = 0.4) +
  labs(title = "e", tag = "DFI") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "bottom"
  )

# PII (thick)
thick_PII <- filter(Neurotransmitters_thick, File == "PII") %>%
  pivot_wider(names_from = PET, values_from = Spearman)

p9 <- ggradar(thick_PII, values.radar = c("-0.7", "0", "0.7"),
  grid.min = -0.7, grid.mid = 0, grid.max = 0.7, plot.legend = FALSE,
  grid.label.size = 11, axis.label.size = 6.5,
  background.circle.colour = "white", gridline.mid.colour = "grey",
  group.colours = "#EF6548", fill = TRUE, fill.alpha = 0.4) +
  labs(title = "f", tag = "PII") +
  theme(
    title = element_text(size = 20, face = "bold"),
    plot.tag.position = "bottom"
  )

p4 + p5 + p6 + p7 + p8 + p9 +
  plot_layout(nrow = 2)
ggsave("plot/sFigure4.svg", width = 16, height = 12)

# Supplementary Figure 5 -------------------------------------------------------
# volume - PII
PII_volume_plot <- plot_mat(PII_volume, "PII_", "PII")
PII_volume_all_plot <- plot_mat(PII_volume_all, "PII_", "PII")
mediation_PII_volume_plot <- rbind(PII_volume_plot, PII_volume_all_plot)

# FC - PII
PII_fc_plot <- plot_mat(PII_fc, "PII_", "PII")
PII_fc_plot$V1 <- str_replace(PII_fc_plot$V1, "_", "-") %>%
  str_replace("dt", "dmn") %>%
  str_replace("dt", "dmn") %>%
  str_replace("cgc", "con") %>%
  str_replace("cgc", "con") %>%
  str_replace("ca", "cpn") %>%
  str_replace("dla", "dan") %>%
  str_replace("vs", "vn") %>%
  toupper()

# combine volume and RSFC
mediation_PII_plot <- rbind(PII_volume_plot, PII_volume_all_plot, PII_fc_plot) %>%
  modify_behavior_name()

mediation_PII_plot$V1 <- factor(
  mediation_PII_plot$V1,
  levels = c(unique(PII_fc_plot$V1), "ifpllh", "locclh", "lobfrlh",
    "mdtmlh", "precnlh", "insulalh", "fusiformrh", "ifplrh", "paracnrh",
    "ptcaterh", "precnrh", "suplrh", "volume"),
  labels = c(unique(PII_fc_plot$V1), "Inferior Parietal (L)",
    "Lateral Occipital (L)", "Lateral Orbitofrontal (L)", "Middle Temporal (L)",
    "Precentral (L)", "Insula (L)", "Fusiformlh (R)", "Inferior Parietal (R)",
    "Paracentral (R)", "Posterior Cingulate (R)", "Precentral (R)",
    "Superior Parietal (R)", "Total Volume")
  )
mediation_PII_plot$V2 <- factor(
  mediation_PII_plot$V2,
  levels = unique(arrange(mediation_PII_plot, desc(inventory), desc(V2))$V2)
)

ggplot(mediation_PII_plot, aes(V1, V2, size = P_group)) +
  geom_point(alpha = 0.8, color = "#EF6548") +
  geom_hline(aes(yintercept = V2), color = "grey") +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  scale_size_manual(
    name = expression(bold(italic(p)[fdr])),
    values = 3:7,
    labels = c("< 0.05", "< 0.01", "< 0.005", "< 0.001", "< 0.0005")) +
  guides(size = guide_legend(title.hjust = 0.5, title.theme = element_text(size = 15))) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(-0.1, "cm"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 25, face = "bold"),
    plot.tag = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold"),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.position = "right",
    legend.box.background = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent')
  )
ggsave("plot/sFigure5.svg", width = 14.5, height = 8, bg = "transparent")

# Supplementary Figure 6 -------------------------------------------------------
# cortico-subcortical RSFCs - DFI
DFI_sub_plot <- plot_mat(DFI_sub, "DFI_", "DFI")
DFI_sub_plot$V1 <- str_replace(DFI_sub_plot$V1, "_", "-") %>%
  str_replace("df", "DMN") %>%
  str_replace("cerc", "CON") %>%
  str_replace("fopa", "FPN") %>%
  str_replace("copa", "CPN") %>%
  str_replace("sa", "SN") %>%
  str_replace("vta", "VAN") %>%
  str_replace("dsa", "DAN") %>%
  str_replace("rst", "RTN") %>%
  str_replace("vs", "VN") %>%
  str_replace("au", "AN") %>%
  str_replace("smh", "SHN") %>%
  str_replace("smm", "SMN") %>%
  str_replace("crcx", "Cerebellum Cortex") %>%
  str_replace("thp", "Thalamus") %>%
  str_replace("cde", "Caudate") %>%
  str_replace("pt", "Putamen") %>%
  str_replace("pl", "Pallidum") %>%
  str_replace("hp", "Hippocampus") %>%
  str_replace("ag", "Amygdala") %>%
  str_replace("aa", "Accumbens") %>%
  str_replace("vtdc", "Ventral Diencephalon") %>%
  str_replace("bs", "Brain Stem")
DFI_sub_plot$V1 <- paste0(
  str_split(DFI_sub_plot$V1, "-", simplify = TRUE)[, 2],
  "-",
  str_split(DFI_sub_plot$V1, "-", simplify = TRUE)[, 1]
)
DFI_sub_plot <- modify_behavior_name(DFI_sub_plot)
DFI_sub_plot$V2 <- factor(
  DFI_sub_plot$V2,
  levels = unique(arrange(DFI_sub_plot, desc(inventory), desc(V2))$V2)
)


ggplot(DFI_sub_plot, aes(V1, V2, size = P_group)) +
  geom_point(alpha = 0.8, color = "#2166AC") +
  geom_hline(aes(yintercept = V2), color = "grey") +
  labs(x = "Cortico-Subcortical RSFCs", y = NULL) +
  theme_classic() +
  scale_size_manual(
    name = expression(bold(italic(p)[fdr])),
    values = 3:7,
    labels = c("< 0.05", "< 0.01", "< 0.005", "< 0.001", "< 0.0005")) +
  guides(size = guide_legend(title.hjust = 0.5, title.theme = element_text(size = 15))) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(-0.1, "cm"),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 25, face = "bold"),
    plot.tag = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold"),
    legend.key.height = unit(1, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.position = "right",
    legend.box.background = element_blank(),
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent')
  )
ggsave("plot/sFigure6.svg", width = 16, height = 10, bg = "transparent")

# Supplementary Figure 7 -------------------------------------------------------
# PII
df3 <- data.frame(
  "vars" = c("Withdrawal depression", "Somatic complaints", "Attention",
    "Rule break", "Internalizing", "Externalizing", "Total problem",
    "Depression", "Somatic", "ADHD", "Opposit", "Conduct",
    "Sluggish cognitive tempo", "Prodromal psychosis (distress)",
    "Prodromal psychosis (total)", "Reputation aggression",
    "Reputation victimization", "Overt victimization"),
  "Beta" = c(-0.066, -0.045, -0.029, -0.057, -0.042, -0.034, -0.033, -0.068,
    -0.043, -0.032, -0.036, -0.041, -0.054, -0.083, -0.082, -0.045, -0.037,
    -0.043)
)
df3 <- df3[order(df3$Beta), ]

ggplot(df3, aes(label = vars, size = Beta, color = Beta)) +
  geom_text_wordcloud(shape = "circle", show.legend = TRUE) +
  scale_size_area(max_size = 10) +
  scale_color_gradient(low = "#E41A1C", high = "#FEE08B") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 30, face = "bold"),
    legend.key.height = unit(1.5, 'cm'),
    legend.key.width = unit(2.5, 'cm')
  ) +
  guides(
    size = FALSE,
    color = guide_colorbar(ticks = FALSE),
    fill = guide_colorbar(
      title = "t value",
      title.position = "top",
      label.position = "bottom",
      ticks = FALSE
    )
  )
ggsave("plot/sFigure7.svg", width = 15, height = 7, bg = "transparent")

# Others -----------------------------------------------------------------------
# one random brain (neurotransmitters brain, Figure 1)
dk_atlas <- ggseg(atlas = dk, fill = "red", colour = "black", size = 1,
  hemi = "left", view = "lateral", show.legend = FALSE) + theme_void()

brains <- data.frame("hemi" = dk_atlas$data$hemi, "side" = dk_atlas$data$side,
  "region" = dk_atlas$data$region)
brains <- brains[!duplicated(brains), ]
brains$fills <- rnorm(nrow(brains))

ggplot(brains) +
  geom_brain(atlas = dk, aes(fill = fills), size = 1, hemi = "left",
    side = "lateral", show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-2, 2), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab") +
  theme_void()
ggsave("plot/random_brain.png", width = 2.17, height = 1.46, bg = "transparent")




# one PFI brain (Figure 5a)
PFI_vol$t <- 4.5
ggplot(PFI_vol) +
  geom_brain(atlas = dk, aes(fill = t), size = 1, hemi = "left", side = "lateral", show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-4.5, 4.5), low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold")
  )
ggsave("plot/PFI_one_vol.svg", width = 15, height = 8)

# one DFI brain (Figure 5b)
DFI_vol$t <- -4.5
ggplot(DFI_vol) +
  geom_brain(atlas = dk, aes(fill = t), size = 1, hemi = "right", side = "lateral", show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-4.5, 4.5),  low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold")
  )
ggsave("plot/DFI_one_vol.svg", width = 15, height = 8)

# one PII brain (sFigure 6)
PII_vol$t <- 4.5
ggplot(PII_vol) +
  geom_brain(atlas = dk, aes(fill = t), size = 1, hemi = "left", side = "lateral", show.legend = FALSE) +
  scale_fill_gradient2(limits = c(-4.5, 4.5),  low = "#9ECAE1", high = "#EF6548",
    mid = "white", midpoint = 0, na.value = "white", space = "Lab", name = "t") +
  theme_void() +
  theme(
    plot.tag = element_text(size = 25, face = "bold"),
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold")
  )
ggsave("plot/PII_one_vol.svg", width = 15, height = 8)

# brain connectivity (sFigure 5)
set.seed(12)
df1 <- matrix(rnorm(68^2), 68, 68)
df1 <- ifelse(df1 > 2.85, 1, 0)
df1[lower.tri(df1)] <- t(df1)[lower.tri(df1)]
brainconn(conmat = df1, atlas = "dk68", view = "left",
  node.size = 35, edge.alpha = 0.7, edge.width = 15, edge.color = "#2470a0",
  show.legend = FALSE) +
  scale_color_brewer(palette = "RdYlGn") +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("plot/brain_connectivity.svg", width = 15, height = 8, bg = "transparent")