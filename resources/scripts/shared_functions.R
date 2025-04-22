#!/usr/bin/env Rscript

library(magrittr)
library(tidyverse)
library(forcats)

stylize_bar <- function(gplot, usertypeColor = TRUE, singleColor = FALSE, sequentialColor = FALSE, xlabel = "Count", ylabel = "", legendpos = "right", rotate = 0, hjustv = 0, labelBars = TRUE, groupVar = NULL){
  if (usertypeColor) {
    fillColors <- c("#E0DD10", "#035C94", "#7EBAC0")
    positionarg = "stack"
  }
  else if (singleColor){
    fillColors <- c("#25445A")
    legendpos <- "none"
    positionarg <- "identity"
  }
  else if (sequentialColor){
    fillColors <- c("#035C94","#035385","#024A77","#024168", "#02395B")
    legendpos = "none"
    positionarg <- "identity"
  }
  if (labelBars){
    if (hjustv == 0 | tolower(ylabel) == "count"){
      gplot <- gplot +
        geom_text(aes(label = after_stat(..count..)), stat = "count",
                  position = positionarg, vjust = -1, size=2) +
        coord_cartesian(clip = "off")
    } else{
      gplot <- gplot +
        geom_text(aes(label = paste0("    ", after_stat(..count..))), stat = "count",
                  position = positionarg, hjust = -1, size=2, group = groupVar, inherit.aes = TRUE) +
        coord_cartesian(clip = "off")
    }
  }
  return(
    gplot +
    theme_classic() +
    ylab(ylabel) +
    xlab(xlabel) +
    theme(legend.title = element_blank(),
          legend.position = legendpos,
          axis.text.x = element_text(angle=rotate, hjust=hjustv),
          text = element_text(size=12)) +
    scale_fill_manual(values = fillColors, na.translate = F)
  )
}

stylize_dumbbell <- function(gplot, xmax = NULL, importance = FALSE, preference = FALSE, xlabel="Average Rank Choice", ylabel=""){
  if (importance){
    textGrobMost <- "Most\nimportant"
    textGrobLeast <- "Least\nimportant"
  }
  else if (preference){
    textGrobMost <- "Most\npreferred"
    textGrobLeast <- "Least\npreferred"
  }
  return(
    gplot +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            text = element_text(size=12)) +
      xlab(xlabel) +
      ylab(ylabel) +
      scale_color_manual(values = c("#E0DD10", "#035C94")) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(1,1,1,1.1, "cm")) +
      scale_x_reverse(limits = c(xmax,1), breaks = xmax:1, labels = xmax:1) +
      annotation_custom(textGrob(textGrobMost, gp=gpar(fontsize=8, fontface = "bold")),xmin=-1,xmax=-1,ymin=-0.5,ymax=-0.5) +
      annotation_custom(textGrob(textGrobLeast, gp=gpar(fontsize=8, fontface= "bold")),xmin=-xmax,xmax=-xmax,ymin=-0.5,ymax=-0.5)
  )
}

PlotToolKnowledge_customization <- function(gplot){
  return(
    gplot +
      scale_x_continuous(breaks = 0:5, labels = 0:5, limits = c(0,5)) +
      ylab("Tool or Data Resource") +
      xlab("Average Knowledge or Comfort Score") +
      theme_bw() +
      theme(panel.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            text = element_text(size=12)) +
      annotation_custom(textGrob("Don't know\nat all", gp=gpar(fontsize=8, fontface = "bold")),xmin=0,xmax=0,ymin=-2,ymax=-2) +
      annotation_custom(textGrob("Extremely\ncomfortable", gp=gpar(fontsize=8, fontface= "bold")),xmin=5,xmax=5,ymin=-2,ymax=-2) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(1,1,1,1.1, "cm")) +
      ggtitle("How would you rate your knowledge of or\ncomfort with these technologies or data features?") +
      scale_color_manual(values = c("#E0DD10", "#035C94")) +
      scale_shape_manual(values = c(4, 16)) +
      theme(legend.title = element_blank())
  )
}

prep_df_whichData <- function(subset_df, onAnVILDF = NULL){
  subset_df %<>%
    separate_longer_delim(AccessWhichControlledData, delim = ", ") %>%
    drop_na(AccessWhichControlledData) %>%
    #group_by(whichControlledAccess) %>%
    #summarize(count = n()) %>%
    mutate(AccessWhichControlledData =
             recode(AccessWhichControlledData,
                    "All of Us*" = "All of Us",
                    "UK Biobank*" = "UK Biobank",
                    "Centers for Common Disease Genomics (CCDG)" = "CCDG",
                    "The Centers for Mendelian Genomics (CMG)" = "CMG",
                    "Clinical Sequencing Evidence-Generating Research (CSER)" = "CSER",
                    "Electronic Medical Records and Genomics (eMERGE)" = "eMERGE",
                    "Gabriella Miller Kids First (GMKF)" = "GMKF",
                    "Genomics Research to Elucidate the Genetics of Rare Diseases (GREGoR)" = "GREGoR",
                    "The Genotype-Tissue Expression Project (GTEx)" = "GTEx",
                    "The Human Pangenome Reference Consortium (HPRC)" = "HPRC",
                    "Population Architecture Using Genomics and Epidemiology (PAGE)" = "PAGE",
                    "Undiagnosed Disease Network (UDN)" = "UDN",
                    "Being able to pull other dbGap data as needed." = "Other",
                    "Cancer omics datasets" = "Other",
                    "GnomAD and ClinVar" = "None", #not controlled access
             )
    ) %>%
    left_join(onAnVILDF, by=c("AccessWhichControlledData" = "whichControlledAccess"))

  return(subset_df)
}

plot_which_data <- function(inputToPlotDF, subtitle = NULL){

  toreturnplot <- ggplot(inputToPlotDF,
                         aes(
                           x = fct_infreq(AccessWhichControlledData),#reorder(AccessWhichControlledData, -count),
                           #y = count,
                           fill = AnVIL_Availability)
                         ) +
    geom_bar() + #stat="identity") +
    theme_classic() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "inside",
          legend.position.inside = c(0.8, 0.8),
          text = element_text(size=12)
          ) +
    xlab("Controlled access datasets") +
    ylab("Count") +
    ggtitle("What large, controlled access datasets do you access\nor would you be interested in accessing using the AnVIL?",
            subtitle = subtitle) +
    geom_text(aes(label = after_stat(..count..), group = AccessWhichControlledData),
              stat = 'count', #'summary',
              #fun = sum,
              vjust = -1,
              size=2) +
    coord_cartesian(clip = "off") +
    scale_fill_manual(values = c("#25445A", "#7EBAC0", "grey"))

  return(toreturnplot)
}

prep_df_typeData <- function(subset_df){
  subset_df %<>%
    separate_longer_delim(TypesOfData, delim=", ") %>%
    #drop_na(TypesOfData) %>%
    #group_by(TypesOfData) %>% summarize(count = n()) %>%
    mutate(TypesOfData =
             recode(TypesOfData,
                    "I don't analyze data on AnVIL" = NA_character_,
                    "I store data in AnVIL. I don’t analyze it." = NA_character_,
                    "Used in training for analysis of genomes (variant calling)" = "Variant Calling"
             )
    ) %>%
    drop_na(TypesOfData)
  return(subset_df)
}

plot_type_data <- function(inputToPlotDF, subtitle = NULL){
  toreturnplot <- ggplot(inputToPlotDF, aes(x = fct_infreq(TypesOfData),#x = reorder(whichTypeData, -count),
                                            #y = count,
                                            fill = "#25445A")) +
    geom_bar() + #stat="identity") +
    ggtitle("What types of data do you or would you analyze using the AnVIL?", subtitle = subtitle) #+
    #geom_text(aes(label = after_stat(y), group = TypesOfData),
    #          stat = 'summary', fun = sum, vjust = -1, size=2) +
    #coord_cartesian(clip = "off")

  toreturnplot %<>% stylize_bar(usertypeColor = FALSE, singleColor = TRUE, xlabel = "Types of data", ylabel = "Count", hjustv = 1, rotate=45)

  return(toreturnplot)
}
