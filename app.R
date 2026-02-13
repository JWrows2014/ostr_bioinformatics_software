# app.R
# Shiny app: Learn + compare commercial bioinformatics software capabilities
#
# Install once:
# install.packages(c("shiny","bslib","dplyr","tidyr","DT","ggplot2","stringr","purrr"))

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(stringr)
library(purrr)

# -----------------------------
# 1) Source manuals (user-provided)
# -----------------------------
manuals <- tibble::tibble(
  package = c("Partek Flow", "QIAGEN CLC Genomics Workbench", "Qlucore Omics Explorer", "Geneious Prime"),
  vendor  = c("Partek / Illumina", "QIAGEN", "Qlucore", "Geneious / Biomatters"),
  manual_url = c(
    "https://help.partek.illumina.com/partek-flow/user-manual",
    "https://resources.qiagenbioinformatics.com/manuals/clcgenomicsworkbench/2203/User_Manual.pdf",
    "https://qlucore.com/sites/default/files/2021-12/Tutorial%203.8%20Mac%20A.pdf",
    "https://manual.geneious.com/en/latest/"
  )
)

# -----------------------------
# 2) Capability taxonomy (edit/extend freely)
# -----------------------------
capabilities <- tibble::tibble(
  capability = c(
    # NGS upstream + core
    "Raw read QC / trimming / demultiplexing",
    "Read mapping to reference",
    "De novo assembly",
    "Long-read support",
    # DNA variation
    "Variant calling (germline/somatic/other)",
    "Variant annotation / consequence prediction",
    # Transcriptomics
    "Bulk RNA-seq (counts/DE workflows)",
    "Single-cell RNA-seq",
    "CITE-seq / multi-omic single-cell",
    "miRNA-seq",
    # Epigenomics
    "ChIP-seq",
    "ATAC-seq",
    "Bisulfite / methylation-seq",
    # Microbial
    "Metagenomics",
    # Biological interpretation
    "Pathway analysis / enrichment",
    "Gene set enrichment (GSEA)",
    "Gene ontology (GO) browsing",
    # Visualization / exploratory
    "Genome browser",
    "PCA / dimensionality reduction",
    "t-SNE / embedding plots",
    "Heatmaps + clustering",
    "Volcano plots",
    # ML / classification
    "Built-in ML classification (SVM/kNN/trees/boosting)",
    # Sequence / molecular biology (bench-top)
    "Multiple sequence alignment",
    "Phylogenetic trees",
    "Primer design",
    "Cloning tools",
    "CRISPR tools",
    "BLAST",
    # Operational / workflow
    "Workflow/pipeline builder",
    "Command-line interface",
    "Shared databases / collaboration",
    "Cloud option"
  ),
  category = c(
    rep("NGS core", 4),
    rep("DNA variation", 2),
    rep("Transcriptomics", 4),
    rep("Epigenomics", 3),
    "Microbial",
    rep("Biological interpretation", 3),
    rep("Visualization", 5),
    "Machine learning",
    rep("Sequence & molecular biology", 6),
    rep("Operational", 4)
  )
)

# -----------------------------
# 3) Starter capability matrix (you will likely refine)
#    Values: "Yes", "Partial", "No"
# -----------------------------
matrix_long <- tibble::tibble(
  package = rep(manuals$package, each = nrow(capabilities)),
  capability = rep(capabilities$capability, times = nrow(manuals)),
  support = "No"
) %>%
  # ---- Partek Flow (from Partek manual structure + CCR feature list) ----
mutate(support = if_else(
  package == "Partek Flow" & capability %in% c(
    "Variant calling (germline/somatic/other)",
    "Bulk RNA-seq (counts/DE workflows)",
    "Single-cell RNA-seq",
    "CITE-seq / multi-omic single-cell",
    "miRNA-seq",
    "ChIP-seq",
    "ATAC-seq",
    "Metagenomics",
    "Pathway analysis / enrichment",
    "Gene set enrichment (GSEA)",
    "Genome browser",
    "Workflow/pipeline builder"
  ), "Yes", support
)) %>%
  mutate(support = if_else(
    package == "Partek Flow" & capability %in% c(
      "Raw read QC / trimming / demultiplexing",
      "Read mapping to reference"
    ), "Partial", support
  )) %>%
  # ---- QIAGEN CLC Genomics Workbench (from CCR feature list) ----
mutate(support = if_else(
  package == "QIAGEN CLC Genomics Workbench" & capability %in% c(
    "Raw read QC / trimming / demultiplexing",
    "Read mapping to reference",
    "De novo assembly",
    "Long-read support",
    "Variant calling (germline/somatic/other)",
    "Variant annotation / consequence prediction",
    "Bulk RNA-seq (counts/DE workflows)",
    "miRNA-seq",
    "ChIP-seq",
    "Bisulfite / methylation-seq",
    "Genome browser",
    "Workflow/pipeline builder"
  ), "Yes", support
)) %>%
  mutate(support = if_else(
    package == "QIAGEN CLC Genomics Workbench" & capability %in% c(
      "Pathway analysis / enrichment"  # often via IPA integration / exports
    ), "Partial", support
  )) %>%
  # ---- Qlucore Omics Explorer (from CCR feature list + tutorial) ----
mutate(support = if_else(
  package == "Qlucore Omics Explorer" & capability %in% c(
    "PCA / dimensionality reduction",
    "t-SNE / embedding plots",
    "Heatmaps + clustering",
    "Volcano plots",
    "Gene set enrichment (GSEA)",
    "Gene ontology (GO) browsing",
    "Built-in ML classification (SVM/kNN/trees/boosting)"
  ), "Yes", support
)) %>%
  mutate(support = if_else(
    package == "Qlucore Omics Explorer" & capability %in% c(
      "Single-cell RNA-seq",
      "Bulk RNA-seq (counts/DE workflows)"
    ), "Partial", support  # focuses on tabular/exploratory; upstream handled elsewhere
  )) %>%
  # ---- Geneious Prime (from Geneious manual ToC items) ----
mutate(support = if_else(
  package == "Geneious Prime" & capability %in% c(
    "Raw read QC / trimming / demultiplexing", # FastQC report; trimming often via tools/plugins
    "Read mapping to reference",
    "De novo assembly",
    "Multiple sequence alignment",
    "Phylogenetic trees",
    "Primer design",
    "Cloning tools",
    "CRISPR tools",
    "BLAST",
    "Workflow/pipeline builder",
    "Command-line interface",
    "Shared databases / collaboration",
    "Cloud option"
  ), "Yes", support
)) %>%
  mutate(support = if_else(
    package == "Geneious Prime" & capability %in% c(
      "Variant calling (germline/somatic/other)",
      "Bulk RNA-seq (counts/DE workflows)"
    ), "Partial", support
  )) %>%
  left_join(capabilities, by = "capability") %>%
  left_join(manuals, by = "package")

# -----------------------------
# 4) “Learn” links (deep links into manuals)
#    Edit these as you discover better anchors/pages.
# -----------------------------
learn_links <- tibble::tibble(
  package = c(
    "Partek Flow","Partek Flow","Partek Flow",
    "QIAGEN CLC Genomics Workbench","QIAGEN CLC Genomics Workbench",
    "Qlucore Omics Explorer","Qlucore Omics Explorer","Qlucore Omics Explorer","Qlucore Omics Explorer",
    "Geneious Prime","Geneious Prime","Geneious Prime","Geneious Prime"
  ),
  topic = c(
    "Manual home","Pipelines overview","Importing data",
    "Manual home (PDF)","NGS tasks overview (alt ref)",
    "Tutorial PDF","Classifier","GSEA/GO","Visualizations",
    "Manual home","Assembly & mapping","Alignments","Phylogenetics"
  ),
  url = c(
    "https://help.partek.illumina.com/partek-flow/user-manual",
    "https://help.partek.illumina.com/partek-flow/user-manual/pipelines",
    "https://help.partek.illumina.com/partek-flow/user-manual/importing-data",
    "https://resources.qiagenbioinformatics.com/manuals/clcgenomicsworkbench/2203/User_Manual.pdf",
    "https://bioinformatics.ccr.cancer.gov/docs/resources-for-bioinformatics/software/clc_genomic_workbench/",
    # Qlucore tutorial PDF with page hints (browser may honor #page=)
    "https://qlucore.com/sites/default/files/2021-12/Tutorial%203.8%20Mac%20A.pdf",
    "https://qlucore.com/sites/default/files/2021-12/Tutorial%203.8%20Mac%20A.pdf#page=37",
    "https://qlucore.com/sites/default/files/2021-12/Tutorial%203.8%20Mac%20A.pdf#page=38",
    "https://qlucore.com/sites/default/files/2021-12/Tutorial%203.8%20Mac%20A.pdf#page=45",
    "https://manual.geneious.com/en/latest/",
    "https://manual.geneious.com/en/latest/assembly_and_mapping/",
    "https://manual.geneious.com/en/latest/sequence_alignments/",
    "https://manual.geneious.com/en/latest/building_phylogenetic_trees/"
  )
)

# -----------------------------
# Shiny UI
# -----------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("OSTR Commercial Bioinformatics Software Finder"),
  layout_sidebar(
    sidebar = sidebar(
      h5("Filter"),
      selectizeInput(
        "pkg",
        "Packages",
        choices = manuals$package,
        selected = manuals$package,
        multiple = TRUE,
        options = list(plugins = list("remove_button"),placeholder="Choose packages...")
      ),
      selectizeInput(
        "cat",
        "Capability categories",
        choices = sort(unique(capabilities$category)),
        selected = sort(unique(capabilities$category)),
        multiple = TRUE,
        options = list(plugins = list("remove_button"),placeholder="Choose packages...")
      ),
      textInput("q", "Search capability text", value = ""),
      checkboxGroupInput(
        "support_filter",
        "Show support levels",
        choices = c("Yes","Partial","No"),
        selected = c("Yes","Partial","No"),
        inline = TRUE
      ),
      hr(),
      h5("Quick links"),
      uiOutput("manual_links")
    ),
    navset_card_tab(
      nav_panel(
        "Compare",
        p("Select packages + categories on the left. Use this to compare 'what each package can do'."),
        DTOutput("matrix_table"),
        br(),
       ),
      nav_panel(
         "Visual comparison",
         plotOutput("heatmap", height = "auto")
      ),
      nav_panel(
        "Learn",
        p("Curated jump-links into manuals/tutorials (edit these in learn_links)."),
        DTOutput("learn_table")
      ),
      nav_panel(
        "Data (edit me)",
        p("This is the underlying long-form dataset the app uses. Export it, version-control it, and refine."),
        downloadButton("download_csv", "Download capability matrix (CSV)"),
        br(), br(),
        DTOutput("raw_table")
      )
    )
  )
)

# -----------------------------
# Shiny server
# -----------------------------
server <- function(input, output, session) {
  
  filtered <- reactive({
    df <- matrix_long %>%
      filter(package %in% input$pkg) %>%
      filter(category %in% input$cat) %>%
      filter(support %in% input$support_filter)
    
    q <- str_trim(input$q)
    if (nzchar(q)) {
      df <- df %>% filter(str_detect(str_to_lower(capability), str_to_lower(q)))
    }
    df
  })
  
  output$manual_links <- renderUI({
    req(input$pkg)
    links <- manuals %>% filter(package %in% input$pkg)
    tagList(
      lapply(seq_len(nrow(links)), function(i) {
        tags$div(
          tags$strong(links$package[i]),
          tags$div(tags$a(href = links$manual_url[i], target = "_blank", "Open manual"))
        )
      })
    )
  })
  
  output$matrix_table <- renderDT({
    df <- filtered() %>%
      select(package, category, capability, support, manual_url) %>%
      arrange(category, capability, package)
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE),
      escape = FALSE
    )
  })
  
  output$heatmap <- renderPlot({
    df <- filtered() %>%
      mutate(score = case_when(
        support == "Yes" ~ 2,
        support == "Partial" ~ 1,
        TRUE ~ 0
      ))
    
    # Show only capabilities that have at least one "Yes" or "Partial" among selected pkgs
    df2 <- df %>%
      group_by(category, capability) %>%
      filter(max(score, na.rm = TRUE) > 0) %>%
      ungroup()
    
    validate(
      need(nrow(df2) > 0, "No capabilities to plot with current filters (try including Partial/No or more categories).")
    )
    
    ggplot(df2, aes(x = package, y = reorder(capability, score, FUN = max), fill = factor(score))) +
      geom_tile(color = "white") +
      labs(x = NULL, y = NULL, fill = "Support\n(0/1/2)") +
      scale_fill_discrete(drop = FALSE) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid = element_blank()
      )
  })
  
  output$learn_table <- renderDT({
    df <- learn_links %>%
      filter(package %in% input$pkg) %>%
      mutate(link = sprintf('<a href="%s" target="_blank">%s</a>', url, url)) %>%
      select(package, topic, link)
    
    datatable(df, rownames = FALSE, escape = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  output$raw_table <- renderDT({
    datatable(
      filtered() %>% select(package, category, capability, support, vendor, manual_url),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() sprintf("capability_matrix_%s.csv", Sys.Date()),
    content = function(file) {
      write.csv(
        matrix_long %>% select(package, vendor, category, capability, support, manual_url),
        file,
        row.names = FALSE
      )
    }
  )
}

shinyApp(ui, server)
