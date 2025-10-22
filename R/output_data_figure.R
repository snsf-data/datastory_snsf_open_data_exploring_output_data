#------------------------------------------------------------------------------#
#                          ---- Packages & set-up ----
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(snf.datastory)
library(here)

# Make sure that when the script is ran in isolation the `params` object is
# available and set by default to English.
if (!exists("params")) {
  params <- NULL
  params$lang <- "en"
}

# Whether the figure produced in this script needs to be saved locally or not
save_fig <- FALSE


#------------------------------------------------------------------------------#
#                         ---- Load & prepare data ----
#------------------------------------------------------------------------------#

## Grants data -----------------------------------------------------------------

# The base URL to download the SNSF datasets
datasets_base_url <- "https://data.snf.ch/public_storage/datasets"

# Read the "Grants" dataset, downloading only the required variables
grants <- readr::read_csv2(
  file.path(datasets_base_url, "Grant.csv"),
  show_col_types = FALSE,
  col_select = c(
    "GrantNumber",
    "EffectiveGrantEndDate",
    "FundingInstrumentReporting",
    "State"
  )
) |>
  # Create an `EffectiveEndYear` variable to next keep only grants which ended
  # between 2005 and 2024.
  mutate(
    EffectiveEndYear = as.integer(str_extract(EffectiveGrantEndDate, "^\\d{4}"))
  ) |>
  # Filter the "Grants" dataset to keep only completed Project funding grants
  # which ended between 2005 and 2024.
  filter(
    FundingInstrumentReporting == "Project funding",
    EffectiveGrantEndDate >= "2005-01-01",
    EffectiveGrantEndDate <= "2024-12-31",
    State == "Completed"
  ) |>
  # Remove the `State` and `FundingInstrumentReporting` variable which are not
  # needed anymore.
  select(!c(State, FundingInstrumentReporting))

## Output data -----------------------------------------------------------------

### Scientific publications ----

# Read the "OutputdataScientificPublication" dataset, downloading only the
# required variables.
output_pub <- readr::read_csv2(
  file.path(datasets_base_url, "OutputdataScientificPublication.csv"),
  show_col_types = FALSE,
  col_select = c(
    "ScientificPublicationId",
    "GrantNumber"
  )
)

### Use inspired ----

# Read the "OutputdataUseInspired" dataset, downloading only the required
# variables.
output_use_inspired <- readr::read_csv2(
  file.path(datasets_base_url, "OutputdataUseInspired.csv"),
  show_col_types = FALSE,
  col_select = c(
    "UseInspiredId",
    "GrantNumber"
  )
)

### Public communication ----

# Read the "OutputdataPublicCommunication" dataset, downloading only the
# required variables.
output_public_comm <- readr::read_csv2(
  file.path(datasets_base_url, "OutputdataPublicCommunication.csv"),
  show_col_types = FALSE,
  col_select = c(
    "PublicCommunicationId",
    "GrantNumber"
  )
)

### Datasets ----

# Read the "OutputdataDataset" dataset, downloading only the required variables
output_dataset <- readr::read_csv2(
  file.path(datasets_base_url, "OutputdataDataset.csv"),
  show_col_types = FALSE,
  col_select = c(
    "DataSetId",
    "GrantNumber"
  )
)

### Knowledge transfer event ----

# Read the "OutputdataKnowledgeTransferEvent" dataset, downloading only the
# required variables
output_knowledge_transfer <- readr::read_csv2(
  file.path(datasets_base_url, "OutputdataKnowledgeTransferEvent.csv"),
  show_col_types = FALSE,
  col_select = c(
    "KnowledgeTransferEventId",
    "GrantNumber"
  )
)


#------------------------------------------------------------------------------#
#                             ---- Process data ----
#------------------------------------------------------------------------------#

# For each of the "Output data" data frame, we will process the data to get the
# number of outputs generated for each grants listed in the `grants` data frame.
# This requires to count the number of entries for each grant number present
# in the "Output data" data frames. As grants without any output for a given
# type will not appear in the corresponding "Output data" dataset. To add them,
# we will need make a join with the `grants` data frame after counting, and
# replace the NA added from the join with 0 in the count variable.

# Output data summary for scientific publications
output_pub_summary <- output_pub |>
  # We count the number of output per grant
  count(GrantNumber) |>
  # We add the grants without any of the corresponding type of output
  right_join(
    select(grants, GrantNumber),
    by = join_by(GrantNumber)
  ) |>
  # For grants added with the join, we replace the NA in `n` with 0 we create
  # an "output_type" variable indicating the type of output.
  mutate(
    n = replace_na(n, 0),
    output_type = "Scientific publications"
  )

# Output data summary for use-inspired outputs
output_use_inspired_summary <- output_use_inspired |>
  # We count the number of output per grant
  count(GrantNumber) |>
  # We add the grants without any of the corresponding type of output
  right_join(
    select(grants, GrantNumber),
    by = join_by(GrantNumber)
  ) |>
  # For grants added with the join, we replace the NA in `n` with 0 we create
  # an "output_type" variable indicating the type of output.
  mutate(
    n = replace_na(n, 0),
    output_type = "Use-inspired outputs"
  )

# Output data summary for public communication outputs
output_public_comm_summary <- output_public_comm |>
  # We count the number of output per grant
  count(GrantNumber) |>
  # We add the grants without any of the corresponding type of output
  right_join(
    select(grants, GrantNumber),
    by = join_by(GrantNumber)
  ) |>
  # For grants added with the join, we replace the NA in `n` with 0 we create
  # an "output_type" variable indicating the type of output.
  mutate(
    n = replace_na(n, 0),
    output_type = "Public communications"
  )

# Output data summary for datasets
output_dataset_summary <- output_dataset |>
  # We count the number of output per grant
  count(GrantNumber) |>
  # We add the grants without any of the corresponding type of output
  right_join(
    select(grants, GrantNumber),
    by = join_by(GrantNumber)
  ) |>
  # For grants added with the join, we replace the NA in `n` with 0 we create
  # an "output_type" variable indicating the type of output.
  mutate(
    n = replace_na(n, 0),
    output_type = "Datasets"
  )

# Output data summary for knowledge transfer events
output_knowledge_transfer_summary <- output_knowledge_transfer |>
  # We count the number of output per grant
  count(GrantNumber) |>
  # We add the grants without any of the corresponding type of output
  right_join(
    select(grants, GrantNumber),
    by = join_by(GrantNumber)
  ) |>
  # For grants added with the join, we replace the NA in `n` with 0 we create
  # an "output_type" variable indicating the type of output.
  mutate(
    n = replace_na(n, 0),
    output_type = "Knowledge transfer events"
  )

# Now we combine all the data frames with the count for each type of output data
# into a single data frame.
all_output_summary <- bind_rows(
  output_pub_summary,
  output_use_inspired_summary,
  output_public_comm_summary,
  output_dataset_summary,
  output_knowledge_transfer_summary
)

# Finally, we can join the `all_output_summary` data frame to the `grants` data
# frame and compute, per grant end year and type of output, the share of grants
# with at least one corresponding output generated. Then, we create a
# translation of the research output types in German and French.
all_grants_with_output_summary <- grants |>
  left_join(
    all_output_summary,
    by = join_by(GrantNumber)
  ) |>
  summarise(
    prop = mean(n > 0),
    .by = c(EffectiveEndYear, output_type)
  ) |>
  mutate(
    # German translation of research output types (incl. line breaks)
    output_type_de = case_match(
      output_type,
      "Scientific publications" ~ "Wissenschaftliche\nPublikationen",
      "Use-inspired outputs" ~ "Anwendungsorientierte\nOutputs",
      "Public communications" ~ "Kommunikation mit der\nÖffentlichkeit",
      "Datasets" ~ "Datensets",
      "Knowledge transfer events" ~ "Veranstaltungen zum\nWissenstransfer"
    ),
    # French translation of research output types (incl. line breaks)
    output_type_fr = case_match(
      output_type,
      "Scientific publications" ~ "Publications scientifiques",
      "Use-inspired outputs" ~ "Résultats orientés vers\nl'application",
      "Public communications" ~ "Communication avec le\ngrand public",
      "Datasets" ~ "Sets de données",
      "Knowledge transfer events" ~
        "Manifestations de transfert\nde connaissances"
    )
  )


#------------------------------------------------------------------------------#
#                            ---- Visualization ----
#------------------------------------------------------------------------------#

# The data with the coordinates and the text for the label accompanying the
# vertical dotted lines.
label_data <- tibble(
  x = c(2011, 2018, 2020),
  y = c(1.15, 1.15, 0.65),
  # Based on the language environment (`params$lang`), the label are displayed
  # in English, German, or French.
  text = c(
    switch(
      params$lang,
      en = paste(
        "The SNSF started collecting",
        "Output data from grantees",
        "in a systematic manner.",
        sep = "\n"
      ),
      de = paste(
        "Der SNF beginnt mit der systematischen",
        "Erhebung der Outputdaten von",
        "Beitragsempfänger:innen.",
        sep = "\n"
      ),
      fr = paste(
        "Le FNS commence la collecte",
        "systématique des Données output",
        "auprès des bénéficiaires de subsides.",
        sep = "\n"
      )
    ),
    switch(
      params$lang,
      en = paste(
        "The SNSF added the Datasets",
        "category to the Output",
        "data in September 2018.",
        sep = "\n"
      ),
      de = paste(
        "Der SNF erweitert die Outputdaten",
        "September 2018 um die",
        "Kategorie «Datensets».",
        sep = "\n"
      ),
      fr = paste(
        "Le FNS ajoute la catégorie",
        "Sets de données aux Données",
        "output en septembre 2018.",
        sep = "\n"
      )
    ),
    switch(
      params$lang,
      en = paste(
        "The SNSF signed the",
        "DORA declaration.",
        sep = "\n"
      ),
      de = paste(
        "Der SNF unterzeichnet",
        "die DORA-Erklärung.",
        sep = "\n"
      ),
      fr = paste(
        "Le FNS signe la",
        "déclaration DORA.",
        sep = "\n"
      ),
    )
  )
)

# The data with the coordinates where to place the vertical dotted lines
# in the figure. These lines highlight events related to output data collection.
vline_data <- tibble(
  x = rep(c(2011, 2018, 2020), 2),
  y = c(0, 1, 0, 1, 0, 0.6)
)

# Generating the figure with the evolution of the share of completed grants with
# the different types of research output.
fig1 <- all_grants_with_output_summary |>
  mutate(
    output_type = switch(
      params$lang,
      en = output_type,
      de = output_type_de,
      fr = output_type_fr
    )
  ) |>
  ggplot() +
  aes(
    x = EffectiveEndYear,
    y = prop,
    color = output_type,
    group = output_type
  ) +
  # Vertical dotted lines highlighting events related to output data collection
  geom_line(
    data = vline_data,
    aes(x = x, y = y, group = x),
    alpha = 0.25,
    linetype = 3,
    linewidth = 0.35,
    inherit.aes = FALSE,
  ) +
  # Text describing the events related to output data collection
  geom_label(
    data = label_data,
    aes(label = text, x = x, y = y, group = x),
    size = 2.5,
    hjust = 0.5,
    fill = "white",
    color = "#909090",
    label.size = NA,
    vjust = 1,
    inherit.aes = FALSE
  ) +
  # The label indicating what type of output data each line is associated with
  geom_text(
    data = \(x) filter(x, EffectiveEndYear == 2024),
    aes(
      label = output_type,
      x = EffectiveEndYear + 0.35,
      y = dplyr::if_else(
        stringr::str_starts(output_type, "Manif|Verans"),
        prop - 0.025,
        prop
      )
    ),
    size = 3,
    hjust = 0,
    lineheight = 0.75,
    color = "black"
  ) +
  geom_point(size = 1.25) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  scale_x_continuous(
    breaks = seq(2005, 2024, 3),
    expand = expansion(add = c(0.5, 5.5))
  ) +
  scale_color_datastory() +
  get_datastory_theme(legend_position = "none")

# Whether or not saving a local copy of the figure with the title included in
# the figure.
if (save_fig) {
  ggsave(
    plot = fig1 +
      labs(
        title = 
          switch(
            params$lang,
            en = paste0(
              "Since 2011, the share of Project funding grants producing a ",
              "range of research outputs has\nbeen increasing"
            ),
            de = paste0(
              "Seit 2011 steigt der Anteil der Projekte mit verschiedenen ",
              "Arten von Forschungsoutput"
            ),
            fr = paste0(
              "Depuis 2011, la part des subsides dans l’encouragement de ",
              "projets qui génère divers types\nde productions scientifiques ",
              "est en hausse"
            ),
          )
      ),
    width = 8,
    height = 3.75,
    units = "in",
    filename = here("output", paste0("output_data_fig_", params$lang, ".png"))
  )
}