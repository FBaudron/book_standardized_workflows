## ----message=FALSE, warning=FALSE--------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("enaR")) install.packages("enaR")
if (!require("GGally")) install.packages("GGally")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("scales")) install.packages("scales")
if (!require("gtsummary")) install.packages("gtsummary")

library(tidyverse)
library(readxl)
library(enaR)
library(GGally)
library(ggplot2)
library(scales)
library(gtsummary)


## ----------------------------------------------------------------------------------------
farm_flows = read_xlsx("Data\\farm_ena_flows_raw.xlsx", sheet = "farm_flows") %>%
  mutate(
    farm_id = as.character(farm_id),
    source = as.character(source),
    destination = as.character(destination),
    N_kg = as.numeric(N_kg),
    DM_kg = as.numeric(DM_kg))

ena_nodes = read_xlsx("Data\\farm_ena_flows_raw.xlsx", sheet = "ena_nodes")%>%
  pull(node) %>%
  as.character()

crop_production = read_xlsx("Data\\farm_ena_flows_raw.xlsx", sheet = "crop_production") %>%
  mutate(farm_id = as.character(farm_id))


## ----------------------------------------------------------------------------------------
internal_flows = farm_flows %>%
  filter(source %in% ena_nodes, destination %in% ena_nodes)
input_flows = farm_flows %>%
  filter(source == "Input", destination %in% ena_nodes)
output_flows = farm_flows %>%
  filter(source %in% ena_nodes, destination == "Output")
dissipation_flows = farm_flows %>%
  filter(source %in% ena_nodes, destination == "Dissipation")


## ----------------------------------------------------------------------------------------
node_balance_N = bind_rows(
  input_flows %>% group_by(farm_id, node = destination) %>% summarise(input_N = sum(N_kg, na.rm = TRUE), .groups = "drop"),
  internal_flows %>% group_by(farm_id, node = destination) %>% summarise(internal_in_N = sum(N_kg, na.rm = TRUE), .groups = "drop"),
  internal_flows %>% group_by(farm_id, node = source) %>% summarise(internal_out_N = sum(N_kg, na.rm = TRUE), .groups = "drop"),
  output_flows %>% group_by(farm_id, node = source) %>% summarise(output_N = sum(N_kg, na.rm = TRUE), .groups = "drop"),
  dissipation_flows %>% group_by(farm_id, node = source) %>% summarise(dissipation_N = sum(N_kg, na.rm = TRUE), .groups = "drop")) %>%
  group_by(farm_id, node) %>%
  summarise(
    input_N = sum(input_N, na.rm = TRUE),
    internal_in_N = sum(internal_in_N, na.rm = TRUE),
    internal_out_N = sum(internal_out_N, na.rm = TRUE),
    output_N = sum(output_N, na.rm = TRUE),
    dissipation_N = sum(dissipation_N, na.rm = TRUE),
    .groups = "drop") %>%
  right_join(tidyr::expand_grid(farm_id = sort(unique(farm_flows$farm_id)), node = ena_nodes), by = c("farm_id", "node")) %>%
  mutate(
    across(c(input_N, internal_in_N, internal_out_N, output_N, dissipation_N), ~ replace_na(.x, 0)),
    balance_before_closure_N = input_N + internal_in_N - internal_out_N - output_N - dissipation_N) %>%
  arrange(farm_id, match(node, ena_nodes))


## ----------------------------------------------------------------------------------------
soil_N_input_flows = node_balance_N %>%
  filter(
    node %in% c("Cereals", "Legumes", "Garden_crops", "Other_crops"),
    balance_before_closure_N < -1e-10) %>%
  transmute(
    farm_id,
    source = "Input",
    destination = node,
    product = "soil_N",
    DM_kg = NA_real_,
    N_kg = -balance_before_closure_N,
    flow_type = "soil_N_input",
    mechanism = "unaccounted_soil_N")


## ----------------------------------------------------------------------------------------
livestock_balance_input_flows = node_balance_N %>%
  filter(
    node %in% c("Cattle", "Small_ruminants"),
    balance_before_closure_N < -1e-10) %>%
  transmute(
    farm_id,
    source = "Input",
    destination = node,
    product = "livestock_N_balance_adjustment",
    DM_kg = NA_real_,
    N_kg = -balance_before_closure_N,
    flow_type = "livestock_N_balance_adjustment",
    mechanism = "unmeasured_livestock_N_drawdown_or_missing_flow")


## ----------------------------------------------------------------------------------------
n_dissipation_flows = node_balance_N %>%
  filter(balance_before_closure_N > 1e-10) %>%
  transmute(
    farm_id,
    source = node,
    destination = "Dissipation",
    product = if_else(
      node %in% c("Cattle", "Small_ruminants"),
      "livestock_N_balance_residual",
      "unaccounted_N"),
    DM_kg = NA_real_,
    N_kg = balance_before_closure_N,
    flow_type = if_else(
      node %in% c("Cattle", "Small_ruminants"),
      "livestock_N_balance_residual",
      "N_dissipation"),
    mechanism = if_else(
      node %in% c("Cattle", "Small_ruminants"),
      "unallocated_livestock_N_possible_stock_retention_or_missing_loss",
      "positive_node_balance"))

accounting_adjustments = bind_rows(
  n_dissipation_flows,
  soil_N_input_flows,
  livestock_balance_input_flows)

farm_flows_final = bind_rows(farm_flows, accounting_adjustments) %>%
  mutate(
    farm_id = as.character(farm_id),
    N_kg = as.numeric(N_kg))


## ----------------------------------------------------------------------------------------
build_farm_network = function(farm_id_value, flows, nodes) {
  farm_data = flows %>%
    filter(farm_id == as.character(farm_id_value))
  # Internal flow matrix: rows are source compartments and columns
  # are destination compartments.
  internal_matrix = farm_data %>%
    filter(source %in% nodes, destination %in% nodes) %>%
    group_by(source, destination) %>%
    summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      source = nodes,
      destination = nodes,
      fill = list(N_kg = 0)) %>%
    tidyr::pivot_wider(
      names_from = destination,
      values_from = N_kg,
      values_fill = 0) %>%
    tibble::column_to_rownames("source") %>%
    as.matrix()

  internal_matrix = internal_matrix[nodes, nodes, drop = FALSE]

  # External N inputs: Input -> each internal compartment.
  input_vector = farm_data %>%
    filter(source == "Input", destination %in% nodes) %>%
    group_by(destination) %>%
    summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop") %>%
    right_join(tibble(node = nodes), by = c("destination" = "node")) %>%
    mutate(N_kg = replace_na(N_kg, 0)) %>%
    tibble::deframe()
  input_vector = input_vector[nodes]
  names(input_vector) = nodes

  # Exports: each internal compartment -> Output.
  export_vector = farm_data %>%
    filter(source %in% nodes, destination == "Output") %>%
    group_by(source) %>%
    summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop") %>%
    right_join(tibble(node = nodes), by = c("source" = "node")) %>%
    mutate(N_kg = replace_na(N_kg, 0)) %>%
    tibble::deframe()
  export_vector = export_vector[nodes]
  names(export_vector) = nodes

  # Dissipation: each internal compartment -> Dissipation.
  respiration_vector = farm_data %>%
    filter(source %in% nodes, destination == "Dissipation") %>%
    group_by(source) %>%
    summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop") %>%
    right_join(tibble(node = nodes), by = c("source" = "node")) %>%
    mutate(N_kg = replace_na(N_kg, 0)) %>%
    tibble::deframe()
  respiration_vector = respiration_vector[nodes]
  names(respiration_vector) = nodes

  # Construct the enaR network object.
  enaR::pack(
    flow = internal_matrix,
    input = input_vector,
    respiration = respiration_vector,
    export = export_vector)
}


## ----------------------------------------------------------------------------------------
extract_ena_stat = function(result, statistic) {
  if (is.null(result)) return(NA_real_)
  stats = if (is.list(result) && "ns" %in% names(result)) result$ns else result
  if (is.matrix(stats) || is.data.frame(stats)) {
    if (is.null(colnames(stats)) || !statistic %in% colnames(stats)) return(NA_real_)
    value = stats[1, statistic]
  } else if (!is.null(names(stats)) && statistic %in% names(stats)) {
    value = stats[[statistic]]
  } else {
    return(NA_real_)
  }
  value = suppressWarnings(as.numeric(value))
  if (length(value) != 1) NA_real_ else value
}


## ----------------------------------------------------------------------------------------
farm_ids = sort(unique(as.character(farm_flows_final$farm_id)))
indicator_rows = vector("list", length(farm_ids))

managed_flow_types = c(
  "mineral_fertilizer",
  "external_manure",
  "purchased_feed",
  "livestock_input")
other_boundary_flow_types = c(
  "communal_feed",
  "communal_manure_return",
  "biological_N_fixation")
accounting_flow_types = c(
  "soil_N_input",
  "livestock_N_balance_adjustment",
  "livestock_N_balance_residual",
  "N_dissipation")


## ----------------------------------------------------------------------------------------
farm_flows_final = farm_flows_final %>%
  mutate(
    input_class = case_when(
      flow_type %in% managed_flow_types ~ "managed_external_input",
      flow_type %in% other_boundary_flow_types ~ "other_boundary_input",
      flow_type %in% accounting_flow_types ~ "accounting_adjustment",
      TRUE ~ NA_character_),
    functional_role = case_when(
      flow_type %in% accounting_flow_types ~ "accounting_adjustment",
      flow_type %in% managed_flow_types ~ "managed_external_input",
      flow_type %in% other_boundary_flow_types ~ "other_boundary_input",
      flow_type %in% c("harvest_consumption", "harvest_sale", "harvest_gift", "livestock_sale", "livestock_output") ~ "productive_output",
      flow_type == "residue_burnt" ~ "other_exports_and_losses",
      source %in% ena_nodes & destination %in% ena_nodes ~ "internal_transfer_recycling",
      destination %in% c("Output", "Dissipation") ~ "other_exports_and_losses",
      TRUE ~ "unclassified"),
    product_group = case_when(
      flow_type %in% c("harvest_consumption", "harvest_sale", "harvest_gift") ~ source,
      flow_type == "livestock_output" ~ "Milk",
      flow_type == "livestock_sale" ~ paste0(source, "_livestock"),
      TRUE ~ NA_character_),
    production_pathway = case_when(
      flow_type == "harvest_consumption" ~ "household_consumption",
      flow_type == "harvest_sale" ~ "market_sale",
      flow_type == "harvest_gift" ~ "gift_or_barter",
      flow_type == "livestock_output" ~ "household_milk",
      flow_type == "livestock_sale" ~ "market_sale",
      TRUE ~ NA_character_))


## ----------------------------------------------------------------------------------------
tst_decomposition = farm_flows_final %>%
  filter(
    (source == "Input" & destination %in% ena_nodes) |
      (source %in% ena_nodes & destination %in% ena_nodes)) %>%
  mutate(
    tst_component = case_when(
      input_class == "managed_external_input" ~ "managed_external_inputs",
      input_class == "other_boundary_input" ~ "other_boundary_inputs",
      input_class == "accounting_adjustment" ~ "accounting_inputs",
      source %in% ena_nodes & destination %in% ena_nodes ~ "internal_flows",
      TRUE ~ "unclassified")) %>%
  group_by(farm_id, tst_component) %>%
  summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop")


## ----------------------------------------------------------------------------------------
tstp_budget = farm_flows_final %>%
  mutate(
    tstp_component = case_when(
      functional_role == "managed_external_input" ~ "1_managed_external_inputs",
      functional_role == "other_boundary_input" ~ "2_other_boundary_inputs",
      functional_role == "internal_transfer_recycling" ~ "3_internal_transfers_recycling",
      functional_role == "productive_output" ~ "4_productive_outputs",
      functional_role == "other_exports_and_losses" ~ "5_other_exports_and_losses",
      functional_role == "accounting_adjustment" ~ "6_accounting_adjustments",
      TRUE ~ "7_unclassified")) %>%
  group_by(farm_id, tstp_component) %>%
  summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop")


## ----------------------------------------------------------------------------------------
input_flow_breakdown = farm_flows_final %>%
  filter(source == "Input", destination %in% ena_nodes) %>%
  group_by(farm_id, input_class, flow_type, mechanism, destination) %>%
  summarise(
    DM_kg = sum(DM_kg, na.rm = TRUE),
    N_kg = sum(N_kg, na.rm = TRUE),
    .groups = "drop")

other_exports_losses_detail = farm_flows_final %>%
  filter(functional_role == "other_exports_and_losses") %>%
  group_by(farm_id, flow_type, mechanism, source, destination, product) %>%
  summarise(
    DM_kg = sum(DM_kg, na.rm = TRUE),
    N_kg = sum(N_kg, na.rm = TRUE),
    .groups = "drop")


## ----------------------------------------------------------------------------------------
farm_production_indicators = farm_flows_final %>%
  filter(functional_role == "productive_output") %>%
  group_by(farm_id, product_group, production_pathway, source, destination) %>%
  summarise(
    DM_kg = sum(DM_kg, na.rm = TRUE),
    N_kg = sum(N_kg, na.rm = TRUE),
    milk_L = if (any(product_group == "Milk")) {
      sum(product_quantity, na.rm = TRUE)
    } else {
      NA_real_
    },
    liveweight_sold_kg = if (any(flow_type == "livestock_sale")) {
      sum(LW_kg, na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  )


## ----------------------------------------------------------------------------------------
household_market_pathways = c(
  "household_consumption",
  "household_milk",
  "market_sale")

farm_household_market_outputs = farm_flows_final %>%
  filter(
    functional_role == "productive_output",
    production_pathway %in% household_market_pathways) %>%
  group_by(farm_id, product_group, production_pathway) %>%
  summarise(
    DM_kg = sum(DM_kg, na.rm = TRUE),
    N_kg = sum(N_kg, na.rm = TRUE),
    .groups = "drop")

farm_household_market_output_totals = farm_household_market_outputs %>%
  group_by(farm_id) %>%
  summarise(
    household_market_output_DM_kg = sum(DM_kg, na.rm = TRUE),
    household_market_output_N_kg = sum(N_kg, na.rm = TRUE),
    .groups = "drop") %>%
  right_join(
    tibble(farm_id = sort(unique(farm_flows_final$farm_id))),
    by = "farm_id") %>%
  mutate(
    across(
      c(household_market_output_DM_kg, household_market_output_N_kg),
      ~ replace_na(.x, 0)))


## ----------------------------------------------------------------------------------------
farm_crop_productivity = crop_production %>%
  group_by(farm_id, crop_group) %>%
  summarise(
    cropped_area_ha = sum(area_ha, na.rm = TRUE),
    harvest_DM_kg = sum(harvest_DM_kg, na.rm = TRUE),
    harvest_N_kg = sum(harvest_N_kg, na.rm = TRUE),
    household_DM_kg = sum(household_DM_kg, na.rm = TRUE),
    household_N_kg = sum(household_N_kg, na.rm = TRUE),
    marketed_DM_kg = sum(marketed_DM_kg, na.rm = TRUE),
    marketed_N_kg = sum(marketed_N_kg, na.rm = TRUE),
    gift_barter_DM_kg = sum(gift_barter_DM_kg, na.rm = TRUE),
    wildlife_DM_kg = sum(wildlife_DM_kg, na.rm = TRUE),
    postharvest_loss_DM_kg = sum(postharvest_loss_DM_kg, na.rm = TRUE),
    intercrop_DM_unallocated_kg = sum(intercrop_DM_unallocated_kg, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    harvest_DM_kg_ha = if_else(cropped_area_ha > 0, harvest_DM_kg / cropped_area_ha, NA_real_),
    harvest_N_kg_ha = if_else(cropped_area_ha > 0, harvest_N_kg / cropped_area_ha, NA_real_))


## ----message=FALSE, warning=FALSE--------------------------------------------------------
invisible(capture.output({

  for (i in seq_along(farm_ids)) {
    id = farm_ids[i]
    network = build_farm_network(id, farm_flows_final, ena_nodes)

    # Accounting adjustments are applied before network construction.
    # Override enaR's internal test for inactive 0-in/0-out nodes.
    flow_results = enaR::enaFlow(network, balance.override = TRUE)
    asc_results = enaR::enaAscendency(network)
    structure_results = enaR::enaStructure(network)

    tst = if (!is.null(flow_results) && "TST" %in% colnames(flow_results$ns)) {
      as.numeric(flow_results$ns[1, "TST"])
    } else {
      NA_real_
    }

    tstp = if (!is.null(flow_results) && "TSTp" %in% colnames(flow_results$ns)) {
      as.numeric(flow_results$ns[1, "TSTp"])
    } else {
      NA_real_
    }

    farm_budget = tst_decomposition %>% filter(farm_id == id)
    managed_input_N = sum(farm_budget$N_kg[farm_budget$tst_component == "managed_external_inputs"], na.rm = TRUE)
    other_boundary_input_N = sum(farm_budget$N_kg[farm_budget$tst_component == "other_boundary_inputs"], na.rm = TRUE)
    accounting_input_N = sum(farm_budget$N_kg[farm_budget$tst_component == "accounting_inputs"], na.rm = TRUE)
    internal_flow_N = sum(farm_budget$N_kg[farm_budget$tst_component == "internal_flows"], na.rm = TRUE)

    farm_tstp_budget = tstp_budget %>% filter(farm_id == id)

    # Recycling is the functional internal-transfer category only. Household
    # consumption and household milk are excluded because they are productive
    # outputs, even though these transfers remain internal links in the enaR
    # network. Market sales are also productive outputs at the farm boundary.
    internal_recycling_N = sum(
      farm_tstp_budget$N_kg[
        farm_tstp_budget$tstp_component == "3_internal_transfers_recycling"],
      na.rm = TRUE
    )

    household_market_output_N = farm_household_market_output_totals$household_market_output_N_kg[
      match(id, farm_household_market_output_totals$farm_id)
    ]

    if (length(household_market_output_N) == 0 || is.na(household_market_output_N)) {
      household_market_output_N = 0
    }

    productive_output_N = sum(farm_tstp_budget$N_kg[farm_tstp_budget$tstp_component == "4_productive_outputs"], na.rm = TRUE)
    other_exports_losses_N = sum(farm_tstp_budget$N_kg[farm_tstp_budget$tstp_component == "5_other_exports_and_losses"], na.rm = TRUE)
    accounting_adjustment_N = sum(farm_tstp_budget$N_kg[farm_tstp_budget$tstp_component == "6_accounting_adjustments"], na.rm = TRUE)

    APL = extract_ena_stat(flow_results, "APL")
    FCI = extract_ena_stat(flow_results, "FCI")
    OH_CAP = extract_ena_stat(asc_results, "OH.CAP")
    ELD = extract_ena_stat(asc_results, "ELD")
    OH_internal = extract_ena_stat(asc_results, "OH.internal")
    CAP_internal = extract_ena_stat(asc_results, "CAP.internal")
    pscc = extract_ena_stat(structure_results, "pscc")
    no_scc_big = extract_ena_stat(structure_results, "no.scc.big")
    connectance_C = extract_ena_stat(structure_results, "C")

    RDC = if (is.finite(OH_internal) && is.finite(CAP_internal) && CAP_internal > 0) {
      OH_internal / CAP_internal
    } else {
      NA_real_
    }

    indicator_rows[[i]] = tibble(
      farm_id = id,
      TST_kgN = tst,
      TSTp_kgN = tstp,
      managed_external_input_kgN = managed_input_N,
      managed_external_input_over_TST = ifelse(is.finite(tst) & tst > 0, managed_input_N / tst, NA_real_),
      other_boundary_input_kgN = other_boundary_input_N,
      other_boundary_input_over_TST = ifelse(is.finite(tst) & tst > 0, other_boundary_input_N / tst, NA_real_),
      accounting_input_kgN = accounting_input_N,
      accounting_input_over_TST = ifelse(is.finite(tst) & tst > 0, accounting_input_N / tst, NA_real_),
      # All internal network transfers, including transfers to Household that
      # are functionally classified as productive outputs.
      all_internal_flows_kgN = internal_flow_N,
      all_internal_flows_over_TST = ifelse(is.finite(tst) & tst > 0, internal_flow_N / tst, NA_real_),
      # Mutually exclusive functional indicators for the recycling/output plot.
      internal_recycling_kgN = internal_recycling_N,
      internal_recycling_over_TSTp = ifelse(is.finite(tstp) & tstp > 0, internal_recycling_N / tstp, NA_real_),
      household_market_outputs_kgN = household_market_output_N,
      household_market_outputs_over_TSTp = ifelse(is.finite(tstp) & tstp > 0, household_market_output_N / tstp, NA_real_),
      # Broader productive-output category in the five-part budget also includes
      # gifts/barter; use household_market_outputs_* for the specific plot.
      productive_outputs_kgN = productive_output_N,
      productive_outputs_over_TSTp = ifelse(is.finite(tstp) & tstp > 0, productive_output_N / tstp, NA_real_),
      other_exports_and_losses_kgN = other_exports_losses_N,
      other_exports_and_losses_over_TSTp = ifelse(is.finite(tstp) & tstp > 0, other_exports_losses_N / tstp, NA_real_),
      accounting_adjustments_kgN = accounting_adjustment_N,
      accounting_adjustments_over_TSTp = ifelse(is.finite(tstp) & tstp > 0, accounting_adjustment_N / tstp, NA_real_),
      APL = APL,
      FCI = FCI,
      pscc = pscc,
      pscc_percent = 100 * pscc,
      no_scc_big = no_scc_big,
      RDC = RDC,
      OH_CAP = OH_CAP,
      ELD = ELD,
      connectance_C = connectance_C
    )
  }

  farm_ena_indicators = bind_rows(indicator_rows) %>% arrange(farm_id)

}))


## ----------------------------------------------------------------------------------------
tst_decomposition = tst_decomposition %>%
  left_join(farm_ena_indicators %>% select(farm_id, TST_kgN), by = "farm_id") %>%
  mutate(share_of_TST = if_else(TST_kgN > 0, N_kg / TST_kgN, NA_real_))

tstp_budget = tstp_budget %>%
  left_join(farm_ena_indicators %>% select(farm_id, TSTp_kgN), by = "farm_id") %>%
  mutate(share_of_TSTp = if_else(TSTp_kgN > 0, N_kg / TSTp_kgN, NA_real_))


## ----------------------------------------------------------------------------------------
budget = tstp_budget %>%
  mutate(
    farm_id = as.character(farm_id),
    tstp_component = as.character(tstp_component),
    N_kg = as.numeric(N_kg),
    share_of_TSTp = as.numeric(share_of_TSTp))


## ----------------------------------------------------------------------------------------
components = c(
  "1_managed_external_inputs",
  "2_other_boundary_inputs",
  "3_internal_transfers_recycling",
  "4_productive_outputs",
  "5_other_exports_and_losses")

component_labels = c(
  "Managed external inputs",
  "Other boundary inputs",
  "Internal transfers & recycling",
  "Productive outputs",
  "Other exports & losses")



## ----------------------------------------------------------------------------------------
farm_metadata = read_excel(
  "Data\\farm_ena_flows_raw.xlsx",
  sheet = "ENA_to_Typology") %>%
  transmute(
    farm_id = as.character(ENA_farm_ID),
    District = stringr::str_trim(as.character(District)),
    Farm_type = stringr::str_trim(as.character(Farm_type)))

farm_metadata = farm_metadata %>%
  mutate(
    Farm_type = factor(Farm_type, levels = c("Type 1", "Type 2", "Type 3")),
    District = factor(District, levels = c("Mbire", "Murehwa")))

district_type_levels = c(
  "Mbire – Type 1", "Mbire – Type 2", "Mbire – Type 3",
  "Murehwa – Type 1", "Murehwa – Type 2", "Murehwa – Type 3")

farm_metadata = farm_metadata %>%
  mutate(
    District_Type = factor(
      paste(as.character(District), as.character(Farm_type), sep = " – "),
      levels = district_type_levels))


## ----------------------------------------------------------------------------------------
absolute_long = budget %>%
  filter(tstp_component %in% components) %>%
  group_by(farm_id, tstp_component) %>%
  summarise(N_kg = sum(N_kg, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(
    farm_id = farm_ids,
    tstp_component = components,
    fill = list(N_kg = 0))

data_absolute = absolute_long %>%
  pivot_wider(
    names_from = tstp_component,
    values_from = N_kg,
    values_fill = 0) %>%
  select(farm_id, all_of(components)) %>%
  rename_with(
    ~ component_labels,
    all_of(components)) %>%
  left_join(farm_metadata, by = "farm_id") %>%
  select(farm_id, District, Farm_type, District_Type, all_of(component_labels))


## ----------------------------------------------------------------------------------------
relative_long = budget %>%
  filter(tstp_component %in% components) %>%
  group_by(farm_id, tstp_component) %>%
  summarise(
    share_of_TSTp = sum(share_of_TSTp, na.rm = TRUE),
    .groups = "drop") %>%
  tidyr::complete(
    farm_id = farm_ids,
    tstp_component = components,
    fill = list(share_of_TSTp = 0))

data_percent = relative_long %>%
  pivot_wider(
    names_from = tstp_component,
    values_from = share_of_TSTp,
    values_fill = 0) %>%
  select(farm_id, all_of(components)) %>%
  rename_with(
    ~ component_labels,
    all_of(components)) %>%
  left_join(farm_metadata, by = "farm_id") %>%
  select(farm_id, District, Farm_type, District_Type, all_of(component_labels))


## ----------------------------------------------------------------------------------------
ena_summary_data = farm_ena_indicators %>%
  left_join(farm_metadata %>% select(farm_id, District_Type), by = "farm_id") %>%
  select(
    District_Type, TST_kgN, TSTp_kgN,
    managed_external_input_kgN, managed_external_input_over_TST,
    other_boundary_input_kgN, other_boundary_input_over_TST,
    accounting_input_kgN, accounting_input_over_TST,
    all_internal_flows_kgN, all_internal_flows_over_TST,
    internal_recycling_kgN, internal_recycling_over_TSTp,
    household_market_outputs_kgN, household_market_outputs_over_TSTp,
    productive_outputs_kgN, productive_outputs_over_TSTp,
    other_exports_and_losses_kgN, other_exports_and_losses_over_TSTp,
    accounting_adjustments_kgN, accounting_adjustments_over_TSTp,
    APL, FCI, pscc_percent, no_scc_big, RDC, OH_CAP, ELD, connectance_C)

gtsummary::tbl_summary(
  ena_summary_data,
  by = District_Type,
  include = -District_Type,
  type = list(
  pscc_percent ~ "continuous",
  all_continuous() ~ "continuous"),
  statistic = all_continuous() ~ "{median} ({p25}, {p75})",
  digits = all_continuous() ~ 2,
  missing = "no",
  label = list(
    TST_kgN ~ "Total system throughput (kg N farm⁻¹ yr⁻¹)",
    TSTp_kgN ~ "Total system throughput, TSTp (kg N farm⁻¹ yr⁻¹)",
    managed_external_input_kgN ~ "Managed external inputs (kg N farm⁻¹ yr⁻¹)",
    managed_external_input_over_TST ~ "Managed input dependency (proportion of TST)",
    other_boundary_input_kgN ~ "Other boundary inputs (kg N farm⁻¹ yr⁻¹)",
    other_boundary_input_over_TST ~ "Other boundary input contribution (proportion of TST)",
    accounting_input_kgN ~ "Accounting inputs (kg N farm⁻¹ yr⁻¹)",
    accounting_input_over_TST ~ "Accounting inputs (proportion of TST)",
    all_internal_flows_kgN ~ "All internal flows (kg N farm⁻¹ yr⁻¹)",
    all_internal_flows_over_TST ~ "All internal flows (proportion of TST)",
    internal_recycling_kgN ~ "Functional internal transfers/recycling (kg N farm⁻¹ yr⁻¹)",
    internal_recycling_over_TSTp ~ "Functional internal transfers/recycling (proportion of TSTp)",
    household_market_outputs_kgN ~ "Household and market outputs (kg N farm⁻¹ yr⁻¹)",
    household_market_outputs_over_TSTp ~ "Household and market outputs (proportion of TSTp)",
    productive_outputs_kgN ~ "All productive outputs, including gifts/barter (kg N farm⁻¹ yr⁻¹)",
    productive_outputs_over_TSTp ~ "All productive outputs (proportion of TSTp)",
    other_exports_and_losses_kgN ~ "Other exports and losses (kg N farm⁻¹ yr⁻¹)",
    other_exports_and_losses_over_TSTp ~ "Other exports and losses (proportion of TSTp)",
    accounting_adjustments_kgN ~ "Accounting adjustments (kg N farm⁻¹ yr⁻¹)",
    accounting_adjustments_over_TSTp ~ "Accounting adjustments (proportion of TSTp)",
    APL ~ "Average path length (APL)",
    FCI ~ "Finn cycling index (FCI)",
    pscc_percent ~ "Nodes in non-trivial strongly connected components (%)",
    no_scc_big ~ "Number of non-trivial strongly connected components",
    RDC ~ "Relative internal redundancy (RDC)",
    OH_CAP ~ "Overhead / capacity (OH.CAP)",
    ELD ~ "Effective link density (ELD)",
    connectance_C ~ "Network connectance (C)"))   %>%
  gtsummary::add_overall(last = TRUE) %>%
  gtsummary::modify_header(label = "**ENA indicator**") %>%
  gtsummary::modify_caption(
    "**Farm-level ENA indicators by district × farm type**"
  ) %>%
  gtsummary::bold_labels() 
  


## ----ena-output-11, message=FALSE, warning=FALSE-----------------------------------------
district_type_colors = c(
  "Mbire – Type 1"   = "#c7e9c0",
  "Mbire – Type 2"   = "#74c476",
  "Mbire – Type 3"   = "#238b45",
  "Murehwa – Type 1" = "#fdd0a2",
  "Murehwa – Type 2" = "#fd8d3c",
  "Murehwa – Type 3" = "#d94701")

# Diagonal: distributions by District x Farm type
diag_density_by_district_type = function(data, mapping, ...) {
  x_values = rlang::eval_tidy(mapping$x, data = data)
  density_data = tibble(
    plot_value = x_values,
    District_Type = data$District_Type)
  ggplot(
    density_data,
    aes(
      x = plot_value,
      colour = District_Type,
      group = District_Type)) +
    geom_density(
      linewidth = 0.8,
      na.rm = TRUE) +
    scale_colour_manual(
      values = district_type_colors,
      limits = district_type_levels,
      drop = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none")
}

# Lower triangle: scatterplots
lower_scatter_by_district_type = function(data, mapping, ...) {
  x_name = rlang::as_name(rlang::get_expr(mapping$x))
  y_name = rlang::as_name(rlang::get_expr(mapping$y))
  ggplot(
    data = data,
    aes(
      x = .data[[x_name]],
      y = .data[[y_name]])) +
    geom_point(
      aes(colour = District_Type),
      shape = 16,
      size = 3.0,
      alpha = 0.75,
      na.rm = TRUE) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      colour = "black",
      linewidth = 0.6,
      na.rm = TRUE) +
    scale_colour_manual(
      values = district_type_colors,
      limits = district_type_levels,
      drop = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none")
}

# Upper triangle: pooled Spearman correlation

upper_spearman = function(data, mapping, ...) {
  x_values = rlang::eval_tidy(mapping$x, data = data)
  y_values = rlang::eval_tidy(mapping$y, data = data)
  keep = stats::complete.cases(x_values, y_values)
  n = sum(keep)
  if (
    n >= 3 &&
    stats::sd(x_values[keep]) > 0 &&
    stats::sd(y_values[keep]) > 0) {
    test = suppressWarnings(
      stats::cor.test(
        x_values[keep],
        y_values[keep],
        method = "spearman",
        exact = FALSE))
    rho = unname(test$estimate)
    p_value = test$p.value
    significance = if (p_value < 0.001) {"***"} 
    else if (p_value < 0.01) {"**"} 
    else if (p_value < 0.05) {"*"} 
    else {""}
    label = sprintf(
      "Spearman\n\u03C1 = %.2f%s\nn = %d",
      rho, significance, n)} else {
    label = sprintf(
      "Spearman\nnot estimable\nn = %d",
      n)}
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = label,
      size = 4.0) +
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0, 1)) +
    theme_void()
}


legend_data = farm_metadata %>%
  distinct(District_Type) %>%
  mutate(legend_y = 1)

legend_plot = ggplot(
  legend_data,
  aes(x = District_Type, y = legend_y, colour = District_Type)) +
  geom_point(shape = 16, size = 4) +
  scale_colour_manual(
    values = district_type_colors,
    limits = district_type_levels,
    breaks = district_type_levels,
    drop = FALSE,
    name = "District × Farm type") +
  theme_void() +
  theme(legend.position = "right")

explicit_legend = GGally::grab_legend(legend_plot)



## ----message=FALSE, warning=FALSE, fig.width=16, fig.height=14, out.width="100%", dpi=150----
p_percent = GGally::ggpairs(
  data_percent,
  columns = 5:9,
  mapping = ggplot2::aes(colour = District_Type),
  diag = list(continuous = diag_density_by_district_type),
  lower = list(continuous = lower_scatter_by_district_type),
  upper = list(continuous = upper_spearman),
  columnLabels = component_labels,
  legend = explicit_legend,
  progress = FALSE) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8.5),
    axis.title = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right")

p_percent


## ----message=FALSE, warning=FALSE, fig.width=16, fig.height=14, out.width="100%", dpi=150----
p_absolute = GGally::ggpairs(
  data_absolute,
  columns = 5:9,
  mapping = ggplot2::aes(colour = District_Type),
  diag = list(continuous = diag_density_by_district_type),
  lower = list(continuous = lower_scatter_by_district_type),
  upper = list(continuous = upper_spearman),
  columnLabels = component_labels,
  legend = explicit_legend,
  progress = FALSE) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8.5),
    axis.title = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right")

p_absolute

