# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

#remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Define paths for your metadata templates, data, and EML

path_templates <- "smelt_2019_winterspring/edi/metadata_templates"
path_data <- "smelt_2019_winterspring/edi/data_objects"
path_eml <- "smelt_2019_winterspring/edi/eml"

# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the
# functions and arguments you don't need AND ... don't forget to read the docs!
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)

EMLassemblyline::template_core_metadata(
  path = path_templates,
  license = "CCBY",
  file.type = ".md")

# Create table attributes template (required when data tables are present)

EMLassemblyline::template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = c("2019_smeltstudy_diet_data.csv",
                 "2019_smeltstudy_growth-cf_data.csv",
                 "2019_smeltstudy_survival_data.csv",
                 "2019_smeltstudy_zoop_data.csv",
                 "2019_smeltstudy_cagevelocity_data.csv",
                 "2019_smeltstudy_deploy_retrieve_data.csv",
                 "2019_smeltstudy_dailycheck_data.csv",
                 "2019_smeltstudy_stations.csv"))
# Look at standard units in the package. Need to create custom units if they aren't in here.
# Run view_unit_dictionary() to look at the units EML has standard. See custom units example for filling out custom units.

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)

EMLassemblyline::template_categorical_variables(
  path = path_templates,
  data.path = path_data)

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

EMLassemblyline::template_geographic_coverage(
  path = path_templates,
  data.path = path_data,
  data.table = "2019_smeltstudy_stations.csv",
  lat.col = "Latitude",
  lon.col = "Longitude",
  site.col = "StationCode")

# Create taxonomic coverage template (Not-required. Use this to report
# taxonomic entities in the metadata)

# no taxonomy this time -----------------------------------
# remotes::install_github("EDIorg/taxonomyCleanr")
# library(taxonomyCleanr)

# taxonomyCleanr::view_taxa_authorities()

# EMLassemblyline::template_taxonomic_coverage(
#   path = path_templates,
#   data.path = path_data,
#   taxa.table = "",
#   taxa.col = "",
#   taxa.name.type = "",
#   taxa.authority = 3)

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.Removed the geographic options because using the geographic template above.

EMLassemblyline::make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml,
  dataset.title = "Environmental and biological data associated with captive-reared Delta Smelt Study, Sacramento-San Joaquin Delta, CA, January-March 2019",
  temporal.coverage = c("2019-01-23", "2019-03-27"),
  #geographic,coordinates = c()
  #geographic.description = "Rio Vista and Deepwater Shipping Channel, Sacramento-San Joaquin Delta",
  maintenance.description = "not updated, associated with completed study",
  data.table = c("2019_smeltstudy_diet_data.csv",
                 "2019_smeltstudy_growth-cf_data.csv",
                 "2019_smeltstudy_survival_data.csv",
                 "2019_smeltstudy_zoop_data.csv",
                 "2019_smeltstudy_cagevelocity_data.csv",
                 "2019_smeltstudy_deploy_retrieve_data.csv",
                 "2019_smeltstudy_dailycheck_data.csv",
                 "2019_smeltstudy_stations.csv"),
  data.table.name = c("Delta Smelt Diet", "Delta Smelt Growth and Condition Factor", "Delta Smelt Survival", "Zooplankton Data",
                      "Velocity Data", "Deployment and Retrieval Information", "Field Data", "Stations"),
  data.table.description = c("Delta Smelt Diet Composition", "Delta Smelt Growth and Condition Factor", "Delta Smelt Survival", "Zooplankton Data",
                             "Velocity Data", "Information associated with deployment and retrieval of fish", "Field data associated with daily checks", "Station locations"),
  data.table.quote.character = c('"', '"','"','"', '"','"','"', '"'), # If you have columns that have commas in the text, you will need to use "quote = TRUE" when you write your R file (write.csv), and then use this to tell make_eml what is going around your character cells. c(apostrophe, quote, apostrophe, comma, etc...)
  other.entity = c("metadata_2019_smeltstudy_winterspring.pdf"),
  other.entity.name = c("Metadata for Smelt"),
  other.entity.description = c("Metadata for Smelt Study"),
  user.id = "aquaticecology",
  user.domain = "EDI",
  package.id = "edi.1248.2")
