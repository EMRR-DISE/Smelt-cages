# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

remotes::install_github("EDIorg/EMLassemblyline")
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
  data.table = "",
  lat.col = "",
  lon.col = "",
  site.col = "")

# Create taxonomic coverage template (Not-required. Use this to report
# taxonomic entities in the metadata)

remotes::install_github("EDIorg/taxonomyCleanr")
library(taxonomyCleanr)

taxonomyCleanr::view_taxa_authorities()

EMLassemblyline::template_taxonomic_coverage(
  path = path_templates,
  data.path = path_data,
  taxa.table = "",
  taxa.col = "",
  taxa.name.type = "",
  taxa.authority = 3)

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.

EMLassemblyline::make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml,
  dataset.title = "",
  temporal.coverage = c("YYYY-MM-DD", "YYYY-MM-DD"),
  geographic.description = "",
  geographic.coordinates = c("N", "E", "S", "W"),
  maintenance.description = "",
  data.table = c(""),
  data.table.name = c(""),
  data.table.description = c(""),
  other.entity = c(""),
  other.entity.name = c(""),
  other.entity.description = c(""),
  user.id = "",
  user.domain = "",
  package.id = "")
