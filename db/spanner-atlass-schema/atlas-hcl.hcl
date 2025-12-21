# Atlas configuration for HCL schema (alternative to atlas.hcl)

# Define the environment for local development using HCL schema
env "dev-hcl" {
  # Source schema definition in HCL format
  src = "file://schema/schema.hcl"

  # Spanner connection URL
  url = "spanner://projects/${var.project_id}/instances/${var.instance}/databases/${var.database}"

  # Development database URL (using Docker for schema inspection)
  dev = "docker://spanner/latest"

  # Migration directory
  migration {
    dir = "file://migrations-hcl"
  }

  # Disable transactions (required for Spanner DDL)
  tx_mode = "none"
}

# Variables
variable "project_id" {
  type    = string
  default = getenv("GOOGLE_CLOUD_PROJECT")
}

variable "instance" {
  type    = string
  default = getenv("SPANNER_INSTANCE")
}

variable "database" {
  type    = string
  default = getenv("SPANNER_DATABASE")
}
