# Atlas configuration for Google Cloud Spanner

# Environment variables (set these in your shell or .env file)
# GOOGLE_CLOUD_PROJECT - Your GCP project ID
# SPANNER_INSTANCE - Your Spanner instance name
# SPANNER_DATABASE - Your Spanner database name

# Define the environment for local development
env "dev" {
  # Source schema definition
  src = "file://schema/schema.sql"

  # Spanner connection URL
  # Format: spanner://projects/{project}/instances/{instance}/databases/{database}
  url = "spanner://projects/${var.project_id}/instances/${var.instance}/databases/${var.database}"

  # Development database URL (using Docker for schema inspection)
  dev = "docker://spanner/latest"

  # Migration directory
  migration {
    dir = "file://migrations"
  }

  # Disable transactions (required for Spanner DDL)
  tx_mode = "none"
}

# Define the environment for production
env "prod" {
  src = "file://schema/schema.sql"
  url = "spanner://projects/${var.project_id}/instances/${var.instance}/databases/${var.database}"
  dev = "docker://spanner/latest"

  migration {
    dir = "file://migrations"
  }

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
