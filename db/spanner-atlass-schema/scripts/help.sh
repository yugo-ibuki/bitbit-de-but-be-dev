#!/bin/bash

cat << 'EOF'
Atlas Spanner PoC - Available Commands

Setup & Configuration:
  npm run setup              Initial setup (create instance & database)
  npm run inspect            Inspect current database schema
  npm run inspect:sql        Generate SQL from DB → schema/schema-generated.sql
  npm run inspect:hcl        Generate HCL from DB → schema/schema-generated.hcl

Schema Management - SQL Version (📝):
  npm run sql:diff           Show differences (SQL schema)
  npm run sql:apply          Apply changes (SQL schema, auto-approve)
  npm run sql:apply:check    Apply changes (SQL schema, with confirmation)
  npm run sql:validate       Validate SQL schema
  npm run sql:lint           Lint SQL schema

Schema Management - HCL Version (🔷):
  npm run hcl:diff           Show differences (HCL schema)
  npm run hcl:apply          Apply changes (HCL schema, auto-approve)
  npm run hcl:apply:check    Apply changes (HCL schema, with confirmation)
  npm run hcl:validate       Validate HCL schema

Default Aliases (→ SQL version):
  npm run diff               → npm run sql:diff
  npm run apply              → npm run sql:apply
  npm run apply:check        → npm run sql:apply:check

Migration Management:
  npm run migrate:new        Generate new migration from schema changes
  npm run migrate:status     Show migration status
  npm run migrate:apply      Apply pending migrations (auto-approve)
  npm run migrate:apply:check Apply pending migrations (with confirmation)
  npm run migrate:down       Rollback last migration

Utilities:
  npm run clean              Clean generated migration files
  npm run help               Show this help message

Examples:

  # Try SQL version
  vim schema/schema.sql
  npm run sql:diff
  npm run sql:apply

  # Try HCL version
  vim schema/schema.hcl
  npm run hcl:diff
  npm run hcl:apply

  # Generate HCL from existing DB
  npm run inspect:hcl
  cat schema/schema-generated.hcl

Environment Variables Required:
  GOOGLE_CLOUD_PROJECT       Your GCP project ID
  SPANNER_INSTANCE           Spanner instance name
  SPANNER_DATABASE           Spanner database name

  Set these in .env file and run: source .env

See also:
  COMPARISON.md              Compare SQL vs HCL workflows
  SCHEMA_SOURCES.md          Schema definition methods
  WORKFLOW.md                Basic workflows
EOF
