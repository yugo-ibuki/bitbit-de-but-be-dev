#!/bin/bash
set -e

echo "Atlas Spanner PoC Setup"
echo

# Check required environment variables
: "${GOOGLE_CLOUD_PROJECT:?Error: GOOGLE_CLOUD_PROJECT not set}"
: "${SPANNER_INSTANCE:?Error: SPANNER_INSTANCE not set}"
: "${SPANNER_DATABASE:?Error: SPANNER_DATABASE not set}"

echo "Project: $GOOGLE_CLOUD_PROJECT"
echo "Instance: $SPANNER_INSTANCE"
echo "Database: $SPANNER_DATABASE"
echo

# Install Atlas if not present
if ! command -v atlas &> /dev/null; then
    echo "Installing Atlas CLI (beta)..."
    curl -sSf https://atlasgo.sh | ATLAS_VERSION="beta" sh
fi

# Check gcloud
command -v gcloud &> /dev/null || { echo "Error: gcloud CLI not installed"; exit 1; }

# Authenticate if needed
gcloud auth list --filter=status:ACTIVE --format="value(account)" &> /dev/null || gcloud auth login

# Set project
gcloud config set project $GOOGLE_CLOUD_PROJECT

# Create instance if needed
if ! gcloud spanner instances describe $SPANNER_INSTANCE &> /dev/null; then
    echo "Creating Spanner instance..."
    gcloud spanner instances create $SPANNER_INSTANCE \
        --config=regional-us-central1 \
        --description="Atlas PoC Instance" \
        --nodes=1
fi

# Create database if needed
if ! gcloud spanner databases describe $SPANNER_DATABASE --instance=$SPANNER_INSTANCE &> /dev/null; then
    echo "Creating database..."
    gcloud spanner databases create $SPANNER_DATABASE --instance=$SPANNER_INSTANCE
fi

echo
echo "Setup complete! Next steps:"
echo "  npm run diff     - Show schema differences"
echo "  npm run apply    - Apply schema changes"
echo "  npm run help     - Show all commands"
