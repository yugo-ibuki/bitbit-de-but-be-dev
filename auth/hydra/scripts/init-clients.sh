#!/bin/bash

# Wait for Hydra to be ready
echo "Waiting for Hydra to be ready..."
until curl -s http://localhost:4445/health/ready > /dev/null 2>&1; do
  sleep 2
done
echo "Hydra is ready!"

# Create a demo OAuth2 client
echo "Creating demo OAuth2 client..."

curl -X POST http://localhost:4445/admin/clients \
  -H "Content-Type: application/json" \
  -d '{
    "client_id": "demo-client",
    "client_secret": "demo-secret",
    "grant_types": ["authorization_code", "refresh_token"],
    "response_types": ["code"],
    "scope": "openid profile email offline_access",
    "redirect_uris": ["http://127.0.0.1:9999/callback"],
    "token_endpoint_auth_method": "client_secret_post"
  }'

echo ""
echo "Demo client created!"
echo ""
echo "Client ID: demo-client"
echo "Client Secret: demo-secret"
echo "Redirect URI: http://127.0.0.1:9999/callback"
echo ""
echo "Test the authorization flow:"
echo "http://localhost:4444/oauth2/auth?client_id=demo-client&response_type=code&scope=openid%20profile%20email&redirect_uri=http://127.0.0.1:9999/callback&state=random-state"
