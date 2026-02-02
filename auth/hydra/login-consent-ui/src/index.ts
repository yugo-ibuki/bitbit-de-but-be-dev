import express, { Request, Response } from 'express';
import bodyParser from 'body-parser';
import path from 'path';

// Hydra API response types
interface HydraClient {
  client_id?: string;
  client_name?: string;
}

interface LoginRequest {
  skip: boolean;
  subject: string;
  client: HydraClient;
}

interface ConsentRequest {
  skip: boolean;
  subject: string;
  client: HydraClient;
  requested_scope: string[];
  requested_access_token_audience: string[];
}

interface RedirectResponse {
  redirect_to: string;
}

const app = express();
const PORT = process.env.PORT || 3000;
const HYDRA_ADMIN_URL = process.env.HYDRA_ADMIN_URL || 'http://localhost:4445';

// Demo users (for development only)
const DEMO_USERS: Record<string, { password: string; name: string }> = {
  'user@example.com': { password: 'password', name: 'Demo User' },
};

app.set('view engine', 'ejs');
app.set('views', path.join(__dirname, '../views'));
app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());

// Health check
app.get('/health', (_req: Request, res: Response) => {
  res.json({ status: 'ok' });
});

// Login page
app.get('/login', async (req: Request, res: Response) => {
  const challenge = req.query.login_challenge as string;

  if (!challenge) {
    return res.status(400).send('Missing login_challenge');
  }

  try {
    // Get login request from Hydra
    const response = await fetch(
      `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/login?login_challenge=${challenge}`
    );
    const loginRequest = (await response.json()) as LoginRequest;

    // If user is already authenticated, skip login
    if (loginRequest.skip) {
      const acceptResponse = await fetch(
        `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/login/accept?login_challenge=${challenge}`,
        {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ subject: loginRequest.subject }),
        }
      );
      const acceptResult = (await acceptResponse.json()) as RedirectResponse;
      return res.redirect(acceptResult.redirect_to);
    }

    res.render('login', {
      challenge,
      error: null,
      client: loginRequest.client,
    });
  } catch (error) {
    console.error('Login error:', error);
    res.status(500).send('Internal server error');
  }
});

// Login form submission
app.post('/login', async (req: Request, res: Response) => {
  const { email, password, challenge } = req.body;

  // Validate demo user
  const user = DEMO_USERS[email];
  if (!user || user.password !== password) {
    return res.render('login', {
      challenge,
      error: 'Invalid email or password',
      client: null,
    });
  }

  try {
    // Accept login request
    const response = await fetch(
      `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/login/accept?login_challenge=${challenge}`,
      {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          subject: email,
          remember: true,
          remember_for: 3600,
        }),
      }
    );
    const result = (await response.json()) as RedirectResponse;
    res.redirect(result.redirect_to);
  } catch (error) {
    console.error('Login accept error:', error);
    res.status(500).send('Internal server error');
  }
});

// Consent page
app.get('/consent', async (req: Request, res: Response) => {
  const challenge = req.query.consent_challenge as string;

  if (!challenge) {
    return res.status(400).send('Missing consent_challenge');
  }

  try {
    // Get consent request from Hydra
    const response = await fetch(
      `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/consent?consent_challenge=${challenge}`
    );
    const consentRequest = (await response.json()) as ConsentRequest;

    // If consent was already given, skip
    if (consentRequest.skip) {
      const acceptResponse = await fetch(
        `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/consent/accept?consent_challenge=${challenge}`,
        {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            grant_scope: consentRequest.requested_scope,
            grant_access_token_audience: consentRequest.requested_access_token_audience,
          }),
        }
      );
      const acceptResult = (await acceptResponse.json()) as RedirectResponse;
      return res.redirect(acceptResult.redirect_to);
    }

    res.render('consent', {
      challenge,
      client: consentRequest.client,
      scopes: consentRequest.requested_scope,
      subject: consentRequest.subject,
    });
  } catch (error) {
    console.error('Consent error:', error);
    res.status(500).send('Internal server error');
  }
});

// Consent form submission
app.post('/consent', async (req: Request, res: Response) => {
  const { challenge, accept } = req.body;
  let scopes = req.body.scopes;

  // Ensure scopes is an array
  if (typeof scopes === 'string') {
    scopes = [scopes];
  }

  try {
    if (accept === 'deny') {
      // Reject consent
      const response = await fetch(
        `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/consent/reject?consent_challenge=${challenge}`,
        {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            error: 'access_denied',
            error_description: 'User denied access',
          }),
        }
      );
      const result = (await response.json()) as RedirectResponse;
      return res.redirect(result.redirect_to);
    }

    // Get consent request to get audience
    const consentResponse = await fetch(
      `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/consent?consent_challenge=${challenge}`
    );
    const consentRequest = (await consentResponse.json()) as ConsentRequest;

    // Accept consent
    const response = await fetch(
      `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/consent/accept?consent_challenge=${challenge}`,
      {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          grant_scope: scopes || [],
          grant_access_token_audience: consentRequest.requested_access_token_audience,
          remember: true,
          remember_for: 3600,
          session: {
            id_token: {
              email: consentRequest.subject,
              name: DEMO_USERS[consentRequest.subject]?.name || consentRequest.subject,
            },
          },
        }),
      }
    );
    const result = (await response.json()) as RedirectResponse;
    res.redirect(result.redirect_to);
  } catch (error) {
    console.error('Consent accept error:', error);
    res.status(500).send('Internal server error');
  }
});

// Logout page
app.get('/logout', async (req: Request, res: Response) => {
  const challenge = req.query.logout_challenge as string;

  if (!challenge) {
    return res.status(400).send('Missing logout_challenge');
  }

  try {
    // Accept logout request
    const response = await fetch(
      `${HYDRA_ADMIN_URL}/admin/oauth2/auth/requests/logout/accept?logout_challenge=${challenge}`,
      {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
      }
    );
    const result = (await response.json()) as RedirectResponse;
    res.redirect(result.redirect_to);
  } catch (error) {
    console.error('Logout error:', error);
    res.status(500).send('Internal server error');
  }
});

app.listen(PORT, () => {
  console.log(`Login/Consent UI running on port ${PORT}`);
  console.log(`Hydra Admin URL: ${HYDRA_ADMIN_URL}`);
});
