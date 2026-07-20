import { useState } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { ApiError, apiRequest, storeCsrfToken } from "../api/client";

export function AdminLoginPage() {
  const navigate = useNavigate();
  const location = useLocation();
  const [password, setPassword] = useState("");
  const [pending, setPending] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const recovery = Boolean((location.state as { recovery?: boolean } | null)?.recovery);

  const submit = async (event: React.FormEvent) => {
    event.preventDefault();
    setPending(true); setError(null);
    try {
      const session = await apiRequest<{ authenticated: true; csrfToken: string }>(
        "/api/admin/login",
        { method: "POST", body: JSON.stringify({ password }) },
      );
      storeCsrfToken(session.csrfToken);
      navigate(recovery ? "/admin/blocks/new" : "/admin", { replace: true });
    } catch (reason) {
      setError(reason instanceof ApiError ? reason.message : "ログインに失敗しました");
    } finally { setPending(false); }
  };

  return (
    <section className="login-panel">
      <p className="eyebrow">ADMINISTRATION</p>
      <h1>管理者ログイン</h1>
      {recovery && <p className="notice">未保存の下書きをこのタブに一時保存しました。ログイン後に復元します。</p>}
      <form onSubmit={submit}>
        <label>管理者パスワード<input type="password" autoComplete="current-password" value={password} onChange={(event) => setPassword(event.target.value)} required /></label>
        {error && <p className="form-error" role="alert">{error}</p>}
        <button className="primary-button" disabled={pending}>{pending ? "確認中" : "ログイン"}</button>
      </form>
    </section>
  );
}
