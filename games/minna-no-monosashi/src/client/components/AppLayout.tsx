import type { ReactNode } from "react";
import { Link } from "react-router-dom";

export function AppLayout({ children }: { children: ReactNode }) {
  return (
    <div className="site-shell">
      <a className="skip-link" href="#main-content">
        本文へ移動
      </a>
      <header className="site-header">
        <Link className="brand" to="/" aria-label="みんなのものさし ホーム">
          <span className="brand-mark" aria-hidden="true">?</span>
          <span>みんなのものさし</span>
        </Link>
        <Link className="admin-link" to="/admin">
          管理
        </Link>
      </header>
      <main id="main-content" className="main-content">
        {children}
      </main>
      <footer className="site-footer">
        <p>正解のない問いを、みんなの答えで眺めてみる。</p>
      </footer>
    </div>
  );
}
