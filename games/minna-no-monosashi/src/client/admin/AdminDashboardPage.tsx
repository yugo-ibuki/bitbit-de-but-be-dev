import { useEffect, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import type { AdminBlockSummary } from "../../shared/contracts";
import { ApiError, adminRequest, clearCsrfToken, storeCsrfToken } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";

const statusMeta = {
  draft: { title: "下書き", empty: "編集中のブロックはありません" },
  published: { title: "公開中", empty: "公開中のブロックはありません" },
  closed: { title: "終了済み", empty: "終了したブロックはありません" },
} as const;

export function AdminDashboardPage() {
  const navigate = useNavigate();
  const [blocks, setBlocks] = useState<AdminBlockSummary[] | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    adminRequest<{ authenticated: true; csrfToken: string }>("/session")
      .then((session) => {
        storeCsrfToken(session.csrfToken);
        return adminRequest<{ blocks: AdminBlockSummary[] }>("/blocks");
      })
      .then((body) => setBlocks(body.blocks))
      .catch((reason: unknown) => {
        if (reason instanceof ApiError && reason.status === 401) navigate("/admin/login", { replace: true });
        else setError(reason instanceof Error ? reason.message : "読み込みに失敗しました");
      });
  }, [navigate]);

  const logout = async () => {
    await adminRequest("/logout", { method: "POST" });
    clearCsrfToken();
    navigate("/admin/login", { replace: true });
  };

  if (error) return <ErrorState message={error} />;
  if (!blocks) return <LoadingState />;

  return (
    <section className="admin-page">
      <div className="admin-heading">
        <div><p className="eyebrow">ADMIN / BLOCKS</p><h1>質問ブロック</h1></div>
        <div className="admin-heading-actions"><button className="text-button" onClick={() => void logout()}>ログアウト</button><Link className="primary-button" to="/admin/blocks/new">新しく作る</Link></div>
      </div>
      <div className="dashboard-columns">
        {(Object.keys(statusMeta) as Array<keyof typeof statusMeta>).map((status) => {
          const items = blocks.filter((block) => block.status === status);
          return (
            <section className="dashboard-column" key={status}>
              <div className="section-heading"><span aria-hidden="true">—</span><h2>{statusMeta[status].title}</h2><small>{items.length}</small></div>
              {items.length === 0 ? <p className="empty-copy">{statusMeta[status].empty}</p> : items.map((block) => (
                <article className="admin-block-card" key={block.id}>
                  <h3>{block.title}</h3><p className="slug-copy">/{block.slug}</p>
                  <dl className="card-meta"><div><dt>質問</dt><dd>{block.questionCount}問</dd></div><div><dt>参加</dt><dd>{block.participantCount}人</dd></div></dl>
                  <Link className="text-link" to={`/admin/blocks/${block.id}/edit`}>{status === "draft" ? "編集する" : "詳細を見る"} →</Link>
                </article>
              ))}
            </section>
          );
        })}
      </div>
    </section>
  );
}
