import { useEffect, useState } from "react";
import { Link, useParams } from "react-router-dom";
import type { AdminBlock } from "../../shared/contracts";
import { adminRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";

export function AdminPreviewPage() {
  const { id = "" } = useParams();
  const [block, setBlock] = useState<AdminBlock | null>(null);
  const [error, setError] = useState<string | null>(null);
  useEffect(() => {
    adminRequest<{ block: AdminBlock }>(`/blocks/${id}`)
      .then((body) => setBlock(body.block))
      .catch((reason: unknown) => setError(reason instanceof Error ? reason.message : "読み込みに失敗しました"));
  }, [id]);
  if (error) return <ErrorState message={error} />;
  if (!block) return <LoadingState />;
  return <section className="admin-page"><div className="admin-heading"><div><p className="eyebrow">ADMIN / PREVIEW</p><h1>{block.title}</h1></div><Link to={`/admin/blocks/${id}/edit`}>編集へ戻る</Link></div><p className="notice">これは表示確認です。回答は送信されません。</p><div className="preview-phone">{block.questions.map((question, index) => <article className="preview-question" key={question.id}><p className="card-number">{String(index + 1).padStart(2, "0")}</p><h2>{question.prompt}</h2><ul>{question.options.map((option) => <li key={option.id}>{option.label}</li>)}</ul></article>)}</div></section>;
}
