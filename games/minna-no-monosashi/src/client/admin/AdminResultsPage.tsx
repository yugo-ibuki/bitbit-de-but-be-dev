import { useEffect, useState } from "react";
import { Link, useNavigate, useParams } from "react-router-dom";
import type { AdminBlock, QuestionResult } from "../../shared/contracts";
import { ApiError, adminRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";
import { ResultChart } from "../components/ResultChart";

export function AdminResultsPage() {
  const { id = "" } = useParams();
  const navigate = useNavigate();
  const [block, setBlock] = useState<AdminBlock | null>(null);
  const [results, setResults] = useState<QuestionResult[] | null>(null);
  const [participantCount, setParticipantCount] = useState(0);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([
      adminRequest<{ block: AdminBlock }>(`/blocks/${id}`),
      adminRequest<{ results: QuestionResult[]; participantCount: number }>(`/blocks/${id}/results`),
    ])
      .then(([detail, aggregate]) => { setBlock(detail.block); setResults(aggregate.results); setParticipantCount(aggregate.participantCount); })
      .catch((reason: unknown) => {
        if (reason instanceof ApiError && reason.status === 401) navigate("/admin/login");
        else setError(reason instanceof Error ? reason.message : "読み込みに失敗しました");
      });
  }, [id, navigate]);

  if (error) return <ErrorState message={error} />;
  if (!block || !results) return <LoadingState />;
  return (
    <section className="admin-page summary-page">
      <div className="admin-heading"><div><p className="eyebrow">ADMIN / RESULTS</p><h1>{block.title}の集計</h1><p>延べではなく、画面には質問ごとの匿名集計だけを表示します。</p></div><Link to={`/admin/blocks/${id}/edit`}>詳細へ戻る</Link></div>
      <p className="admin-stat"><strong>{participantCount}</strong><span>参加者数</span></p>
      <div className="summary-results">
        {block.questions.map((question, index) => <article className="summary-item" key={question.id}><p className="card-number">{String(index + 1).padStart(2, "0")}</p><h2>{question.prompt}</h2>{results[index] && <ResultChart result={results[index]} />}</article>)}
      </div>
    </section>
  );
}
