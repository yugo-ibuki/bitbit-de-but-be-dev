import { useEffect, useState } from "react";
import { Link, useParams } from "react-router-dom";
import type { PublicBlockDetail, QuestionResult } from "../../shared/contracts";
import { apiRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";
import { ResultChart } from "../components/ResultChart";

export function ClosedResultsPage() {
  const { slug = "" } = useParams();
  const [block, setBlock] = useState<PublicBlockDetail | null>(null);
  const [results, setResults] = useState<QuestionResult[] | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([
      apiRequest<{ block: PublicBlockDetail }>(`/api/blocks/${slug}`),
      apiRequest<{ results: QuestionResult[] }>(`/api/blocks/${slug}/results`),
    ])
      .then(([detail, aggregate]) => { setBlock(detail.block); setResults(aggregate.results); })
      .catch((reason: unknown) => setError(reason instanceof Error ? reason.message : "通信に失敗しました"));
  }, [slug]);

  if (error) return <ErrorState message={error} />;
  if (!block || !results) return <LoadingState />;

  return (
    <section className="summary-page">
      <div className="summary-hero"><p className="eyebrow">CLOSED EXHIBITION</p><h1>{block.title}</h1><p>{block.participantCount}人が残した、全体のものさしです。</p></div>
      <div className="summary-results">
        {block.questions.map((question, index) => (
          <article className="summary-item" key={question.id}>
            <p className="card-number">{String(index + 1).padStart(2, "0")}</p>
            <h2>{question.prompt}</h2>
            {results[index] && <ResultChart result={results[index]} />}
          </article>
        ))}
      </div>
      <Link className="secondary-button" to="/">問いの一覧へ戻る</Link>
    </section>
  );
}
