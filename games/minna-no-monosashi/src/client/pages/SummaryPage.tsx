import { useEffect, useMemo, useState } from "react";
import { Link, useParams } from "react-router-dom";
import type { PublicBlockDetail, QuestionResult } from "../../shared/contracts";
import { apiRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";
import { ResultChart } from "../components/ResultChart";
import { getDeviceIdentity } from "../deviceToken";

export function SummaryPage() {
  const { slug = "" } = useParams();
  const identity = useMemo(() => getDeviceIdentity(), []);
  const [block, setBlock] = useState<PublicBlockDetail | null>(null);
  const [results, setResults] = useState<QuestionResult[] | null>(null);
  const [incompletePosition, setIncompletePosition] = useState<number | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    apiRequest<{ block: PublicBlockDetail }>(`/api/blocks/${slug}`)
      .then(async ({ block: loaded }) => {
        setBlock(loaded);
        const progress = await apiRequest<{ answeredQuestionIds: string[] }>(`/api/blocks/${slug}/progress`, {}, identity.token);
        const missingIndex = loaded.questions.findIndex((question) => !progress.answeredQuestionIds.includes(question.id));
        if (missingIndex !== -1) {
          setIncompletePosition(missingIndex + 1);
          setResults([]);
          return;
        }
        setResults(await Promise.all(loaded.questions.map((question) =>
          apiRequest<{ result: QuestionResult }>(`/api/blocks/${slug}/questions/${question.id}/results`, {}, identity.token).then((body) => body.result),
        )));
      })
      .catch((reason: unknown) => setError(reason instanceof Error ? reason.message : "通信に失敗しました"));
  }, [identity.token, slug]);

  if (error) return <ErrorState message={error} />;
  if (!block || !results) return <LoadingState />;
  if (incompletePosition !== null) {
    return <section className="intro-card"><h1>まだ答えていない問いがあります</h1><Link className="primary-button" to={`/play/${slug}/questions/${incompletePosition}`}>回答を続ける</Link></section>;
  }

  return (
    <section className="summary-page">
      <div className="summary-hero"><p className="eyebrow">YOUR MEASURE</p><h1>{block.title}の振り返り</h1><p>あなたの答えと、みんなのものさしを並べました。</p></div>
      <div className="summary-results">
        {block.questions.map((question, index) => (
          <article className="summary-item" key={question.id}>
            <p className="card-number">{String(index + 1).padStart(2, "0")}</p>
            <h2>{question.prompt}</h2>
            {results[index] && <ResultChart result={results[index]} />}
          </article>
        ))}
      </div>
      <Link className="secondary-button" to="/">別の問いを探す</Link>
    </section>
  );
}
