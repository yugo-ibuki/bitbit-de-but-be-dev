import { useEffect, useMemo, useState } from "react";
import { useLocation, useNavigate, useParams } from "react-router-dom";
import type { PublicBlockDetail, QuestionResult } from "../../shared/contracts";
import { apiRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";
import { ResultChart } from "../components/ResultChart";
import { getDeviceIdentity } from "../deviceToken";

export function QuestionResultPage() {
  const { slug = "", position = "1" } = useParams();
  const location = useLocation();
  const navigate = useNavigate();
  const identity = useMemo(() => getDeviceIdentity(), []);
  const initialResult = (location.state as { result?: QuestionResult } | null)?.result ?? null;
  const [block, setBlock] = useState<PublicBlockDetail | null>(null);
  const [result, setResult] = useState<QuestionResult | null>(initialResult);
  const [answered, setAnswered] = useState<string[]>([]);
  const [error, setError] = useState<string | null>(null);
  const questionIndex = Number(position) - 1;

  useEffect(() => {
    apiRequest<{ block: PublicBlockDetail }>(`/api/blocks/${slug}`)
      .then(async ({ block: loaded }) => {
        setBlock(loaded);
        const question = loaded.questions[questionIndex];
        if (!question) throw new Error("質問が見つかりません");
        const requests: Promise<unknown>[] = [
          apiRequest<{ answeredQuestionIds: string[] }>(`/api/blocks/${slug}/progress`, {}, identity.token).then((body) => setAnswered(body.answeredQuestionIds)),
        ];
        if (!initialResult) {
          requests.push(apiRequest<{ result: QuestionResult }>(`/api/blocks/${slug}/questions/${question.id}/results`, {}, identity.token).then((body) => setResult(body.result)));
        }
        await Promise.all(requests);
      })
      .catch((reason: unknown) => setError(reason instanceof Error ? reason.message : "通信に失敗しました"));
  }, [identity.token, initialResult, questionIndex, slug]);

  if (error) return <ErrorState message={error} />;
  if (!block || !result) return <LoadingState label="結果を集計しています" />;
  const question = block.questions[questionIndex];
  if (!question) return <ErrorState message="質問が見つかりません" />;

  const nextIndex = block.questions.findIndex((candidate) => !answered.includes(candidate.id));
  const next = () => navigate(nextIndex === -1 ? `/play/${slug}/summary` : `/play/${slug}/questions/${nextIndex + 1}`);

  return (
    <section className="result-page">
      <p className="eyebrow">みんなの答え</p>
      <h1 className="result-question">{question.prompt}</h1>
      <ResultChart result={result} />
      <button className="primary-button" onClick={next}>
        {nextIndex === -1 ? "すべての結果を振り返る" : "次の問いへ"}
      </button>
    </section>
  );
}
