import { useEffect, useMemo, useState } from "react";
import { Link, useNavigate, useParams } from "react-router-dom";
import type { AnswerOutcome, PublicBlockDetail } from "../../shared/contracts";
import { ApiError, apiRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";
import { QuestionForm } from "../components/QuestionForm";
import { getDeviceIdentity } from "../deviceToken";

export function QuestionPage() {
  const { slug = "", position = "1" } = useParams();
  const navigate = useNavigate();
  const identity = useMemo(() => getDeviceIdentity(), []);
  const [block, setBlock] = useState<PublicBlockDetail | null>(null);
  const [error, setError] = useState<string | null>(null);
  const questionIndex = Number(position) - 1;

  useEffect(() => {
    apiRequest<{ block: PublicBlockDetail }>(`/api/blocks/${slug}`)
      .then(({ block: loaded }) => {
        if (loaded.status === "closed") {
          navigate(`/results/${slug}`, { replace: true });
        } else {
          setBlock(loaded);
        }
      })
      .catch((reason: unknown) => setError(reason instanceof Error ? reason.message : "通信に失敗しました"));
  }, [navigate, slug]);

  if (error) return <ErrorState message={error} />;
  if (!block) return <LoadingState />;
  const question = block.questions[questionIndex];
  if (!question) {
    return <section className="async-state"><h1>この問いは見つかりません</h1><Link to={`/play/${slug}`}>最初に戻る</Link></section>;
  }

  const submit = async (optionId: string) => {
    try {
      const { result } = await apiRequest<{ result: AnswerOutcome }>(
        `/api/blocks/${slug}/questions/${question.id}/responses`,
        { method: "POST", body: JSON.stringify({ optionId }) },
        identity.token,
      );
      navigate(`/play/${slug}/questions/${position}/result`, {
        state: { result },
      });
    } catch (reason) {
      if (reason instanceof ApiError && reason.code === "ANSWER_LOCKED") {
        navigate(`/play/${slug}/questions/${position}/result`);
        return;
      }
      throw reason;
    }
  };

  return (
    <section className="question-page">
      <div className="question-progress" aria-label={`${block.questionCount}問中${questionIndex + 1}問目`}>
        <span>{String(questionIndex + 1).padStart(2, "0")}</span>
        <span className="progress-line" aria-hidden="true"><span style={{ width: `${((questionIndex + 1) / block.questionCount) * 100}%` }} /></span>
        <span>{String(block.questionCount).padStart(2, "0")}</span>
      </div>
      <p className="eyebrow">{block.title}</p>
      <h1 className="question-title">{question.prompt}</h1>
      <QuestionForm question={question} onSubmit={submit} />
    </section>
  );
}
