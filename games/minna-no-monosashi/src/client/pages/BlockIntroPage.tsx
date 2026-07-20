import { useEffect, useMemo, useState } from "react";
import { Link, useNavigate, useParams } from "react-router-dom";
import type { PublicBlockDetail } from "../../shared/contracts";
import { apiRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";
import { getDeviceIdentity } from "../deviceToken";

export function BlockIntroPage() {
  const { slug = "" } = useParams();
  const navigate = useNavigate();
  const identity = useMemo(() => getDeviceIdentity(), []);
  const [block, setBlock] = useState<PublicBlockDetail | null>(null);
  const [answered, setAnswered] = useState<string[]>([]);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    Promise.all([
      apiRequest<{ block: PublicBlockDetail }>(`/api/blocks/${slug}`),
      apiRequest<{ answeredQuestionIds: string[] }>(`/api/blocks/${slug}/progress`, {}, identity.token),
    ])
      .then(([detail, progress]) => {
        setBlock(detail.block);
        setAnswered(progress.answeredQuestionIds);
      })
      .catch((reason: unknown) => setError(reason instanceof Error ? reason.message : "通信に失敗しました"));
  }, [identity.token, slug]);

  if (error) return <ErrorState message={error} />;
  if (!block) return <LoadingState />;
  if (block.status === "closed") {
    return <section className="intro-card"><p className="eyebrow">CLOSED</p><h1>{block.title}</h1><p>回答受付は終了しました。みんなの結果をご覧いただけます。</p><Link className="primary-button" to={`/results/${slug}`}>全体結果を見る</Link></section>;
  }

  const nextIndex = block.questions.findIndex((question) => !answered.includes(question.id));
  const completed = nextIndex === -1;
  const start = () => navigate(completed ? `/play/${slug}/summary` : `/play/${slug}/questions/${nextIndex + 1}`);

  return (
    <section className="intro-card">
      <p className="eyebrow">QUESTION COLLECTION</p>
      <h1>{block.title}</h1>
      <p className="intro-description">{block.description}</p>
      <dl className="intro-meta">
        <div><dt>問いの数</dt><dd>{block.questionCount}問</dd></div>
        <div><dt>これまでの参加</dt><dd>{block.participantCount}人</dd></div>
      </dl>
      {!identity.persistent && <p className="notice" role="note">このブラウザでは保存機能が使えないため、再読み込みすると回答状況を復元できません。</p>}
      <p className="privacy-note">回答はこのブラウザごとに1問1回です。氏名などの個人情報は収集しません。</p>
      <button className="primary-button" onClick={start}>
        {completed ? "振り返りを見る" : answered.length > 0 ? "続きから" : "はじめる"}
      </button>
    </section>
  );
}
