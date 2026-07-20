import { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import type { PublicBlockSummary } from "../../shared/contracts";
import { apiRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";

export function HomePage() {
  const [blocks, setBlocks] = useState<PublicBlockSummary[] | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [attempt, setAttempt] = useState(0);

  useEffect(() => {
    let active = true;
    setError(null);
    apiRequest<{ blocks: PublicBlockSummary[] }>("/api/blocks")
      .then((body) => active && setBlocks(body.blocks))
      .catch((reason: unknown) => {
        if (active) setError(reason instanceof Error ? reason.message : "通信に失敗しました");
      });
    return () => { active = false; };
  }, [attempt]);

  if (error) return <ErrorState message={error} onRetry={() => setAttempt((value) => value + 1)} />;
  if (!blocks) return <LoadingState />;

  const published = blocks.filter((block) => block.status === "published");
  const closed = blocks.filter((block) => block.status === "closed");

  return (
    <div className="page-stack home-page">
      <section className="hero">
        <p className="eyebrow">YOUR EVERYDAY, MEASURED TOGETHER</p>
        <h1>みんなのものさし</h1>
        <p className="hero-copy">
          「普通」だと思っていた境界線を、名前のない誰かの答えと見比べる小さな展示です。
        </p>
      </section>
      <BlockSection title="開催中の問い" blocks={published} empty="現在、回答できる問いはありません。" />
      {closed.length > 0 && <BlockSection title="これまでの問い" blocks={closed} />}
    </div>
  );
}

function BlockSection({
  title,
  blocks,
  empty,
}: {
  title: string;
  blocks: PublicBlockSummary[];
  empty?: string;
}) {
  return (
    <section className="block-section">
      <div className="section-heading"><span aria-hidden="true">—</span><h2>{title}</h2></div>
      {blocks.length === 0 ? <p className="empty-copy">{empty}</p> : (
        <div className="block-grid">
          {blocks.map((block, index) => (
            <article className="block-card" key={block.slug}>
              <p className="card-number">{String(index + 1).padStart(2, "0")}</p>
              <h3>{block.title}</h3>
              <p>{block.description}</p>
              <dl className="card-meta">
                <div><dt>質問</dt><dd>{block.questionCount}問</dd></div>
                <div><dt>参加</dt><dd>{block.participantCount}人</dd></div>
              </dl>
              <Link className="text-link" to={block.status === "closed" ? `/results/${block.slug}` : `/play/${block.slug}`}>
                {block.status === "closed" ? "結果を見る" : "問いに答える"}<span aria-hidden="true"> →</span>
              </Link>
            </article>
          ))}
        </div>
      )}
    </section>
  );
}
