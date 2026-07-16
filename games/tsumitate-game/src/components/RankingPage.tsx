import type { RankingEntry } from '../game/rankingStorage'

interface RankingPageProps {
  challengeKey: string
  entries: RankingEntry[]
  onBack: () => void
}

const timeFormatter = new Intl.DateTimeFormat('ja-JP', {
  hour: '2-digit',
  minute: '2-digit',
  hour12: false,
  timeZone: 'Asia/Tokyo',
})

function formatTime(timestamp: number): string {
  return timeFormatter.format(new Date(timestamp))
}

export function RankingPage({
  challengeKey,
  entries,
  onBack,
}: RankingPageProps) {
  const visibleEntries = entries.slice(0, 10)
  const bestHeight = entries[0]?.height ?? 0

  return (
    <main className="ranking-page">
      <div className="ranking-grid" aria-hidden="true" />
      <div className="ranking-shell">
        <header className="ranking-header">
          <button className="ranking-back" type="button" onClick={onBack}>
            <span aria-hidden="true">←</span>
            ゲームに戻る
          </button>
          <div className="ranking-date">
            <span>DAILY CHALLENGE</span>
            <b>{challengeKey.replaceAll('-', '.')}</b>
          </div>
        </header>

        <section className="ranking-hero">
          <div>
            <p className="ranking-eyebrow">HEIGHT ARCHIVE</p>
            <h1>デイリーランキング</h1>
            <p className="ranking-lead">
              積み上げて、壊した瞬間の高さを記録。
              <br />
              今日のベストを更新しよう。
            </p>
          </div>
          <span className="ranking-local-badge">
            <i aria-hidden="true" />
            この端末の記録
          </span>
        </section>

        <section className="ranking-summary" aria-label="今日の記録概要">
          <article>
            <span>PERSONAL BEST</span>
            <strong aria-label={`${bestHeight.toFixed(2)} m`}>
              {bestHeight.toFixed(2)} <small>m</small>
            </strong>
          </article>
          <article>
            <span>ATTEMPTS</span>
            <strong aria-label={`${entries.length} 回`}>
              {entries.length} <small>回</small>
            </strong>
          </article>
          <article className="ranking-goal">
            <span>NEXT TARGET</span>
            <strong>
              {entries.length > 0 ? (bestHeight + 0.01).toFixed(2) : '0.01'}{' '}
              <small>m</small>
            </strong>
          </article>
        </section>

        <section className="ranking-board" aria-labelledby="ranking-board-title">
          <div className="ranking-board-heading">
            <div>
              <span>TOP 10</span>
              <h2 id="ranking-board-title">本日の記録</h2>
            </div>
            <span className="ranking-sort">高さ順 · 同点は少ない個数が上位</span>
          </div>

          {visibleEntries.length === 0 ? (
            <div className="ranking-empty">
              <span aria-hidden="true">⌁</span>
              <h3>まだ記録がありません</h3>
              <p>物体を積み上げて「破壊する」を押すと、ここに記録されます。</p>
              <button type="button" onClick={onBack}>最初の記録をつくる</button>
            </div>
          ) : (
            <div className="ranking-table-wrap">
              <table className="ranking-table">
                <thead>
                  <tr>
                    <th scope="col">RANK</th>
                    <th scope="col">HEIGHT</th>
                    <th scope="col">USED</th>
                    <th className="ranking-time" scope="col">TIME</th>
                  </tr>
                </thead>
                <tbody>
                  {visibleEntries.map((entry, index) => {
                    const rank = index + 1
                    return (
                      <tr
                        className={rank <= 3 ? `rank-top rank-${rank}` : undefined}
                        key={entry.id}
                        aria-label={`${rank}位`}
                      >
                        <td>
                          <span className="rank-position">
                            {rank <= 3 && <i aria-hidden="true" />}
                            {String(rank).padStart(2, '0')}
                          </span>
                        </td>
                        <td><strong>{entry.height.toFixed(2)} m</strong></td>
                        <td>{entry.usedCount} 個</td>
                        <td className="ranking-time">{formatTime(entry.completedAt)}</td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          )}
        </section>

        <footer className="ranking-footer">
          記録はこのブラウザに保存されます
        </footer>
      </div>
    </main>
  )
}
