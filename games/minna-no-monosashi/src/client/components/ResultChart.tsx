import type { QuestionResult } from "../../shared/contracts";

export function ResultChart({ result }: { result: QuestionResult }) {
  return (
    <section className="result-chart" aria-label="回答結果">
      <p className="result-total">{result.totalResponses}人の回答</p>
      <div className="result-list">
        {result.options.map((option) => {
          const selected = option.id === result.selectedOptionId;
          return (
            <div
              className={`result-row${selected ? " is-selected" : ""}`}
              key={option.id}
              aria-label={`${option.label}: ${option.count}人、${option.percentage}パーセント`}
            >
              <div className="result-label">
                <span>{option.label}</span>
                {selected && <span className="you-badge"><span aria-hidden="true">✓</span> あなた</span>}
              </div>
              <div className="result-track" aria-hidden="true">
                <span className="result-bar" style={{ width: `${option.percentage}%` }} />
              </div>
              <div className="result-values">
                <strong>{option.percentage}%</strong>
                <span>{option.count}人</span>
              </div>
            </div>
          );
        })}
      </div>
    </section>
  );
}
