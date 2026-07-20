import { useEffect, useState } from "react";
import type { PublicQuestion } from "../../shared/contracts";

export function QuestionForm({
  question,
  onSubmit,
}: {
  question: PublicQuestion;
  onSubmit: (optionId: string) => Promise<unknown>;
}) {
  const [selectedOptionId, setSelectedOptionId] = useState("");
  const [pending, setPending] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setSelectedOptionId("");
    setError(null);
  }, [question.id]);

  const submit = async (event: React.FormEvent) => {
    event.preventDefault();
    if (!selectedOptionId || pending) return;
    setPending(true);
    setError(null);
    try {
      await onSubmit(selectedOptionId);
    } catch (reason) {
      setError(reason instanceof Error ? reason.message : "送信に失敗しました");
    } finally {
      setPending(false);
    }
  };

  return (
    <form className="question-form" onSubmit={submit}>
      <fieldset disabled={pending}>
        <legend className="visually-hidden">答えを一つ選んでください</legend>
        <div className="choice-list">
          {question.options.map((option) => (
            <label className="choice" key={option.id}>
              <input
                type="radio"
                name={`answer-${question.id}`}
                value={option.id}
                checked={selectedOptionId === option.id}
                onChange={() => setSelectedOptionId(option.id)}
              />
              <span className="choice-indicator" aria-hidden="true" />
              <span>{option.label}</span>
            </label>
          ))}
        </div>
      </fieldset>
      {error && <p className="form-error" role="alert">{error}</p>}
      <p className="visually-hidden" aria-live="polite">
        {pending ? "回答を送信しています" : error ?? ""}
      </p>
      <button className="primary-button" type="submit" disabled={!selectedOptionId || pending}>
        {pending ? "送信中" : error ? "もう一度送る" : "この答えにする"}
      </button>
    </form>
  );
}
