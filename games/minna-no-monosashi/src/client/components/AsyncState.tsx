export function LoadingState({ label = "読み込んでいます" }: { label?: string }) {
  return <p className="async-state" role="status">{label}</p>;
}

export function ErrorState({
  message,
  onRetry,
}: {
  message: string;
  onRetry?: () => void;
}) {
  return (
    <section className="async-state error-panel" role="alert">
      <h1>うまく読み込めませんでした</h1>
      <p>{message}</p>
      {onRetry && <button onClick={onRetry}>もう一度試す</button>}
    </section>
  );
}
