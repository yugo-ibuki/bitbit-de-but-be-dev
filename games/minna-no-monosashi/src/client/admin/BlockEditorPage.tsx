import { useEffect, useState } from "react";
import { Link, useNavigate, useParams } from "react-router-dom";
import type { DraftBlockInput, ValidationIssue } from "../../domain/types";
import type { AdminBlock } from "../../shared/contracts";
import { ApiError, adminRequest } from "../api/client";
import { ErrorState, LoadingState } from "../components/AsyncState";

const EMPTY_DRAFT: DraftBlockInput = {
  title: "",
  slug: "",
  description: "",
  questions: [{ prompt: "", options: ["", ""] }],
};

const RECOVERY_KEY = "minna-no-monosashi:unsaved-admin-draft:v1";

function moveItem<T>(items: T[], from: number, to: number): T[] {
  if (to < 0 || to >= items.length) return items;
  const next = [...items];
  const [item] = next.splice(from, 1);
  if (item !== undefined) next.splice(to, 0, item);
  return next;
}

export function BlockEditorForm({
  initialDraft,
  status = "draft",
  validationIssues = [],
  saving = false,
  onSave,
}: {
  initialDraft: DraftBlockInput;
  status?: AdminBlock["status"];
  validationIssues?: ValidationIssue[];
  saving?: boolean;
  onSave?: (draft: DraftBlockInput) => void | Promise<void>;
}) {
  const [draft, setDraft] = useState(initialDraft);
  const editable = status === "draft";
  const issueFor = (field: string) =>
    validationIssues.find((issue) => issue.field === field)?.message;

  useEffect(() => setDraft(initialDraft), [initialDraft]);

  const updateQuestion = (
    questionIndex: number,
    update: (question: DraftBlockInput["questions"][number]) => DraftBlockInput["questions"][number],
  ) => setDraft((current) => ({
    ...current,
    questions: current.questions.map((question, index) =>
      index === questionIndex ? update(question) : question,
    ),
  }));

  return (
    <form className="editor-form" onSubmit={(event) => { event.preventDefault(); void onSave?.(draft); }}>
      {!editable && <p className="notice">公開済みの内容は変更できません</p>}
      <div className="editor-basics">
        <label>タイトル<input value={draft.title} disabled={!editable} maxLength={80} onChange={(event) => setDraft({ ...draft, title: event.target.value })} /></label>
        {issueFor("title") && <p className="field-error">{issueFor("title")}</p>}
        <label>URL識別子<input value={draft.slug} disabled={!editable} maxLength={64} pattern="[a-z0-9-]+" onChange={(event) => setDraft({ ...draft, slug: event.target.value })} /></label>
        {issueFor("slug") && <p className="field-error">{issueFor("slug")}</p>}
        <label>説明<textarea value={draft.description} disabled={!editable} maxLength={400} rows={4} onChange={(event) => setDraft({ ...draft, description: event.target.value })} /></label>
      </div>

      <div className="editor-questions">
        <div className="editor-section-heading"><h2>質問</h2><span>{draft.questions.length} / 10</span></div>
        {issueFor("questions") && <p className="field-error">{issueFor("questions")}</p>}
        {draft.questions.map((question, questionIndex) => (
          <fieldset className="question-editor" key={questionIndex} aria-label={`質問${questionIndex + 1}`}>
            <legend>質問 {String(questionIndex + 1).padStart(2, "0")}</legend>
            {editable && (
              <div className="reorder-buttons">
                <button type="button" aria-label={`${questionIndex + 1}問目を上へ`} disabled={questionIndex === 0} onClick={() => setDraft((current) => ({ ...current, questions: moveItem(current.questions, questionIndex, questionIndex - 1) }))}>↑</button>
                <button type="button" aria-label={`${questionIndex + 1}問目を下へ`} disabled={questionIndex === draft.questions.length - 1} onClick={() => setDraft((current) => ({ ...current, questions: moveItem(current.questions, questionIndex, questionIndex + 1) }))}>↓</button>
                <button type="button" aria-label={`${questionIndex + 1}問目を削除`} disabled={draft.questions.length === 1} onClick={() => setDraft((current) => ({ ...current, questions: current.questions.filter((_, index) => index !== questionIndex) }))}>削除</button>
              </div>
            )}
            <label>質問文 {questionIndex + 1}<textarea value={question.prompt} disabled={!editable} maxLength={200} rows={2} onChange={(event) => updateQuestion(questionIndex, (current) => ({ ...current, prompt: event.target.value }))} /></label>
            {issueFor(`questions.${questionIndex}.prompt`) && <p className="field-error">{issueFor(`questions.${questionIndex}.prompt`)}</p>}
            <div className="option-editors">
              <p className="input-label">選択肢</p>
              {question.options.map((option, optionIndex) => (
                <div className="option-editor" key={optionIndex}>
                  <label><span className="visually-hidden">{questionIndex + 1}問目の選択肢{optionIndex + 1}</span><input value={option} disabled={!editable} maxLength={80} onChange={(event) => updateQuestion(questionIndex, (current) => ({ ...current, options: current.options.map((value, index) => index === optionIndex ? event.target.value : value) }))} /></label>
                  {editable && <div className="reorder-buttons compact">
                    <button type="button" aria-label={`${questionIndex + 1}問目の選択肢${optionIndex + 1}を上へ`} disabled={optionIndex === 0} onClick={() => updateQuestion(questionIndex, (current) => ({ ...current, options: moveItem(current.options, optionIndex, optionIndex - 1) }))}>↑</button>
                    <button type="button" aria-label={`${questionIndex + 1}問目の選択肢${optionIndex + 1}を下へ`} disabled={optionIndex === question.options.length - 1} onClick={() => updateQuestion(questionIndex, (current) => ({ ...current, options: moveItem(current.options, optionIndex, optionIndex + 1) }))}>↓</button>
                    <button type="button" aria-label={`${questionIndex + 1}問目の選択肢${optionIndex + 1}を削除`} disabled={question.options.length <= 2} onClick={() => updateQuestion(questionIndex, (current) => ({ ...current, options: current.options.filter((_, index) => index !== optionIndex) }))}>×</button>
                  </div>}
                </div>
              ))}
              {issueFor(`questions.${questionIndex}.options`) && <p className="field-error">{issueFor(`questions.${questionIndex}.options`)}</p>}
              {editable && <button className="inline-button" type="button" onClick={() => updateQuestion(questionIndex, (current) => ({ ...current, options: [...current.options, ""] }))}>選択肢を追加</button>}
            </div>
          </fieldset>
        ))}
        {editable && <button className="secondary-button" type="button" disabled={draft.questions.length >= 10} onClick={() => setDraft((current) => ({ ...current, questions: [...current.questions, { prompt: "", options: ["", ""] }] }))}>質問を追加</button>}
      </div>
      {editable && <div className="sticky-actions"><button className="primary-button" type="submit" disabled={saving}>{saving ? "保存中" : "下書きを保存"}</button></div>}
    </form>
  );
}

export function BlockEditorPage() {
  const { id } = useParams();
  const navigate = useNavigate();
  const [block, setBlock] = useState<AdminBlock | null>(null);
  const [newDraft] = useState<DraftBlockInput>(() => {
    if (id) return EMPTY_DRAFT;
    try {
      const recovered = sessionStorage.getItem(RECOVERY_KEY);
      return recovered ? JSON.parse(recovered) as DraftBlockInput : EMPTY_DRAFT;
    } catch { return EMPTY_DRAFT; }
  });
  const [loading, setLoading] = useState(Boolean(id));
  const [saving, setSaving] = useState(false);
  const [issues, setIssues] = useState<ValidationIssue[]>([]);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!id) return;
    adminRequest<{ block: AdminBlock }>(`/blocks/${id}`)
      .then(({ block: loaded }) => setBlock(loaded))
      .catch((reason: unknown) => {
        if (reason instanceof ApiError && reason.status === 401) navigate("/admin/login");
        else setError(reason instanceof Error ? reason.message : "読み込みに失敗しました");
      })
      .finally(() => setLoading(false));
  }, [id, navigate]);

  const asDraft = (source: AdminBlock): DraftBlockInput => ({
    title: source.title,
    slug: source.slug,
    description: source.description,
    questions: source.questions.map((question) => ({ prompt: question.prompt, options: question.options.map((option) => option.label) })),
  });

  const save = async (draft: DraftBlockInput) => {
    setSaving(true); setError(null); setIssues([]);
    try {
      const response = await adminRequest<{ block: AdminBlock }>(id ? `/blocks/${id}` : "/blocks", {
        method: id ? "PUT" : "POST",
        body: JSON.stringify(draft),
      });
      setBlock(response.block);
      sessionStorage.removeItem(RECOVERY_KEY);
      if (!id) navigate(`/admin/blocks/${response.block.id}/edit`, { replace: true });
    } catch (reason) {
      if (reason instanceof ApiError && reason.status === 401) {
        sessionStorage.setItem(RECOVERY_KEY, JSON.stringify(draft));
        navigate("/admin/login", { state: { recovery: true } });
      } else if (reason instanceof ApiError && reason.code === "VALIDATION_FAILED") {
        setIssues(reason.details as ValidationIssue[]);
      } else setError(reason instanceof Error ? reason.message : "保存に失敗しました");
    } finally { setSaving(false); }
  };

  const action = async (name: "publish" | "close" | "clone") => {
    if (!block) return;
    try {
      const response = await adminRequest<{ block: AdminBlock }>(`/blocks/${block.id}/${name}`, { method: "POST" });
      if (name === "clone") navigate(`/admin/blocks/${response.block.id}/edit`);
      else setBlock(response.block);
    } catch (reason) {
      if (reason instanceof ApiError && reason.code === "VALIDATION_FAILED") setIssues(reason.details as ValidationIssue[]);
      else setError(reason instanceof Error ? reason.message : "操作に失敗しました");
    }
  };

  if (loading) return <LoadingState />;
  if (error && !block && id) return <ErrorState message={error} />;
  const draft = block ? asDraft(block) : newDraft;

  return (
    <section className="admin-page editor-page">
      <div className="admin-heading"><div><p className="eyebrow">ADMIN / EDITOR</p><h1>{block ? block.title : "新しいブロック"}</h1></div><Link to="/admin">一覧へ戻る</Link></div>
      {error && <p className="form-error" role="alert">{error}</p>}
      {block && <div className="block-actions">
        <Link className="secondary-button" to={`/admin/blocks/${block.id}/preview`}>プレビュー</Link>
        <Link className="secondary-button" to={`/admin/blocks/${block.id}/results`}>集計を見る</Link>
        {block.status === "draft" && <button className="primary-button" onClick={() => void action("publish")}>公開する</button>}
        {block.status === "published" && <button className="danger-button" onClick={() => void action("close")}>回答受付を終了</button>}
        {block.status !== "draft" && <button className="secondary-button" onClick={() => void action("clone")}>複製して下書きを作る</button>}
      </div>}
      <BlockEditorForm initialDraft={draft} status={block?.status} validationIssues={issues} saving={saving} onSave={save} />
    </section>
  );
}
