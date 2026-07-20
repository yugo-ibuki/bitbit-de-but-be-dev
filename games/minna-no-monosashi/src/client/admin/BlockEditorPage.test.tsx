import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { BlockEditorForm } from "./BlockEditorPage";

const question = (index: number) => ({
  prompt: `質問${index + 1}`,
  options: ["はい", "いいえ"],
});

it("allows at most ten questions and keeps at least two options", async () => {
  const user = userEvent.setup();
  render(
    <BlockEditorForm
      initialDraft={{
        title: "境界線",
        slug: "boundaries",
        description: "",
        questions: Array.from({ length: 9 }, (_, index) => question(index)),
      }}
    />,
  );

  await user.click(screen.getByRole("button", { name: "質問を追加" }));
  expect(screen.getAllByRole("group", { name: /質問\d+/ })).toHaveLength(10);
  expect(screen.getByRole("button", { name: "質問を追加" })).toBeDisabled();
  expect(
    screen.getByRole("button", { name: "1問目の選択肢1を削除" }),
  ).toBeDisabled();
});

it("moves questions and displays field validation issues", async () => {
  const user = userEvent.setup();
  render(
    <BlockEditorForm
      initialDraft={{
        title: "境界線",
        slug: "boundaries",
        description: "",
        questions: [question(0), question(1)],
      }}
      validationIssues={[
        {
          field: "questions.0.options",
          message: "選択肢は重複しない1〜80文字を2個以上入力してください",
        },
      ]}
    />,
  );

  await user.click(screen.getByRole("button", { name: "1問目を下へ" }));
  expect(screen.getAllByLabelText(/質問文/)[0]).toHaveValue("質問2");
  expect(
    screen.getByText("選択肢は重複しない1〜80文字を2個以上入力してください"),
  ).toBeVisible();
});

it("makes every field read-only after publication", () => {
  render(
    <BlockEditorForm
      status="published"
      initialDraft={{
        title: "境界線",
        slug: "boundaries",
        description: "",
        questions: [question(0)],
      }}
    />,
  );

  expect(screen.getByLabelText("タイトル")).toBeDisabled();
  expect(screen.queryByRole("button", { name: "質問を追加" })).not.toBeInTheDocument();
  expect(screen.getByText("公開済みの内容は変更できません")).toBeVisible();
});
