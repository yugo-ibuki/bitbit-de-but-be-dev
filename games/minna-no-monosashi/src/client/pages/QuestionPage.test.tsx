import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { vi } from "vitest";
import { QuestionForm } from "../components/QuestionForm";

const question = {
  id: "q1",
  prompt: "朝早いのは？",
  position: 0,
  options: [
    { id: "o1", label: "6時", position: 0 },
    { id: "o2", label: "7時", position: 1 },
  ],
};

it("submits one selected option and disables controls while pending", async () => {
  const user = userEvent.setup();
  let resolveAnswer!: () => void;
  const submitAnswer = vi.fn(
    () => new Promise<void>((resolve) => { resolveAnswer = resolve; }),
  );
  render(<QuestionForm question={question} onSubmit={submitAnswer} />);

  await user.click(screen.getByRole("radio", { name: "7時" }));
  await user.click(screen.getByRole("button", { name: "この答えにする" }));

  expect(submitAnswer).toHaveBeenCalledWith("o2");
  expect(screen.getByRole("button", { name: "送信中" })).toBeDisabled();
  expect(screen.getByRole("radio", { name: "7時" })).toBeDisabled();
  resolveAnswer();
});

it("keeps the selection after a network failure so it can retry", async () => {
  const user = userEvent.setup();
  const submitAnswer = vi.fn().mockRejectedValueOnce(new Error("通信エラー"));
  render(<QuestionForm question={question} onSubmit={submitAnswer} />);

  await user.click(screen.getByRole("radio", { name: "6時" }));
  await user.click(screen.getByRole("button", { name: "この答えにする" }));

  expect(await screen.findByRole("alert")).toHaveTextContent("通信エラー");
  expect(screen.getByRole("radio", { name: "6時" })).toBeChecked();
  expect(screen.getByRole("button", { name: "もう一度送る" })).toBeEnabled();
});
