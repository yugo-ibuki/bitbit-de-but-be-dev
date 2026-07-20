import { render, screen } from "@testing-library/react";
import { ResultChart } from "./ResultChart";

it("shows counts, percentages, and a text marker for the selected answer", () => {
  render(
    <ResultChart
      result={{
        questionId: "q1",
        selectedOptionId: "o2",
        totalResponses: 3,
        options: [
          { id: "o1", label: "6時", count: 2, percentage: 67 },
          { id: "o2", label: "7時", count: 1, percentage: 33 },
        ],
      }}
    />,
  );

  expect(screen.getByText("3人の回答")).toBeInTheDocument();
  expect(screen.getByText("67%")).toBeInTheDocument();
  expect(screen.getByText("2人")).toBeInTheDocument();
  expect(screen.getByText("あなた")).toBeInTheDocument();
  expect(screen.getByLabelText("6時: 2人、67パーセント")).toBeInTheDocument();
});
