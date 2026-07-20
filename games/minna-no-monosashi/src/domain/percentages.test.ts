import { allocatePercentages } from "./percentages";

it("uses largest remainders so displayed values total 100", () => {
  expect(allocatePercentages([1, 1, 1])).toEqual([34, 33, 33]);
  expect(allocatePercentages([0, 0])).toEqual([0, 0]);
  expect(allocatePercentages([18, 42, 27, 13])).toEqual([18, 42, 27, 13]);
});
