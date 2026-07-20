import { render, screen } from "@testing-library/react";
import { vi } from "vitest";
import { App } from "./App";

it("shows the product name and accessible page structure", async () => {
  vi.stubGlobal(
    "fetch",
    vi.fn().mockResolvedValue(
      new Response(JSON.stringify({ blocks: [] }), {
        headers: { "content-type": "application/json" },
      }),
    ),
  );
  render(<App />);
  expect(
    await screen.findByRole("heading", { name: "みんなのものさし" }),
  ).toBeInTheDocument();
  expect(screen.getByRole("link", { name: "本文へ移動" })).toHaveAttribute(
    "href",
    "#main-content",
  );
  expect(screen.getAllByRole("main")).toHaveLength(1);
  vi.unstubAllGlobals();
});
