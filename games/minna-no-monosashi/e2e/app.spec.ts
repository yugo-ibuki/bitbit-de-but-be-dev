import { expect, test } from "@playwright/test";

test("admin publishes ten questions and anonymous visitors answer once", async ({
  browser,
  page,
}, testInfo) => {
  const runId = `${testInfo.project.name}-${Date.now()}`.replace(/[^a-z0-9-]/g, "-");
  const slug = `e2e-${runId}`;

  await page.goto("/admin/login");
  await page.getByLabel("管理者パスワード").fill("dev-admin");
  await page.getByRole("button", { name: "ログイン" }).click();
  await expect(page.getByRole("heading", { name: "質問ブロック" })).toBeVisible();
  await page.getByRole("link", { name: "新しく作る" }).click();

  await page.getByLabel("タイトル").fill(`E2E 日常の境界線 ${runId}`);
  await page.getByLabel("URL識別子").fill(slug);
  await page.getByLabel("説明").fill("ブラウザテストで作成した質問ブロックです。");
  for (let index = 1; index < 10; index += 1) {
    await page.getByRole("button", { name: "質問を追加" }).click();
  }
  for (let index = 1; index <= 10; index += 1) {
    await page.getByLabel(`質問文 ${index}`, { exact: true }).fill(`${index}問目の境界線は？`);
    await page.getByLabel(`${index}問目の選択肢1`, { exact: true }).fill("こちら");
    await page.getByLabel(`${index}問目の選択肢2`, { exact: true }).fill("あちら");
  }
  await page.getByRole("button", { name: "下書きを保存" }).click();
  await expect(page).toHaveURL(/\/admin\/blocks\/.+\/edit/);
  const editUrl = page.url();
  await page.getByRole("button", { name: "公開する" }).click();
  await expect(page.getByText("公開済みの内容は変更できません")).toBeVisible();

  await page.goto(`/play/${slug}`);
  await page.getByRole("button", { name: "はじめる" }).click();
  for (let index = 1; index <= 10; index += 1) {
    await expect(page.getByText(`${index}問目の境界線は？`)).toBeVisible();
    await page.getByRole("radio", { name: "こちら" }).click();
    await page.getByRole("button", { name: "この答えにする" }).click();
    await expect(page.getByText("あなた")).toBeVisible();
    await page
      .getByRole("button", {
        name: index === 10 ? "すべての結果を振り返る" : "次の問いへ",
      })
      .click();
  }
  await expect(page.getByRole("heading", { name: /振り返り/ })).toBeVisible();

  const visitorB = await browser.newContext();
  const pageB = await visitorB.newPage();
  await pageB.goto(`/play/${slug}`);
  await pageB.getByRole("button", { name: "はじめる" }).click();
  await pageB.getByRole("radio", { name: "あちら" }).click();
  await pageB.getByRole("button", { name: "この答えにする" }).click();
  await expect(pageB.getByText("2人の回答")).toBeVisible();

  await page.goto(editUrl);
  await page.getByRole("button", { name: "回答受付を終了" }).click();
  await expect(page.getByText("公開済みの内容は変更できません")).toBeVisible();

  const visitorC = await browser.newContext();
  const pageC = await visitorC.newPage();
  await pageC.goto(`/play/${slug}`);
  await expect(pageC.getByRole("link", { name: "全体結果を見る" })).toBeVisible();
  await pageC.getByRole("link", { name: "全体結果を見る" }).click();
  await expect(pageC.getByText("2人の回答").first()).toBeVisible();

  await visitorB.close();
  await visitorC.close();
});
