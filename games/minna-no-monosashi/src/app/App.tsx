import { BrowserRouter, Route, Routes } from "react-router-dom";
import { AppLayout } from "../client/components/AppLayout";
import { BlockIntroPage } from "../client/pages/BlockIntroPage";
import { HomePage } from "../client/pages/HomePage";

function NotFoundPage() {
  return (
    <section className="async-state">
      <p className="eyebrow">404</p>
      <h1>ページが見つかりません</h1>
      <a href="/">問いの一覧へ戻る</a>
    </section>
  );
}

export function App() {
  return (
    <BrowserRouter>
      <AppLayout>
        <Routes>
          <Route path="/" element={<HomePage />} />
          <Route path="/play/:slug" element={<BlockIntroPage />} />
          <Route path="*" element={<NotFoundPage />} />
        </Routes>
      </AppLayout>
    </BrowserRouter>
  );
}
