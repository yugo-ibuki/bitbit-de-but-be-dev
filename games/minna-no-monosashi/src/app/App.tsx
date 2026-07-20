import { BrowserRouter, Route, Routes } from "react-router-dom";
import { AdminDashboardPage } from "../client/admin/AdminDashboardPage";
import { AdminLoginPage } from "../client/admin/AdminLoginPage";
import { AdminPreviewPage } from "../client/admin/AdminPreviewPage";
import { AdminResultsPage } from "../client/admin/AdminResultsPage";
import { BlockEditorPage } from "../client/admin/BlockEditorPage";
import { AppLayout } from "../client/components/AppLayout";
import { BlockIntroPage } from "../client/pages/BlockIntroPage";
import { ClosedResultsPage } from "../client/pages/ClosedResultsPage";
import { HomePage } from "../client/pages/HomePage";
import { QuestionPage } from "../client/pages/QuestionPage";
import { QuestionResultPage } from "../client/pages/QuestionResultPage";
import { SummaryPage } from "../client/pages/SummaryPage";

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
          <Route path="/play/:slug/questions/:position" element={<QuestionPage />} />
          <Route path="/play/:slug/questions/:position/result" element={<QuestionResultPage />} />
          <Route path="/play/:slug/summary" element={<SummaryPage />} />
          <Route path="/results/:slug" element={<ClosedResultsPage />} />
          <Route path="/admin/login" element={<AdminLoginPage />} />
          <Route path="/admin" element={<AdminDashboardPage />} />
          <Route path="/admin/blocks/new" element={<BlockEditorPage />} />
          <Route path="/admin/blocks/:id/edit" element={<BlockEditorPage />} />
          <Route path="/admin/blocks/:id/preview" element={<AdminPreviewPage />} />
          <Route path="/admin/blocks/:id/results" element={<AdminResultsPage />} />
          <Route path="*" element={<NotFoundPage />} />
        </Routes>
      </AppLayout>
    </BrowserRouter>
  );
}
