import { use } from 'react';

interface Document {
  id: number;
  title: string;
}

// Document Modelの機能デモ
async function fetchDocuments(): Promise<Document[]> {
  await new Promise(resolve => setTimeout(resolve, 800));
  return [
    { id: 1, title: '重要な文書1' },
    { id: 2, title: '重要な文書2' }
  ];
}

export function DocumentList() {
  const documents = use(fetchDocuments());

  return (
    <div className="document-list">
      <h2>ドキュメント一覧</h2>
      <ul>
        {documents.map((doc: Document) => (
          <li key={doc.id}>{doc.title}</li>
        ))}
      </ul>
    </div>
  );
} 