import { use, useOptimistic, Suspense } from 'react';
import { experimental_useFormStatus as useFormStatus } from 'react-dom';
import { TodoList } from './components/TodoList';
import { DocumentList } from './components/DocumentList';
import './App.css';

// データフェッチのシミュレーション
async function fetchTodos() {
  await new Promise(resolve => setTimeout(resolve, 1000));
  return [
    { id: 1, title: 'React 19を学ぶ' },
    { id: 2, title: '新機能を試す' }
  ];
}

export default function App() {
  const todos = use(fetchTodos());
  const [optimisticTodos, addOptimisticTodo] = useOptimistic(
    todos,
    (state, newTodo) => [...state, newTodo]
  );

  async function handleSubmit(formData: FormData) {
    const title = formData.get('title') as string;
    const newTodo = { id: Date.now(), title };
    
    addOptimisticTodo(newTodo);
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  return (
    <div className="app">
      <h1>React 19 新機能デモ</h1>
      
      <form action={handleSubmit}>
        <input type="text" name="title" placeholder="新しいTodoを入力" />
        <SubmitButton />
      </form>

      <Suspense fallback={<div>Todoを読み込み中...</div>}>
        <TodoList todos={optimisticTodos} />
      </Suspense>

      <Suspense fallback={<div>ドキュメントを読み込み中...</div>}>
        <DocumentList />
      </Suspense>
    </div>
  );
}

function SubmitButton() {
  const { pending } = useFormStatus();
  return (
    <button type="submit" disabled={pending}>
      {pending ? '追加中...' : '追加'}
    </button>
  );
}