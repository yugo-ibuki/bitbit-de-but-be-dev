import { memo } from 'react';

interface Todo {
  id: number;
  title: string;
}

interface TodoListProps {
  todos: Todo[];
}

export const TodoList = memo(function TodoList({ todos }: TodoListProps) {
  return (
    <div className="todo-list">
      <h2>Todoリスト</h2>
      <ul>
        {todos.map(todo => (
          <li key={todo.id}>{todo.title}</li>
        ))}
      </ul>
    </div>
  );
}); 