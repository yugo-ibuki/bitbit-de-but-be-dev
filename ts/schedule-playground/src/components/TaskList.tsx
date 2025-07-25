import React, { useState } from 'react';
import { Task } from '../types';

interface TaskListProps {
  tasks: Task[];
  onTaskAdd: (task: Omit<Task, 'id'>) => void;
  onTaskUpdate: (id: string, updates: Partial<Task>) => void;
  onTaskDelete: (id: string) => void;
}

const TaskList: React.FC<TaskListProps> = ({
  tasks,
  onTaskAdd,
  onTaskUpdate,
  onTaskDelete,
}) => {
  const [isAddingTask, setIsAddingTask] = useState(false);
  const [newTask, setNewTask] = useState({
    title: '',
    description: '',
    startTime: '',
    endTime: '',
    priority: 'medium' as const,
    category: '',
  });

  const handleAddTask = () => {
    if (newTask.title && newTask.startTime && newTask.endTime) {
      onTaskAdd({
        ...newTask,
        startTime: new Date(newTask.startTime),
        endTime: new Date(newTask.endTime),
        completed: false,
      });
      
      setNewTask({
        title: '',
        description: '',
        startTime: '',
        endTime: '',
        priority: 'medium',
        category: '',
      });
      setIsAddingTask(false);
    }
  };

  const getPriorityColor = (priority: string) => {
    switch (priority) {
      case 'high': return '#fc8181';
      case 'medium': return '#f6ad55';
      case 'low': return '#68d391';
      default: return '#a0aec0';
    }
  };

  return (
    <div className="task-list">
      <div className="task-list-header">
        <h3>タスク一覧</h3>
        <button
          className="btn btn-primary"
          onClick={() => setIsAddingTask(true)}
        >
          + 新しいタスク
        </button>
      </div>

      {isAddingTask && (
        <div className="task-form">
          <input
            type="text"
            placeholder="タスクのタイトル"
            value={newTask.title}
            onChange={(e) => setNewTask({ ...newTask, title: e.target.value })}
          />
          <textarea
            placeholder="説明（任意）"
            value={newTask.description}
            onChange={(e) => setNewTask({ ...newTask, description: e.target.value })}
          />
          <input
            type="datetime-local"
            value={newTask.startTime}
            onChange={(e) => setNewTask({ ...newTask, startTime: e.target.value })}
          />
          <input
            type="datetime-local"
            value={newTask.endTime}
            onChange={(e) => setNewTask({ ...newTask, endTime: e.target.value })}
          />
          <select
            value={newTask.priority}
            onChange={(e) => setNewTask({ ...newTask, priority: e.target.value as any })}
          >
            <option value="low">低</option>
            <option value="medium">中</option>
            <option value="high">高</option>
          </select>
          <input
            type="text"
            placeholder="カテゴリ（任意）"
            value={newTask.category}
            onChange={(e) => setNewTask({ ...newTask, category: e.target.value })}
          />
          <div className="form-actions">
            <button className="btn btn-primary" onClick={handleAddTask}>
              追加
            </button>
            <button 
              className="btn btn-secondary" 
              onClick={() => setIsAddingTask(false)}
            >
              キャンセル
            </button>
          </div>
        </div>
      )}

      <div className="tasks">
        {tasks.map((task) => (
          <div key={task.id} className={`task-item ${task.completed ? 'completed' : ''}`}>
            <div className="task-main">
              <div className="task-header">
                <input
                  type="checkbox"
                  checked={task.completed}
                  onChange={(e) => onTaskUpdate(task.id, { completed: e.target.checked })}
                />
                <h4>{task.title}</h4>
                <div
                  className="priority-indicator"
                  style={{ backgroundColor: getPriorityColor(task.priority) }}
                />
              </div>
              
              {task.description && (
                <p className="task-description">{task.description}</p>
              )}
              
              <div className="task-time">
                {task.startTime.toLocaleString()} - {task.endTime.toLocaleString()}
              </div>
              
              {task.category && (
                <span className="task-category">{task.category}</span>
              )}
            </div>
            
            <button
              className="delete-btn"
              onClick={() => onTaskDelete(task.id)}
            >
              削除
            </button>
          </div>
        ))}
        
        {tasks.length === 0 && (
          <p className="no-tasks">タスクがありません</p>
        )}
      </div>

      <style>{`
        .task-list {
          height: 100%;
        }
        
        .task-list-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 20px;
        }
        
        .task-form {
          background: #f7fafc;
          padding: 20px;
          border-radius: 8px;
          margin-bottom: 20px;
        }
        
        .task-form input,
        .task-form textarea,
        .task-form select {
          width: 100%;
          margin-bottom: 10px;
          padding: 8px;
          border: 1px solid #e2e8f0;
          border-radius: 4px;
        }
        
        .task-form textarea {
          resize: vertical;
          min-height: 60px;
        }
        
        .form-actions {
          display: flex;
          gap: 10px;
        }
        
        .tasks {
          display: flex;
          flex-direction: column;
          gap: 10px;
        }
        
        .task-item {
          background: #f7fafc;
          border-radius: 8px;
          padding: 15px;
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
          transition: all 0.2s ease;
        }
        
        .task-item:hover {
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        }
        
        .task-item.completed {
          opacity: 0.6;
        }
        
        .task-item.completed .task-main {
          text-decoration: line-through;
        }
        
        .task-main {
          flex: 1;
        }
        
        .task-header {
          display: flex;
          align-items: center;
          gap: 10px;
          margin-bottom: 8px;
        }
        
        .task-header h4 {
          margin: 0;
          flex: 1;
        }
        
        .priority-indicator {
          width: 12px;
          height: 12px;
          border-radius: 50%;
        }
        
        .task-description {
          margin: 8px 0;
          color: #666;
          font-size: 14px;
        }
        
        .task-time {
          font-size: 12px;
          color: #999;
          margin-bottom: 8px;
        }
        
        .task-category {
          background: #e2e8f0;
          color: #4a5568;
          padding: 4px 8px;
          border-radius: 12px;
          font-size: 12px;
        }
        
        .delete-btn {
          background: #fc8181;
          color: white;
          border: none;
          padding: 8px 12px;
          border-radius: 4px;
          cursor: pointer;
          font-size: 12px;
        }
        
        .delete-btn:hover {
          background: #f56565;
        }
        
        .no-tasks {
          text-align: center;
          color: #a0aec0;
          font-style: italic;
          margin: 40px 0;
        }
      `}</style>
    </div>
  );
};

export default TaskList;