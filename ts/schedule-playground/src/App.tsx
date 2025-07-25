import React, { useState } from 'react';
import Calendar from './components/Calendar';
import TaskList from './components/TaskList';
import TimelineContainer from './components/TimelineContainer';
import { Task } from './types';
import './styles/App.css';

const App: React.FC = () => {
  const [selectedDate, setSelectedDate] = useState(new Date());
  const [tasks, setTasks] = useState<Task[]>([]);

  const handleDateSelect = (date: Date) => {
    setSelectedDate(date);
  };

  const handleTaskAdd = (taskData: Omit<Task, 'id'>) => {
    const newTask: Task = {
      ...taskData,
      id: Date.now().toString(),
    };
    setTasks(prev => [...prev, newTask]);
  };

  const handleTaskUpdate = (id: string, updates: Partial<Task>) => {
    setTasks(prev =>
      prev.map(task =>
        task.id === id ? { ...task, ...updates } : task
      )
    );
  };

  const handleTaskDelete = (id: string) => {
    setTasks(prev => prev.filter(task => task.id !== id));
  };

  const getTasksForSelectedDate = () => {
    return tasks.filter(task => {
      const taskDate = new Date(task.startTime);
      return taskDate.toDateString() === selectedDate.toDateString();
    });
  };

  return (
    <div className="app">
      <header className="header">
        <h1>📅 スケジュール管理プレイグラウンド</h1>
        <p>ReactとTypeScriptで作られたシンプルなスケジュール管理アプリです</p>
      </header>

      <main className="main-content">
        {/* Google Calendar風タイムライン表示 */}
        <div className="timeline-section">
          <TimelineContainer 
            initialDate={selectedDate}
            initialView="week"
          />
        </div>

        {/* 既存のカレンダーとサイドバー（参考用に残す） */}
        <div className="legacy-section">
          <div className="calendar-section">
            <h2>従来のカレンダー</h2>
            <Calendar
              onDateSelect={handleDateSelect}
              selectedDate={selectedDate}
            />
          </div>

          <div className="sidebar">
            <h3>選択された日付</h3>
            <p className="selected-date">
              {selectedDate.toLocaleDateString('ja-JP', {
                year: 'numeric',
                month: 'long',
                day: 'numeric',
                weekday: 'long'
              })}
            </p>
            
            <div className="date-tasks">
              <h4>この日のタスク: {getTasksForSelectedDate().length}件</h4>
              {getTasksForSelectedDate().map(task => (
                <div key={task.id} className="date-task-item">
                  <span className={`task-title ${task.completed ? 'completed' : ''}`}>
                    {task.title}
                  </span>
                  <span className="task-time">
                    {task.startTime.toLocaleTimeString('ja-JP', { 
                      hour: '2-digit', 
                      minute: '2-digit' 
                    })}
                  </span>
                </div>
              ))}
            </div>
          </div>
        </div>
      </main>

      <section className="task-list">
        <TaskList
          tasks={tasks}
          onTaskAdd={handleTaskAdd}
          onTaskUpdate={handleTaskUpdate}
          onTaskDelete={handleTaskDelete}
        />
      </section>

      <style jsx>{`
        .selected-date {
          font-size: 18px;
          font-weight: bold;
          color: #667eea;
          margin-bottom: 20px;
        }
        
        .date-tasks {
          margin-top: 20px;
        }
        
        .date-task-item {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 8px 0;
          border-bottom: 1px solid #e2e8f0;
        }
        
        .task-title {
          flex: 1;
          font-weight: 500;
        }
        
        .task-title.completed {
          text-decoration: line-through;
          opacity: 0.6;
        }
        
        .task-time {
          font-size: 12px;
          color: #666;
        }
      `}</style>
    </div>
  );
};

export default App;