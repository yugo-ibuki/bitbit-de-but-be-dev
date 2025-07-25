import React, { useState } from 'react';
import { CalendarDay } from '../types';

interface CalendarProps {
  onDateSelect: (date: Date) => void;
  selectedDate: Date;
}

const Calendar: React.FC<CalendarProps> = ({ onDateSelect, selectedDate }) => {
  const [currentMonth, setCurrentMonth] = useState(new Date());

  const getDaysInMonth = (date: Date): CalendarDay[] => {
    const year = date.getFullYear();
    const month = date.getMonth();
    const firstDay = new Date(year, month, 1);
    const lastDay = new Date(year, month + 1, 0);
    const daysCount = lastDay.getDate();
    const startDayOfWeek = firstDay.getDay();

    const days: CalendarDay[] = [];

    for (let i = 0; i < startDayOfWeek; i++) {
      const prevDate = new Date(year, month, -startDayOfWeek + i + 1);
      days.push({
        date: prevDate,
        tasks: [],
        events: [],
        isToday: false,
        isSelected: false,
      });
    }

    for (let i = 1; i <= daysCount; i++) {
      const currentDate = new Date(year, month, i);
      const today = new Date();
      days.push({
        date: currentDate,
        tasks: [],
        events: [],
        isToday: 
          currentDate.toDateString() === today.toDateString(),
        isSelected: 
          currentDate.toDateString() === selectedDate.toDateString(),
      });
    }

    return days;
  };

  const navigateMonth = (direction: 'prev' | 'next') => {
    setCurrentMonth(prev => {
      const newMonth = new Date(prev);
      if (direction === 'prev') {
        newMonth.setMonth(prev.getMonth() - 1);
      } else {
        newMonth.setMonth(prev.getMonth() + 1);
      }
      return newMonth;
    });
  };

  const days = getDaysInMonth(currentMonth);
  const weekdays = ['日', '月', '火', '水', '木', '金', '土'];

  return (
    <div className="calendar">
      <div className="calendar-header">
        <button 
          className="btn btn-secondary"
          onClick={() => navigateMonth('prev')}
        >
          ←
        </button>
        <h3>
          {currentMonth.getFullYear()}年 {currentMonth.getMonth() + 1}月
        </h3>
        <button 
          className="btn btn-secondary"
          onClick={() => navigateMonth('next')}
        >
          →
        </button>
      </div>
      
      <div className="calendar-weekdays">
        {weekdays.map(day => (
          <div key={day} className="weekday">
            {day}
          </div>
        ))}
      </div>
      
      <div className="calendar-grid">
        {days.map((day, index) => (
          <div
            key={index}
            className={`calendar-day ${day.isToday ? 'today' : ''} ${day.isSelected ? 'selected' : ''}`}
            onClick={() => onDateSelect(day.date)}
          >
            <span className="day-number">
              {day.date.getDate()}
            </span>
            {day.tasks.length > 0 && (
              <div className="task-indicator">
                {day.tasks.length}
              </div>
            )}
          </div>
        ))}
      </div>
      
      <style>{`
        .calendar {
          width: 100%;
        }
        
        .calendar-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 20px;
        }
        
        .calendar-weekdays {
          display: grid;
          grid-template-columns: repeat(7, 1fr);
          gap: 1px;
          margin-bottom: 10px;
        }
        
        .weekday {
          text-align: center;
          font-weight: bold;
          padding: 10px;
          background: #f7fafc;
          color: #4a5568;
        }
        
        .calendar-grid {
          display: grid;
          grid-template-columns: repeat(7, 1fr);
          gap: 1px;
          background: #e2e8f0;
        }
        
        .calendar-day {
          background: white;
          min-height: 80px;
          padding: 8px;
          cursor: pointer;
          position: relative;
          display: flex;
          flex-direction: column;
          transition: all 0.2s ease;
        }
        
        .calendar-day:hover {
          background: #f7fafc;
        }
        
        .calendar-day.today {
          background: #ebf8ff;
          border: 2px solid #3182ce;
        }
        
        .calendar-day.selected {
          background: #667eea;
          color: white;
        }
        
        .day-number {
          font-weight: bold;
          margin-bottom: 4px;
        }
        
        .task-indicator {
          background: #fc8181;
          color: white;
          border-radius: 50%;
          width: 20px;
          height: 20px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 12px;
          margin-top: auto;
        }
      `}</style>
    </div>
  );
};

export default Calendar;