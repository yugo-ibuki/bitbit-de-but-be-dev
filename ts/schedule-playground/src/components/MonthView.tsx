import React, { useState, useCallback } from 'react';
import { ScheduleEvent } from '../types';
import { 
  formatTime,
  isToday,
  isSameDay,
  getMonthStart
} from '../utils/dateUtils';

interface MonthViewProps {
  currentDate: Date;
  events: ScheduleEvent[];
  onEventCreate: (startTime: Date, endTime: Date) => void;
  onEventClick?: (event: ScheduleEvent) => void;
}

interface CalendarDay {
  date: Date;
  events: ScheduleEvent[];
  isCurrentMonth: boolean;
  isToday: boolean;
  isSelected: boolean;
}

/**
 * Google Calendar風の月表示ビュー
 * 月単位のカレンダーグリッド表示と予定表示
 */
const MonthView: React.FC<MonthViewProps> = ({
  currentDate,
  events,
  onEventCreate,
  onEventClick
}) => {
  const [selectedDate, setSelectedDate] = useState<Date | null>(null);

  // 月のカレンダーデータを生成
  const generateCalendarDays = useCallback((): CalendarDay[] => {
    const year = currentDate.getFullYear();
    const month = currentDate.getMonth();
    
    // 月の最初の日
    const firstDayOfMonth = new Date(year, month, 1);
    // 月の最後の日
    const lastDayOfMonth = new Date(year, month + 1, 0);
    
    // カレンダーの最初の日（前月の日曜日から）
    const startDate = new Date(firstDayOfMonth);
    startDate.setDate(startDate.getDate() - firstDayOfMonth.getDay());
    
    // カレンダーの最後の日（次月の土曜日まで）
    const endDate = new Date(lastDayOfMonth);
    endDate.setDate(endDate.getDate() + (6 - lastDayOfMonth.getDay()));
    
    const days: CalendarDay[] = [];
    const current = new Date(startDate);
    
    while (current <= endDate) {
      const dayEvents = events.filter(event => 
        isSameDay(new Date(event.startTime), current)
      );
      
      days.push({
        date: new Date(current),
        events: dayEvents,
        isCurrentMonth: current.getMonth() === month,
        isToday: isToday(current),
        isSelected: selectedDate ? isSameDay(current, selectedDate) : false
      });
      
      current.setDate(current.getDate() + 1);
    }
    
    return days;
  }, [currentDate, events, selectedDate]);

  const calendarDays = generateCalendarDays();
  const weekdays = ['日', '月', '火', '水', '木', '金', '土'];

  // 日付クリック - 新しい予定を作成
  const handleDayClick = useCallback((date: Date) => {
    setSelectedDate(date);
    
    // その日の9:00-10:00で新しい予定を作成
    const startTime = new Date(date);
    startTime.setHours(9, 0, 0, 0);
    
    const endTime = new Date(date);
    endTime.setHours(10, 0, 0, 0);
    
    onEventCreate(startTime, endTime);
  }, [onEventCreate]);

  // イベントクリック
  const handleEventClick = useCallback((event: ScheduleEvent, e: React.MouseEvent) => {
    e.stopPropagation(); // 日付クリックを防ぐ
    if (onEventClick) {
      onEventClick(event);
    }
  }, [onEventClick]);

  // イベントの表示スタイルを計算
  const getEventStyle = (event: ScheduleEvent) => {
    const priority = event.priority || 'medium';
    const colors = {
      high: '#ea4335',
      medium: '#1a73e8',
      low: '#34a853'
    };
    
    return {
      backgroundColor: event.color || colors[priority],
      color: 'white'
    };
  };

  // 一日に表示する最大イベント数
  const MAX_EVENTS_PER_DAY = 3;

  return (
    <div className="month-view">
      {/* 月のヘッダー */}
      <div className="month-header">
        {weekdays.map(day => (
          <div key={day} className="weekday-header">
            {day}
          </div>
        ))}
      </div>

      {/* カレンダーグリッド */}
      <div className="calendar-grid">
        {calendarDays.map((day, index) => (
          <div
            key={index}
            className={`calendar-day ${
              day.isCurrentMonth ? 'current-month' : 'other-month'
            } ${day.isToday ? 'today' : ''} ${day.isSelected ? 'selected' : ''}`}
            onClick={() => handleDayClick(day.date)}
          >
            {/* 日付番号 */}
            <div className="day-number">
              {day.date.getDate()}
            </div>

            {/* イベント一覧 */}
            <div className="day-events">
              {day.events.slice(0, MAX_EVENTS_PER_DAY).map(event => (
                <div
                  key={event.id}
                  className="event-item"
                  style={getEventStyle(event)}
                  onClick={(e) => handleEventClick(event, e)}
                  title={`${event.title}\n${formatTime(event.startTime)} - ${formatTime(event.endTime)}`}
                >
                  <div className="event-time">
                    {formatTime(event.startTime, '24h')}
                  </div>
                  <div className="event-title">
                    {event.title}
                  </div>
                </div>
              ))}
              
              {/* 追加のイベントがある場合 */}
              {day.events.length > MAX_EVENTS_PER_DAY && (
                <div className="more-events">
                  +{day.events.length - MAX_EVENTS_PER_DAY} more
                </div>
              )}
            </div>
          </div>
        ))}
      </div>


    </div>
  );
};

export default MonthView;