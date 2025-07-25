import React, { useState, useRef, useCallback, useEffect } from 'react';
import { ScheduleEvent, ViewType } from '../types';
import { 
  calculateTimeFromMousePosition, 
  snapToTimeSlot, 
  validateTimeRange,
  formatTime,
  formatTimeRange,
  generateTimeSlots
} from '../utils/dateUtils';

interface WeekViewProps {
  currentDate: Date;
  events: ScheduleEvent[];
  onEventCreate: (startTime: Date, endTime: Date) => void;
  onEventUpdate: (eventId: string, updates: Partial<ScheduleEvent>) => void;
  onEventDelete: (eventId: string) => void;
  onEventClick?: (event: ScheduleEvent) => void;
}

interface DragState {
  isDragging: boolean;
  startTime: Date | null;
  endTime: Date | null;
  startY: number;
  currentY: number;
}

/**
 * Google Calendar風の週表示ビュー
 * ドラッグアンドドロップで時間範囲を選択して予定を作成
 */
const WeekView: React.FC<WeekViewProps> = ({
  currentDate,
  events,
  onEventCreate,
  onEventUpdate,
  onEventDelete
}) => {
  const [dragState, setDragState] = useState<DragState>({
    isDragging: false,
    startTime: null,
    endTime: null,
    startY: 0,
    currentY: 0
  });

  const timeGridRef = useRef<HTMLDivElement>(null);
  const [selectedEvent, setSelectedEvent] = useState<ScheduleEvent | null>(null);

  // 週の日付を生成
  const getWeekDates = useCallback(() => {
    const dates: Date[] = [];
    const startOfWeek = new Date(currentDate);
    const dayOfWeek = startOfWeek.getDay();
    startOfWeek.setDate(startOfWeek.getDate() - dayOfWeek);

    for (let i = 0; i < 7; i++) {
      const date = new Date(startOfWeek);
      date.setDate(startOfWeek.getDate() + i);
      dates.push(date);
    }
    return dates;
  }, [currentDate]);

  const weekDates = getWeekDates();

  // 時間スロットを生成（30分間隔）
  const timeSlots = generateTimeSlots(currentDate, 'week', 30);
  const hours = Array.from({ length: 24 }, (_, i) => i);

  // マウスダウン - ドラッグ開始
  const handleMouseDown = useCallback((e: React.MouseEvent) => {
    if (!timeGridRef.current) return;

    const rect = timeGridRef.current.getBoundingClientRect();
    const startTime = calculateTimeFromMousePosition(
      e.clientY,
      rect,
      currentDate,
      'week',
      30
    );

    setDragState({
      isDragging: true,
      startTime: snapToTimeSlot(startTime, 30),
      endTime: null,
      startY: e.clientY,
      currentY: e.clientY
    });

    // マウスイベントをドキュメント全体で監視
    document.addEventListener('mousemove', handleMouseMove);
    document.addEventListener('mouseup', handleMouseUp);
  }, [currentDate]);

  // マウス移動 - ドラッグ中
  const handleMouseMove = useCallback((e: MouseEvent) => {
    if (!dragState.isDragging || !timeGridRef.current || !dragState.startTime) return;

    const rect = timeGridRef.current.getBoundingClientRect();
    const currentTime = calculateTimeFromMousePosition(
      e.clientY,
      rect,
      currentDate,
      'week',
      30
    );

    const snappedEndTime = snapToTimeSlot(currentTime, 30);
    
    // 最小15分の時間を確保
    let endTime = snappedEndTime;
    if (endTime <= dragState.startTime) {
      endTime = new Date(dragState.startTime);
      endTime.setMinutes(endTime.getMinutes() + 30);
    }

    setDragState(prev => ({
      ...prev,
      endTime,
      currentY: e.clientY
    }));
  }, [dragState.isDragging, dragState.startTime, currentDate]);

  // マウスアップ - ドラッグ終了
  const handleMouseUp = useCallback(() => {
    if (dragState.isDragging && dragState.startTime && dragState.endTime) {
      if (validateTimeRange(dragState.startTime, dragState.endTime)) {
        onEventCreate(dragState.startTime, dragState.endTime);
      }
    }

    setDragState({
      isDragging: false,
      startTime: null,
      endTime: null,
      startY: 0,
      currentY: 0
    });

    // イベントリスナーを削除
    document.removeEventListener('mousemove', handleMouseMove);
    document.removeEventListener('mouseup', handleMouseUp);
  }, [dragState, onEventCreate, handleMouseMove]);

  // コンポーネントのクリーンアップ
  useEffect(() => {
    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
    };
  }, [handleMouseMove, handleMouseUp]);

  // ドラッグ選択の視覚的表示を計算
  const getDragSelectionStyle = () => {
    if (!dragState.isDragging || !dragState.startTime || !dragState.endTime || !timeGridRef.current) {
      return { display: 'none' };
    }

    const rect = timeGridRef.current.getBoundingClientRect();
    const startY = Math.min(dragState.startY, dragState.currentY) - rect.top;
    const height = Math.abs(dragState.currentY - dragState.startY);

    return {
      position: 'absolute' as const,
      top: `${startY}px`,
      left: '60px',
      right: '0',
      height: `${height}px`,
      backgroundColor: 'rgba(26, 115, 232, 0.2)',
      border: '2px solid #1a73e8',
      borderRadius: '4px',
      pointerEvents: 'none' as const,
      zIndex: 10
    };
  };

  // イベントクリック
  const handleEventClick = useCallback((event: ScheduleEvent) => {
    setSelectedEvent(event);
  }, []);

  return (
    <div className="week-view">
      {/* 週のヘッダー */}
      <div className="week-header">
        <div className="time-column-header"></div>
        {weekDates.map((date, index) => (
          <div key={index} className="day-header">
            <div className="day-name">
              {date.toLocaleDateString('ja-JP', { weekday: 'short' })}
            </div>
            <div className="day-number">
              {date.getDate()}
            </div>
          </div>
        ))}
      </div>

      {/* タイムグリッド */}
      <div className="time-grid-container">
        <div 
          className="time-grid"
          ref={timeGridRef}
          onMouseDown={handleMouseDown}
        >
          {/* 時間ラベル */}
          <div className="time-labels">
            {hours.map(hour => (
              <div key={hour} className="time-label">
                {formatTime(new Date(2024, 0, 1, hour, 0), '24h')}
              </div>
            ))}
          </div>

          {/* 日付列 */}
          {weekDates.map((date, dayIndex) => (
            <div key={dayIndex} className="day-column">
              {/* 時間スロット */}
              {hours.map(hour => (
                <div key={hour} className="hour-slot">
                  <div className="half-hour-slot"></div>
                  <div className="half-hour-slot"></div>
                </div>
              ))}

              {/* この日のイベント */}
              {events
                .filter(event => {
                  const eventDate = new Date(event.startTime);
                  return eventDate.toDateString() === date.toDateString();
                })
                .map(event => {
                  const startHour = event.startTime.getHours();
                  const startMinute = event.startTime.getMinutes();
                  const duration = (event.endTime.getTime() - event.startTime.getTime()) / (1000 * 60);
                  
                  const top = (startHour * 60 + startMinute) * (60 / 60); // 1分 = 1px
                  const height = duration * (60 / 60);

                  return (
                    <div
                      key={event.id}
                      className={`event-block ${selectedEvent?.id === event.id ? 'selected' : ''}`}
                      style={{
                        position: 'absolute',
                        top: `${top}px`,
                        left: '4px',
                        right: '4px',
                        height: `${height}px`,
                        backgroundColor: event.color || '#1a73e8',
                        borderRadius: '4px',
                        padding: '4px 8px',
                        color: 'white',
                        fontSize: '12px',
                        cursor: 'pointer',
                        zIndex: 5
                      }}
                      onClick={() => handleEventClick(event)}
                    >
                      <div className="event-title">{event.title}</div>
                      <div className="event-time">
                        {formatTimeRange(event.startTime, event.endTime)}
                      </div>
                    </div>
                  );
                })}
            </div>
          ))}

          {/* ドラッグ選択の視覚的表示 */}
          <div style={getDragSelectionStyle()}>
            {dragState.isDragging && dragState.startTime && dragState.endTime && (
              <div className="drag-time-indicator">
                {formatTimeRange(dragState.startTime, dragState.endTime)}
              </div>
            )}
          </div>
        </div>
      </div>

      <style>{`
        .week-view {
          height: 100%;
          display: flex;
          flex-direction: column;
          background: white;
        }

        .week-header {
          display: grid;
          grid-template-columns: 60px repeat(7, 1fr);
          border-bottom: 1px solid #e0e0e0;
          background: #fafafa;
        }

        .time-column-header {
          border-right: 1px solid #e0e0e0;
        }

        .day-header {
          padding: 12px 8px;
          text-align: center;
          border-right: 1px solid #e0e0e0;
        }

        .day-name {
          font-size: 12px;
          color: #5f6368;
          margin-bottom: 4px;
        }

        .day-number {
          font-size: 16px;
          font-weight: 500;
          color: #202124;
        }

        .time-grid-container {
          flex: 1;
          overflow: auto;
          position: relative;
        }

        .time-grid {
          display: grid;
          grid-template-columns: 60px repeat(7, 1fr);
          position: relative;
          min-height: 1440px; /* 24時間 × 60px */
        }

        .time-labels {
          border-right: 1px solid #e0e0e0;
          background: #fafafa;
        }

        .time-label {
          height: 60px;
          padding: 8px;
          font-size: 12px;
          color: #5f6368;
          border-bottom: 1px solid #f0f0f0;
          display: flex;
          align-items: flex-start;
        }

        .day-column {
          border-right: 1px solid #e0e0e0;
          position: relative;
          cursor: crosshair;
        }

        .hour-slot {
          height: 60px;
          border-bottom: 1px solid #f0f0f0;
          position: relative;
        }

        .half-hour-slot {
          height: 30px;
          border-bottom: 1px solid #f8f8f8;
        }

        .half-hour-slot:hover {
          background: rgba(26, 115, 232, 0.05);
        }

        .event-block {
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2);
          transition: all 0.2s ease;
        }

        .event-block:hover {
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
          transform: translateY(-1px);
        }

        .event-block.selected {
          box-shadow: 0 0 0 2px #fff, 0 0 0 4px #1a73e8;
        }

        .event-title {
          font-weight: 500;
          margin-bottom: 2px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .event-time {
          font-size: 10px;
          opacity: 0.9;
        }

        .drag-time-indicator {
          position: absolute;
          top: 4px;
          left: 8px;
          background: #1a73e8;
          color: white;
          padding: 4px 8px;
          border-radius: 4px;
          font-size: 12px;
          font-weight: 500;
        }

        @media (max-width: 768px) {
          .time-grid {
            grid-template-columns: 50px repeat(7, 1fr);
          }

          .day-header {
            padding: 8px 4px;
          }

          .day-name {
            font-size: 10px;
          }

          .day-number {
            font-size: 14px;
          }

          .time-label {
            padding: 4px;
            font-size: 10px;
          }
        }
      `}</style>
    </div>
  );
};

export default WeekView;