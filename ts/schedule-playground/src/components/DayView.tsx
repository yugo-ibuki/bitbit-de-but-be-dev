import React, { useState, useRef, useCallback, useEffect } from 'react';
import { ScheduleEvent } from '../types';
import { 
  calculateTimeFromMousePosition, 
  snapToTimeSlot, 
  validateTimeRange,
  formatTime,
  formatTimeRange,
  isToday
} from '../utils/dateUtils';

interface DayViewProps {
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
 * Google Calendar風の日表示ビュー
 * 1日の詳細タイムライン表示とドラッグアンドドロップ機能
 */
const DayView: React.FC<DayViewProps> = ({
  currentDate,
  events,
  onEventCreate,
  onEventUpdate,
  onEventDelete,
  onEventClick
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

  // その日のイベントをフィルタリング
  const dayEvents = events.filter(event => {
    const eventDate = new Date(event.startTime);
    return eventDate.toDateString() === currentDate.toDateString();
  });

  // 時間スロット（15分間隔で詳細表示）
  const timeSlots = Array.from({ length: 96 }, (_, i) => {
    const hour = Math.floor(i / 4);
    const minute = (i % 4) * 15;
    return { hour, minute, index: i };
  });

  // 時間ラベル（1時間ごと）
  const hourLabels = Array.from({ length: 24 }, (_, i) => i);

  // マウスダウン - ドラッグ開始
  const handleMouseDown = useCallback((e: React.MouseEvent) => {
    if (!timeGridRef.current) return;

    const rect = timeGridRef.current.getBoundingClientRect();
    const startTime = calculateTimeFromMousePosition(
      e.clientY,
      rect,
      currentDate,
      'day',
      15 // 15分間隔
    );

    setDragState({
      isDragging: true,
      startTime: snapToTimeSlot(startTime, 15),
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
      'day',
      15
    );

    const snappedEndTime = snapToTimeSlot(currentTime, 15);
    
    // 最小15分の時間を確保
    let endTime = snappedEndTime;
    if (endTime <= dragState.startTime) {
      endTime = new Date(dragState.startTime);
      endTime.setMinutes(endTime.getMinutes() + 15);
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
      left: '80px',
      right: '16px',
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
    if (onEventClick) {
      onEventClick(event);
    }
  }, [onEventClick]);

  // イベントの位置とサイズを計算
  const getEventStyle = (event: ScheduleEvent) => {
    const startHour = event.startTime.getHours();
    const startMinute = event.startTime.getMinutes();
    const duration = (event.endTime.getTime() - event.startTime.getTime()) / (1000 * 60);
    
    // 15分 = 20px として計算
    const pixelsPerMinute = 20 / 15;
    const top = (startHour * 60 + startMinute) * pixelsPerMinute;
    const height = duration * pixelsPerMinute;

    return {
      position: 'absolute' as const,
      top: `${top}px`,
      left: '84px',
      right: '20px',
      height: `${Math.max(height, 20)}px`, // 最小高さ20px
      backgroundColor: event.color || '#1a73e8',
      borderRadius: '4px',
      padding: '4px 8px',
      color: 'white',
      fontSize: '12px',
      cursor: 'pointer',
      zIndex: 5,
      boxShadow: '0 1px 3px rgba(0, 0, 0, 0.2)',
      transition: 'all 0.2s ease'
    };
  };

  return (
    <div className="day-view">
      {/* 日のヘッダー */}
      <div className="day-header">
        <div className="date-info">
          <div className="day-name">
            {currentDate.toLocaleDateString('ja-JP', { weekday: 'long' })}
          </div>
          <div className={`day-number ${isToday(currentDate) ? 'today' : ''}`}>
            {currentDate.getDate()}
          </div>
          <div className="month-year">
            {currentDate.toLocaleDateString('ja-JP', { year: 'numeric', month: 'long' })}
          </div>
        </div>
        <div className="event-count">
          {dayEvents.length}件の予定
        </div>
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
            {hourLabels.map(hour => (
              <div key={hour} className="time-label">
                <span className="hour-text">
                  {formatTime(new Date(2024, 0, 1, hour, 0), '24h')}
                </span>
              </div>
            ))}
          </div>

          {/* メインタイムライン */}
          <div className="timeline-column">
            {/* 時間スロット */}
            {timeSlots.map(slot => (
              <div 
                key={slot.index} 
                className={`time-slot ${slot.minute === 0 ? 'hour-boundary' : ''}`}
              >
                {slot.minute === 0 && (
                  <div className="hour-line"></div>
                )}
              </div>
            ))}

            {/* イベント */}
            {dayEvents.map(event => (
              <div
                key={event.id}
                className={`event-block ${selectedEvent?.id === event.id ? 'selected' : ''}`}
                style={getEventStyle(event)}
                onClick={() => handleEventClick(event)}
              >
                <div className="event-title">{event.title}</div>
                <div className="event-time">
                  {formatTimeRange(event.startTime, event.endTime)}
                </div>
                {event.description && (
                  <div className="event-description">
                    {event.description}
                  </div>
                )}
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
      </div>

      <style>{`
        .day-view {
          height: 100%;
          display: flex;
          flex-direction: column;
          background: white;
        }

        .day-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 20px 24px;
          border-bottom: 1px solid #e0e0e0;
          background: #fafafa;
        }

        .date-info {
          display: flex;
          align-items: center;
          gap: 16px;
        }

        .day-name {
          font-size: 14px;
          color: #5f6368;
          font-weight: 500;
        }

        .day-number {
          font-size: 32px;
          font-weight: 300;
          color: #202124;
          min-width: 48px;
          text-align: center;
        }

        .day-number.today {
          background: #1a73e8;
          color: white;
          border-radius: 50%;
          width: 48px;
          height: 48px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: 500;
        }

        .month-year {
          font-size: 16px;
          color: #5f6368;
        }

        .event-count {
          font-size: 14px;
          color: #5f6368;
          background: #f8f9fa;
          padding: 8px 16px;
          border-radius: 16px;
        }

        .time-grid-container {
          flex: 1;
          overflow: auto;
          position: relative;
        }

        .time-grid {
          display: grid;
          grid-template-columns: 80px 1fr;
          position: relative;
          min-height: 1920px; /* 24時間 × 80px */
        }

        .time-labels {
          border-right: 1px solid #e0e0e0;
          background: #fafafa;
          position: sticky;
          left: 0;
          z-index: 2;
        }

        .time-label {
          height: 80px; /* 1時間 = 80px */
          padding: 8px;
          display: flex;
          align-items: flex-start;
          border-bottom: 1px solid #f0f0f0;
        }

        .hour-text {
          font-size: 12px;
          color: #5f6368;
          font-weight: 500;
        }

        .timeline-column {
          position: relative;
          cursor: crosshair;
        }

        .time-slot {
          height: 20px; /* 15分 = 20px */
          border-bottom: 1px solid #f8f8f8;
          position: relative;
        }

        .time-slot.hour-boundary {
          border-bottom: 1px solid #e0e0e0;
        }

        .time-slot:hover {
          background: rgba(26, 115, 232, 0.05);
        }

        .hour-line {
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 1px;
          background: #e0e0e0;
        }

        .event-block {
          transition: all 0.2s ease;
          overflow: hidden;
        }

        .event-block:hover {
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
          transform: translateX(-2px);
        }

        .event-block.selected {
          box-shadow: 0 0 0 2px #fff, 0 0 0 4px #1a73e8;
        }

        .event-title {
          font-weight: 600;
          margin-bottom: 2px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .event-time {
          font-size: 10px;
          opacity: 0.9;
          margin-bottom: 2px;
        }

        .event-description {
          font-size: 10px;
          opacity: 0.8;
          overflow: hidden;
          text-overflow: ellipsis;
          display: -webkit-box;
          -webkit-line-clamp: 2;
          -webkit-box-orient: vertical;
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
          .day-header {
            padding: 16px;
            flex-direction: column;
            gap: 12px;
            align-items: flex-start;
          }

          .date-info {
            gap: 12px;
          }

          .day-number {
            font-size: 28px;
          }

          .time-grid {
            grid-template-columns: 60px 1fr;
          }

          .time-label {
            padding: 4px;
          }

          .hour-text {
            font-size: 10px;
          }

          .event-block {
            left: 64px !important;
            right: 8px !important;
          }
        }
      `}</style>
    </div>
  );
};

export default DayView;