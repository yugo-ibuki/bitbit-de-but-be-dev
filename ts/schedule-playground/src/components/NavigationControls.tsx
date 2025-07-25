import React from 'react';
import { ViewType } from '../types';
import { formatDate } from '../utils/dateUtils';

interface NavigationControlsProps {
  currentDate: Date;
  viewType: ViewType;
  onDateChange: (date: Date) => void;
  onTodayClick: () => void;
}

/**
 * ナビゲーションコントロールコンポーネント
 * 前へ・次へボタン、今日ボタン、現在の日付表示を提供
 * 要件: 5.1, 5.2, 5.3, 5.4
 */
const NavigationControls: React.FC<NavigationControlsProps> = ({
  currentDate,
  viewType,
  onDateChange,
  onTodayClick
}) => {
  // 前へナビゲーション
  const handlePrevious = () => {
    const newDate = new Date(currentDate);
    
    switch (viewType) {
      case 'month':
        // 前月へ移動
        newDate.setMonth(newDate.getMonth() - 1);
        break;
      case 'week':
        // 前週へ移動（7日前）
        newDate.setDate(newDate.getDate() - 7);
        break;
      case 'day':
        // 前日へ移動
        newDate.setDate(newDate.getDate() - 1);
        break;
    }
    
    onDateChange(newDate);
  };

  // 次へナビゲーション
  const handleNext = () => {
    const newDate = new Date(currentDate);
    
    switch (viewType) {
      case 'month':
        // 次月へ移動
        newDate.setMonth(newDate.getMonth() + 1);
        break;
      case 'week':
        // 次週へ移動（7日後）
        newDate.setDate(newDate.getDate() + 7);
        break;
      case 'day':
        // 次日へ移動
        newDate.setDate(newDate.getDate() + 1);
        break;
    }
    
    onDateChange(newDate);
  };

  // 今日ボタンのクリック処理
  const handleTodayClick = () => {
    onTodayClick();
  };

  // 現在の日付を適切な形式でフォーマット
  const getFormattedDate = () => {
    switch (viewType) {
      case 'month':
        return currentDate.toLocaleDateString('ja-JP', {
          year: 'numeric',
          month: 'long'
        });
      case 'week':
        // 週表示では週の範囲を表示
        const weekStart = new Date(currentDate);
        const dayOfWeek = weekStart.getDay();
        weekStart.setDate(weekStart.getDate() - dayOfWeek);
        
        const weekEnd = new Date(weekStart);
        weekEnd.setDate(weekEnd.getDate() + 6);
        
        // 同じ月の場合
        if (weekStart.getMonth() === weekEnd.getMonth()) {
          return `${weekStart.getFullYear()}年${weekStart.getMonth() + 1}月 ${weekStart.getDate()}日 - ${weekEnd.getDate()}日`;
        } else {
          // 異なる月にまたがる場合
          return `${weekStart.toLocaleDateString('ja-JP', { month: 'long', day: 'numeric' })} - ${weekEnd.toLocaleDateString('ja-JP', { month: 'long', day: 'numeric' })}`;
        }
      case 'day':
        return formatDate(currentDate);
      default:
        return formatDate(currentDate);
    }
  };

  // 今日かどうかをチェック
  const isToday = () => {
    const today = new Date();
    
    switch (viewType) {
      case 'month':
        return (
          currentDate.getFullYear() === today.getFullYear() &&
          currentDate.getMonth() === today.getMonth()
        );
      case 'week':
        // 今日が現在表示されている週に含まれているかチェック
        const weekStart = new Date(currentDate);
        const dayOfWeek = weekStart.getDay();
        weekStart.setDate(weekStart.getDate() - dayOfWeek);
        weekStart.setHours(0, 0, 0, 0);
        
        const weekEnd = new Date(weekStart);
        weekEnd.setDate(weekEnd.getDate() + 6);
        weekEnd.setHours(23, 59, 59, 999);
        
        return today >= weekStart && today <= weekEnd;
      case 'day':
        return (
          currentDate.getFullYear() === today.getFullYear() &&
          currentDate.getMonth() === today.getMonth() &&
          currentDate.getDate() === today.getDate()
        );
      default:
        return false;
    }
  };

  return (
    <div className="navigation-controls">
      {/* 日付表示エリア */}
      <div className="date-display">
        <h2 className="current-date">
          {getFormattedDate()}
        </h2>
        <button 
          className={`today-button ${isToday() ? 'current' : ''}`}
          onClick={handleTodayClick}
          disabled={isToday()}
          title="今日に移動"
        >
          今日
        </button>
      </div>

      {/* ナビゲーションボタン */}
      <div className="nav-buttons">
        <button 
          className="nav-button prev"
          onClick={handlePrevious}
          title={`前の${viewType === 'month' ? '月' : viewType === 'week' ? '週' : '日'}へ`}
          aria-label={`前の${viewType === 'month' ? '月' : viewType === 'week' ? '週' : '日'}へ移動`}
        >
          <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor">
            <path d="M10.5 3.5L6 8l4.5 4.5" stroke="currentColor" strokeWidth="1.5" fill="none" strokeLinecap="round" strokeLinejoin="round"/>
          </svg>
        </button>
        
        <button 
          className="nav-button next"
          onClick={handleNext}
          title={`次の${viewType === 'month' ? '月' : viewType === 'week' ? '週' : '日'}へ`}
          aria-label={`次の${viewType === 'month' ? '月' : viewType === 'week' ? '週' : '日'}へ移動`}
        >
          <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor">
            <path d="M5.5 3.5L10 8l-4.5 4.5" stroke="currentColor" strokeWidth="1.5" fill="none" strokeLinecap="round" strokeLinejoin="round"/>
          </svg>
        </button>
      </div>

      <style>{`
        .navigation-controls {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 24px;
          width: 100%;
        }

        .date-display {
          display: flex;
          align-items: center;
          gap: 16px;
        }

        .current-date {
          margin: 0;
          font-size: 20px;
          font-weight: 600;
          color: #202124;
          white-space: nowrap;
        }

        .today-button {
          padding: 8px 16px;
          border: 1px solid #dadce0;
          background: white;
          border-radius: 4px;
          cursor: pointer;
          font-size: 14px;
          color: #1a73e8;
          transition: all 0.2s ease;
          white-space: nowrap;
        }

        .today-button:hover:not(:disabled) {
          background: #f8f9fa;
          border-color: #1a73e8;
        }

        .today-button:disabled,
        .today-button.current {
          background: #e8f0fe;
          border-color: #1a73e8;
          color: #1a73e8;
          cursor: default;
          opacity: 0.7;
        }

        .nav-buttons {
          display: flex;
          gap: 4px;
        }

        .nav-button {
          width: 40px;
          height: 40px;
          border: 1px solid #dadce0;
          background: white;
          border-radius: 50%;
          cursor: pointer;
          display: flex;
          align-items: center;
          justify-content: center;
          color: #5f6368;
          transition: all 0.2s ease;
        }

        .nav-button:hover {
          background: #f8f9fa;
          border-color: #1a73e8;
          color: #1a73e8;
        }

        .nav-button:active {
          background: #e8f0fe;
        }

        .nav-button svg {
          pointer-events: none;
        }

        /* レスポンシブデザイン */
        @media (max-width: 1024px) {
          .navigation-controls {
            gap: 20px;
          }

          .current-date {
            font-size: 19px;
          }
        }

        @media (max-width: 768px) {
          .navigation-controls {
            flex-direction: column;
            gap: 16px;
          }

          .date-display {
            flex-direction: column;
            gap: 12px;
            text-align: center;
          }

          .current-date {
            font-size: 18px;
            line-height: 1.2;
          }

          .today-button {
            min-height: 44px; /* Touch-friendly */
          }

          .nav-buttons {
            gap: 12px;
            justify-content: center;
          }

          .nav-button {
            width: 48px;
            height: 48px;
          }
        }

        @media (max-width: 480px) {
          .navigation-controls {
            gap: 12px;
          }

          .date-display {
            gap: 8px;
          }

          .current-date {
            font-size: 16px;
            text-align: center;
            word-break: break-word;
          }

          .today-button {
            padding: 8px 16px;
            font-size: 13px;
            min-height: 40px;
          }

          .nav-buttons {
            gap: 16px;
          }

          .nav-button {
            width: 44px;
            height: 44px;
          }
        }

        /* Touch device optimizations */
        @media (hover: none) and (pointer: coarse) {
          .today-button {
            min-height: 48px;
            padding: 12px 20px;
          }

          .nav-button {
            width: 52px;
            height: 52px;
          }

          .today-button:hover:not(:disabled),
          .nav-button:hover {
            background: inherit;
            border-color: inherit;
            color: inherit;
          }

          .today-button:active:not(:disabled) {
            background: #e8f0fe;
            border-color: #1a73e8;
            transform: scale(0.95);
          }

          .nav-button:active {
            background: #f8f9fa;
            border-color: #1a73e8;
            color: #1a73e8;
            transform: scale(0.95);
          }
        }

        /* Landscape mobile optimization */
        @media (max-width: 768px) and (orientation: landscape) {
          .navigation-controls {
            flex-direction: row;
            justify-content: space-between;
            align-items: center;
            gap: 16px;
          }

          .date-display {
            flex-direction: row;
            gap: 16px;
            text-align: left;
          }

          .current-date {
            font-size: 16px;
          }
        }
      `}</style>
    </div>
  );
};

export default NavigationControls;