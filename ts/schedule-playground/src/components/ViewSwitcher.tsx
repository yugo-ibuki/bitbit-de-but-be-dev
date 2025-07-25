import React from 'react';
import { ViewType } from '../types';

interface ViewSwitcherProps {
  currentView: ViewType;
  onViewChange: (view: ViewType) => void;
}

/**
 * Google Calendar風の表示切り替えコンポーネント
 * 月・週・日の表示モードを切り替える
 */
const ViewSwitcher: React.FC<ViewSwitcherProps> = ({ currentView, onViewChange }) => {
  const views: { type: ViewType; label: string; icon: string }[] = [
    { type: 'month', label: '月', icon: '📅' },
    { type: 'week', label: '週', icon: '📊' },
    { type: 'day', label: '日', icon: '📋' }
  ];

  return (
    <div className="view-switcher">
      <div className="view-buttons">
        {views.map(view => (
          <button
            key={view.type}
            className={`view-button ${currentView === view.type ? 'active' : ''}`}
            onClick={() => onViewChange(view.type)}
            title={`${view.label}表示に切り替え`}
          >
            <span className="view-icon">{view.icon}</span>
            <span className="view-label">{view.label}</span>
          </button>
        ))}
      </div>

      <style>{`
        .view-switcher {
          display: flex;
          align-items: center;
          gap: 8px;
        }

        .view-buttons {
          display: flex;
          background: #f8f9fa;
          border-radius: 8px;
          padding: 4px;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
        }

        .view-button {
          display: flex;
          align-items: center;
          gap: 6px;
          padding: 8px 16px;
          border: none;
          background: transparent;
          border-radius: 6px;
          cursor: pointer;
          transition: all 0.2s ease;
          font-size: 14px;
          font-weight: 500;
          color: #5f6368;
          min-width: 70px;
          justify-content: center;
        }

        .view-button:hover {
          background: #e8f0fe;
          color: #1a73e8;
        }

        .view-button.active {
          background: #1a73e8;
          color: white;
          box-shadow: 0 1px 3px rgba(26, 115, 232, 0.3);
        }

        .view-button.active:hover {
          background: #1557b0;
        }

        .view-icon {
          font-size: 16px;
        }

        .view-label {
          font-weight: 500;
        }

        /* Responsive design for mobile devices */
        @media (max-width: 768px) {
          .view-button {
            min-width: 50px;
            padding: 8px 12px;
            min-height: 44px; /* Touch-friendly size */
          }
          
          .view-label {
            display: none;
          }
          
          .view-icon {
            font-size: 18px;
          }
        }

        @media (max-width: 480px) {
          .view-buttons {
            padding: 2px;
          }

          .view-button {
            min-width: 44px;
            padding: 6px 8px;
            min-height: 40px;
          }
          
          .view-icon {
            font-size: 16px;
          }
        }

        /* Touch device optimizations */
        @media (hover: none) and (pointer: coarse) {
          .view-button {
            min-height: 48px;
            min-width: 60px;
          }

          .view-button:hover {
            background: transparent;
          }

          .view-button:active {
            background: #e8f0fe;
            transform: scale(0.95);
          }

          .view-button.active:active {
            background: #1557b0;
          }
        }
      `}</style>
    </div>
  );
};

export default ViewSwitcher;