import React from 'react';

export interface LoadingSpinnerProps {
  size?: 'small' | 'medium' | 'large';
  message?: string;
  overlay?: boolean;
  className?: string;
}

/**
 * ローディング状態表示コンポーネント
 * データ保存中やロード中の状態を視覚的に表示
 */
const LoadingSpinner: React.FC<LoadingSpinnerProps> = ({
  size = 'medium',
  message,
  overlay = false,
  className = ''
}) => {
  const getSizeClass = () => {
    switch (size) {
      case 'small':
        return 'loading-spinner--small';
      case 'large':
        return 'loading-spinner--large';
      default:
        return 'loading-spinner--medium';
    }
  };

  const content = (
    <div className={`loading-spinner ${getSizeClass()} ${className}`}>
      <div className="loading-spinner__spinner">
        <div className="loading-spinner__circle"></div>
      </div>
      {message && (
        <div className="loading-spinner__message">{message}</div>
      )}

      <style>{`
        .loading-spinner {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          gap: 12px;
        }

        .loading-spinner__spinner {
          position: relative;
        }

        .loading-spinner__circle {
          border: 3px solid #f3f4f6;
          border-top: 3px solid #1a73e8;
          border-radius: 50%;
          animation: spin 1s linear infinite;
        }

        .loading-spinner--small .loading-spinner__circle {
          width: 20px;
          height: 20px;
          border-width: 2px;
        }

        .loading-spinner--medium .loading-spinner__circle {
          width: 32px;
          height: 32px;
          border-width: 3px;
        }

        .loading-spinner--large .loading-spinner__circle {
          width: 48px;
          height: 48px;
          border-width: 4px;
        }

        .loading-spinner__message {
          color: #5f6368;
          text-align: center;
          font-size: 14px;
          font-weight: 500;
        }

        .loading-spinner--small .loading-spinner__message {
          font-size: 12px;
        }

        .loading-spinner--large .loading-spinner__message {
          font-size: 16px;
        }

        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
      `}</style>
    </div>
  );

  if (overlay) {
    return (
      <div className="loading-overlay">
        {content}
        <style>{`
          .loading-overlay {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(255, 255, 255, 0.8);
            display: flex;
            align-items: center;
            justify-content: center;
            z-index: 9999;
            backdrop-filter: blur(2px);
          }
        `}</style>
      </div>
    );
  }

  return content;
};

export default LoadingSpinner;