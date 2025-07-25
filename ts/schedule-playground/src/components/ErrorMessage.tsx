import React from 'react';
import './ErrorMessage.css';

export type ErrorType = 'error' | 'warning' | 'info' | 'success';

export interface ErrorMessageProps {
  type: ErrorType;
  message: string;
  title?: string;
  onClose?: () => void;
  autoClose?: boolean;
  autoCloseDelay?: number;
  className?: string;
}

/**
 * エラーメッセージ表示コンポーネント
 * データ保存失敗、バリデーションエラー、一般的な通知を表示
 */
const ErrorMessage: React.FC<ErrorMessageProps> = ({
  type,
  message,
  title,
  onClose,
  autoClose = false,
  autoCloseDelay = 5000,
  className = ''
}) => {
  const [isVisible, setIsVisible] = React.useState(true);

  // 自動クローズ機能
  React.useEffect(() => {
    if (autoClose && autoCloseDelay > 0) {
      const timer = setTimeout(() => {
        handleClose();
      }, autoCloseDelay);

      return () => clearTimeout(timer);
    }
  }, [autoClose, autoCloseDelay]);

  const handleClose = () => {
    setIsVisible(false);
    if (onClose) {
      // アニメーション完了後にコールバックを実行
      setTimeout(onClose, 300);
    }
  };

  if (!isVisible) {
    return null;
  }

  const getIcon = () => {
    switch (type) {
      case 'error':
        return '⚠️';
      case 'warning':
        return '⚠️';
      case 'info':
        return 'ℹ️';
      case 'success':
        return '✅';
      default:
        return 'ℹ️';
    }
  };

  const getTypeClass = () => {
    switch (type) {
      case 'error':
        return 'error-message--error';
      case 'warning':
        return 'error-message--warning';
      case 'info':
        return 'error-message--info';
      case 'success':
        return 'error-message--success';
      default:
        return 'error-message--info';
    }
  };

  return (
    <div className={`error-message ${getTypeClass()} ${className}`}>
      <div className="error-message__content">
        <div className="error-message__icon">
          {getIcon()}
        </div>
        <div className="error-message__text">
          {title && <div className="error-message__title">{title}</div>}
          <div className="error-message__message">{message}</div>
        </div>
        {onClose && (
          <button
            className="error-message__close"
            onClick={handleClose}
            aria-label="閉じる"
          >
            ×
          </button>
        )}
      </div>


    </div>
  );
};

export default ErrorMessage;