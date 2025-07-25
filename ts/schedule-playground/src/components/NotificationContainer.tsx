import React from 'react';
import ErrorMessage, { ErrorType } from './ErrorMessage';
import { ErrorState } from '../hooks/useErrorHandler';
import './NotificationContainer.css';

export interface NotificationContainerProps {
  errors: ErrorState[];
  onClearError: (index: number) => void;
  position?: 'top-right' | 'top-left' | 'bottom-right' | 'bottom-left' | 'top-center';
  maxVisible?: number;
}

/**
 * 通知コンテナコンポーネント
 * 複数のエラーメッセージを管理・表示
 */
const NotificationContainer: React.FC<NotificationContainerProps> = ({
  errors,
  onClearError,
  position = 'top-right',
  maxVisible = 5
}) => {
  // 表示する通知を制限
  const visibleErrors = errors.slice(-maxVisible);

  if (visibleErrors.length === 0) {
    return null;
  }

  const getPositionClass = () => {
    switch (position) {
      case 'top-left':
        return 'notification-container--top-left';
      case 'bottom-right':
        return 'notification-container--bottom-right';
      case 'bottom-left':
        return 'notification-container--bottom-left';
      case 'top-center':
        return 'notification-container--top-center';
      default:
        return 'notification-container--top-right';
    }
  };

  return (
    <div className={`notification-container ${getPositionClass()}`}>
      {visibleErrors.map((error, index) => (
        <ErrorMessage
          key={`${error.message}-${index}`}
          type={error.type as ErrorType}
          message={error.message}
          title={error.title}
          onClose={() => onClearError(errors.length - maxVisible + index)}
          autoClose={error.type === 'success' || error.type === 'info'}
          autoCloseDelay={error.type === 'success' ? 3000 : 5000}
        />
      ))}


    </div>
  );
};

export default NotificationContainer;