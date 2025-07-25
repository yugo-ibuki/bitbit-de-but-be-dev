import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import { useErrorHandler } from '../../hooks/useErrorHandler';
import { ValidationError } from '../../services/scheduleService';
import { StorageError } from '../../utils/storageManager';
import NotificationContainer from '../NotificationContainer';

// Test component that uses the error handler
const TestErrorComponent: React.FC = () => {
  const { 
    errors, 
    clearError, 
    showSuccess, 
    showError, 
    showWarning, 
    showInfo, 
    handleError,
    withErrorHandling,
    isLoading 
  } = useErrorHandler();

  const triggerValidationError = () => {
    const error = new ValidationError('title_required', 'Title is required', 'title');
    handleError(error);
  };

  const triggerStorageError = () => {
    const error = new StorageError('quota_exceeded', 'Storage quota exceeded');
    handleError(error);
  };

  const triggerAsyncError = async () => {
    await withErrorHandling(async () => {
      throw new Error('Async operation failed');
    }, 'テスト操作');
  };

  return (
    <div>
      <button onClick={() => showSuccess('成功メッセージ')}>Show Success</button>
      <button onClick={() => showError('エラーメッセージ')}>Show Error</button>
      <button onClick={() => showWarning('警告メッセージ')}>Show Warning</button>
      <button onClick={() => showInfo('情報メッセージ')}>Show Info</button>
      <button onClick={triggerValidationError}>Trigger Validation Error</button>
      <button onClick={triggerStorageError}>Trigger Storage Error</button>
      <button onClick={triggerAsyncError}>Trigger Async Error</button>
      
      {isLoading && <div data-testid="loading">Loading...</div>}
      
      <NotificationContainer
        errors={errors}
        onClearError={clearError}
        position="top-right"
      />
    </div>
  );
};

describe('Error Handling Integration', () => {
  it('displays success message correctly', async () => {
    render(<TestErrorComponent />);
    
    fireEvent.click(screen.getByText('Show Success'));
    
    expect(screen.getByText('成功メッセージ')).toBeInTheDocument();
    expect(screen.getByText('✅')).toBeInTheDocument();
  });

  it('displays error message correctly', async () => {
    render(<TestErrorComponent />);
    
    fireEvent.click(screen.getByText('Show Error'));
    
    expect(screen.getByText('エラーメッセージ')).toBeInTheDocument();
    expect(screen.getByText('⚠️')).toBeInTheDocument();
  });

  it('handles validation errors with proper formatting', async () => {
    render(<TestErrorComponent />);
    
    fireEvent.click(screen.getByText('Trigger Validation Error'));
    
    expect(screen.getByText('バリデーションエラー')).toBeInTheDocument();
    expect(screen.getByText('タイトルは必須です')).toBeInTheDocument();
  });

  it('handles storage errors with proper formatting', async () => {
    render(<TestErrorComponent />);
    
    fireEvent.click(screen.getByText('Trigger Storage Error'));
    
    expect(screen.getByText('データ保存エラー')).toBeInTheDocument();
    expect(screen.getByText('ストレージの容量が不足しています。不要なデータを削除してください。')).toBeInTheDocument();
  });

  it('handles async errors with loading state', async () => {
    render(<TestErrorComponent />);
    
    fireEvent.click(screen.getByText('Trigger Async Error'));
    
    // Should show loading state briefly
    expect(screen.getByTestId('loading')).toBeInTheDocument();
    
    // Wait for error to appear
    await waitFor(() => {
      expect(screen.getByText('テスト操作でエラーが発生しました')).toBeInTheDocument();
    });
    
    // Loading should be gone
    expect(screen.queryByTestId('loading')).not.toBeInTheDocument();
  });

  it('allows clearing individual errors', async () => {
    render(<TestErrorComponent />);
    
    // Add multiple errors
    fireEvent.click(screen.getByText('Show Error'));
    fireEvent.click(screen.getByText('Show Warning'));
    
    expect(screen.getByText('エラーメッセージ')).toBeInTheDocument();
    expect(screen.getByText('警告メッセージ')).toBeInTheDocument();
    
    // Close the first error
    const closeButtons = screen.getAllByLabelText('閉じる');
    fireEvent.click(closeButtons[0]);
    
    // Wait for animation
    await waitFor(() => {
      expect(screen.queryByText('エラーメッセージ')).not.toBeInTheDocument();
    }, { timeout: 500 });
    
    // Second error should still be visible
    expect(screen.getByText('警告メッセージ')).toBeInTheDocument();
  });

  it('auto-closes success and info messages', async () => {
    render(<TestErrorComponent />);
    
    fireEvent.click(screen.getByText('Show Success'));
    
    expect(screen.getByText('成功メッセージ')).toBeInTheDocument();
    
    // Wait for auto-close (success messages auto-close after 3 seconds)
    await waitFor(() => {
      expect(screen.queryByText('成功メッセージ')).not.toBeInTheDocument();
    }, { timeout: 4000 });
  }, 10000);
});