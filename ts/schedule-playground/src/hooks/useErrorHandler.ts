import { useState, useCallback } from 'react';
import { ValidationError } from '../services/scheduleService';
import { StorageError } from '../utils/storageManager';

export interface ErrorState {
  type: 'error' | 'warning' | 'info' | 'success';
  message: string;
  title?: string;
  field?: string;
  code?: string;
}

/**
 * エラーハンドリング用カスタムフック
 * アプリケーション全体で統一されたエラー処理を提供
 */
export const useErrorHandler = () => {
  const [errors, setErrors] = useState<ErrorState[]>([]);
  const [isLoading, setIsLoading] = useState(false);

  // エラーを追加
  const addError = useCallback((error: ErrorState) => {
    setErrors(prev => [...prev, { ...error, id: Date.now().toString() } as ErrorState & { id: string }]);
  }, []);

  // エラーをクリア
  const clearError = useCallback((index: number) => {
    setErrors(prev => prev.filter((_, i) => i !== index));
  }, []);

  // 全エラーをクリア
  const clearAllErrors = useCallback(() => {
    setErrors([]);
  }, []);

  // 成功メッセージを表示
  const showSuccess = useCallback((message: string, title?: string) => {
    addError({
      type: 'success',
      message,
      title
    });
  }, [addError]);

  // 情報メッセージを表示
  const showInfo = useCallback((message: string, title?: string) => {
    addError({
      type: 'info',
      message,
      title
    });
  }, [addError]);

  // 警告メッセージを表示
  const showWarning = useCallback((message: string, title?: string) => {
    addError({
      type: 'warning',
      message,
      title
    });
  }, [addError]);

  // エラーメッセージを表示
  const showError = useCallback((message: string, title?: string) => {
    addError({
      type: 'error',
      message,
      title
    });
  }, [addError]);

  // 例外からエラーメッセージを生成
  const handleError = useCallback((error: unknown, context?: string) => {
    console.error('Error occurred:', error, context ? `Context: ${context}` : '');

    if (error instanceof ValidationError) {
      addError({
        type: 'warning',
        message: getValidationErrorMessage(error),
        title: 'バリデーションエラー',
        field: error.field,
        code: error.code
      });
    } else if (error instanceof StorageError) {
      addError({
        type: 'error',
        message: getStorageErrorMessage(error),
        title: 'データ保存エラー',
        code: error.type
      });
    } else if (error instanceof Error) {
      addError({
        type: 'error',
        message: error.message,
        title: context ? `${context}でエラーが発生しました` : 'エラーが発生しました'
      });
    } else {
      addError({
        type: 'error',
        message: '予期しないエラーが発生しました',
        title: context ? `${context}でエラーが発生しました` : 'エラーが発生しました'
      });
    }
  }, [addError]);

  // 非同期処理のラッパー（ローディング状態とエラーハンドリングを自動化）
  const withErrorHandling = useCallback(async <T>(
    asyncFn: () => Promise<T>,
    context?: string,
    showLoadingSpinner = true
  ): Promise<T | null> => {
    try {
      if (showLoadingSpinner) {
        setIsLoading(true);
      }
      const result = await asyncFn();
      return result;
    } catch (error) {
      handleError(error, context);
      return null;
    } finally {
      if (showLoadingSpinner) {
        setIsLoading(false);
      }
    }
  }, [handleError]);

  return {
    errors,
    isLoading,
    addError,
    clearError,
    clearAllErrors,
    showSuccess,
    showInfo,
    showWarning,
    showError,
    handleError,
    withErrorHandling,
    setIsLoading
  };
};

// バリデーションエラーメッセージの生成
function getValidationErrorMessage(error: ValidationError): string {
  switch (error.code) {
    case 'title_required':
      return 'タイトルは必須です';
    case 'title_too_long':
      return 'タイトルは100文字以内で入力してください';
    case 'description_too_long':
      return '説明は500文字以内で入力してください';
    case 'time_required':
      return '開始時間と終了時間は必須です';
    case 'invalid_time_range':
      return '開始時間は終了時間より前に設定してください';
    case 'duration_too_short':
      return '予定は最低15分以上に設定してください';
    case 'past_time':
      return '過去の時間には予定を作成できません';
    case 'time_conflict':
      return error.message; // 具体的な競合情報を含む
    case 'event_not_found':
      return '指定された予定が見つかりません';
    default:
      return error.message;
  }
}

// ストレージエラーメッセージの生成
function getStorageErrorMessage(error: StorageError): string {
  switch (error.type) {
    case 'quota_exceeded':
      return 'ストレージの容量が不足しています。不要なデータを削除してください。';
    case 'access_denied':
      return 'データの保存が拒否されました。ブラウザの設定を確認してください。';
    case 'unavailable':
      return 'データの保存ができません。ブラウザがローカルストレージをサポートしていない可能性があります。';
    default:
      return 'データの保存中にエラーが発生しました。';
  }
}