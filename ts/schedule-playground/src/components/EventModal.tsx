import React, { useState, useEffect } from 'react';
import { ScheduleEvent, EventFormData } from '../types';
import { formatTime } from '../utils/dateUtils';
import { useErrorHandler } from '../hooks/useErrorHandler';
import LoadingSpinner from './LoadingSpinner';

interface EventModalProps {
  isOpen: boolean;
  event?: ScheduleEvent | null;
  startTime?: Date;
  endTime?: Date;
  onSave: (eventData: EventFormData) => void;
  onDelete?: (eventId: string) => void;
  onClose: () => void;
}

/**
 * 予定作成・編集モーダル
 * タイトルと内容を入力してスケジュールを作成・編集
 */
const EventModal: React.FC<EventModalProps> = ({
  isOpen,
  event,
  startTime,
  endTime,
  onSave,
  onDelete,
  onClose
}) => {
  const [formData, setFormData] = useState<EventFormData>({
    title: '',
    description: '',
    startTime: new Date(),
    endTime: new Date(),
    priority: 'medium'
  });

  const [localErrors, setLocalErrors] = useState<Record<string, string>>({});
  const { isLoading, withErrorHandling } = useErrorHandler();

  // モーダルが開かれた時にフォームデータを初期化
  useEffect(() => {
    if (isOpen) {
      if (event) {
        // 既存イベントの編集
        setFormData({
          title: event.title,
          description: event.description,
          startTime: new Date(event.startTime),
          endTime: new Date(event.endTime),
          priority: event.priority || 'medium',
          color: event.color,
          category: event.category,
          location: event.location
        });
      } else if (startTime && endTime) {
        // 新規イベントの作成
        setFormData({
          title: '',
          description: '',
          startTime: new Date(startTime),
          endTime: new Date(endTime),
          priority: 'medium'
        });
      }
      setLocalErrors({});
    }
  }, [isOpen, event, startTime, endTime]);

  // フォームデータの更新
  const handleInputChange = (field: keyof EventFormData, value: any) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
    
    // エラーをクリア
    if (localErrors[field]) {
      setLocalErrors(prev => ({
        ...prev,
        [field]: ''
      }));
    }
  };

  // 日時の更新
  const handleDateTimeChange = (field: 'startTime' | 'endTime', dateStr: string, timeStr: string) => {
    const [year, month, day] = dateStr.split('-').map(Number);
    const [hours, minutes] = timeStr.split(':').map(Number);
    
    const newDateTime = new Date(year, month - 1, day, hours, minutes);
    handleInputChange(field, newDateTime);
  };

  // バリデーション
  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.title.trim()) {
      newErrors.title = 'タイトルは必須です';
    } else if (formData.title.length > 100) {
      newErrors.title = 'タイトルは100文字以内で入力してください';
    }

    if (formData.description && formData.description.length > 500) {
      newErrors.description = '説明は500文字以内で入力してください';
    }

    if (formData.startTime >= formData.endTime) {
      newErrors.time = '開始時間は終了時間より前に設定してください';
    }

    const duration = formData.endTime.getTime() - formData.startTime.getTime();
    if (duration < 15 * 60 * 1000) {
      newErrors.time = '予定は最低15分以上に設定してください';
    }

    setLocalErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  // 保存処理
  const handleSave = async () => {
    if (!validateForm()) return;

    const result = await withErrorHandling(async () => {
      await onSave(formData);
      onClose();
    }, '予定の保存');
  };

  // 削除処理
  const handleDelete = async () => {
    if (!event || !onDelete) return;
    
    if (window.confirm('この予定を削除しますか？')) {
      const result = await withErrorHandling(async () => {
        await onDelete(event.id);
        onClose();
      }, '予定の削除');
    }
  };

  // キーボードイベント
  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      onClose();
    } else if (e.key === 'Enter' && (e.metaKey || e.ctrlKey)) {
      handleSave();
    }
  };

  if (!isOpen) return null;

  const formatDateForInput = (date: Date) => {
    return date.toISOString().split('T')[0];
  };

  const formatTimeForInput = (date: Date) => {
    return date.toTimeString().slice(0, 5);
  };

  return (
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal-content" onClick={e => e.stopPropagation()} onKeyDown={handleKeyDown}>
        <div className="modal-header">
          <h2>{event ? '予定を編集' : '新しい予定'}</h2>
          <button className="close-button" onClick={onClose}>×</button>
        </div>

        <div className="modal-body">
          {/* タイトル */}
          <div className="form-group">
            <label htmlFor="title">タイトル *</label>
            <input
              id="title"
              type="text"
              value={formData.title}
              onChange={e => handleInputChange('title', e.target.value)}
              placeholder="予定のタイトルを入力"
              className={localErrors.title ? 'error' : ''}
              autoFocus
            />
            {localErrors.title && <span className="error-message">{localErrors.title}</span>}
          </div>

          {/* 説明 */}
          <div className="form-group">
            <label htmlFor="description">説明</label>
            <textarea
              id="description"
              value={formData.description}
              onChange={e => handleInputChange('description', e.target.value)}
              placeholder="予定の詳細を入力（任意）"
              rows={3}
              className={localErrors.description ? 'error' : ''}
            />
            {localErrors.description && <span className="error-message">{localErrors.description}</span>}
          </div>

          {/* 日時設定 */}
          <div className="datetime-section">
            <div className="form-group">
              <label>開始日時</label>
              <div className="datetime-inputs">
                <input
                  type="date"
                  value={formatDateForInput(formData.startTime)}
                  onChange={e => handleDateTimeChange('startTime', e.target.value, formatTimeForInput(formData.startTime))}
                />
                <input
                  type="time"
                  value={formatTimeForInput(formData.startTime)}
                  onChange={e => handleDateTimeChange('startTime', formatDateForInput(formData.startTime), e.target.value)}
                />
              </div>
            </div>

            <div className="form-group">
              <label>終了日時</label>
              <div className="datetime-inputs">
                <input
                  type="date"
                  value={formatDateForInput(formData.endTime)}
                  onChange={e => handleDateTimeChange('endTime', e.target.value, formatTimeForInput(formData.endTime))}
                />
                <input
                  type="time"
                  value={formatTimeForInput(formData.endTime)}
                  onChange={e => handleDateTimeChange('endTime', formatDateForInput(formData.endTime), e.target.value)}
                />
              </div>
            </div>
            {localErrors.time && <span className="error-message">{localErrors.time}</span>}
          </div>

          {/* 優先度 */}
          <div className="form-group">
            <label htmlFor="priority">優先度</label>
            <select
              id="priority"
              value={formData.priority}
              onChange={e => handleInputChange('priority', e.target.value as 'low' | 'medium' | 'high')}
            >
              <option value="low">低</option>
              <option value="medium">中</option>
              <option value="high">高</option>
            </select>
          </div>

          {/* カテゴリ */}
          <div className="form-group">
            <label htmlFor="category">カテゴリ</label>
            <input
              id="category"
              type="text"
              value={formData.category || ''}
              onChange={e => handleInputChange('category', e.target.value)}
              placeholder="仕事、プライベートなど"
            />
          </div>

          {/* 場所 */}
          <div className="form-group">
            <label htmlFor="location">場所</label>
            <input
              id="location"
              type="text"
              value={formData.location || ''}
              onChange={e => handleInputChange('location', e.target.value)}
              placeholder="会議室、住所など"
            />
          </div>
        </div>

        <div className="modal-footer">
          <div className="footer-left">
            {event && onDelete && (
              <button
                className="delete-button"
                onClick={handleDelete}
                disabled={isLoading}
              >
                削除
              </button>
            )}
          </div>
          <div className="footer-right">
            <button
              className="cancel-button"
              onClick={onClose}
              disabled={isLoading}
            >
              キャンセル
            </button>
            <button
              className="save-button"
              onClick={handleSave}
              disabled={isLoading || !formData.title.trim()}
            >
              {isLoading ? '保存中...' : '保存'}
            </button>
          </div>
        </div>
      </div>

      <style>{`
        .modal-overlay {
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background: rgba(0, 0, 0, 0.5);
          display: flex;
          align-items: center;
          justify-content: center;
          z-index: 1000;
          padding: 20px;
        }

        .modal-content {
          background: white;
          border-radius: 12px;
          box-shadow: 0 10px 25px rgba(0, 0, 0, 0.2);
          width: 100%;
          max-width: 500px;
          max-height: 90vh;
          overflow: hidden;
          display: flex;
          flex-direction: column;
        }

        .modal-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 20px 24px;
          border-bottom: 1px solid #e0e0e0;
        }

        .modal-header h2 {
          margin: 0;
          font-size: 20px;
          font-weight: 600;
          color: #202124;
        }

        .close-button {
          background: none;
          border: none;
          font-size: 24px;
          cursor: pointer;
          color: #5f6368;
          padding: 4px;
          border-radius: 4px;
          transition: all 0.2s ease;
        }

        .close-button:hover {
          background: #f8f9fa;
          color: #202124;
        }

        .modal-body {
          padding: 24px;
          overflow-y: auto;
          flex: 1;
        }

        .form-group {
          margin-bottom: 20px;
        }

        .form-group label {
          display: block;
          margin-bottom: 6px;
          font-weight: 500;
          color: #202124;
          font-size: 14px;
        }

        .form-group input,
        .form-group textarea,
        .form-group select {
          width: 100%;
          padding: 10px 12px;
          border: 1px solid #dadce0;
          border-radius: 6px;
          font-size: 14px;
          transition: all 0.2s ease;
          box-sizing: border-box;
        }

        .form-group input:focus,
        .form-group textarea:focus,
        .form-group select:focus {
          outline: none;
          border-color: #1a73e8;
          box-shadow: 0 0 0 2px rgba(26, 115, 232, 0.2);
        }

        .form-group input.error,
        .form-group textarea.error {
          border-color: #ea4335;
        }

        .form-group textarea {
          resize: vertical;
          min-height: 80px;
        }

        .datetime-section {
          background: #f8f9fa;
          padding: 16px;
          border-radius: 8px;
          margin-bottom: 20px;
        }

        .datetime-inputs {
          display: flex;
          gap: 12px;
        }

        .datetime-inputs input {
          flex: 1;
        }

        .error-message {
          color: #ea4335;
          font-size: 12px;
          margin-top: 4px;
          display: block;
        }

        .modal-footer {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 16px 24px;
          border-top: 1px solid #e0e0e0;
          background: #f8f9fa;
        }

        .footer-left,
        .footer-right {
          display: flex;
          gap: 12px;
        }

        .delete-button {
          background: #ea4335;
          color: white;
          border: none;
          padding: 10px 20px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 14px;
          font-weight: 500;
          transition: all 0.2s ease;
        }

        .delete-button:hover:not(:disabled) {
          background: #d33b2c;
        }

        .cancel-button {
          background: transparent;
          color: #5f6368;
          border: 1px solid #dadce0;
          padding: 10px 20px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 14px;
          font-weight: 500;
          transition: all 0.2s ease;
        }

        .cancel-button:hover:not(:disabled) {
          background: #f8f9fa;
          border-color: #5f6368;
        }

        .save-button {
          background: #1a73e8;
          color: white;
          border: none;
          padding: 10px 20px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 14px;
          font-weight: 500;
          transition: all 0.2s ease;
        }

        .save-button:hover:not(:disabled) {
          background: #1557b0;
        }

        .save-button:disabled,
        .delete-button:disabled,
        .cancel-button:disabled {
          opacity: 0.6;
          cursor: not-allowed;
        }

        /* Tablet responsive styles */
        @media (max-width: 1024px) {
          .modal-overlay {
            padding: 16px;
          }

          .modal-content {
            max-width: 480px;
          }
        }

        /* Mobile responsive styles */
        @media (max-width: 768px) {
          .modal-overlay {
            padding: 8px;
            align-items: flex-start;
            padding-top: 20px;
          }

          .modal-content {
            margin: 0;
            max-width: none;
            max-height: calc(100vh - 40px);
            border-radius: 8px;
          }

          .modal-header {
            padding: 16px 20px;
          }

          .modal-header h2 {
            font-size: 18px;
          }

          .modal-body {
            padding: 20px;
          }

          .form-group {
            margin-bottom: 16px;
          }

          .datetime-section {
            padding: 12px;
            margin-bottom: 16px;
          }

          .datetime-inputs {
            flex-direction: column;
            gap: 8px;
          }

          .modal-footer {
            padding: 12px 20px;
            flex-direction: column;
            gap: 12px;
          }

          .footer-left,
          .footer-right {
            width: 100%;
            justify-content: center;
          }

          .delete-button,
          .cancel-button,
          .save-button {
            width: 100%;
            padding: 12px 20px;
            min-height: 44px; /* Touch-friendly */
          }
        }

        @media (max-width: 480px) {
          .modal-overlay {
            padding: 4px;
            padding-top: 10px;
          }

          .modal-content {
            max-height: calc(100vh - 20px);
            border-radius: 4px;
          }

          .modal-header {
            padding: 12px 16px;
          }

          .modal-header h2 {
            font-size: 16px;
          }

          .modal-body {
            padding: 16px;
          }

          .form-group {
            margin-bottom: 12px;
          }

          .form-group label {
            font-size: 13px;
          }

          .form-group input,
          .form-group textarea,
          .form-group select {
            padding: 8px 10px;
            font-size: 13px;
          }

          .datetime-section {
            padding: 10px;
            margin-bottom: 12px;
          }

          .modal-footer {
            padding: 10px 16px;
          }

          .delete-button,
          .cancel-button,
          .save-button {
            padding: 10px 16px;
            font-size: 13px;
            min-height: 40px;
          }
        }

        /* Touch device optimizations */
        @media (hover: none) and (pointer: coarse) {
          .close-button {
            padding: 8px;
            min-width: 44px;
            min-height: 44px;
          }

          .close-button:hover {
            background: inherit;
          }

          .close-button:active {
            background: #f8f9fa;
          }

          .delete-button,
          .cancel-button,
          .save-button {
            min-height: 48px;
          }

          .delete-button:hover:not(:disabled),
          .cancel-button:hover:not(:disabled),
          .save-button:hover:not(:disabled) {
            background: inherit;
          }

          .delete-button:active:not(:disabled) {
            background: #d33b2c;
            transform: scale(0.98);
          }

          .cancel-button:active:not(:disabled) {
            background: #f8f9fa;
            transform: scale(0.98);
          }

          .save-button:active:not(:disabled) {
            background: #1557b0;
            transform: scale(0.98);
          }
        }
      `}</style>
    </div>
  );
};

export default EventModal;