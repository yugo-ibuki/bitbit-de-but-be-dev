import React, { useState, useEffect, useCallback } from 'react';
import { ViewType, ScheduleEvent, TimelineState, EventFormData } from '../types';
import { scheduleService } from '../services/scheduleService';
import { getDateRangeForView, formatDate } from '../utils/dateUtils';
import { useErrorHandler } from '../hooks/useErrorHandler';
import ViewSwitcher from './ViewSwitcher';
import NavigationControls from './NavigationControls';
import WeekView from './WeekView';
import MonthView from './MonthView';
import DayView from './DayView';
import EventModal from './EventModal';
import NotificationContainer from './NotificationContainer';
import LoadingSpinner from './LoadingSpinner';
import '../styles/timeline.css';

interface TimelineContainerProps {
  initialDate?: Date;
  initialView?: ViewType;
}

/**
 * Google Calendar風のタイムラインメインコンテナ
 * 全体の状態管理とレイアウト制御を担当
 */
const TimelineContainer: React.FC<TimelineContainerProps> = ({
  initialDate = new Date(),
  initialView = 'week'
}) => {
  const [timelineState, setTimelineState] = useState<TimelineState>({
    currentDate: initialDate,
    viewType: initialView,
    events: [],
    selectedEvent: null
  });

  const { 
    errors, 
    isLoading, 
    clearError, 
    handleError, 
    withErrorHandling, 
    showSuccess,
    setIsLoading 
  } = useErrorHandler();
  
  // モーダル状態
  const [modalState, setModalState] = useState<{
    isOpen: boolean;
    event?: ScheduleEvent | null;
    startTime?: Date;
    endTime?: Date;
  }>({
    isOpen: false,
    event: null
  });

  // ScheduleServiceの初期化
  useEffect(() => {
    const initializeService = async () => {
      const result = await withErrorHandling(async () => {
        await scheduleService.initialize();
        // 初期化後にイベントを読み込み
        const dateRange = getDateRangeForView(timelineState.currentDate, timelineState.viewType);
        const events = await scheduleService.getEvents(dateRange);
        setTimelineState(prev => ({ ...prev, events }));
      }, 'データの初期化');
    };

    initializeService();
  }, [timelineState.currentDate, timelineState.viewType, withErrorHandling]);

  // イベントの読み込み
  const loadEvents = useCallback(async () => {
    const result = await withErrorHandling(async () => {
      const dateRange = getDateRangeForView(timelineState.currentDate, timelineState.viewType);
      const events = await scheduleService.getEvents(dateRange);
      
      setTimelineState(prev => ({
        ...prev,
        events
      }));
    }, '予定の読み込み', false);
  }, [timelineState.currentDate, timelineState.viewType, withErrorHandling]);

  // 日付やビューが変更された時にイベントを再読み込み
  useEffect(() => {
    if (!isLoading) {
      loadEvents();
    }
  }, [timelineState.currentDate, timelineState.viewType, loadEvents, isLoading]);

  // ビュー切り替え
  const handleViewChange = useCallback((newView: ViewType) => {
    setTimelineState(prev => ({
      ...prev,
      viewType: newView,
      selectedEvent: null
    }));
  }, []);

  // 日付変更
  const handleDateChange = useCallback((newDate: Date) => {
    setTimelineState(prev => ({
      ...prev,
      currentDate: newDate,
      selectedEvent: null
    }));
  }, []);

  // 今日ボタン
  const handleTodayClick = useCallback(() => {
    handleDateChange(new Date());
  }, [handleDateChange]);



  // イベント作成（ドラッグアンドドロップ用）
  const handleEventCreate = useCallback((startTime: Date, endTime: Date) => {
    // ドラッグアンドドロップ後にモーダルを表示
    setModalState({
      isOpen: true,
      event: null,
      startTime,
      endTime
    });
  }, []);

  // イベント更新
  const handleEventUpdate = useCallback(async (eventId: string, updates: Partial<ScheduleEvent>) => {
    const result = await withErrorHandling(async () => {
      await scheduleService.updateEvent(eventId, updates);
      await loadEvents();
    }, '予定の更新');
  }, [loadEvents, withErrorHandling]);

  // イベント削除
  const handleEventDelete = useCallback(async (eventId: string) => {
    const result = await withErrorHandling(async () => {
      await scheduleService.deleteEvent(eventId);
      await loadEvents();
      
      // 削除されたイベントが選択されていた場合は選択を解除
      setTimelineState(prev => ({
        ...prev,
        selectedEvent: prev.selectedEvent?.id === eventId ? null : prev.selectedEvent
      }));
      
      showSuccess('予定を削除しました');
    }, '予定の削除');
  }, [loadEvents, withErrorHandling, showSuccess]);

  // モーダルでの予定保存
  const handleModalSave = useCallback(async (eventData: EventFormData) => {
    const result = await withErrorHandling(async () => {
      if (modalState.event) {
        // 既存イベントの更新
        await scheduleService.updateEvent(modalState.event.id, eventData);
        showSuccess('予定を更新しました');
      } else {
        // 新規イベントの作成
        await scheduleService.createEvent(eventData);
        showSuccess('予定を作成しました');
      }
      
      // イベントリストを更新
      await loadEvents();
      
      // モーダルを閉じる
      setModalState({ isOpen: false, event: null });
    }, modalState.event ? '予定の更新' : '予定の作成');
  }, [modalState.event, loadEvents, withErrorHandling, showSuccess]);

  // モーダルでの予定削除
  const handleModalDelete = useCallback(async (eventId: string) => {
    const result = await withErrorHandling(async () => {
      await scheduleService.deleteEvent(eventId);
      await loadEvents();
      
      // モーダルを閉じる
      setModalState({ isOpen: false, event: null });
      
      // 削除されたイベントが選択されていた場合は選択を解除
      setTimelineState(prev => ({
        ...prev,
        selectedEvent: prev.selectedEvent?.id === eventId ? null : prev.selectedEvent
      }));
      
      showSuccess('予定を削除しました');
    }, '予定の削除');
  }, [loadEvents, withErrorHandling, showSuccess]);

  // モーダルを閉じる
  const handleModalClose = useCallback(() => {
    setModalState({ isOpen: false, event: null });
  }, []);

  // イベントクリック（編集用）
  const handleEventClick = useCallback((event: ScheduleEvent) => {
    setModalState({
      isOpen: true,
      event,
      startTime: undefined,
      endTime: undefined
    });
  }, []);

  if (isLoading) {
    return (
      <div className="timeline-container loading">
        <LoadingSpinner 
          size="large" 
          message="データを読み込み中..." 
        />
        
        <style>{`
          .timeline-container.loading {
            display: flex;
            justify-content: center;
            align-items: center;
            height: 400px;
            background: #f8f9fa;
            border-radius: 8px;
          }

          @media (max-width: 768px) {
            .timeline-container.loading {
              height: 300px;
              margin: 8px;
              border-radius: 4px;
            }
          }

          @media (max-width: 480px) {
            .timeline-container.loading {
              height: 250px;
              margin: 4px;
            }
          }
        `}</style>
      </div>
    );
  }

  return (
    <div className="timeline-container">
      {/* ヘッダー */}
      <div className="timeline-header">
        <div className="header-navigation">
          <NavigationControls
            currentDate={timelineState.currentDate}
            viewType={timelineState.viewType}
            onDateChange={handleDateChange}
            onTodayClick={handleTodayClick}
          />
        </div>
        
        <div className="header-view-switcher">
          <ViewSwitcher 
            currentView={timelineState.viewType}
            onViewChange={handleViewChange}
          />
        </div>
      </div>

      {/* 通知表示 */}
      <NotificationContainer
        errors={errors}
        onClearError={clearError}
        position="top-right"
      />

      {/* メインコンテンツエリア */}
      <div className="timeline-content">
        {timelineState.viewType === 'month' ? (
          <MonthView
            currentDate={timelineState.currentDate}
            events={timelineState.events}
            onEventCreate={handleEventCreate}
            onEventClick={handleEventClick}
          />
        ) : timelineState.viewType === 'week' ? (
          <WeekView
            currentDate={timelineState.currentDate}
            events={timelineState.events}
            onEventCreate={handleEventCreate}
            onEventUpdate={handleEventUpdate}
            onEventDelete={handleEventDelete}
            onEventClick={handleEventClick}
          />
        ) : timelineState.viewType === 'day' ? (
          <DayView
            currentDate={timelineState.currentDate}
            events={timelineState.events}
            onEventCreate={handleEventCreate}
            onEventUpdate={handleEventUpdate}
            onEventDelete={handleEventDelete}
            onEventClick={handleEventClick}
          />
        ) : (
          <div className="timeline-placeholder">
            <div className="placeholder-content">
              <h3>🚧 {timelineState.viewType}表示を実装中</h3>
              <p>現在の日付: {formatDate(timelineState.currentDate)}</p>
              <p>読み込まれた予定: {timelineState.events.length}件</p>
            </div>
          </div>
        )}
      </div>

      {/* イベント作成・編集モーダル */}
      <EventModal
        isOpen={modalState.isOpen}
        event={modalState.event}
        startTime={modalState.startTime}
        endTime={modalState.endTime}
        onSave={handleModalSave}
        onDelete={handleModalDelete}
        onClose={handleModalClose}
      />


    </div>
  );
};

export default TimelineContainer;