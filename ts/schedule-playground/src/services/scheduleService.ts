import { ScheduleEvent, DateRange } from '../types';
import { StorageManager, StorageError, storageManager } from '../utils/storageManager';

/**
 * 予定管理のコアロジック
 * CRUD操作、データバリデーション、StorageManagerとの統合を提供
 */
export class ScheduleService {
  private storageManager: StorageManager;
  private events: ScheduleEvent[] = [];

  constructor(storageManager: StorageManager) {
    this.storageManager = storageManager;
  }

  /**
   * 初期化 - ストレージからデータを読み込み
   */
  async initialize(): Promise<void> {
    try {
      this.events = await this.storageManager.load();
    } catch (error) {
      console.error('Failed to initialize ScheduleService:', error);
      this.events = [];
    }
  }

  /**
   * 指定された日付範囲の予定を取得
   */
  async getEvents(dateRange?: DateRange): Promise<ScheduleEvent[]> {
    if (!dateRange) {
      return [...this.events];
    }

    return this.events.filter(event => {
      const eventStart = new Date(event.startTime);
      const eventEnd = new Date(event.endTime);
      
      // イベントが指定範囲と重複するかチェック
      return (
        (eventStart >= dateRange.start && eventStart <= dateRange.end) ||
        (eventEnd >= dateRange.start && eventEnd <= dateRange.end) ||
        (eventStart <= dateRange.start && eventEnd >= dateRange.end)
      );
    });
  }

  /**
   * 新しい予定を作成
   */
  async createEvent(
    eventData: Omit<ScheduleEvent, 'id' | 'createdAt' | 'updatedAt'>
  ): Promise<ScheduleEvent> {
    // バリデーション
    this.validateEventData(eventData);

    const now = new Date();
    const newEvent: ScheduleEvent = {
      ...eventData,
      id: this.generateId(),
      createdAt: now,
      updatedAt: now
    };

    // 時間の重複チェック
    this.validateTimeConflict(newEvent);

    this.events.push(newEvent);
    await this.saveToStorage();

    return newEvent;
  }

  /**
   * 既存の予定を更新
   */
  async updateEvent(id: string, updates: Partial<ScheduleEvent>): Promise<ScheduleEvent> {
    const eventIndex = this.events.findIndex(event => event.id === id);
    if (eventIndex === -1) {
      throw new ValidationError('event_not_found', `Event with id ${id} not found`, 'id');
    }

    const existingEvent = this.events[eventIndex];
    const updatedEvent: ScheduleEvent = {
      ...existingEvent,
      ...updates,
      id: existingEvent.id, // IDは変更不可
      createdAt: existingEvent.createdAt, // 作成日時は変更不可
      updatedAt: new Date()
    };

    // バリデーション
    this.validateEventData(updatedEvent);

    // 時間の重複チェック（自分自身は除外）
    this.validateTimeConflict(updatedEvent, id);

    this.events[eventIndex] = updatedEvent;
    await this.saveToStorage();

    return updatedEvent;
  }

  /**
   * 予定を削除
   */
  async deleteEvent(id: string): Promise<void> {
    const eventIndex = this.events.findIndex(event => event.id === id);
    if (eventIndex === -1) {
      throw new ValidationError('event_not_found', `Event with id ${id} not found`, 'id');
    }

    this.events.splice(eventIndex, 1);
    await this.saveToStorage();
  }

  /**
   * 特定の日付の予定を取得
   */
  async getEventsForDate(date: Date): Promise<ScheduleEvent[]> {
    const startOfDay = new Date(date);
    startOfDay.setHours(0, 0, 0, 0);
    
    const endOfDay = new Date(date);
    endOfDay.setHours(23, 59, 59, 999);

    return this.getEvents({ start: startOfDay, end: endOfDay });
  }

  /**
   * 時間範囲での予定検索（ドラッグアンドドロップ用）
   */
  async findEventsInTimeRange(startTime: Date, endTime: Date): Promise<ScheduleEvent[]> {
    return this.events.filter(event => {
      const eventStart = new Date(event.startTime);
      const eventEnd = new Date(event.endTime);
      
      // 時間範囲が重複するかチェック
      return (
        (eventStart < endTime && eventEnd > startTime)
      );
    });
  }

  /**
   * ドラッグアンドドロップでの予定作成用メソッド
   */
  async createEventFromTimeRange(
    startTime: Date, 
    endTime: Date, 
    title: string = '新しい予定', 
    description: string = ''
  ): Promise<ScheduleEvent> {
    // 時間の妥当性チェック
    if (startTime >= endTime) {
      throw new ValidationError(
        'invalid_time_range', 
        'Start time must be before end time', 
        'startTime'
      );
    }

    // 最小時間（15分）チェック
    const minDuration = 15 * 60 * 1000; // 15分をミリ秒で
    if (endTime.getTime() - startTime.getTime() < minDuration) {
      throw new ValidationError(
        'duration_too_short', 
        'Event duration must be at least 15 minutes', 
        'duration'
      );
    }

    return this.createEvent({
      title,
      description,
      startTime,
      endTime
    });
  }

  /**
   * ストレージに保存
   */
  private async saveToStorage(): Promise<void> {
    try {
      await this.storageManager.save(this.events);
    } catch (error) {
      if (error instanceof StorageError) {
        throw error;
      }
      throw new StorageError('unavailable', `Failed to save events: ${error}`);
    }
  }

  /**
   * イベントデータのバリデーション
   */
  private validateEventData(eventData: Partial<ScheduleEvent>): void {
    if (!eventData.title || eventData.title.trim().length === 0) {
      throw new ValidationError('title_required', 'Event title is required', 'title');
    }

    if (eventData.title.length > 100) {
      throw new ValidationError('title_too_long', 'Event title must be 100 characters or less', 'title');
    }

    if (!eventData.startTime || !eventData.endTime) {
      throw new ValidationError('time_required', 'Start time and end time are required', 'time');
    }

    if (eventData.startTime >= eventData.endTime) {
      throw new ValidationError('invalid_time_range', 'Start time must be before end time', 'time');
    }

    // 過去の日付チェック（現在時刻より1時間前まで許可）
    const oneHourAgo = new Date(Date.now() - 60 * 60 * 1000);
    if (eventData.startTime < oneHourAgo) {
      throw new ValidationError('past_time', 'Cannot create events in the past', 'startTime');
    }

    if (eventData.description && eventData.description.length > 500) {
      throw new ValidationError('description_too_long', 'Event description must be 500 characters or less', 'description');
    }
  }

  /**
   * 時間の重複チェック
   */
  private validateTimeConflict(newEvent: ScheduleEvent, excludeId?: string): void {
    const conflictingEvents = this.events.filter(event => {
      if (excludeId && event.id === excludeId) {
        return false; // 更新時は自分自身を除外
      }

      const eventStart = new Date(event.startTime);
      const eventEnd = new Date(event.endTime);
      const newStart = new Date(newEvent.startTime);
      const newEnd = new Date(newEvent.endTime);

      // 時間が重複するかチェック
      return (
        (newStart < eventEnd && newEnd > eventStart)
      );
    });

    if (conflictingEvents.length > 0) {
      throw new ValidationError(
        'time_conflict', 
        `Time conflicts with existing event: ${conflictingEvents[0].title}`, 
        'time'
      );
    }
  }

  /**
   * ユニークIDの生成
   */
  private generateId(): string {
    return `event_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`;
  }

  /**
   * デバッグ用：全イベントの取得
   */
  getAllEvents(): ScheduleEvent[] {
    return [...this.events];
  }

  /**
   * デバッグ用：イベント数の取得
   */
  getEventCount(): number {
    return this.events.length;
  }
}

// ValidationErrorクラスの実装
export class ValidationError extends Error {
  public readonly code: string;
  public readonly field: string;

  constructor(code: string, message: string, field: string) {
    super(message);
    this.name = 'ValidationError';
    this.code = code;
    this.field = field;
  }
}

// シングルトンインスタンスをエクスポート
export const scheduleService = new ScheduleService(storageManager);