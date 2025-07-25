import { ScheduleEvent } from '../types';

/**
 * LocalStorageベースのデータ永続化サービス
 * 予定データの保存・読み込み・削除機能を提供
 */
export class StorageManager {
  private readonly STORAGE_KEY = 'schedule-timeline-events';
  private readonly STORAGE_VERSION = '1.0';
  private readonly VERSION_KEY = 'schedule-timeline-version';

  /**
   * LocalStorageが利用可能かチェック
   */
  isAvailable(): boolean {
    try {
      const testKey = '__storage_test__';
      localStorage.setItem(testKey, 'test');
      localStorage.removeItem(testKey);
      return true;
    } catch (error) {
      return false;
    }
  }

  /**
   * 予定データを保存
   */
  async save(events: ScheduleEvent[]): Promise<void> {
    try {
      if (!this.isAvailable()) {
        throw new StorageError('unavailable', 'LocalStorage is not available');
      }

      const data = {
        version: this.STORAGE_VERSION,
        events: events,
        timestamp: new Date().toISOString()
      };

      const serializedData = JSON.stringify(data);
      
      // ストレージ容量チェック
      try {
        localStorage.setItem(this.STORAGE_KEY, serializedData);
        localStorage.setItem(this.VERSION_KEY, this.STORAGE_VERSION);
      } catch (error) {
        if (error instanceof DOMException && error.code === 22) {
          throw new StorageError('quota_exceeded', 'Storage quota exceeded');
        }
        throw error;
      }
    } catch (error) {
      if (error instanceof StorageError) {
        throw error;
      }
      throw new StorageError('unavailable', `Failed to save data: ${error}`);
    }
  }

  /**
   * 予定データを読み込み
   */
  async load(): Promise<ScheduleEvent[]> {
    try {
      if (!this.isAvailable()) {
        throw new StorageError('unavailable', 'LocalStorage is not available');
      }

      const serializedData = localStorage.getItem(this.STORAGE_KEY);
      if (!serializedData) {
        return []; // 初回起動時は空配列を返す
      }

      const data = JSON.parse(serializedData);
      
      // バージョンチェック
      if (data.version !== this.STORAGE_VERSION) {
        console.warn(`Storage version mismatch. Expected: ${this.STORAGE_VERSION}, Found: ${data.version}`);
        // 将来的にマイグレーション処理を追加可能
      }

      // データの妥当性チェック
      if (!Array.isArray(data.events)) {
        throw new Error('Invalid data format: events must be an array');
      }

      // 日付文字列をDateオブジェクトに変換
      const events: ScheduleEvent[] = data.events.map((event: any) => ({
        ...event,
        startTime: new Date(event.startTime),
        endTime: new Date(event.endTime),
        createdAt: new Date(event.createdAt),
        updatedAt: new Date(event.updatedAt)
      }));

      return events;
    } catch (error) {
      if (error instanceof StorageError) {
        throw error;
      }
      throw new StorageError('unavailable', `Failed to load data: ${error}`);
    }
  }

  /**
   * 特定の予定を削除
   */
  async remove(eventId: string): Promise<void> {
    try {
      const events = await this.load();
      const filteredEvents = events.filter(event => event.id !== eventId);
      await this.save(filteredEvents);
    } catch (error) {
      if (error instanceof StorageError) {
        throw error;
      }
      throw new StorageError('unavailable', `Failed to remove event: ${error}`);
    }
  }

  /**
   * 全データを削除
   */
  async clear(): Promise<void> {
    try {
      if (!this.isAvailable()) {
        throw new StorageError('unavailable', 'LocalStorage is not available');
      }

      localStorage.removeItem(this.STORAGE_KEY);
      localStorage.removeItem(this.VERSION_KEY);
    } catch (error) {
      throw new StorageError('unavailable', `Failed to clear data: ${error}`);
    }
  }

  /**
   * ストレージ使用量情報を取得（デバッグ用）
   */
  getStorageInfo(): { used: number; available: boolean } {
    try {
      if (!this.isAvailable()) {
        return { used: 0, available: false };
      }

      const data = localStorage.getItem(this.STORAGE_KEY);
      const used = data ? new Blob([data]).size : 0;
      
      return {
        used,
        available: true
      };
    } catch (error) {
      return { used: 0, available: false };
    }
  }
}

// StorageErrorクラスの実装
export class StorageError extends Error {
  public readonly type: 'quota_exceeded' | 'access_denied' | 'unavailable';

  constructor(type: 'quota_exceeded' | 'access_denied' | 'unavailable', message: string) {
    super(message);
    this.name = 'StorageError';
    this.type = type;
  }
}

// シングルトンインスタンスをエクスポート
export const storageManager = new StorageManager();