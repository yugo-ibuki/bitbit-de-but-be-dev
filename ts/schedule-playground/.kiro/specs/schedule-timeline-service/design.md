# 設計文書

## 概要

スケジュール管理サービスは、既存のReact/TypeScriptプロジェクトを拡張し、タイムライン型のスケジュール表示機能を追加します。月・週・日の3つの表示粒度を持ち、直感的なドラッグ&ドロップ操作で予定を管理できるインタラクティブなUIを提供します。

## アーキテクチャ

### システム構成

```
┌─────────────────────────────────────────┐
│              Presentation Layer          │
├─────────────────────────────────────────┤
│ TimelineView │ ViewSwitcher │ Navigation │
├─────────────────────────────────────────┤
│              Business Logic Layer        │
├─────────────────────────────────────────┤
│ ScheduleManager │ EventHandler │ Utils   │
├─────────────────────────────────────────┤
│              Data Layer                  │
├─────────────────────────────────────────┤
│ LocalStorage │ StateManager │ Types      │
└─────────────────────────────────────────┘
```

### 技術スタック

- **フロントエンド**: React 18.2.0 + TypeScript 5.0.0
- **状態管理**: React Hooks (useState, useReducer, useContext)
- **データ永続化**: Browser LocalStorage API
- **スタイリング**: CSS Modules + CSS Grid/Flexbox
- **日付処理**: Native Date API

## コンポーネント設計

### 1. TimelineContainer (メインコンテナ)

**責務**: 全体の状態管理とレイアウト制御

```typescript
interface TimelineContainerProps {
  initialDate?: Date;
  initialView?: ViewType;
}

interface TimelineState {
  currentDate: Date;
  viewType: ViewType;
  events: ScheduleEvent[];
  selectedEvent: ScheduleEvent | null;
}
```

### 2. ViewSwitcher (表示切り替え)

**責務**: 月・週・日の表示モード切り替え

```typescript
interface ViewSwitcherProps {
  currentView: ViewType;
  onViewChange: (view: ViewType) => void;
}

type ViewType = 'month' | 'week' | 'day';
```

### 3. TimelineGrid (タイムライン表示)

**責務**: 各ビューに応じたグリッド表示とイベント配置、ドラッグアンドドロップによる時間範囲選択

```typescript
interface TimelineGridProps {
  viewType: ViewType;
  currentDate: Date;
  events: ScheduleEvent[];
  onEventCreate: (timeSlot: TimeSlot) => void;
  onEventUpdate: (event: ScheduleEvent) => void;
  onEventDelete: (eventId: string) => void;
  onTimeRangeSelect: (startTime: Date, endTime: Date) => void; // ドラッグ選択
}

// ドラッグアンドドロップ機能
interface DragSelection {
  isSelecting: boolean;
  startTime: Date | null;
  endTime: Date | null;
  startPosition: { x: number; y: number } | null;
  currentPosition: { x: number; y: number } | null;
}

interface TimeSlotElement {
  time: Date;
  element: HTMLElement;
  bounds: DOMRect;
}
```

### 4. EventModal (予定編集)

**責務**: 予定の作成・編集フォーム（タイトルと内容の入力、将来の拡張対応）

```typescript
interface EventModalProps {
  event?: ScheduleEvent;
  timeSlot: TimeSlot;
  isOpen: boolean;
  onSave: (event: Omit<ScheduleEvent, 'id' | 'createdAt' | 'updatedAt'>) => void;
  onClose: () => void;
}

// フォームの基本フィールド（現在実装）
interface BasicEventForm {
  title: string;
  description: string;
  startTime: Date;
  endTime: Date;
}

// 拡張フィールド（将来実装）
interface ExtendedEventForm extends BasicEventForm {
  color?: string;
  category?: string;
  isAllDay?: boolean;
  priority?: 'low' | 'medium' | 'high';
  location?: string;
  attendees?: string[];
  tags?: string[];
}
```

### 5. NavigationControls (ナビゲーション)

**責務**: 日付の前後移動と「今日」ボタン

```typescript
interface NavigationControlsProps {
  currentDate: Date;
  viewType: ViewType;
  onDateChange: (date: Date) => void;
  onTodayClick: () => void;
}
```

## データモデル

### ScheduleEvent (予定データ)

```typescript
interface ScheduleEvent {
  id: string;
  title: string;
  description: string;
  startTime: Date;
  endTime: Date;
  // 拡張性のための追加フィールド
  color?: string;
  category?: string;
  isAllDay?: boolean;
  priority?: 'low' | 'medium' | 'high';
  location?: string;
  attendees?: string[];
  recurrence?: RecurrenceRule;
  tags?: string[];
  customFields?: Record<string, any>; // 将来の拡張用
  createdAt: Date;
  updatedAt: Date;
}

// 将来の拡張用インターフェース
interface RecurrenceRule {
  frequency: 'daily' | 'weekly' | 'monthly' | 'yearly';
  interval: number;
  endDate?: Date;
  count?: number;
}
```

### TimeSlot (時間スロット)

```typescript
interface TimeSlot {
  start: Date;
  end: Date;
  viewType: ViewType;
  gridPosition: {
    row: number;
    column: number;
  };
}
```

### ViewConfiguration (表示設定)

```typescript
interface ViewConfiguration {
  month: {
    daysPerRow: 7;
    weeksToShow: 6;
    showTimeSlots: false;
  };
  week: {
    daysPerRow: 7;
    hoursPerDay: 24;
    timeSlotDuration: 60; // minutes
  };
  day: {
    hoursPerDay: 24;
    timeSlotDuration: 30; // minutes
    showHalfHours: true;
  };
}
```

## ドラッグアンドドロップ機能設計

### 1. 時間範囲選択機能

**ユーザー操作フロー:**
1. ユーザーが特定の時間スロット（例：10:00）でマウスダウン
2. ドラッグして別の時間スロット（例：12:00）まで移動
3. マウスアップで時間範囲が確定（10:00-12:00）
4. 自動的にイベント作成モーダルが表示
5. タイトルと内容を入力してスケジュール作成

### 2. ドラッグ選択の視覚的フィードバック

```typescript
interface DragVisualFeedback {
  selectionOverlay: {
    backgroundColor: 'rgba(102, 126, 234, 0.2)';
    border: '2px solid #667eea';
    borderRadius: '4px';
  };
  timeIndicator: {
    position: 'absolute';
    backgroundColor: '#667eea';
    color: 'white';
    padding: '4px 8px';
    borderRadius: '4px';
    fontSize: '12px';
  };
}
```

### 3. 時間スロット計算

```typescript
interface TimeSlotCalculator {
  // マウス位置から時間を計算
  calculateTimeFromPosition(
    mouseY: number, 
    containerBounds: DOMRect, 
    viewType: ViewType
  ): Date;
  
  // 時間範囲を30分単位にスナップ
  snapToTimeSlot(time: Date, slotDuration: number): Date;
  
  // ドラッグ範囲の妥当性チェック
  validateTimeRange(startTime: Date, endTime: Date): boolean;
}
```

### 4. イベント作成フロー

```typescript
interface EventCreationFlow {
  // ドラッグ完了時の処理
  onDragComplete: (startTime: Date, endTime: Date) => void;
  
  // モーダル表示
  showEventModal: (timeSlot: TimeSlot) => void;
  
  // イベント保存
  saveEvent: (eventData: BasicEventForm) => Promise<void>;
}
```

## インタフェース設計

### 1. ScheduleService (データ操作)

```typescript
interface ScheduleService {
  getEvents(dateRange: DateRange): Promise<ScheduleEvent[]>;
  createEvent(event: Omit<ScheduleEvent, 'id' | 'createdAt' | 'updatedAt'>): Promise<ScheduleEvent>;
  updateEvent(id: string, updates: Partial<ScheduleEvent>): Promise<ScheduleEvent>;
  deleteEvent(id: string): Promise<void>;
  saveToStorage(): Promise<void>;
  loadFromStorage(): Promise<ScheduleEvent[]>;
}
```

### 2. TimelineRenderer (表示制御)

```typescript
interface TimelineRenderer {
  renderMonthView(date: Date, events: ScheduleEvent[]): JSX.Element;
  renderWeekView(date: Date, events: ScheduleEvent[]): JSX.Element;
  renderDayView(date: Date, events: ScheduleEvent[]): JSX.Element;
  calculateEventPosition(event: ScheduleEvent, viewType: ViewType): GridPosition;
  handleEventDrop(event: ScheduleEvent, newTimeSlot: TimeSlot): void;
}
```

### 3. StorageManager (永続化)

```typescript
interface StorageManager {
  save<T>(key: string, data: T): Promise<void>;
  load<T>(key: string): Promise<T | null>;
  remove(key: string): Promise<void>;
  clear(): Promise<void>;
  isAvailable(): boolean;
}
```

## エラーハンドリング

### エラー分類

1. **データ保存エラー**: LocalStorage容量不足、アクセス拒否
2. **バリデーションエラー**: 不正な日付、重複する予定
3. **UI操作エラー**: 無効なドラッグ&ドロップ、範囲外の日付選択

### エラー処理戦略

```typescript
interface ErrorHandler {
  handleStorageError(error: StorageError): void;
  handleValidationError(error: ValidationError): void;
  handleUIError(error: UIError): void;
  showErrorMessage(message: string, type: ErrorType): void;
}

type ErrorType = 'warning' | 'error' | 'info';
```

## テスト戦略

### 1. 単体テスト

- **コンポーネントテスト**: React Testing Library
- **ロジックテスト**: Jest
- **カバレッジ目標**: 80%以上

### 2. 統合テスト

- **ユーザーフローテスト**: 予定作成から削除まで
- **ビュー切り替えテスト**: 月→週→日の表示切り替え
- **データ永続化テスト**: LocalStorage読み書き

### 3. E2Eテスト

- **クリティカルパステスト**: 主要機能の動作確認
- **レスポンシブテスト**: 異なる画面サイズでの動作確認

## パフォーマンス考慮事項

### 1. レンダリング最適化

- **React.memo**: 不要な再レンダリング防止
- **useMemo/useCallback**: 計算結果とコールバック関数のメモ化
- **仮想化**: 大量のイベント表示時のパフォーマンス向上

### 2. データ処理最適化

- **遅延読み込み**: 表示範囲外のデータは必要時に読み込み
- **キャッシュ戦略**: 頻繁にアクセスするデータのメモリキャッシュ
- **バッチ処理**: 複数の操作をまとめて実行

### 3. ストレージ最適化

- **データ圧縮**: JSON圧縮によるストレージ使用量削減
- **定期クリーンアップ**: 古いデータの自動削除
- **容量監視**: ストレージ使用量の監視とアラート

## セキュリティ考慮事項

### 1. データ保護

- **入力サニタイゼーション**: XSS攻撃防止
- **データバリデーション**: 不正なデータの検証
- **ローカルストレージ暗号化**: 機密データの暗号化（将来拡張）

### 2. プライバシー

- **データローカル保存**: サーバーへのデータ送信なし
- **ブラウザ間分離**: 他のサイトからのアクセス防止