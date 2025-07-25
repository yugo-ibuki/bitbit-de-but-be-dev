import { ViewType, DateRange, TimeSlot, GridPosition } from '../types';

/**
 * 日付・時間ユーティリティ関数
 * 日付範囲計算、時間スロット生成、グリッド位置計算などのヘルパー関数
 */

/**
 * 指定されたビュータイプに応じた日付範囲を取得
 */
export function getDateRangeForView(date: Date, viewType: ViewType): DateRange {
  const start = new Date(date);
  const end = new Date(date);

  switch (viewType) {
    case 'month':
      // 月の最初の日から最後の日まで
      start.setDate(1);
      start.setHours(0, 0, 0, 0);
      end.setMonth(end.getMonth() + 1, 0); // 次の月の0日 = 今月の最後の日
      end.setHours(23, 59, 59, 999);
      break;

    case 'week':
      // 週の最初の日（日曜日）から最後の日（土曜日）まで
      const dayOfWeek = start.getDay();
      start.setDate(start.getDate() - dayOfWeek);
      start.setHours(0, 0, 0, 0);
      end.setDate(start.getDate() + 6);
      end.setHours(23, 59, 59, 999);
      break;

    case 'day':
      // その日の0時から23時59分まで
      start.setHours(0, 0, 0, 0);
      end.setHours(23, 59, 59, 999);
      break;
  }

  return { start, end };
}

/**
 * 指定された日付範囲の日付配列を生成
 */
export function generateDateArray(startDate: Date, endDate: Date): Date[] {
  const dates: Date[] = [];
  const current = new Date(startDate);

  while (current <= endDate) {
    dates.push(new Date(current));
    current.setDate(current.getDate() + 1);
  }

  return dates;
}

/**
 * 時間スロットの配列を生成（ドラッグアンドドロップ用）
 */
export function generateTimeSlots(
  date: Date,
  viewType: ViewType,
  slotDuration: number = 30
): TimeSlot[] {
  const slots: TimeSlot[] = [];

  if (viewType === 'month') {
    // 月表示では時間スロットは生成しない
    return slots;
  }

  const startHour = 0;
  const endHour = 24;
  const slotsPerHour = 60 / slotDuration;

  for (let hour = startHour; hour < endHour; hour++) {
    for (let slot = 0; slot < slotsPerHour; slot++) {
      const minutes = slot * slotDuration;
      const start = new Date(date);
      start.setHours(hour, minutes, 0, 0);

      const end = new Date(start);
      end.setMinutes(end.getMinutes() + slotDuration);

      slots.push({
        start,
        end,
        viewType,
        gridPosition: {
          row: hour * slotsPerHour + slot,
          column: viewType === 'week' ? date.getDay() : 0
        }
      });
    }
  }

  return slots;
}

/**
 * マウス位置から時間を計算（ドラッグアンドドロップ用）
 */
export function calculateTimeFromMousePosition(
  mouseY: number,
  containerBounds: DOMRect,
  date: Date,
  viewType: ViewType,
  slotDuration: number = 30
): Date {
  // コンテナ内での相対位置を計算
  const relativeY = mouseY - containerBounds.top;
  const containerHeight = containerBounds.height;

  // 24時間を基準とした時間の割合を計算
  const timeRatio = Math.max(0, Math.min(1, relativeY / containerHeight));

  // 時間を計算（0-24時間）
  const totalMinutes = timeRatio * 24 * 60;

  // スロット単位にスナップ
  const snappedMinutes = Math.round(totalMinutes / slotDuration) * slotDuration;

  const resultTime = new Date(date);
  resultTime.setHours(0, 0, 0, 0);
  resultTime.setMinutes(snappedMinutes);

  return resultTime;
}

/**
 * 時間を指定されたスロット単位にスナップ
 */
export function snapToTimeSlot(time: Date, slotDuration: number = 30): Date {
  const snappedTime = new Date(time);
  const minutes = snappedTime.getMinutes();
  const snappedMinutes = Math.round(minutes / slotDuration) * slotDuration;

  snappedTime.setMinutes(snappedMinutes, 0, 0);

  // 60分を超えた場合は次の時間に繰り上げ
  if (snappedMinutes >= 60) {
    snappedTime.setHours(snappedTime.getHours() + 1);
    snappedTime.setMinutes(snappedMinutes - 60);
  }

  return snappedTime;
}

/**
 * 時間範囲の妥当性をチェック
 */
export function validateTimeRange(startTime: Date, endTime: Date): boolean {
  // 開始時間が終了時間より前であること
  if (startTime >= endTime) {
    return false;
  }

  // 最小時間（15分）をチェック
  const minDuration = 15 * 60 * 1000; // 15分をミリ秒で
  if (endTime.getTime() - startTime.getTime() < minDuration) {
    return false;
  }

  // 最大時間（24時間）をチェック
  const maxDuration = 24 * 60 * 60 * 1000; // 24時間をミリ秒で
  if (endTime.getTime() - startTime.getTime() > maxDuration) {
    return false;
  }

  return true;
}

/**
 * イベントのグリッド位置を計算
 */
export function calculateEventGridPosition(
  event: { startTime: Date; endTime: Date },
  viewType: ViewType,
  containerBounds: DOMRect,
  slotDuration: number = 30
): GridPosition {
  const startTime = new Date(event.startTime);
  const endTime = new Date(event.endTime);

  // 開始時間から行位置を計算
  const startHour = startTime.getHours();
  const startMinute = startTime.getMinutes();
  const startRow = Math.floor((startHour * 60 + startMinute) / slotDuration);

  // 終了時間から行スパンを計算
  const endHour = endTime.getHours();
  const endMinute = endTime.getMinutes();
  const endRow = Math.floor((endHour * 60 + endMinute) / slotDuration);
  const rowSpan = Math.max(1, endRow - startRow);

  // 列位置を計算
  let column = 0;
  if (viewType === 'week') {
    column = startTime.getDay(); // 0=日曜日, 1=月曜日, ...
  }

  return {
    row: startRow,
    column,
    rowSpan,
    columnSpan: 1
  };
}

/**
 * 時間を読みやすい形式でフォーマット
 */
export function formatTime(date: Date, format: '12h' | '24h' = '24h'): string {
  const hours = date.getHours();
  const minutes = date.getMinutes();

  if (format === '12h') {
    const period = hours >= 12 ? 'PM' : 'AM';
    const displayHours = hours === 0 ? 12 : hours > 12 ? hours - 12 : hours;
    return `${displayHours}:${minutes.toString().padStart(2, '0')} ${period}`;
  } else {
    return `${hours.toString().padStart(2, '0')}:${minutes.toString().padStart(2, '0')}`;
  }
}

/**
 * 時間範囲を読みやすい形式でフォーマット
 */
export function formatTimeRange(startTime: Date, endTime: Date, format: '12h' | '24h' = '24h'): string {
  return `${formatTime(startTime, format)} - ${formatTime(endTime, format)}`;
}

/**
 * 日付を読みやすい形式でフォーマット
 */
export function formatDate(date: Date, locale: string = 'ja-JP'): string {
  return date.toLocaleDateString(locale, {
    year: 'numeric',
    month: 'long',
    day: 'numeric',
    weekday: 'long'
  });
}

/**
 * 今日かどうかをチェック
 */
export function isToday(date: Date): boolean {
  const today = new Date();
  return (
    date.getFullYear() === today.getFullYear() &&
    date.getMonth() === today.getMonth() &&
    date.getDate() === today.getDate()
  );
}

/**
 * 同じ日かどうかをチェック
 */
export function isSameDay(date1: Date, date2: Date): boolean {
  return (
    date1.getFullYear() === date2.getFullYear() &&
    date1.getMonth() === date2.getMonth() &&
    date1.getDate() === date2.getDate()
  );
}

/**
 * 週の開始日を取得（日曜日基準）
 */
export function getWeekStart(date: Date): Date {
  const start = new Date(date);
  const dayOfWeek = start.getDay();
  start.setDate(start.getDate() - dayOfWeek);
  start.setHours(0, 0, 0, 0);
  return start;
}

/**
 * 月の開始日を取得
 */
export function getMonthStart(date: Date): Date {
  const start = new Date(date);
  start.setDate(1);
  start.setHours(0, 0, 0, 0);
  return start;
}

/**
 * ドラッグ選択の視覚的範囲を計算
 */
export function calculateDragSelectionBounds(
  startY: number,
  currentY: number,
  containerBounds: DOMRect
): { top: number; height: number } {
  const minY = Math.max(0, Math.min(startY, currentY) - containerBounds.top);
  const maxY = Math.min(containerBounds.height, Math.max(startY, currentY) - containerBounds.top);

  return {
    top: minY,
    height: maxY - minY
  };
}